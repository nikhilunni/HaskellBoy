module APU
       ( tickAPU
       , triggerChannel1
       , triggerChannel2
       , triggerChannel3
       , triggerChannel4 ) where

 import Control.Monad (when)
 import Control.Monad.ST (RealWorld)
 import Data.Word
 import Data.STRef
 import Data.Bits ((.&.), (.|.), testBit, shiftR, shiftL)

 import Types
 import Memory

 -- Frame sequencer: 512 Hz = every 8192 T-cycles
 -- Steps 0,2,4,6: length counters
 -- Steps 2,6: sweep (ch1)
 -- Step 7: envelopes (ch1,2,4)
 {-# INLINE tickAPU #-}
 tickAPU :: Cycles -> GBC ()
 tickAPU delta = do
   power <- readRef apuPower
   when power $ do
     cnt <- readRef apuFrameSeqCounter
     let newCnt = cnt + delta
     if newCnt >= 8192
       then do
         writeRef apuFrameSeqCounter (newCnt - 8192)
         step <- readRef apuFrameSeqStep
         case step of
           0 -> tickLengthCounters
           1 -> return ()
           2 -> tickLengthCounters >> tickSweep
           3 -> return ()
           4 -> tickLengthCounters
           5 -> return ()
           6 -> tickLengthCounters >> tickSweep
           7 -> tickEnvelopes
           _ -> return ()
         writeRef apuFrameSeqStep ((step + 1) .&. 7)
       else writeRef apuFrameSeqCounter newCnt

 tickLengthCounters :: GBC ()
 tickLengthCounters = do
   -- Channel 1
   nr14 <- readArr memory 0xFF14
   when (testBit nr14 6) $ do
     len <- readRef ch1LengthCounter
     when (len > 0) $ do
       let len' = len - 1
       writeRef ch1LengthCounter len'
       when (len' == 0) $ writeRef ch1Active False
   -- Channel 2
   nr24 <- readArr memory 0xFF19
   when (testBit nr24 6) $ do
     len <- readRef ch2LengthCounter
     when (len > 0) $ do
       let len' = len - 1
       writeRef ch2LengthCounter len'
       when (len' == 0) $ writeRef ch2Active False
   -- Channel 3
   nr34 <- readArr memory 0xFF1E
   when (testBit nr34 6) $ do
     len <- readRef ch3LengthCounter
     when (len > 0) $ do
       let len' = len - 1
       writeRef ch3LengthCounter len'
       when (len' == 0) $ writeRef ch3Active False
   -- Channel 4
   nr44 <- readArr memory 0xFF23
   when (testBit nr44 6) $ do
     len <- readRef ch4LengthCounter
     when (len > 0) $ do
       let len' = len - 1
       writeRef ch4LengthCounter len'
       when (len' == 0) $ writeRef ch4Active False

 tickEnvelopes :: GBC ()
 tickEnvelopes = do
   -- Channel 1
   tickEnvelope ch1Active ch1EnvelopeTimer ch1EnvelopeVolume 0xFF12
   -- Channel 2
   tickEnvelope ch2Active ch2EnvelopeTimer ch2EnvelopeVolume 0xFF17
   -- Channel 4
   tickEnvelope ch4Active ch4EnvelopeTimer ch4EnvelopeVolume 0xFF21

 tickEnvelope :: (Memory RealWorld -> STRef RealWorld Bool)
              -> (Memory RealWorld -> STRef RealWorld Word8)
              -> (Memory RealWorld -> STRef RealWorld Word8)
              -> Word16 -> GBC ()
 tickEnvelope activeRef timerRef volRef nrx2Addr = do
   active <- readRef activeRef
   when active $ do
     nrx2 <- readArr memory nrx2Addr
     let period = nrx2 .&. 0x07
     when (period /= 0) $ do
       timer <- readRef timerRef
       let timer' = if timer > 0 then timer - 1 else 0
       if timer' == 0
         then do
           writeRef timerRef period
           vol <- readRef volRef
           let dir = testBit nrx2 3  -- True = increase
           if dir
             then when (vol < 15) $ writeRef volRef (vol + 1)
             else when (vol > 0)  $ writeRef volRef (vol - 1)
         else writeRef timerRef timer'

 tickSweep :: GBC ()
 tickSweep = do
   active <- readRef ch1Active
   enabled <- readRef ch1SweepEnabled
   when (active && enabled) $ do
     timer <- readRef ch1SweepTimer
     let timer' = if timer > 0 then timer - 1 else 0
     if timer' == 0
       then do
         nr10 <- readArr memory 0xFF10
         let period = (nr10 `shiftR` 4) .&. 0x07
             -- Period 0 is treated as 8
             effectivePeriod = if period == 0 then 8 else period
         writeRef ch1SweepTimer effectivePeriod
         -- Only calculate new frequency when period != 0
         when (period /= 0) $ do
           shadow <- readRef ch1SweepShadow
           let shift = nr10 .&. 0x07
               negate' = testBit nr10 3
               delta = shadow `shiftR` fromIntegral shift
               newFreq = if negate'
                           then shadow - delta
                           else shadow + delta
           if newFreq > 2047
             then writeRef ch1Active False
             else when (shift /= 0) $ do
               writeRef ch1SweepShadow newFreq
               -- Write new frequency back to NR13/NR14
               writeArr memory 0xFF13 (fromIntegral $ newFreq .&. 0xFF)
               nr14 <- readArr memory 0xFF14
               writeArr memory 0xFF14 ((nr14 .&. 0xF8) .|. fromIntegral ((newFreq `shiftR` 8) .&. 0x07))
               -- Second overflow check with new frequency
               let delta2 = newFreq `shiftR` fromIntegral shift
                   check = if negate' then newFreq - delta2 else newFreq + delta2
               when (check > 2047) $ writeRef ch1Active False
       else writeRef ch1SweepTimer timer'

 -- Extra length clocking on trigger: if length enable is set and frame sequencer
 -- step is not a length-clock step (odd step), clock the length counter once.
 extraLengthClock :: (Memory RealWorld -> STRef RealWorld Word16)
                  -> (Memory RealWorld -> STRef RealWorld Bool)
                  -> Word16  -- max length
                  -> Bool    -- is trigger also set?
                  -> GBC ()
 extraLengthClock lenRef activeRef maxLen isTrigger = do
   step <- readRef apuFrameSeqStep
   -- Extra clock happens when the next step does NOT clock length
   -- Length clocks on steps 0,2,4,6 so the current step being even means we just clocked
   -- If step is 1,3,5,7 the next step would clock length (0,2,4,6), so no extra
   -- Wait - step is what was last incremented to. If step is 0, the NEXT thing that will
   -- happen is step 0 clocking. So if step is even, we're about to clock length.
   -- Extra clock happens when the frame sequencer step is at an even value (0,2,4,6)
   -- because the length counter was just NOT clocked on the previous step.
   -- Actually: the step counter represents the NEXT step to execute.
   -- Steps 0,2,4,6 clock length. If the next step is NOT a length step (step is odd),
   -- we don't get an extra clock. If it IS (step is even), we do.
   -- Hmm, the test says "Enabling in first half should clock" - first half = even steps.
   when (odd step) $ do
     len <- readRef lenRef
     when (len > 0) $ do
       let len' = len - 1
       writeRef lenRef len'
       -- If trigger is set and length becomes 0, reload to max - 1
       when (len' == 0) $
         if isTrigger
           then writeRef lenRef (maxLen - 1)
           else writeRef activeRef False

 triggerChannel1 :: GBC ()
 triggerChannel1 = do
   writeRef ch1Active True
   -- Reload length counter if zero
   len <- readRef ch1LengthCounter
   when (len == 0) $ writeRef ch1LengthCounter 64
   -- Extra length clocking if length enable is set
   nr14 <- readArr memory 0xFF14
   when (testBit nr14 6) $
     extraLengthClock ch1LengthCounter ch1Active 64 True
   -- Restart envelope
   nr12 <- readArr memory 0xFF12
   writeRef ch1EnvelopeVolume (nr12 `shiftR` 4)
   writeRef ch1EnvelopeTimer (nr12 .&. 0x07)
   -- Restart sweep
   nr10 <- readArr memory 0xFF10
   let sweepPeriod = (nr10 `shiftR` 4) .&. 0x07
       sweepShift = nr10 .&. 0x07
   -- Load shadow frequency from NR13/NR14
   nr13 <- readArr memory 0xFF13
   let freq = fromIntegral nr13 .|. ((fromIntegral (nr14 .&. 0x07)) `shiftL` 8) :: Word16
   writeRef ch1SweepShadow freq
   writeRef ch1SweepTimer (if sweepPeriod == 0 then 8 else sweepPeriod)
   writeRef ch1SweepEnabled (sweepPeriod /= 0 || sweepShift /= 0)
   -- Overflow check on trigger if shift != 0
   when (sweepShift /= 0) $ do
     let negate' = testBit nr10 3
         delta = freq `shiftR` fromIntegral sweepShift
         newFreq = if negate' then freq - delta else freq + delta
     when (newFreq > 2047) $ writeRef ch1Active False
   -- DAC check: if top 5 bits of NR12 are 0, channel is off
   when (nr12 .&. 0xF8 == 0) $ writeRef ch1Active False

 triggerChannel2 :: GBC ()
 triggerChannel2 = do
   writeRef ch2Active True
   len <- readRef ch2LengthCounter
   when (len == 0) $ writeRef ch2LengthCounter 64
   nr24 <- readArr memory 0xFF19
   when (testBit nr24 6) $
     extraLengthClock ch2LengthCounter ch2Active 64 True
   nr22 <- readArr memory 0xFF17
   writeRef ch2EnvelopeVolume (nr22 `shiftR` 4)
   writeRef ch2EnvelopeTimer (nr22 .&. 0x07)
   when (nr22 .&. 0xF8 == 0) $ writeRef ch2Active False

 triggerChannel3 :: GBC ()
 triggerChannel3 = do
   writeRef ch3Active True
   len <- readRef ch3LengthCounter
   when (len == 0) $ writeRef ch3LengthCounter 256
   nr34 <- readArr memory 0xFF1E
   when (testBit nr34 6) $
     extraLengthClock ch3LengthCounter ch3Active 256 True
   -- DAC check: NR30 bit 7
   nr30 <- readArr memory 0xFF1A
   when (not $ testBit nr30 7) $ writeRef ch3Active False

 triggerChannel4 :: GBC ()
 triggerChannel4 = do
   writeRef ch4Active True
   len <- readRef ch4LengthCounter
   when (len == 0) $ writeRef ch4LengthCounter 64
   nr44 <- readArr memory 0xFF23
   when (testBit nr44 6) $
     extraLengthClock ch4LengthCounter ch4Active 64 True
   nr42 <- readArr memory 0xFF21
   writeRef ch4EnvelopeVolume (nr42 `shiftR` 4)
   writeRef ch4EnvelopeTimer (nr42 .&. 0x07)
   when (nr42 .&. 0xF8 == 0) $ writeRef ch4Active False
