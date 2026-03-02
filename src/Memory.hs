{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Memory
 where

 import Control.Monad.ST

 import Data.Word
 import Data.Int (Int16)
 import Data.Array.ST
 import Data.STRef
 import Data.Bits ((.&.), (.|.), setBit, shiftL, shiftR)

 import Effectful (Eff, (:>))
 import Effectful.Dispatch.Static (getStaticRep, unsafeEff_)

 import Types
 import Effects.Env (EmulatorEnv, StaticRep(EmulatorEnv))

 new :: Bool -> Word8 -> ST s (Memory s)
 new gbcMode numRomBanks = do
   memory' <- newArray_ (0x0000, 0xFFFF)
   bgPalettes' <- newArray_ (0x0, 0x3F)
   registers' <- newArray (0x0, 0x7) 0
   romBanks' <- newArray_ ((1,0x4000), (numRomBanks-1, 0x7FFF))
   vramBank' <- newArray_ (0x8000, 0x9FFF)
   wramBanks' <- newArray_ ((1,0xD000), (7,0xDFFF))
   sp' <-  newSTRef 0xFFFE
   pc' <-  newSTRef 0x0000
   cycles' <- newSTRef 0
   ime' <- newSTRef False
   imePending' <- newSTRef False
   halt' <- newSTRef False
   mode' <- newSTRef OAMSearch
   line' <- newSTRef 0
   transferred' <- newSTRef False
   gpu_cycles' <- newSTRef 0
   gbc_mode' <- newSTRef gbcMode
   bootRomActive' <- newSTRef False
   cartFirstPage' <- newArray (0x0000, 0x00FF) 0
   romBank' <- newSTRef 1
   romBankMask' <- newSTRef 0x1F
   framebuffer' <- newArray (0, 160*144*3 - 1) 0
   serialBuffer' <- newSTRef []
   timerCounter' <- newSTRef 0
   divCounter' <- newSTRef 0
   haltBug' <- newSTRef False
   numRomBanks' <- newSTRef numRomBanks
   branchCycles' <- newSTRef 0
   mCycleCounter' <- newSTRef 0
   apuPower' <- newSTRef False
   apuFrameSeqStep' <- newSTRef 0
   apuFrameSeqCounter' <- newSTRef 0
   ch1Active' <- newSTRef False
   ch1LengthCounter' <- newSTRef 0
   ch1EnvelopeTimer' <- newSTRef 0
   ch1EnvelopeVolume' <- newSTRef 0
   ch1SweepTimer' <- newSTRef 0
   ch1SweepEnabled' <- newSTRef False
   ch1SweepShadow' <- newSTRef 0
   ch2Active' <- newSTRef False
   ch2LengthCounter' <- newSTRef 0
   ch2EnvelopeTimer' <- newSTRef 0
   ch2EnvelopeVolume' <- newSTRef 0
   ch3Active' <- newSTRef False
   ch3LengthCounter' <- newSTRef 0
   ch4Active' <- newSTRef False
   ch4LengthCounter' <- newSTRef 0
   ch4EnvelopeTimer' <- newSTRef 0
   ch4EnvelopeVolume' <- newSTRef 0
   joypadButtons' <- newSTRef 0xFF
   joypadDirPad' <- newSTRef 0xFF
   ch1FreqTimer' <- newSTRef 0
   ch1DutyPos' <- newSTRef 0
   ch2FreqTimer' <- newSTRef 0
   ch2DutyPos' <- newSTRef 0
   ch3FreqTimer' <- newSTRef 0
   ch3WavePos' <- newSTRef 0
   ch4FreqTimer' <- newSTRef 0
   ch4LFSR' <- newSTRef 0x7FFF
   audioBuffer' <- newArray (0, 4095) 0
   audioBufPos' <- newSTRef 0
   audioSampleAccum' <- newSTRef 0
   return Memory { memory = memory'
                 , bgPalettes = bgPalettes'
                 , registers = registers'
                 , romBanks = romBanks'
                 , vramBank = vramBank'
                 , wramBanks = wramBanks'
                 , sp = sp'
                 , pc = pc'
                 , cycles = cycles'
                 , ime = ime'
                 , imePending = imePending'
                 , halt = halt'
                 , mode = mode'
                 , line = line'
                 , transferred = transferred'
                 , gpu_cycles = gpu_cycles'
                 , gbc_mode = gbc_mode'
                 , bootRomActive = bootRomActive'
                 , cartFirstPage = cartFirstPage'
                 , romBank = romBank'
                 , romBankMask = romBankMask'
                 , framebuffer = framebuffer'
                 , serialBuffer = serialBuffer'
                 , timerCounter = timerCounter'
                 , divCounter = divCounter'
                 , haltBug = haltBug'
                 , numRomBanks = numRomBanks'
                 , branchCycles = branchCycles'
                 , mCycleCounter = mCycleCounter'
                 , apuPower = apuPower'
                 , apuFrameSeqStep = apuFrameSeqStep'
                 , apuFrameSeqCounter = apuFrameSeqCounter'
                 , ch1Active = ch1Active'
                 , ch1LengthCounter = ch1LengthCounter'
                 , ch1EnvelopeTimer = ch1EnvelopeTimer'
                 , ch1EnvelopeVolume = ch1EnvelopeVolume'
                 , ch1SweepTimer = ch1SweepTimer'
                 , ch1SweepEnabled = ch1SweepEnabled'
                 , ch1SweepShadow = ch1SweepShadow'
                 , ch2Active = ch2Active'
                 , ch2LengthCounter = ch2LengthCounter'
                 , ch2EnvelopeTimer = ch2EnvelopeTimer'
                 , ch2EnvelopeVolume = ch2EnvelopeVolume'
                 , ch3Active = ch3Active'
                 , ch3LengthCounter = ch3LengthCounter'
                 , ch4Active = ch4Active'
                 , ch4LengthCounter = ch4LengthCounter'
                 , ch4EnvelopeTimer = ch4EnvelopeTimer'
                 , ch4EnvelopeVolume = ch4EnvelopeVolume'
                 , joypadButtons = joypadButtons'
                 , joypadDirPad = joypadDirPad'
                 , ch1FreqTimer = ch1FreqTimer'
                 , ch1DutyPos = ch1DutyPos'
                 , ch2FreqTimer = ch2FreqTimer'
                 , ch2DutyPos = ch2DutyPos'
                 , ch3FreqTimer = ch3FreqTimer'
                 , ch3WavePos = ch3WavePos'
                 , ch4FreqTimer = ch4FreqTimer'
                 , ch4LFSR = ch4LFSR'
                 , audioBuffer = audioBuffer'
                 , audioBufPos = audioBufPos'
                 , audioSampleAccum = audioSampleAccum'
                 }

 {-# INLINE readArr #-}
 readArr :: (EmulatorEnv :> es, Ix i) => (Memory RealWorld -> STUArray RealWorld i Word8) -> i -> Eff es Word8
 readArr accessor i = do
   EmulatorEnv mem <- getStaticRep
   unsafeEff_ $ stToIO $ readArray (accessor mem) i

 {-# INLINE writeArr #-}
 writeArr :: (EmulatorEnv :> es, Ix i) => (Memory RealWorld -> STUArray RealWorld i Word8) -> i -> Word8 -> Eff es ()
 writeArr accessor i val = do
   EmulatorEnv mem <- getStaticRep
   unsafeEff_ $ stToIO $ writeArray (accessor mem) i val

 {-# INLINE readArrI16 #-}
 readArrI16 :: (EmulatorEnv :> es, Ix i) => (Memory RealWorld -> STUArray RealWorld i Int16) -> i -> Eff es Int16
 readArrI16 accessor i = do
   EmulatorEnv mem <- getStaticRep
   unsafeEff_ $ stToIO $ readArray (accessor mem) i

 {-# INLINE writeArrI16 #-}
 writeArrI16 :: (EmulatorEnv :> es, Ix i) => (Memory RealWorld -> STUArray RealWorld i Int16) -> i -> Int16 -> Eff es ()
 writeArrI16 accessor i val = do
   EmulatorEnv mem <- getStaticRep
   unsafeEff_ $ stToIO $ writeArray (accessor mem) i val

 {-# INLINE readRef #-}
 readRef :: EmulatorEnv :> es => (Memory RealWorld -> STRef RealWorld e) -> Eff es e
 readRef accessor = do
   EmulatorEnv mem <- getStaticRep
   unsafeEff_ $ stToIO $ readSTRef (accessor mem)

 {-# INLINE writeRef #-}
 writeRef :: EmulatorEnv :> es => (Memory RealWorld -> STRef RealWorld e) -> e -> Eff es ()
 writeRef accessor val = do
   EmulatorEnv mem <- getStaticRep
   unsafeEff_ $ stToIO $ writeSTRef (accessor mem) val

 readReg :: EmulatorEnv :> es => Register -> Eff es Word8
 readReg reg = readArr registers $ eNum reg

 writeReg :: EmulatorEnv :> es => Register -> Word8 -> Eff es ()
 writeReg F val = writeArr registers (eNum F) (val .&. 0xF0)
 writeReg reg val = writeArr registers (eNum reg) val

 --TODO : Is there a better way to constrain the pairings?
 readReg2 :: EmulatorEnv :> es => Register -> Register -> Eff es Word16
 readReg2 A F = readR A F
 readReg2 B C = readR B C
 readReg2 D E = readR D E
 readReg2 H L = readR H L

 readR :: EmulatorEnv :> es => Register -> Register -> Eff es Word16
 readR reg1 reg2 = do
   a <- readArr registers (eNum reg1)
   b <- readArr registers (eNum reg2)
   return $ ((fromIntegral a) `shiftL` 8) .|. (fromIntegral b)

 writeReg2 :: EmulatorEnv :> es => Register -> Register -> Word16 -> Eff es ()
 writeReg2 A F = writeR A F
 writeReg2 B C = writeR B C
 writeReg2 D E = writeR D E
 writeReg2 H L = writeR H L

 writeR :: EmulatorEnv :> es => Register -> Register -> Word16 -> Eff es ()
 writeR reg1 reg2 val = do
   writeReg reg1 (fromIntegral $ val `shiftR` 8)
   writeReg reg2 (fromIntegral $ val .&. 0xFF)

 requestInterrupt :: EmulatorEnv :> es => Interrupt -> Eff es ()
 requestInterrupt interrupt = do
   flagIF <- readArr memory 0xFF0F
   writeArr memory 0xFF0F (setBit flagIF (fromEnum interrupt))

 boot_rom :: [Word8]
 boot_rom = [0x31, 0xfe, 0xff, 0xaf, 0x21, 0xff, 0x9f, 0x32, 0xcb, 0x7c, 0x20, 0xfb,
             0x21, 0x26, 0xff, 0x0e, 0x11, 0x3e, 0x80, 0x32, 0xe2, 0x0c, 0x3e, 0xf3,
             0xe2, 0x32, 0x3e, 0x77, 0x77, 0x3e, 0xfc, 0xe0, 0x47, 0x11, 0x04, 0x01,
             0x21, 0x10, 0x80, 0x1a, 0xcd, 0x95, 0x00, 0xcd, 0x96, 0x00, 0x13, 0x7b,
             0xfe, 0x34, 0x20, 0xf3, 0x11, 0xd8, 0x00, 0x06, 0x08, 0x1a, 0x13, 0x22,
             0x23, 0x05, 0x20, 0xf9, 0x3e, 0x19, 0xea, 0x10, 0x99, 0x21, 0x2f, 0x99,
             0x0e, 0x0c, 0x3d, 0x28, 0x08, 0x32, 0x0d, 0x20, 0xf9, 0x2e, 0x0f, 0x18,
             0xf3, 0x67, 0x3e, 0x64, 0x57, 0xe0, 0x42, 0x3e, 0x91, 0xe0, 0x40, 0x04,
             0x1e, 0x02, 0x0e, 0x0c, 0xf0, 0x44, 0xfe, 0x90, 0x20, 0xfa, 0x0d, 0x20,
             0xf7, 0x1d, 0x20, 0xf2, 0x0e, 0x13, 0x24, 0x7c, 0x1e, 0x83, 0xfe, 0x62,
             0x28, 0x06, 0x1e, 0xc1, 0xfe, 0x64, 0x20, 0x06, 0x7b, 0xe2, 0x0c, 0x3e,
             0x87, 0xe2, 0xf0, 0x42, 0x90, 0xe0, 0x42, 0x15, 0x20, 0xd2, 0x05, 0x20,
             0x4f, 0x16, 0x20, 0x18, 0xcb, 0x4f, 0x06, 0x04, 0xc5, 0xcb, 0x11, 0x17,
             0xc1, 0xcb, 0x11, 0x17, 0x05, 0x20, 0xf5, 0x22, 0x23, 0x22, 0x23, 0xc9,
             0xce, 0xed, 0x66, 0x66, 0xcc, 0x0d, 0x00, 0x0b, 0x03, 0x73, 0x00, 0x83,
             0x00, 0x0c, 0x00, 0x0d, 0x00, 0x08, 0x11, 0x1f, 0x88, 0x89, 0x00, 0x0e,
             0xdc, 0xcc, 0x6e, 0xe6, 0xdd, 0xdd, 0xd9, 0x99, 0xbb, 0xbb, 0x67, 0x63,
             0x6e, 0x0e, 0xec, 0xcc, 0xdd, 0xdc, 0x99, 0x9f, 0xbb, 0xb9, 0x33, 0x3e,
             0x3c, 0x42, 0xb9, 0xa5, 0xb9, 0xa5, 0x42, 0x3c, 0x21, 0x04, 0x01, 0x11,
             0xa8, 0x00, 0x1a, 0x13, 0xbe, 0x20, 0xfe, 0x23, 0x7d, 0xfe, 0x34, 0x20,
             0xf5, 0x06, 0x19, 0x78, 0x86, 0x23, 0x05, 0x20, 0xfb, 0x86, 0x00, 0x00,
             0x3e, 0x01, 0xe0, 0x50]

 newForTest :: IO (Memory RealWorld)
 newForTest = stToIO $ do
   memory' <- newArray (0x0000, 0xFFFF) 0
   bgPalettes' <- newArray (0x0, 0x3F) 0
   registers' <- newArray (0x0, 0x7) 0
   romBanks' <- newArray ((1,0x4000), (1, 0x7FFF)) 0
   vramBank' <- newArray (0x8000, 0x9FFF) 0
   wramBanks' <- newArray ((1,0xD000), (7,0xDFFF)) 0
   sp' <- newSTRef 0x0000
   pc' <- newSTRef 0x0000
   cycles' <- newSTRef 0
   ime' <- newSTRef False
   imePending' <- newSTRef False
   halt' <- newSTRef False
   mode' <- newSTRef OAMSearch
   line' <- newSTRef 0
   transferred' <- newSTRef False
   gpu_cycles' <- newSTRef 0
   gbc_mode' <- newSTRef False
   bootRomActive' <- newSTRef False
   cartFirstPage' <- newArray (0x0000, 0x00FF) 0
   romBank' <- newSTRef 1
   romBankMask' <- newSTRef 0x1F
   framebuffer' <- newArray (0, 160*144*3 - 1) 0
   serialBuffer' <- newSTRef []
   timerCounter' <- newSTRef 0
   divCounter' <- newSTRef 0
   haltBug' <- newSTRef False
   numRomBanks' <- newSTRef 2
   branchCycles' <- newSTRef 0
   mCycleCounter' <- newSTRef 0
   apuPower' <- newSTRef False
   apuFrameSeqStep' <- newSTRef 0
   apuFrameSeqCounter' <- newSTRef 0
   ch1Active' <- newSTRef False
   ch1LengthCounter' <- newSTRef 0
   ch1EnvelopeTimer' <- newSTRef 0
   ch1EnvelopeVolume' <- newSTRef 0
   ch1SweepTimer' <- newSTRef 0
   ch1SweepEnabled' <- newSTRef False
   ch1SweepShadow' <- newSTRef 0
   ch2Active' <- newSTRef False
   ch2LengthCounter' <- newSTRef 0
   ch2EnvelopeTimer' <- newSTRef 0
   ch2EnvelopeVolume' <- newSTRef 0
   ch3Active' <- newSTRef False
   ch3LengthCounter' <- newSTRef 0
   ch4Active' <- newSTRef False
   ch4LengthCounter' <- newSTRef 0
   ch4EnvelopeTimer' <- newSTRef 0
   ch4EnvelopeVolume' <- newSTRef 0
   joypadButtons' <- newSTRef 0xFF
   joypadDirPad' <- newSTRef 0xFF
   ch1FreqTimer' <- newSTRef 0
   ch1DutyPos' <- newSTRef 0
   ch2FreqTimer' <- newSTRef 0
   ch2DutyPos' <- newSTRef 0
   ch3FreqTimer' <- newSTRef 0
   ch3WavePos' <- newSTRef 0
   ch4FreqTimer' <- newSTRef 0
   ch4LFSR' <- newSTRef 0x7FFF
   audioBuffer' <- newArray (0, 4095) 0
   audioBufPos' <- newSTRef 0
   audioSampleAccum' <- newSTRef 0
   return Memory { memory = memory'
                 , bgPalettes = bgPalettes'
                 , registers = registers'
                 , romBanks = romBanks'
                 , vramBank = vramBank'
                 , wramBanks = wramBanks'
                 , sp = sp'
                 , pc = pc'
                 , cycles = cycles'
                 , ime = ime'
                 , imePending = imePending'
                 , halt = halt'
                 , mode = mode'
                 , line = line'
                 , transferred = transferred'
                 , gpu_cycles = gpu_cycles'
                 , gbc_mode = gbc_mode'
                 , bootRomActive = bootRomActive'
                 , cartFirstPage = cartFirstPage'
                 , romBank = romBank'
                 , romBankMask = romBankMask'
                 , framebuffer = framebuffer'
                 , serialBuffer = serialBuffer'
                 , timerCounter = timerCounter'
                 , divCounter = divCounter'
                 , haltBug = haltBug'
                 , numRomBanks = numRomBanks'
                 , branchCycles = branchCycles'
                 , mCycleCounter = mCycleCounter'
                 , apuPower = apuPower'
                 , apuFrameSeqStep = apuFrameSeqStep'
                 , apuFrameSeqCounter = apuFrameSeqCounter'
                 , ch1Active = ch1Active'
                 , ch1LengthCounter = ch1LengthCounter'
                 , ch1EnvelopeTimer = ch1EnvelopeTimer'
                 , ch1EnvelopeVolume = ch1EnvelopeVolume'
                 , ch1SweepTimer = ch1SweepTimer'
                 , ch1SweepEnabled = ch1SweepEnabled'
                 , ch1SweepShadow = ch1SweepShadow'
                 , ch2Active = ch2Active'
                 , ch2LengthCounter = ch2LengthCounter'
                 , ch2EnvelopeTimer = ch2EnvelopeTimer'
                 , ch2EnvelopeVolume = ch2EnvelopeVolume'
                 , ch3Active = ch3Active'
                 , ch3LengthCounter = ch3LengthCounter'
                 , ch4Active = ch4Active'
                 , ch4LengthCounter = ch4LengthCounter'
                 , ch4EnvelopeTimer = ch4EnvelopeTimer'
                 , ch4EnvelopeVolume = ch4EnvelopeVolume'
                 , joypadButtons = joypadButtons'
                 , joypadDirPad = joypadDirPad'
                 , ch1FreqTimer = ch1FreqTimer'
                 , ch1DutyPos = ch1DutyPos'
                 , ch2FreqTimer = ch2FreqTimer'
                 , ch2DutyPos = ch2DutyPos'
                 , ch3FreqTimer = ch3FreqTimer'
                 , ch3WavePos = ch3WavePos'
                 , ch4FreqTimer = ch4FreqTimer'
                 , ch4LFSR = ch4LFSR'
                 , audioBuffer = audioBuffer'
                 , audioBufPos = audioBufPos'
                 , audioSampleAccum = audioSampleAccum'
                 }

 newHeadless :: Word8 -> IO (Memory RealWorld)
 newHeadless numRomBanks = stToIO $ do
   memory' <- newArray_ (0x0000, 0xFFFF)
   bgPalettes' <- newArray_ (0x0, 0x3F)
   registers' <- newArray (0x0, 0x7) 0
   romBanks' <- newArray_ ((1,0x4000), (numRomBanks-1, 0x7FFF))
   vramBank' <- newArray_ (0x8000, 0x9FFF)
   wramBanks' <- newArray_ ((1,0xD000), (7,0xDFFF))
   sp' <- newSTRef 0xFFFE
   pc' <- newSTRef 0x0000
   cycles' <- newSTRef 0
   ime' <- newSTRef False
   imePending' <- newSTRef False
   halt' <- newSTRef False
   mode' <- newSTRef OAMSearch
   line' <- newSTRef 0
   transferred' <- newSTRef False
   gpu_cycles' <- newSTRef 0
   gbc_mode' <- newSTRef False
   bootRomActive' <- newSTRef False
   cartFirstPage' <- newArray (0x0000, 0x00FF) 0
   romBank' <- newSTRef 1
   romBankMask' <- newSTRef 0x1F
   framebuffer' <- newArray (0, 160*144*3 - 1) 0
   serialBuffer' <- newSTRef []
   timerCounter' <- newSTRef 0
   divCounter' <- newSTRef 0
   haltBug' <- newSTRef False
   numRomBanks' <- newSTRef numRomBanks
   branchCycles' <- newSTRef 0
   mCycleCounter' <- newSTRef 0
   apuPower' <- newSTRef False
   apuFrameSeqStep' <- newSTRef 0
   apuFrameSeqCounter' <- newSTRef 0
   ch1Active' <- newSTRef False
   ch1LengthCounter' <- newSTRef 0
   ch1EnvelopeTimer' <- newSTRef 0
   ch1EnvelopeVolume' <- newSTRef 0
   ch1SweepTimer' <- newSTRef 0
   ch1SweepEnabled' <- newSTRef False
   ch1SweepShadow' <- newSTRef 0
   ch2Active' <- newSTRef False
   ch2LengthCounter' <- newSTRef 0
   ch2EnvelopeTimer' <- newSTRef 0
   ch2EnvelopeVolume' <- newSTRef 0
   ch3Active' <- newSTRef False
   ch3LengthCounter' <- newSTRef 0
   ch4Active' <- newSTRef False
   ch4LengthCounter' <- newSTRef 0
   ch4EnvelopeTimer' <- newSTRef 0
   ch4EnvelopeVolume' <- newSTRef 0
   joypadButtons' <- newSTRef 0xFF
   joypadDirPad' <- newSTRef 0xFF
   ch1FreqTimer' <- newSTRef 0
   ch1DutyPos' <- newSTRef 0
   ch2FreqTimer' <- newSTRef 0
   ch2DutyPos' <- newSTRef 0
   ch3FreqTimer' <- newSTRef 0
   ch3WavePos' <- newSTRef 0
   ch4FreqTimer' <- newSTRef 0
   ch4LFSR' <- newSTRef 0x7FFF
   audioBuffer' <- newArray (0, 4095) 0
   audioBufPos' <- newSTRef 0
   audioSampleAccum' <- newSTRef 0
   return Memory { memory = memory'
                 , bgPalettes = bgPalettes'
                 , registers = registers'
                 , romBanks = romBanks'
                 , vramBank = vramBank'
                 , wramBanks = wramBanks'
                 , sp = sp'
                 , pc = pc'
                 , cycles = cycles'
                 , ime = ime'
                 , imePending = imePending'
                 , halt = halt'
                 , mode = mode'
                 , line = line'
                 , transferred = transferred'
                 , gpu_cycles = gpu_cycles'
                 , gbc_mode = gbc_mode'
                 , bootRomActive = bootRomActive'
                 , cartFirstPage = cartFirstPage'
                 , romBank = romBank'
                 , romBankMask = romBankMask'
                 , framebuffer = framebuffer'
                 , serialBuffer = serialBuffer'
                 , timerCounter = timerCounter'
                 , divCounter = divCounter'
                 , haltBug = haltBug'
                 , numRomBanks = numRomBanks'
                 , branchCycles = branchCycles'
                 , mCycleCounter = mCycleCounter'
                 , apuPower = apuPower'
                 , apuFrameSeqStep = apuFrameSeqStep'
                 , apuFrameSeqCounter = apuFrameSeqCounter'
                 , ch1Active = ch1Active'
                 , ch1LengthCounter = ch1LengthCounter'
                 , ch1EnvelopeTimer = ch1EnvelopeTimer'
                 , ch1EnvelopeVolume = ch1EnvelopeVolume'
                 , ch1SweepTimer = ch1SweepTimer'
                 , ch1SweepEnabled = ch1SweepEnabled'
                 , ch1SweepShadow = ch1SweepShadow'
                 , ch2Active = ch2Active'
                 , ch2LengthCounter = ch2LengthCounter'
                 , ch2EnvelopeTimer = ch2EnvelopeTimer'
                 , ch2EnvelopeVolume = ch2EnvelopeVolume'
                 , ch3Active = ch3Active'
                 , ch3LengthCounter = ch3LengthCounter'
                 , ch4Active = ch4Active'
                 , ch4LengthCounter = ch4LengthCounter'
                 , ch4EnvelopeTimer = ch4EnvelopeTimer'
                 , ch4EnvelopeVolume = ch4EnvelopeVolume'
                 , joypadButtons = joypadButtons'
                 , joypadDirPad = joypadDirPad'
                 , ch1FreqTimer = ch1FreqTimer'
                 , ch1DutyPos = ch1DutyPos'
                 , ch2FreqTimer = ch2FreqTimer'
                 , ch2DutyPos = ch2DutyPos'
                 , ch3FreqTimer = ch3FreqTimer'
                 , ch3WavePos = ch3WavePos'
                 , ch4FreqTimer = ch4FreqTimer'
                 , ch4LFSR = ch4LFSR'
                 , audioBuffer = audioBuffer'
                 , audioBufPos = audioBufPos'
                 , audioSampleAccum = audioSampleAccum'
                 }
