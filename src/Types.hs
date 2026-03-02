{-# LANGUAGE BangPatterns #-}

module Types
 where

 import Data.Word
 import Data.Int (Int16)
 import Data.Array.ST
 import Data.STRef

 data Register = A
               | F
               | B
               | C
               | D
               | E
               | H
               | L
               deriving (Show, Enum)

 eNum :: (Enum a, Num c) => a -> c
 eNum = fromIntegral . fromEnum

 data GPUMode = HBlankMode | VBlankMode | OAMSearch | Transfer deriving (Eq, Enum, Show)

 data Interrupt = VBlank | STAT | Timer | Serial | Joypad deriving (Show, Enum)

 data Memory s = Memory { memory :: STUArray s Word16 Word8
                        , bgPalettes :: STUArray s Word8 Word8
                        , registers :: STUArray s Word8 Word8
                        , romBanks  :: STUArray s (Word8,Word16) Word8
                        , vramBank :: STUArray s Word16 Word8
                        , wramBanks :: STUArray s (Word8,Word16) Word8
                        , sp :: STRef s Word16
                        , pc :: STRef s Word16
                        , cycles :: STRef s Word16
                        , ime :: STRef s Bool --Interrupt Master Enable Flag
                        , imePending :: STRef s Bool --EI delayed enable
                        , halt :: STRef s Bool --Are we halted or not
                        , mode :: STRef s GPUMode -- GPU mode
                        , line :: STRef s Word8 -- GPU line
                        , transferred :: STRef s Bool --Have we done GPU scanline?
                        , gpu_cycles :: STRef s Word16 -- Internal cycle count for GPU
                        , gbc_mode :: STRef s Bool --Are we in GBC mode?
                        , bootRomActive :: STRef s Bool --Is boot ROM overlaid?
                        , cartFirstPage :: STUArray s Word16 Word8 --Saved cartridge 0x00-0xFF
                        , romBank :: STRef s Word8 --Current switchable ROM bank
                        , romBankMask :: STRef s Word8 --Bank register mask (MBC-dependent)
                        , framebuffer :: STUArray s Int Word8
                        , serialBuffer :: STRef s String --Accumulated serial output
                        , timerCounter :: STRef s Word16 --Internal timer cycle counter
                        , divCounter :: STRef s Word16 --Internal DIV cycle counter
                        , haltBug :: STRef s Bool --HALT bug: repeat next byte
                        , numRomBanks :: STRef s Word8 --Total ROM bank count
                        , branchCycles :: STRef s Word16 --Extra cycles for taken branches
                        , mCycleCounter :: STRef s Word64 --Total M-cycle counter
                        -- APU state
                        , apuPower :: STRef s Bool
                        , apuFrameSeqStep :: STRef s Word8    -- 0-7
                        , apuFrameSeqCounter :: STRef s Word16 -- T-cycle counter for 512 Hz
                        -- Channel 1 (pulse + sweep)
                        , ch1Active :: STRef s Bool
                        , ch1LengthCounter :: STRef s Word16
                        , ch1EnvelopeTimer :: STRef s Word8
                        , ch1EnvelopeVolume :: STRef s Word8
                        , ch1SweepTimer :: STRef s Word8
                        , ch1SweepEnabled :: STRef s Bool
                        , ch1SweepShadow :: STRef s Word16
                        -- Channel 2 (pulse)
                        , ch2Active :: STRef s Bool
                        , ch2LengthCounter :: STRef s Word16
                        , ch2EnvelopeTimer :: STRef s Word8
                        , ch2EnvelopeVolume :: STRef s Word8
                        -- Channel 3 (wave)
                        , ch3Active :: STRef s Bool
                        , ch3LengthCounter :: STRef s Word16
                        -- Channel 4 (noise)
                        , ch4Active :: STRef s Bool
                        , ch4LengthCounter :: STRef s Word16
                        , ch4EnvelopeTimer :: STRef s Word8
                        , ch4EnvelopeVolume :: STRef s Word8
                        -- Joypad state
                        , joypadButtons :: STRef s Word8  -- A/B/Select/Start (active-low)
                        , joypadDirPad :: STRef s Word8   -- Up/Down/Left/Right (active-low)
                        -- Channel waveform state
                        , ch1FreqTimer :: STRef s Int
                        , ch1DutyPos :: STRef s Word8
                        , ch2FreqTimer :: STRef s Int
                        , ch2DutyPos :: STRef s Word8
                        , ch3FreqTimer :: STRef s Int
                        , ch3WavePos :: STRef s Word8
                        , ch4FreqTimer :: STRef s Int
                        , ch4LFSR :: STRef s Word16
                        -- Audio output
                        , audioBuffer :: STUArray s Int Int16
                        , audioBufPos :: STRef s Int
                        , audioSampleAccum :: STRef s Int
                        }

 data CartMode = DMGMode | GBCMode

 data MBCType = MBC0
              | MBC1
              | MBC2
              | MBC3
              | MBC4
              | MBC5
              | HuC1

 data Color = Color !Word8 !Word8 !Word8 !Word8 deriving Show --A/R/G/B
 data Pixel = Pixel Color Integer Integer deriving Show -- Color/X/Y
 type Palette = (Color, Color, Color, Color) --Color 0,1,2,3
 data GBColor = Col0 | Col1 | Col2 | Col3 deriving (Eq)

 white :: Color
 white      = Color 0xFF 0xFF 0xFF 0xFF

 light_gray :: Color
 light_gray = Color 0xFF 0xE0 0xE0 0xE0

 dark_gray :: Color
 dark_gray  = Color 0xFF 0x66 0x66 0x66

 black :: Color
 black      = Color 0xFF 0x00 0x00 0x00

 type Opcode = Word8
 type Cycles = Word16
 type LineNum = Word8

 fst3 :: (a, b, c) -> a
 fst3 (a,_,_) = a

 snd3 :: (a, b, c) -> b
 snd3 (_,b,_) = b

 thd3 :: (a, b, c) -> c
 thd3 (_,_,c) = c

 lift8 :: Functor f => f Word8 -> f Word16
 lift8 = fmap ((+0xFF00) . fromIntegral)
