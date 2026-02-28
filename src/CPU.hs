module CPU where

 import Data.Word
 import Data.Int (Int8)
 import Data.Bits ( (.&.) , (.|.), clearBit, setBit, bit, xor,
                    shiftL, shiftR, rotateR, rotateL, Bits, complement,
                    testBit)

 import Control.Conditional hiding (when)
 import Control.Monad

 import Types
 import Memory
 import MemoryRules
 import Monad

 data Instruction =
   {-See : http://marc.rawer.de/Gameboy/Docs/GBCPUman.pdf,
         http://www.chrisantonellis.com/files/gameboy/gb-programming-manual.pdf -}
 --8-bit loads
      LDrn Register Word8             --Load immediate into register
    | LDrr Register Register          --Load register into register
    | LDrHL Register                  --Load (HL) into register
    | LDrBC Register                  --Load (BC) into register
    | LDrDE Register                  --Load (DE) into register
    | LDrnn Register Word16           --Load (immediate) into register
    | LDHLr Register                  --Load register into (HL)
    | LDBCr Register                  --Load register into (BC)
    | LDDEr Register                  --Load register into (DE)
    | LDHLn Word8                     --Load immediate into (HL)
    | LDnnr Word16 Register           --Load register into (immediate)
    | LDrr' Register Register         --Load (FF00 + register) into register
    | LDrr'' Register Register        --Load register into (FF00 + register)
    | LDnr Word8 Register             --Load register into (FF00 + immediate)
    | LDrn' Register Word8            --Load (FF00 + immediate) into register
 --16-bit loads
    | LDrrnn Register Register Word16 --Load immediate into [register,register]
    | LDSPnn Word16                   --Load immediate into SP
    | LDSPHL                          --Load HL into SP
    | LDHLSPn Word8                   --Load SP+n into HL
    | LDnnSP Word16                   --Load SP into (nn)
    | PUSHrr Register Register        --Push [register,register] onto Stack, decrement SP twice
    | POPrr Register Register         --Pop two bytes off stack into (Register,Register), increment SP twice
 --8-bit ALU
    | ADDr Register                   --Add register to A
    | ADDn Word8                      --Add immediate to A
    | ADDHL                           --Add (HL) to A
    | ADCr Register                   --Add register+carry flag to A
    | ADCn Word8                      --Add immediate+carry flag to A
    | ADCHL                           --Add (HL)+carry flag to A
    | SUBr Register                   --Sub register from A
    | SUBn Word8                      --Sub immediate from A
    | SUBHL                           --Sub (HL) from A
    | SBCr Register                   --Sub register+carry flag from A
    | SBCn Word8                      --Sub immediate+carry flag from A
    | SBCHL                           --Sub (HL)+carry flag from A
    | ANDr Register                   --AND register with A
    | ANDn Word8                      --AND immediate with A
    | ANDHL                           --AND (HL) with A
    | ORr Register                    --OR register with A
    | ORn Word8                       --OR immediate with A
    | ORHL                            --OR (HL) with A
    | XORr Register                   --XOR register with A
    | XORn Word8                      --XOR immediate with A
    | XORHL                           --XOR (HL) with A
    | CPr Register                    --Compare register with A
    | CPn Word8                       --Compare immediate with A
    | CPHL                            --Compare (HL) with A
    | INCr Register                   --Increment register
    | INCHL                           --Increment (HL)
    | DECr Register                   --Decrement register
    | DECHL                           --Decrement (HL)
 --16-bit ALU
    | ADDHLrr Register Register       --Add (Register,Register) to HL
    | ADDHLSP                         --Add SP to HL
    | ADDSPn Word8                    --Add immediate to SP
    | INCrr Register Register         --Increment (Register,Register)
    | INCSP                           --Increment SP
    | DECrr Register Register         --Decrement (Register,Register)
    | DECSP                           --Decrement SP
 --Misc.
    | SWAPr Register                  --Swap upper & lower nibles of register
    | SWAPHL                          --Swap upper & lower nibles of (HL)
    | DAA                             --Decimal adjust A
    | CPL                             --Complement A
    | CCF                             --Complement carry flag
    | SCF                             --Set carry flag
    | NOP                             --No operation
    | HALT_INSTR                      --Power down CPU until interrupt
    | STOP                            --Halt CPU & LCD until button pressed
    | DI                              --Disable interrupts (IME = 0)
    | EI                              --Enable interrupts  (IME = 1)
 --Unprefixed rotates (Z always 0)
    | RLCA_instr                      --Rotate A left, old bit 7 to carry
    | RRCA_instr                      --Rotate A right, old bit 0 to carry
    | RLA_instr                       --Rotate A left through carry
    | RRA_instr                       --Rotate A right through carry
 --Rotates & Shifts
    | RLCr Register                   --Rotate register left. Old bit 7 to carry flag
    | RLCHL                           --Rotate (HL) left. Old bit 7 to carry flag
    | RLr Register                    --Rotate register left through carry flag
    | RLHL                            --Rotate (HL) left through carry flag
    | RRCr Register                   --Rotate register right. Old bit 0 to carry flag
    | RRCHL                           --Rotate (HL) right. Old bit 0 to carry flag
    | RRr Register                    --Rotate register right through carry flag
    | RRHL                            --Rotate (HL) right through carry flag
    | SLAr Register                   --Shift register left into carry. LSB set to 0
    | SLAHL                           --Shift (HL) left into carry. LSB set to 0
    | SRAr Register                   --Shift register right into carry. MSB doesn't change.
    | SRAHL                           --Shift (HL) right into carry. MSB doesn't change.
    | SRLr Register                   --Shift register right into carry. MSB set to 0
    | SRLHL                           --Shift (HL) right into carry. MSB set to 0
 --Bit Opcodes
    | BITnr Register Word8            --Test immediate in register
    | BITnHL Word8                    --Test immediate in (HL)
    | SETnr Register Word8            --Set bit b in register
    | SETnHL Word8                    --Set bit b in (HL)
    | RESnr Register Word8            --Reset bit b in register
    | RESnHL Word8                    --Reset bit b in (HL)
 --Jumps
    | JPnn Word16                     --Jump to address nn
    | JPccnn FlagCondition Word16     --Jump to nn if cc conditions are true (see specs)
    | JPHL                            --Jump to address (HL)
    | JRn Word8                       --Jump to current address + n
    | JRccn FlagCondition Word8       --Jump to current address + n if cc conditions are true (see specs)
 --Calls
    | CALLnn Word16                   --Push address of next instruction onto stack, and jump to address nn
    | CALLccnn FlagCondition Word16   --Call address nn if cc conditions are true (see specs)
 --Restarts
    | RSTn RestartAddress             --Push current address on stack, jump to address n
 --Returns
    | RET                             --Pop two bytes from stack, and jump to that address
    | RETcc FlagCondition             --RET if cc conditions are true (see specs)
    | RETI                            --RET, then enable interrupts (IME = 1)
 --HL auto-increment/decrement loads
    | LDIA                            --LD (HL+),A : write A to (HL), then HL++
    | LDAI                            --LD A,(HL+) : read (HL) into A, then HL++
    | LDDA                            --LD (HL-),A : write A to (HL), then HL--
    | LDAD                            --LD A,(HL-) : read (HL) into A, then HL--
 --Helper Instructions
    | ERROR                           --Invalid instruction
    deriving Show

--Helper Functions
 data Flag = FlagZ | FlagN | FlagH | FlagC deriving Show
 data Bit = Zero | One deriving (Enum, Show)

 data RestartAddress = R00 | R08 | R10 | R18 | R20 | R28 | R30 | R38 | R40 | R48 | R50 | R58 | R60 deriving Show

 complementBit :: Bit -> Bit
 complementBit One  = Zero
 complementBit Zero = One

 instance ToBool Bit where
   toBool Zero = False
   toBool One = True

 getRestartAddress :: RestartAddress -> Word16
 getRestartAddress R00 = 0x0000
 getRestartAddress R08 = 0x0008
 getRestartAddress R10 = 0x0010
 getRestartAddress R18 = 0x0018
 getRestartAddress R20 = 0x0020
 getRestartAddress R28 = 0x0028
 getRestartAddress R30 = 0x0030
 getRestartAddress R38 = 0x0038
 getRestartAddress R40 = 0x0040
 getRestartAddress R48 = 0x0048
 getRestartAddress R50 = 0x0050
 getRestartAddress R58 = 0x0058
 getRestartAddress R60 = 0x0060

 data FlagCondition = CondNZ | CondZ | CondNC | CondC deriving Show

 testFlagCondition :: FlagCondition -> GBC Bool
 testFlagCondition CondNZ = do
   flagZ <- getFlagBit FlagZ
   return $ case flagZ of
     One -> False
     Zero -> True
 testFlagCondition CondZ = do
   flagZ <- getFlagBit FlagZ
   return $ case flagZ of
     One -> True
     Zero -> False
 testFlagCondition CondNC = do
   flagC <- getFlagBit FlagC
   return $ case flagC of
     One -> False
     Zero -> True
 testFlagCondition CondC = do
   flagC <- getFlagBit FlagC
   return $ case flagC of
     One -> True
     Zero -> False

 class IsBit a where
   toBit :: a -> Bit

 instance IsBit Integer where
   toBit num = case num of
     0 -> Zero
     _ -> One

 instance IsBit Word8 where
   toBit num = case num of
     0 -> Zero
     _ -> One

 instance IsBit Bool where
   toBit False = Zero
   toBit True = One

 instance IsBit Bit where
   toBit = id

 set :: (Bits a, IsBit b) => a -> Int -> b -> a
 set num idx bit = case (toBit bit) of
   Zero -> num `clearBit` idx
   One  -> num `setBit`   idx

 updateFlag :: (IsBit b) => Flag -> b -> GBC ()
 updateFlag flag bit = do
   fReg <- readReg F
   case flag of
     FlagZ -> writeReg F (set fReg 7 bit)
     FlagN -> writeReg F (set fReg 6 bit)
     FlagH -> writeReg F (set fReg 5 bit)
     FlagC -> writeReg F (set fReg 4 bit)

 updateFlags :: [(Flag,Bit)] -> GBC ()
 updateFlags [] = return ()
 updateFlags ((f,b):xs) = updateFlag f b >> updateFlags xs

 addA :: Word8 -> GBC ()
 addA val = do
   aVal <- readReg A
   let a16 = fromIntegral aVal :: Word16
       v16 = fromIntegral val :: Word16
       result16 = a16 + v16
       result = fromIntegral result16 :: Word8
   writeReg A result
   updateFlags [(FlagZ, toBit $ result == 0),
                (FlagN, Zero),
                (FlagH, toBit $ (a16 .&. 0xF) + (v16 .&. 0xF) > 0xF),
                (FlagC, toBit $ result16 > 0xFF)]

 adcA :: Word8 -> GBC ()
 adcA val = do
   aVal <- readReg A
   carryBit <- getFlagBit FlagC
   let c = fromIntegral (fromEnum carryBit) :: Word16
       a16 = fromIntegral aVal :: Word16
       v16 = fromIntegral val :: Word16
       result16 = a16 + v16 + c
       result = fromIntegral result16 :: Word8
   writeReg A result
   updateFlags [(FlagZ, toBit $ result == 0),
                (FlagN, Zero),
                (FlagH, toBit $ (a16 .&. 0xF) + (v16 .&. 0xF) + c > 0xF),
                (FlagC, toBit $ result16 > 0xFF)]

 subA :: Word8 -> GBC ()
 subA val = do
   aVal <- readReg A
   let result = aVal - val
   writeReg A result
   updateFlags [(FlagZ, toBit $ result == 0),
                (FlagN, One),
                (FlagH, toBit $ (aVal .&. 0xF) < (val .&. 0xF)),
                (FlagC, toBit $ aVal < val)]

 sbcA :: Word8 -> GBC ()
 sbcA val = do
   aVal <- readReg A
   carryBit <- getFlagBit FlagC
   let c = fromIntegral (fromEnum carryBit) :: Word16
       a16 = fromIntegral aVal :: Word16
       v16 = fromIntegral val :: Word16
       result16 = a16 - v16 - c
       result = fromIntegral result16 :: Word8
   writeReg A result
   updateFlags [(FlagZ, toBit $ result == 0),
                (FlagN, One),
                (FlagH, toBit $ (a16 .&. 0xF) < (v16 .&. 0xF) + c),
                (FlagC, toBit $ a16 < v16 + c)]

 andA :: Word8 -> GBC ()
 andA val = do
   aVal <- readReg A
   let result = aVal .&. val
   writeReg A result
   updateFlags [(FlagZ, toBit $ result == 0),
                (FlagN, Zero),
                (FlagH, One),
                (FlagC, Zero)]

 orA :: Word8 -> GBC ()
 orA val = do
   aVal <- readReg A
   let result = aVal .|. val
   writeReg A result
   updateFlags [(FlagZ, toBit $ result == 0),
                (FlagN, Zero),
                (FlagH, Zero),
                (FlagC, Zero)]

 xorA :: Word8 -> GBC ()
 xorA val = do
   aVal <- readReg A
   let result = aVal `xor` val
   writeReg A result
   updateFlags [(FlagZ, toBit $ result == 0),
                (FlagN, Zero),
                (FlagH, Zero),
                (FlagC, Zero)]

 getBit :: (Bits a, Num a) => a -> Int -> Bit
 getBit num i = toBit $ 0 /= ((bit i) .&. num) `shiftR` i

 isBitSet :: (Bits a, Num a) => a -> Int -> Bool
 isBitSet a = toBool <$> getBit a

 getFlagBit :: Flag -> GBC Bit
 getFlagBit flag = do
   fReg <- readReg F
   case flag of
     FlagZ -> return $ getBit fReg 7
     FlagN -> return $ getBit fReg 6
     FlagH -> return $ getBit fReg 5
     FlagC -> return $ getBit fReg 4

 load8_imm :: GBC Word8
 load8_imm = do
   pcVal <- readRef pc
   val <- readMem pcVal
   writeRef pc (pcVal + 1)
   return val

 load16_imm :: GBC Word16
 load16_imm = do
   lower <- load8_imm
   upper <- load8_imm
   return $ ((fromIntegral upper) `shiftL` 8) + (fromIntegral lower)

 (.^.) :: Bits a => a -> a -> a
 (.^.) = xor
------------------

 data Interrupt = VBlank | STAT | Timer | Serial | Joypad deriving (Show, Enum)

 loadIF :: GBC Word8
 loadIF = readMem 0xFF0F

 storeIF :: Word8 -> GBC ()
 storeIF w = writeMem 0xFF0F w

 loadIE :: GBC Word8
 loadIE = readMem 0xFFFF

 storeIE :: Word8 -> GBC ()
 storeIE w = writeMem 0xFFFF w

 interrupts       = [VBlank, STAT, Timer, Serial, Joypad]
 interruptOffsets = [R40,    R48,  R50,   R58,    R60]

 getInterrupts :: Word8 -> [Interrupt]
 getInterrupts flags = filter (\a -> isBitSet flags (fromEnum a)) interrupts

 processInterrupt :: Interrupt -> GBC ()
 processInterrupt interrupt = do
   executeInstruction DI
   (flip clearBit (fromEnum interrupt)) <$> loadIF >>= storeIF --Unset corresponding bit in IF flag
   writeRef ime False
   executeInstruction $ RSTn $ interruptOffsets !! (fromEnum interrupt) --Execute interrupt handler code

 --Need to reset IME after handling each interrupt
 handleInterrupts :: GBC ()
 handleInterrupts = do
   flagIF <- loadIF
   flagIE <- loadIE
   let pending = flagIF .&. flagIE
   when (pending /= 0) $ writeRef halt False
   whenM (readRef ime) $ do
     sequence_ $ processInterrupt <$> (getInterrupts pending)


 requestInterrupt :: Interrupt -> GBC ()
 requestInterrupt interrupt = (flip setBit (fromEnum interrupt)) <$> loadIF >>= storeIF
------------------

 streamNextInstruction :: GBC (Instruction, Cycles)
 streamNextInstruction = do
   pcVal <- readRef pc
   next <- readMem pcVal
   writeRef pc (pcVal + 1)
   if (next == 0xCB) then loadCBInstr else loadNormalInstr next

  where loadNormalInstr next = do
          liftM2 (,) (decodeInstruction next)
                     (return $ cycleLookup !! (fromIntegral next))
        loadCBInstr = do
          suffix <- load8_imm
          liftM2 (,) (decodeCBInstruction suffix)
                     (return $ cbCycleLookup !! (fromIntegral suffix))

 --TODO : Convert to immutable vector
 --See : http://imrannazar.com/Gameboy-Z80-Opcode-Map
 opcodeLookups :: [GBC Instruction]
 opcodeLookups = [return NOP,                             --0
                  LDrrnn B C <$> load16_imm,
                  return $ LDBCr A,
                  return $ INCrr B C,
                  return $ INCr B,
                  return $ DECr B,
                  LDrn B <$> load8_imm,
                  return RLCA_instr,
                  LDnnSP <$> load16_imm,
                  return $ ADDHLrr B C,
                  return $ LDrBC A,
                  return $ DECrr B C,
                  return $ INCr C,
                  return $ DECr C,
                  LDrn C <$> load8_imm,
                  return RRCA_instr,

                  return $ STOP,                          --1
                  LDrrnn D E <$> load16_imm,
                  return $ LDDEr A,
                  return $ INCrr D E,
                  return $ INCr D,
                  return $ DECr D,
                  LDrn D <$> load8_imm,
                  return RLA_instr,
                  JRn <$> load8_imm,
                  return $ ADDHLrr D E,
                  return $ LDrDE A,
                  return $ DECrr D E,
                  return $ INCr E,
                  return $ DECr E,
                  LDrn E <$> load8_imm,
                  return RRA_instr,

                  JRccn CondNZ <$> load8_imm,             --2
                  LDrrnn H L <$> load16_imm,
                  return LDIA,
                  return $ INCrr H L,
                  return $ INCr H,
                  return $ DECr H,
                  LDrn H <$> load8_imm,
                  return DAA,
                  JRccn CondZ <$> load8_imm,
                  return $ ADDHLrr H L,
                  return LDAI,
                  return $ DECrr H L,
                  return $ INCr L,
                  return $ DECr L,
                  LDrn L <$> load8_imm,
                  return CPL,

                  JRccn CondNC <$> load8_imm,             --3
                  LDSPnn <$> load16_imm,
                  return LDDA,
                  return $ INCSP,
                  return INCHL,
                  return DECHL,
                  LDHLn <$> load8_imm,
                  return SCF,
                  JRccn CondC <$> load8_imm,
                  return ADDHLSP,
                  return LDAD,
                  return DECSP,
                  return $ INCr A,
                  return $ DECr A,
                  LDrn A <$> load8_imm,
                  return CCF,

                  return $ LDrr B B,                      --4
                  return $ LDrr B C,
                  return $ LDrr B D,
                  return $ LDrr B E,
                  return $ LDrr B H,
                  return $ LDrr B L,
                  return $ LDrHL B,
                  return $ LDrr B A,
                  return $ LDrr C B,
                  return $ LDrr C C,
                  return $ LDrr C D,
                  return $ LDrr C E,
                  return $ LDrr C H,
                  return $ LDrr C L,
                  return $ LDrHL C,
                  return $ LDrr C A,

                  return $ LDrr D B,                      --5
                  return $ LDrr D C,
                  return $ LDrr D D,
                  return $ LDrr D E,
                  return $ LDrr D H,
                  return $ LDrr D L,
                  return $ LDrHL D,
                  return $ LDrr D A,
                  return $ LDrr E B,
                  return $ LDrr E C,
                  return $ LDrr E D,
                  return $ LDrr E E,
                  return $ LDrr E H,
                  return $ LDrr E L,
                  return $ LDrHL E,
                  return $ LDrr E A,

                  return $ LDrr H B,                      --6
                  return $ LDrr H C,
                  return $ LDrr H D,
                  return $ LDrr H E,
                  return $ LDrr H H,
                  return $ LDrr H L,
                  return $ LDrHL H,
                  return $ LDrr H A,
                  return $ LDrr L B,
                  return $ LDrr L C,
                  return $ LDrr L D,
                  return $ LDrr L E,
                  return $ LDrr L H,
                  return $ LDrr L L,
                  return $ LDrHL L,
                  return $ LDrr L A,

                  return $ LDHLr B,                       --7
                  return $ LDHLr C,
                  return $ LDHLr D,
                  return $ LDHLr E,
                  return $ LDHLr H,
                  return $ LDHLr L,
                  return HALT_INSTR,
                  return $ LDHLr A,
                  return $ LDrr A B,
                  return $ LDrr A C,
                  return $ LDrr A D,
                  return $ LDrr A E,
                  return $ LDrr A H,
                  return $ LDrr A L,
                  return $ LDrHL A,
                  return $ LDrr A A,

                  return $ ADDr B,                        --8
                  return $ ADDr C,
                  return $ ADDr D,
                  return $ ADDr E,
                  return $ ADDr H,
                  return $ ADDr L,
                  return ADDHL,
                  return $ ADDr A,
                  return $ ADCr B,
                  return $ ADCr C,
                  return $ ADCr D,
                  return $ ADCr E,
                  return $ ADCr H,
                  return $ ADCr L,
                  return ADCHL,
                  return $ ADCr A,

                  return $ SUBr B,                        --9
                  return $ SUBr C,
                  return $ SUBr D,
                  return $ SUBr E,
                  return $ SUBr H,
                  return $ SUBr L,
                  return SUBHL,
                  return $ SUBr A,
                  return $ SBCr B,
                  return $ SBCr C,
                  return $ SBCr D,
                  return $ SBCr E,
                  return $ SBCr H,
                  return $ SBCr L,
                  return SBCHL,
                  return $ SBCr A,

                  return $ ANDr B,                        --A
                  return $ ANDr C,
                  return $ ANDr D,
                  return $ ANDr E,
                  return $ ANDr H,
                  return $ ANDr L,
                  return ANDHL,
                  return $ ANDr A,
                  return $ XORr B,
                  return $ XORr C,
                  return $ XORr D,
                  return $ XORr E,
                  return $ XORr H,
                  return $ XORr L,
                  return XORHL,
                  return $ XORr A,

                  return $ ORr B,                         --B
                  return $ ORr C,
                  return $ ORr D,
                  return $ ORr E,
                  return $ ORr H,
                  return $ ORr L,
                  return ORHL,
                  return $ ORr A,
                  return $ CPr B,
                  return $ CPr C,
                  return $ CPr D,
                  return $ CPr E,
                  return $ CPr H,
                  return $ CPr L,
                  return CPHL,
                  return $ CPr A,

                  return $ RETcc CondNZ,                  --C
                  return $ POPrr B C,
                  JPccnn CondNZ <$> load16_imm,
                  JPnn <$> load16_imm,
                  CALLccnn CondNZ <$> load16_imm,
                  return $ PUSHrr B C,
                  ADDn <$> load8_imm,
                  return $ RSTn R00,
                  return $ RETcc CondZ,
                  return RET,
                  JPccnn CondZ <$> load16_imm,
                  return ERROR,
                  CALLccnn CondZ <$> load16_imm,
                  CALLnn <$> load16_imm,
                  ADCn <$> load8_imm,
                  return $ RSTn R08,

                  return $ RETcc CondNC,                  --D
                  return $ POPrr D E,
                  JPccnn CondNC <$> load16_imm,
                  return ERROR,
                  CALLccnn CondNC <$> load16_imm,
                  return $ PUSHrr D E,
                  SUBn <$> load8_imm,
                  return $ RSTn R10,
                  return $ RETcc CondC,
                  return RETI,
                  JPccnn CondC <$> load16_imm,
                  return ERROR,
                  CALLccnn CondC <$> load16_imm,
                  return ERROR,
                  SBCn <$> load8_imm,
                  return $ RSTn R18,

                  LDnr `liftM` load8_imm `ap` (return A), --E
                  return $ POPrr H L,
                  return $ LDrr'' C A,
                  return ERROR,
                  return ERROR,
                  return $ PUSHrr H L,
                  ANDn <$> load8_imm,
                  return $ RSTn R20,
                  ADDSPn <$> load8_imm,
                  return JPHL,
                  LDnnr `liftM` load16_imm `ap` (return A),
                  return ERROR,
                  return ERROR,
                  return ERROR,
                  XORn <$> load8_imm,
                  return $ RSTn R28,

                  LDrn' A <$> load8_imm,                  --F
                  return $ POPrr A F,
                  return $ LDrr' A C,
                  return DI,
                  return ERROR,
                  return $ PUSHrr A F,
                  ORn <$> load8_imm,
                  return $ RSTn R30,
                  LDHLSPn  <$> load8_imm,
                  return LDSPHL,
                  LDrnn A <$> load16_imm,
                  return EI,
                  return ERROR,
                  return ERROR,
                  CPn <$> load8_imm,
                  return $ RSTn R38
                 ]

 cbOpcodeLookups :: [GBC Instruction]
 cbOpcodeLookups = [return $ RLCr B,                --0
                    return $ RLCr C,
                    return $ RLCr D,
                    return $ RLCr E,
                    return $ RLCr H,
                    return $ RLCr L,
                    return $ RLCHL,
                    return $ RLCr A,
                    return $ RRCr B,
                    return $ RRCr C,
                    return $ RRCr D,
                    return $ RRCr E,
                    return $ RRCr H,
                    return $ RRCr L,
                    return $ RRCHL,
                    return $ RRCr A,

                    return $ RLr B,                --1
                    return $ RLr C,
                    return $ RLr D,
                    return $ RLr E,
                    return $ RLr H,
                    return $ RLr L,
                    return $ RLHL,
                    return $ RLr A,
                    return $ RRr B,
                    return $ RRr C,
                    return $ RRr D,
                    return $ RRr E,
                    return $ RRr H,
                    return $ RRr L,
                    return $ RRHL,
                    return $ RRr A,

                    return $ SLAr B,                --2
                    return $ SLAr C,
                    return $ SLAr D,
                    return $ SLAr E,
                    return $ SLAr H,
                    return $ SLAr L,
                    return $ SLAHL,
                    return $ SLAr A,
                    return $ SRAr B,
                    return $ SRAr C,
                    return $ SRAr D,
                    return $ SRAr E,
                    return $ SRAr H,
                    return $ SRAr L,
                    return $ SRAHL,
                    return $ SRAr A,

                    return $ SWAPr B,                --3
                    return $ SWAPr C,
                    return $ SWAPr D,
                    return $ SWAPr E,
                    return $ SWAPr H,
                    return $ SWAPr L,
                    return $ SWAPHL,
                    return $ SWAPr A,
                    return $ SRLr B,
                    return $ SRLr C,
                    return $ SRLr D,
                    return $ SRLr E,
                    return $ SRLr H,
                    return $ SRLr L,
                    return $ SRLHL,
                    return $ SRLr A,

                    return $ BITnr B 0,                --4
                    return $ BITnr C 0,
                    return $ BITnr D 0,
                    return $ BITnr E 0,
                    return $ BITnr H 0,
                    return $ BITnr L 0,
                    return $ BITnHL 0,
                    return $ BITnr A 0,
                    return $ BITnr B 1,
                    return $ BITnr C 1,
                    return $ BITnr D 1,
                    return $ BITnr E 1,
                    return $ BITnr H 1,
                    return $ BITnr L 1,
                    return $ BITnHL 1,
                    return $ BITnr A 1,

                    return $ BITnr B 2,                --5
                    return $ BITnr C 2,
                    return $ BITnr D 2,
                    return $ BITnr E 2,
                    return $ BITnr H 2,
                    return $ BITnr L 2,
                    return $ BITnHL 2,
                    return $ BITnr A 2,
                    return $ BITnr B 3,
                    return $ BITnr C 3,
                    return $ BITnr D 3,
                    return $ BITnr E 3,
                    return $ BITnr H 3,
                    return $ BITnr L 3,
                    return $ BITnHL 3,
                    return $ BITnr A 3,

                    return $ BITnr B 4,                --6
                    return $ BITnr C 4,
                    return $ BITnr D 4,
                    return $ BITnr E 4,
                    return $ BITnr H 4,
                    return $ BITnr L 4,
                    return $ BITnHL 4,
                    return $ BITnr A 4,
                    return $ BITnr B 5,
                    return $ BITnr C 5,
                    return $ BITnr D 5,
                    return $ BITnr E 5,
                    return $ BITnr H 5,
                    return $ BITnr L 5,
                    return $ BITnHL 5,
                    return $ BITnr A 5,

                    return $ BITnr B 6,                --7
                    return $ BITnr C 6,
                    return $ BITnr D 6,
                    return $ BITnr E 6,
                    return $ BITnr H 6,
                    return $ BITnr L 6,
                    return $ BITnHL 6,
                    return $ BITnr A 6,
                    return $ BITnr B 7,
                    return $ BITnr C 7,
                    return $ BITnr D 7,
                    return $ BITnr E 7,
                    return $ BITnr H 7,
                    return $ BITnr L 7,
                    return $ BITnHL 7,
                    return $ BITnr A 7,

                    return $ RESnr B 0,                --8
                    return $ RESnr C 0,
                    return $ RESnr D 0,
                    return $ RESnr E 0,
                    return $ RESnr H 0,
                    return $ RESnr L 0,
                    return $ RESnHL 0,
                    return $ RESnr A 0,
                    return $ RESnr B 1,
                    return $ RESnr C 1,
                    return $ RESnr D 1,
                    return $ RESnr E 1,
                    return $ RESnr H 1,
                    return $ RESnr L 1,
                    return $ RESnHL 1,
                    return $ RESnr A 1,

                    return $ RESnr B 2,                --9
                    return $ RESnr C 2,
                    return $ RESnr D 2,
                    return $ RESnr E 2,
                    return $ RESnr H 2,
                    return $ RESnr L 2,
                    return $ RESnHL 2,
                    return $ RESnr A 2,
                    return $ RESnr B 3,
                    return $ RESnr C 3,
                    return $ RESnr D 3,
                    return $ RESnr E 3,
                    return $ RESnr H 3,
                    return $ RESnr L 3,
                    return $ RESnHL 3,
                    return $ RESnr A 3,

                    return $ RESnr B 4,                --A
                    return $ RESnr C 4,
                    return $ RESnr D 4,
                    return $ RESnr E 4,
                    return $ RESnr H 4,
                    return $ RESnr L 4,
                    return $ RESnHL 4,
                    return $ RESnr A 4,
                    return $ RESnr B 5,
                    return $ RESnr C 5,
                    return $ RESnr D 5,
                    return $ RESnr E 5,
                    return $ RESnr H 5,
                    return $ RESnr L 5,
                    return $ RESnHL 5,
                    return $ RESnr A 5,

                    return $ RESnr B 6,                --B
                    return $ RESnr C 6,
                    return $ RESnr D 6,
                    return $ RESnr E 6,
                    return $ RESnr H 6,
                    return $ RESnr L 6,
                    return $ RESnHL 6,
                    return $ RESnr A 6,
                    return $ RESnr B 7,
                    return $ RESnr C 7,
                    return $ RESnr D 7,
                    return $ RESnr E 7,
                    return $ RESnr H 7,
                    return $ RESnr L 7,
                    return $ RESnHL 7,
                    return $ RESnr A 7,

                    return $ SETnr B 0,                --C
                    return $ SETnr C 0,
                    return $ SETnr D 0,
                    return $ SETnr E 0,
                    return $ SETnr H 0,
                    return $ SETnr L 0,
                    return $ SETnHL 0,
                    return $ SETnr A 0,
                    return $ SETnr B 1,
                    return $ SETnr C 1,
                    return $ SETnr D 1,
                    return $ SETnr E 1,
                    return $ SETnr H 1,
                    return $ SETnr L 1,
                    return $ SETnHL 1,
                    return $ SETnr A 1,

                    return $ SETnr B 2,                --D
                    return $ SETnr C 2,
                    return $ SETnr D 2,
                    return $ SETnr E 2,
                    return $ SETnr H 2,
                    return $ SETnr L 2,
                    return $ SETnHL 2,
                    return $ SETnr A 2,
                    return $ SETnr B 3,
                    return $ SETnr C 3,
                    return $ SETnr D 3,
                    return $ SETnr E 3,
                    return $ SETnr H 3,
                    return $ SETnr L 3,
                    return $ SETnHL 3,
                    return $ SETnr A 3,

                    return $ SETnr B 4,                --E
                    return $ SETnr C 4,
                    return $ SETnr D 4,
                    return $ SETnr E 4,
                    return $ SETnr H 4,
                    return $ SETnr L 4,
                    return $ SETnHL 4,
                    return $ SETnr A 4,
                    return $ SETnr B 5,
                    return $ SETnr C 5,
                    return $ SETnr D 5,
                    return $ SETnr E 5,
                    return $ SETnr H 5,
                    return $ SETnr L 5,
                    return $ SETnHL 5,
                    return $ SETnr A 5,

                    return $ SETnr B 6,                --F
                    return $ SETnr C 6,
                    return $ SETnr D 6,
                    return $ SETnr E 6,
                    return $ SETnr H 6,
                    return $ SETnr L 6,
                    return $ SETnHL 6,
                    return $ SETnr A 6,
                    return $ SETnr B 7,
                    return $ SETnr C 7,
                    return $ SETnr D 7,
                    return $ SETnr E 7,
                    return $ SETnr H 7,
                    return $ SETnr L 7,
                    return $ SETnHL 7,
                    return $ SETnr A 7
                    ]

 cycleLookup :: [Cycles]
 cycleLookup = [4,12,8,8,4,4,8,4,20,8,8,8,4,4,8,4,        --0
                4,12,8,8,4,4,8,4,8,8,8,8,4,4,8,4,         --1
                8,12,8,8,4,4,8,4,8,8,8,8,4,4,8,4,         --2
                8,12,8,8,12,12,12,4,8,8,8,8,4,4,8,4,      --3
                4,4,4,4,4,4,8,4,4,4,4,4,4,4,8,4,          --4
                4,4,4,4,4,4,8,4,4,4,4,4,4,4,8,4,          --5
                4,4,4,4,4,4,8,4,4,4,4,4,4,4,8,4,          --6
                8,8,8,8,8,8,4,8,4,4,4,4,4,4,8,4,          --7
                4,4,4,4,4,4,8,4,4,4,4,4,4,4,8,4,          --8
                4,4,4,4,4,4,8,4,4,4,4,4,4,4,8,4,          --9
                4,4,4,4,4,4,8,4,4,4,4,4,4,4,8,4,          --A
                4,4,4,4,4,4,8,4,4,4,4,4,4,4,8,4,          --B
                8,12,12,12,12,16,8,32,8,8,12,0,12,12,8,32,--C
                8,12,12,0,12,16,8,32,8,8,12,0,12,0,8,32,  --D
                12,12,8,0,0,16,8,32,16,4,16,0,0,0,8,32,   --E
                12,12,8,4,0,16,8,32,12,8,16,4,0,0,8,32]   --F


 cbCycleLookup :: [Cycles]
 cbCycleLookup = [8,8,8,8,8,8,16,8,8,8,8,8,8,8,16,8,      --0
                  8,8,8,8,8,8,16,8,8,8,8,8,8,8,16,8,      --1
                  8,8,8,8,8,8,16,8,8,8,8,8,8,8,16,8,      --2
                  8,8,8,8,8,8,16,8,8,8,8,8,8,8,16,8,      --3
                  8,8,8,8,8,8,16,8,8,8,8,8,8,8,16,8,      --4
                  8,8,8,8,8,8,16,8,8,8,8,8,8,8,16,8,      --5
                  8,8,8,8,8,8,16,8,8,8,8,8,8,8,16,8,      --6
                  8,8,8,8,8,8,16,8,8,8,8,8,8,8,16,8,      --7
                  8,8,8,8,8,8,16,8,8,8,8,8,8,8,16,8,      --8
                  8,8,8,8,8,8,16,8,8,8,8,8,8,8,16,8,      --9
                  8,8,8,8,8,8,16,8,8,8,8,8,8,8,16,8,      --A
                  8,8,8,8,8,8,16,8,8,8,8,8,8,8,16,8,      --B
                  8,8,8,8,8,8,16,8,8,8,8,8,8,8,16,8,      --C
                  8,8,8,8,8,8,16,8,8,8,8,8,8,8,16,8,      --D
                  8,8,8,8,8,8,16,8,8,8,8,8,8,8,16,8,      --E
                  8,8,8,8,8,8,16,8,8,8,8,8,8,8,16,8]      --F

 lengthLookup :: [Word16]
 lengthLookup = [1,3,1,1,1,1,2,1,3,1,1,1,1,1,2,1,         --0
                 2,3,1,1,1,1,2,1,2,1,1,1,1,1,2,1,         --1
                 2,3,1,1,1,1,2,1,2,1,1,1,1,1,2,1,         --2
                 2,3,1,1,1,1,2,1,2,1,1,1,1,1,2,1,         --3
                 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,         --4
                 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,         --5
                 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,         --6
                 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,         --7
                 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,         --8
                 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,         --9
                 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,         --A
                 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,         --B
                 1,1,3,3,3,1,2,1,1,1,3,2,3,3,2,1,         --C
                 1,1,3,1,3,1,2,1,1,1,3,1,3,1,2,1,         --D
                 2,1,1,1,1,1,2,1,2,1,3,1,1,1,2,1,         --E
                 2,1,1,1,1,1,2,1,2,1,3,1,1,1,2,1]         --F

 decodeInstruction :: Opcode -> GBC Instruction
 decodeInstruction op = opcodeLookups !! (fromIntegral op)

 decodeCBInstruction :: Opcode -> GBC Instruction
 decodeCBInstruction op = cbOpcodeLookups !! (fromIntegral op)

 executeInstruction :: Instruction -> GBC ()
 executeInstruction instr = case instr of
--8-bit loads
   LDrn reg imm -> writeReg reg imm
   LDrr reg1 reg2 -> readReg reg2 >>= writeReg reg1
   LDrHL reg -> readReg2 H L >>= readMem >>= writeReg reg
   LDrBC reg -> readReg2 B C >>= readMem >>= writeReg reg
   LDrDE reg -> readReg2 D E >>= readMem >>= writeReg reg
   LDrnn reg imm16 -> readMem imm16 >>= writeReg reg
   LDHLr reg -> join $ writeMem <$> (readReg2 H L) <*> (readReg reg)
   LDBCr reg -> join $ writeMem <$> (readReg2 B C) <*> (readReg reg)
   LDDEr reg -> join $ writeMem <$> (readReg2 D E) <*> (readReg reg)
   LDHLn imm -> join $ writeMem <$> (readReg2 H L) <*> (return imm)
   LDnnr imm16 reg -> join $ writeMem <$> (return imm16) <*> (readReg reg)
   LDrr' reg1 reg2 -> join $ writeReg reg1 <$> (readMem =<< (lift8 $ readReg reg2))
   LDrr'' reg1 reg2 -> join $ writeMem <$> (lift8 $ readReg reg1) <*> readReg reg2
   LDnr imm reg -> readReg reg >>= writeMem (fromIntegral imm + 0xFF00)
   LDrn' reg imm -> join $ writeReg reg <$> (readMem $ fromIntegral imm + 0xFF00)
   LDIA -> do -- LD (HL+),A : write A to (HL), then HL++
     addr <- readReg2 H L
     readReg A >>= writeMem addr
     writeReg2 H L (addr + 1)
   LDAI -> do -- LD A,(HL+) : read (HL) into A, then HL++
     addr <- readReg2 H L
     readMem addr >>= writeReg A
     writeReg2 H L (addr + 1)
   LDDA -> do -- LD (HL-),A : write A to (HL), then HL--
     addr <- readReg2 H L
     readReg A >>= writeMem addr
     writeReg2 H L (addr - 1)
   LDAD -> do -- LD A,(HL-) : read (HL) into A, then HL--
     addr <- readReg2 H L
     readMem addr >>= writeReg A
     writeReg2 H L (addr - 1)
--16-bit loads
   LDrrnn reg1 reg2 imm16 -> writeReg2 reg1 reg2 imm16
   LDSPnn imm16 -> writeRef sp imm16
   LDSPHL -> readReg2 H L >>= writeRef sp
   LDHLSPn imm -> do
     spVal <- readRef sp
     let imm16 = fromIntegral imm :: Word16
         offset = fromIntegral (fromIntegral imm :: Int8) :: Word16
         result = spVal + offset
     writeReg2 H L result
     updateFlags [(FlagZ, Zero),
                  (FlagN, Zero),
                  (FlagH, toBit $ (spVal .&. 0x0F) + (imm16 .&. 0x0F) > 0x0F),
                  (FlagC, toBit $ (spVal .&. 0xFF) + imm16 > 0xFF)]
   LDnnSP imm16 -> do
     spVal <- readRef sp
     writeMem imm16 (fromIntegral $ spVal .&. 0xFF)
     writeMem (imm16 + 1) (fromIntegral $ spVal `shiftR` 8)
   PUSHrr reg1 reg2 -> do --left-most byte => SP-1, right-most byte => SP-2
     spVal <- readRef sp
     readReg reg1 >>= writeMem ((spVal-1) .&. 0xFFFF)
     readReg reg2 >>= writeMem ((spVal-2) .&. 0xFFFF)
     writeRef sp $ spVal - 2
   POPrr reg1 reg2 -> do --SP => right, SP+1 => left
     spVal <- readRef sp
     right <- readMem spVal
     left <- readMem $ (spVal+1) .&. 0xFFFF
     writeReg2 reg1 reg2 $ ((fromIntegral left) `shiftL` 8) .|. (fromIntegral right)
     writeRef sp $ spVal + 2
--8-bit ALU
   ADDr reg -> addA =<< readReg reg
   ADDn imm -> addA imm
   ADDHL    -> addA =<< (readReg2 H L >>= readMem)
   ADCr reg -> adcA =<< readReg reg
   ADCn imm -> adcA imm
   ADCHL    -> adcA =<< (readReg2 H L >>= readMem)
   SUBr reg -> subA =<< readReg reg
   SUBn imm -> subA imm
   SUBHL    -> subA =<< (readReg2 H L >>= readMem)
   SBCr reg -> sbcA =<< readReg reg
   SBCn imm -> sbcA imm
   SBCHL    -> sbcA =<< (readReg2 H L >>= readMem)
   ANDr reg -> andA =<< readReg reg
   ANDn imm -> andA imm
   ANDHL    -> andA =<< (readReg2 H L >>= readMem)
   ORr reg  -> orA =<< readReg reg
   ORn imm  -> orA imm
   ORHL     -> orA =<< (readReg2 H L >>= readMem)
   XORr reg -> xorA =<< readReg reg
   XORn imm -> xorA imm
   XORHL    -> xorA =<< (readReg2 H L >>= readMem)
   CPr reg -> do
     aVal <- readReg A
     regVal <- readReg reg
     let diff = aVal - regVal
     updateFlags [(FlagZ, toBit $ aVal == regVal),
                  (FlagN, One),
                  (FlagH, toBit $ diff .&. 0x0F > aVal .&. 0x0F),
                  (FlagC, toBit $ diff > aVal)]
   CPn imm -> do
     aVal <- readReg A
     let diff = aVal - imm
     updateFlags [(FlagZ, toBit $ diff == 0),
                  (FlagN, One),
                  (FlagH, toBit $ diff .&. 0x0F > aVal .&. 0x0F),
                  (FlagC, toBit $ diff > aVal)]
   CPHL -> do
     mem <- readReg2 H L >>= readMem
     aVal <- readReg A
     let diff = aVal - mem
     updateFlags [(FlagZ, toBit $ diff == 0),
                  (FlagN, One),
                  (FlagH, toBit $ diff .&. 0x0F > aVal .&. 0x0F),
                  (FlagC, toBit $ diff > aVal)]
   INCr reg -> do
     regVal <- readReg reg
     let sum = regVal + 1
     writeReg reg sum
     updateFlags [(FlagZ, toBit $ sum == 0),
                  (FlagN, Zero),
                  (FlagH, toBit $ sum .&. 0x0F < regVal .&. 0x0F)]
   INCHL -> do
     hl <- readReg2 H L
     mem <- readMem hl
     let sum = mem + 1
     writeMem hl sum
     updateFlags [(FlagZ, toBit $ sum == 0),
                  (FlagN, Zero),
                  (FlagH, toBit $ sum .&. 0x0F < mem .&. 0x0F)]
   DECr reg -> do
     regVal <- readReg reg
     let diff = regVal - 1
     writeReg reg diff
     updateFlags [(FlagZ, toBit $ diff == 0),
                  (FlagN, One),
                  (FlagH, toBit $ diff .&. 0x0F > regVal .&. 0x0F)]
   DECHL -> do
     hl <- readReg2 H L
     mem <- readMem hl
     let diff = mem - 1
     writeMem hl diff
     updateFlags [(FlagZ, toBit $ diff == 0),
                  (FlagN, One),
                  (FlagH, toBit $ diff .&. 0x0F > mem .&. 0x0F)]
   ADDHLrr reg1 reg2 -> do
     regVal <- readReg2 reg1 reg2
     hlVal  <- readReg2 H L
     let sum = regVal + hlVal
     writeReg2 H L sum
     updateFlags [(FlagN, Zero),
                  (FlagH, toBit $ sum .&. 0x0FFF < hlVal .&. 0x0FFF),
                  (FlagC, toBit $ sum < hlVal)]
   ADDHLSP -> do
     regVal <- readRef sp
     hlVal  <- readReg2 H L
     let sum = regVal + hlVal
     writeReg2 H L sum
     updateFlags [(FlagN, Zero),
                  (FlagH, toBit $ sum .&. 0x0FFF < hlVal .&. 0x0FFF),
                  (FlagC, toBit $ sum < hlVal)]
   ADDSPn imm -> do
     spVal <- readRef sp
     let imm16 = fromIntegral imm :: Word16
         offset = fromIntegral (fromIntegral imm :: Int8) :: Word16
         result = spVal + offset
     writeRef sp result
     updateFlags [(FlagZ, Zero),
                  (FlagN, Zero),
                  (FlagH, toBit $ (spVal .&. 0x0F) + (imm16 .&. 0x0F) > 0x0F),
                  (FlagC, toBit $ (spVal .&. 0xFF) + imm16 > 0xFF)]
   INCrr reg1 reg2 -> fmap (+1) (readReg2 reg1 reg2) >>= writeReg2 reg1 reg2
   INCSP -> fmap (+1) (readRef sp) >>= writeRef sp
   DECrr reg1 reg2 -> fmap (flip (-) 1) (readReg2 reg1 reg2) >>= writeReg2 reg1 reg2
   DECSP -> fmap (flip (-) 1) (readRef sp) >>= writeRef sp
   SWAPr reg -> do
     regVal <- readReg reg
     let out = ((regVal .&. 0xF0) `shiftR` 4) + ((regVal .&. 0x0F) `shiftL` 4)
     writeReg reg out
     updateFlags [(FlagZ, toBit $ out == 0),
                  (FlagN, Zero),
                  (FlagH, Zero),
                  (FlagC, Zero)]
   SWAPHL -> do
     hlVal <- readReg2 H L
     mem <- readMem hlVal
     let out = ((mem .&. 0xF0) `shiftR` 4) + ((mem .&. 0x0F) `shiftL` 4)
     writeMem hlVal out
     updateFlags [(FlagZ, toBit $ out == 0),
                  (FlagN, Zero),
                  (FlagH, Zero),
                  (FlagC, Zero)]
   DAA -> do
     regA  <- readReg A
     flagH <- toBool <$> getFlagBit FlagH
     flagC <- toBool <$> getFlagBit FlagC
     flagN <- toBool <$> getFlagBit FlagN
     if not flagN then do
       let correction = (if flagC || regA > 0x99 then 0x60 else 0x00)
                       + (if flagH || (regA .&. 0x0F) > 0x09 then 0x06 else 0x00)
           result = regA + correction
       writeReg A result
       updateFlags [(FlagZ, toBit $ result == 0),
                    (FlagH, Zero),
                    (FlagC, if flagC || regA > 0x99 then One else Zero)]
     else do
       let correction = (if flagC then 0x60 else 0x00)
                       + (if flagH then 0x06 else 0x00)
           result = regA - correction
       writeReg A result
       updateFlags [(FlagZ, toBit $ result == 0),
                    (FlagH, Zero)]
   CPL -> do
     aVal <- readReg A
     writeReg A $ complement aVal
     updateFlags [(FlagN, One),
                  (FlagH, One)]
   CCF -> do
     carryBit <- getFlagBit FlagC
     updateFlags[(FlagN, Zero),
                 (FlagH, Zero),
                 (FlagC, complementBit carryBit)]
   SCF -> do
     updateFlags[(FlagN, Zero),
                 (FlagH, Zero),
                 (FlagC, One)]
   NOP  -> return ()
   HALT_INSTR -> writeRef halt True
   STOP -> return () --TODO
   DI   -> writeRef ime False
   EI   -> writeRef imePending True
   RLCA_instr -> do
     regVal <- readReg A
     let out = regVal `rotateL` 1
     writeReg A out
     updateFlags[(FlagZ, Zero),
                 (FlagN, Zero),
                 (FlagH, Zero),
                 (FlagC, getBit regVal 7)]
   RRCA_instr -> do
     regVal <- readReg A
     let out = regVal `rotateR` 1
     writeReg A out
     updateFlags[(FlagZ, Zero),
                 (FlagN, Zero),
                 (FlagH, Zero),
                 (FlagC, getBit regVal 0)]
   RLA_instr -> do
     regVal <- readReg A
     carryBit <- getFlagBit FlagC
     let out = (regVal `shiftL` 1) .|. (fromIntegral $ fromEnum carryBit)
     writeReg A out
     updateFlags[(FlagZ, Zero),
                 (FlagN, Zero),
                 (FlagH, Zero),
                 (FlagC, getBit regVal 7)]
   RRA_instr -> do
     regVal <- readReg A
     carryBit <- getFlagBit FlagC
     let out = (regVal `shiftR` 1) .|. ((fromIntegral $ fromEnum carryBit) `shiftL` 7)
     writeReg A out
     updateFlags[(FlagZ, Zero),
                 (FlagN, Zero),
                 (FlagH, Zero),
                 (FlagC, getBit regVal 0)]
   RLCr reg -> do
     regVal <- readReg reg
     let out = regVal `rotateL` 1
     writeReg reg out
     updateFlags[(FlagZ, toBit $ out == 0),
                 (FlagN, Zero),
                 (FlagH, Zero),
                 (FlagC, getBit regVal 7)]
   RLCHL -> do
     hl <- readReg2 H L
     mem <- readMem hl
     let out = mem `rotateL` 1
     writeMem hl out
     updateFlags[(FlagZ, toBit $ out == 0),
                 (FlagN, Zero),
                 (FlagH, Zero),
                 (FlagC, getBit mem 7)]
   RLr reg -> do
     regVal <- readReg reg
     carryBit <- getFlagBit FlagC
     let out = (regVal `shiftL` 1) .|. (fromIntegral $ fromEnum carryBit)
     writeReg reg out
     updateFlags[(FlagZ, toBit $ out == 0),
                 (FlagN, Zero),
                 (FlagH, Zero),
                 (FlagC, getBit regVal 7)]
   RLHL -> do
     hl <- readReg2 H L
     mem <- readMem hl
     carryBit <- getFlagBit FlagC
     let out = (mem `shiftL` 1) .|. (fromIntegral $ fromEnum carryBit)
     writeMem hl out
     updateFlags[(FlagZ, toBit $ out == 0),
                 (FlagN, Zero),
                 (FlagH, Zero),
                 (FlagC, getBit mem 7)]
   RRCr reg -> do
     regVal <- readReg reg
     let out = regVal `rotateR` 1
     writeReg reg out
     updateFlags[(FlagZ, toBit $ out == 0),
                 (FlagN, Zero),
                 (FlagH, Zero),
                 (FlagC, getBit regVal 0)]
   RRCHL -> do
     hl <- readReg2 H L
     mem <- readMem hl
     let out = mem `rotateR` 1
     writeMem hl out
     updateFlags[(FlagZ, toBit $ out == 0),
                 (FlagN, Zero),
                 (FlagH, Zero),
                 (FlagC, getBit mem 0)]
   RRr reg -> do
     regVal <- readReg reg
     carryBit <- getFlagBit FlagC
     let out = (regVal `shiftR` 1) .|. ((fromIntegral $ fromEnum carryBit) `shiftL` 7)
     writeReg reg out
     updateFlags[(FlagZ, toBit $ out == 0),
                 (FlagN, Zero),
                 (FlagH, Zero),
                 (FlagC, getBit regVal 0)]
   RRHL -> do
     hl <- readReg2 H L
     mem <- readMem hl
     carryBit <- getFlagBit FlagC
     let out = (mem `shiftR` 1) .|. ((fromIntegral $ fromEnum carryBit) `shiftL` 7)
     writeMem hl out
     updateFlags[(FlagZ, toBit $ out == 0),
                 (FlagN, Zero),
                 (FlagH, Zero),
                 (FlagC, getBit mem 0)]
   SLAr reg -> do
     regVal <- readReg reg
     let out = regVal `shiftL` 1
     writeReg reg out
     updateFlags[(FlagZ, toBit $ out == 0),
                 (FlagN, Zero),
                 (FlagH, Zero),
                 (FlagC, getBit regVal 7)]
   SLAHL -> do
     hl <- readReg2 H L
     mem <- readMem hl
     let out = mem `shiftL` 1
     writeMem hl out
     updateFlags[(FlagZ, toBit $ out == 0),
                 (FlagN, Zero),
                 (FlagH, Zero),
                 (FlagC, getBit mem 7)]
   SRAr reg -> do
     regVal <- readReg reg
     carryBit <- getFlagBit FlagC
     let out = (regVal `shiftR` 1) .|. (if testBit regVal 7 then (bit 7) else 0) --Keep MSB
     writeReg reg out
     updateFlags[(FlagZ, toBit $ out == 0),
                 (FlagN, Zero),
                 (FlagH, Zero),
                 (FlagC, getBit regVal 0)]
   SRAHL -> do
     hl <- readReg2 H L
     mem <- readMem hl
     let out = (mem `shiftR` 1) .|. (if testBit mem 7 then (bit 7) else 0) --Keep MSB
     writeMem hl out
     updateFlags[(FlagZ, toBit $ out == 0),
                 (FlagN, Zero),
                 (FlagH, Zero),
                 (FlagC, getBit mem 0)]
   SRLr reg -> do
     regVal <- readReg reg
     let out = regVal `shiftR` 1
     writeReg reg out
     updateFlags[(FlagZ, toBit $ out == 0),
                 (FlagN, Zero),
                 (FlagH, Zero),
                 (FlagC, getBit regVal 0)]
   SRLHL -> do
     hl <- readReg2 H L
     mem <- readMem hl
     let out = mem `shiftR` 1
     writeMem hl out
     updateFlags[(FlagZ, toBit $ out == 0),
                 (FlagN, Zero),
                 (FlagH, Zero),
                 (FlagC, getBit mem 0)]
   BITnr reg imm -> do
     regVal <- readReg reg
     updateFlags[(FlagZ, toBit $ not $ testBit regVal (fromIntegral imm) ),
                 (FlagN, Zero),
                 (FlagH, One)]
   BITnHL imm -> do
     hl <- readReg2 H L
     mem <- readMem hl
     updateFlags[(FlagZ, toBit $ not $ testBit mem (fromIntegral imm) ),
                 (FlagN, Zero),
                 (FlagH, One)]
   SETnr reg imm -> do
     regVal <- readReg reg
     writeReg reg $ regVal `setBit` (fromIntegral imm)
   SETnHL imm -> do
     hl <- readReg2 H L
     mem <- readMem hl
     writeMem hl $ mem `setBit` (fromIntegral imm)
   RESnr reg imm -> do
     regVal <- readReg reg
     writeReg reg $ regVal `clearBit` (fromIntegral imm)
   RESnHL imm -> do
     hl <- readReg2 H L
     mem <- readMem hl
     writeMem hl $ mem `clearBit` (fromIntegral imm)
   JPnn imm -> do
     writeRef pc imm
   JPccnn cond imm -> do
     ifM (testFlagCondition cond) (writeRef pc imm) (return ())
   JPHL -> writeRef pc =<< (readReg2 H L)
   JRn imm -> do
     currAddr <- readRef pc
     writeRef pc $ currAddr + fromIntegral (fromIntegral imm :: Int8)
   JRccn cond imm -> do
     currAddr <- readRef pc
     toJump <- testFlagCondition cond
     let jumpAddr = currAddr + fromIntegral (fromIntegral imm :: Int8)
     if toJump then writeRef pc jumpAddr else return ()
   CALLnn imm -> do
     spVal <- readRef sp
     pcVal <- readRef pc
     writeMem ((spVal-1) .&. 0xFFFF) (fromIntegral $ pcVal `shiftR` 8)
     writeMem ((spVal-2) .&. 0xFFFF) (fromIntegral $ pcVal .&. 0x00FF)
     writeRef sp $ spVal - 2
     writeRef pc imm
   CALLccnn cond imm -> do
     toJump <- testFlagCondition cond
     if toJump then executeInstruction $ CALLnn imm else return ()
   RSTn rAddress -> do
     spVal <- readRef sp
     pcVal <- readRef pc
     writeMem ((spVal-1) .&. 0xFFFF) (fromIntegral $ pcVal `shiftR` 8)
     writeMem ((spVal-2) .&. 0xFFFF) (fromIntegral $ pcVal .&. 0x00FF)
     writeRef sp $ spVal - 2
     writeRef pc $ getRestartAddress rAddress
   RET -> do
     spVal <- readRef sp
     lowerByte <- readMem spVal
     upperByte <- readMem $ spVal + 1
     let jumpAddr = ( (fromIntegral upperByte) `shiftL` 8 ) + (fromIntegral lowerByte)
     writeRef sp $ spVal + 2
     writeRef pc jumpAddr
   RETcc cond -> do
     toJump <- testFlagCondition cond
     if toJump then executeInstruction RET else return ()
   RETI -> do
     writeRef ime True
     executeInstruction RET
   ERROR -> emulationError "Invalid instruction"
