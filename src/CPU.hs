module CPU
       (  Instruction
       ) where

 import Data.Word
 import Data.Either
 import Data.Bits
 import Data.Bool

 import Memory
 import Monad


 loadNextInstruction :: Emulator m => m Instruction
 loadNextInstruction = do
   
   
   

 data Instruction =   
 --See : http://marc.rawer.de/Gameboy/Docs/GBCPUman.pdf
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
    | LDHLn Word16                    --Load immediate into (HL)
    | LDnnr Word16 Register           --Load register into (immediate)
    | LDrr' Register Register         --Load (FF00 + register) into register
    | LDrr'' Register Register        --Load register into (FF00 + register)
    | LDnr Word8 Register             --Load register into (FF00 + immediate)
    | LDrn' Register Word8            --Load (FF00 + immediate) into register
 --16-bit loads
    | LDrrnn Register Register Word16 --Load immediate into [register,register]
    | LDSPHL                          --Load HL into SP
    | LDHLSPn Word8                   --Load SP+n into (HL)
    | LDnnSP Word16                   --Load SP into (nn)
    | PUSHrr Register Register        --Push [register,register] onto Stack, decrement SP twice
    | POPrr Register Register         --Pop two bytes off stack into (Register,Register), increment SP twice
 --8-bit ALU
    | ADDr Register                   --Add register to A
    | ADDn Word8                      --Add immediate to A
    | ADDHL                           --Add (HL) to A
    | ADCr Register                   --Add (register+carry flag) to A
    | ADCn Word8                      --Add (immediate+carry flag) to A
    | ADCHL                           --Add ((HL)+carry flag) to A
    | SUBr Register                   --Sub register from A
    | SUBn Word8                      --Sub immediate from A
    | SUBHL                           --Sub (HL) from A
    | SBCr Register                   --Sub (register+carry flag) from A 
    | SBCn Word8                      --Sub (immediate+carry flag) from A 
    | SBCHL                           --Sub ((HL)+carry flag) from A 
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
    | HALT                            --Power down CPU until interupt
    | STOP                            --Halt CPU & LCD until button pressed
    | DI                              --Disable interrupts
    | EI                              --Enable interrupts
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
    | BITnr Word8                     --Test immediate in register
    | BITnHL Word8                    --Test immediate in (HL)
    | SETnr Register Word8            --Set bit b in register
    | SETnHL Word8                    --Set bit b in (HL)
    | RESnr Register Word8            --Reset bit b in register
    | RESnHL Word8                    --Reset bit b in (HL)
 --Jumps
    | JPnn Word16                     --Jump to address nn
    | JPccnn Word16 Word16            --Jump to nn if cc conditions are true (see specs)
    | JPHL                            --Jump to address (HL)
    | JPn Word8                       --Jump to (current address + n)
    | JRccn Word8 Word16              --Jump to (current address + n) if cc conditions are true (see specs)
 --Calls
    | CALLnn Word16                   --Push address of next instruction onto stack, and jump to address nn
    | CALLccnn Word16 Word16          --Call address nn if cc conditions are true (see specs)
 --Restarts
    | RSTn Word8                      --Push current address on stack, jump to address n
 --Returns
    | RET                             --Pop two bytes from stack, and jump to that address
    | RETcc Word16                    --RET if cc conditions are true (see specs)
    | RETI                            --RET, then enable interrupts

 
--Helper Functions  
 ld :: Emulator m => Register -> Either Register Word8 -> m ()
 ld reg1 (Left reg2) = do
   a <- load $ OneRegister reg2
   store (OneRegister reg1) a
 ld reg1 (Right word) = do
   store (OneRegister reg1) (MemVal8 word)

 data Flag = FlagZ | FlagN | FlagH | FlagC
 data Bit = Zero | One deriving Enum

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

 toBitNum :: (IsBit b) => b -> Int
 toBitNum bit = fromEnum.toBit $ bit

 set :: (Bits a, IsBit b) => a -> Int -> b -> a
 set num idx bit = case (toBitNum bit) of
   0 -> num `clearBit` idx
   1 -> num `setBit` idx

 updateFlag :: (IsBit b) => Flag -> b -> Word8 -> Word8
 updateFlag flag bit fReg = case flag of
   FlagZ -> set fReg 7 bit
   FlagN -> set fReg 6 bit
   FlagH -> set fReg 5 bit
   FlagC -> set fReg 4 bit


 updateFlags :: (Emulator m) => [(Flag,Bit)] -> m ()
 updateFlags xs = do
   MemVal8 fReg <- load (OneRegister F)
   let newfReg = foldl (\acc (flag,bit) -> updateFlag flag (toBit bit) acc) fReg xs
   store (OneRegister F) (MemVal8 newfReg)

 intify = toInteger.fromIntegral
------------------
 
 executeInstruction :: Emulator m => Instruction -> m ()
 executeInstruction instr = case instr of
--8-bit loads   
   LDrn reg imm -> ld reg (Right imm)
   LDrr reg1 reg2 -> ld reg1 (Left reg2)
   LDrHL reg -> do
     MemVal16 hl <- load (TwoRegister H L)
     mem <- load (MemAddr hl)
     store (OneRegister reg) mem
   LDrBC reg -> do
     MemVal16 bc <- load (TwoRegister B C)
     mem <- load (MemAddr bc)
     store (OneRegister reg) mem
   LDrDE reg -> do
     MemVal16 de <- load (TwoRegister D E)
     mem <- load (MemAddr de)
     store (OneRegister reg) mem
   LDrnn reg imm16 -> do
     m <- load (MemAddr imm16)
     store (OneRegister reg) m
   LDHLr reg -> do
     m <- load (OneRegister reg)
     MemVal16 mem <- load (TwoRegister H L)
     store (MemAddr mem) m
   LDBCr reg -> do
     m <- load (OneRegister reg)
     MemVal16 mem <- load (TwoRegister B C)
     store (MemAddr mem) m
   LDDEr reg -> do
     m <- load (OneRegister reg)
     MemVal16 mem <- load (TwoRegister D E)
     store (MemAddr mem) m
   LDHLn imm -> do
     MemVal16 mem <- load (TwoRegister H L)
     store (MemAddr mem) (MemVal16 imm)
   LDnnr imm16 reg -> do
     m <- load (OneRegister reg)
     store (MemAddr imm16) m
   LDrr' reg1 reg2 -> do
     MemVal8 loc <- load (OneRegister reg2)
     mem <- load $ MemAddr $ (0xFF00) + (fromIntegral loc)
     store (OneRegister reg1) mem
   LDrr'' reg1 reg2 -> do
     val <- load (OneRegister reg2)
     MemVal8 mem <- load (OneRegister reg1)
     store (MemAddr $ (0xFF00) + (fromIntegral mem)) val
   LDnr imm reg -> do
     val <- load (OneRegister reg)
     store (MemAddr $ 0xFF00 + (fromIntegral imm)) val
   LDrn' reg imm -> do
     val <- load (MemAddr $ 0xFF00 + (fromIntegral imm))
     store (OneRegister reg) val
--16-bit loads
   LDrrnn reg1 reg2 imm16 -> do
     store (TwoRegister reg1 reg2) (MemVal16 imm16 )
   LDSPHL -> do
     val <- load (TwoRegister H L)
     store SP val
   LDHLSPn imm -> do
     MemVal16 val <- load SP
     store (TwoRegister H L) $ MemVal16 $ val + (fromIntegral imm)
   LDnnSP imm16 -> store SP (MemVal16 imm16)
   PUSHrr reg1 reg2 -> do --left-most byte => SP-1, right-most byte => SP-2
     MemVal16 sp <- load SP
     hi <- load (OneRegister reg1)
     lo <- load (OneRegister reg2)
     store (MemAddr $ (sp-1) .&. 0xFFFF) hi
     store (MemAddr $ (sp-2) .&. 0xFFFF) lo
     store (SP) $ MemVal16 (sp-2)
   POPrr reg1 reg2 -> do --SP => right, SP+1 => left
     MemVal16 sp <- load SP
     MemVal8 right <- load (MemAddr $ sp)
     MemVal8 left <- load (MemAddr $ (sp+1) .&. 0xFFFF)
     store (TwoRegister reg1 reg2) $ MemVal16 $ (shiftL . fromIntegral) left 8 .|. fromIntegral right
     store (SP) $ MemVal16 $ (sp+2) & .&. 0xFFFF
--8-bit ALU
   ADDr reg -> do
     MemVal8 regVal <- load (OneRegister reg)
     MemVal8 aVal <- load (OneRegister A)
     let sum = regVal+aVal
     store (OneRegister A) (MemVal8 sum)
     MemVal8 regF <- load (OneRegister F)
     updateFlags [(FlagZ, toBit $ intify $ regVal+aVal),
                  (FlagN, Zero),
                  (FlagH, toBit $ sum .&. 0x0F < aVal .&. 0x0F)]
   
     
