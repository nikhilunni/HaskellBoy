module CPU
       (--execute
       ) where

 import Data.Word
 import Data.Either
 
 import Memory
 import Monad

 data Instruction =   
 --See : http://marc.rawer.de/Gameboy/Docs/GBCPUman.pdf
 --8-bit loads
      LDrn    --Load immediate into register
    | LDrr    --Load register into register
    | LDrHL   --Load (HL) into register
    | LDrBC   --Load (BC) into register
    | LDrDE   --Load (DE) into register
    | LDrnn   --Load (immediate) into register
    | LDHLr   --Load register into (HL)
    | LDBCr   --Load register into (BC)
    | LDDEr   --Load register into (DE)
    | LDHLn   --Load immediate into (HL)
    | LDnnr   --Load register into (immediate)
    | LDrr'   --Load (FF00 + register) into register
    | LDrr''  --Load register into (FF00 + register)
    | LDDrHL  --Load (HL) into register, decrement HL
    | LDDHLr  --Load register into (HL), decrement HL
    | LDIrHL  --Load (HL) into register, increment HL
    | LDIHLr  --Load register into (HL), increment HL
    | LDnr    --Load register into (FF00 + immediate)
    | LDrn'   --Load (FF00 + immediate) into register
 --16-bit loads
    | LDrrnn  --Load immediate into (register,register)
    | LDSPHL  --Load HL into SP
    | LDHLSPn --Load SP+n into (HL)
    | LDnnSP  --Load SP into (nn)
    | PUSHnn  --Push (Register,Register) onto Stack, decrement SP twice
    | POPnn   --Pop two bytes off stack into (Register,Register), increment SP twice
 --8-bit ALU
    | ADDr    --Add register to A
    | ADDn    --Add immediate to A
    | ADDHL   --Add (HL) to 
    | ADCr    --Add (register+carry flag) to A
    | ADCn    --Add (immediate+carry flag) to A
    | ADCHL   --Add ((HL)+carry flag) to A
    | SUBr    --Sub register from A
    | SUBn    --Sub immediate from A
    | SUBHL   --Sub (HL) from A
    | SBCr    --Sub (register+carry flag) from A 
    | SBCn    --Sub (immediate+carry flag) from A 
    | SBCHL   --Sub ((HL)+carry flag) from A 
    | ANDr    --AND register with A
    | ANDn    --AND immediate with A
    | ANDHL   --AND (HL) with A
    | ORr     --OR register with A
    | ORn     --OR immediate with A
    | ORHL    --OR (HL) with A
    | XORr    --XOR register with A
    | XORn    --XOR immediate with A
    | XORHL   --XOR (HL) with A
    | CPr     --Compare register with A
    | CPn     --Compare immediate with A
    | CPHL    --Compare (HL) with A
    | INCr    --Increment register
    | INCHL   --Increment (HL)
    | DECr    --Decrement register
    | DECHL   --Decrement (HL)
 --16-bit ALU
    | ADDHLrr --Add (Register,Register) to HL
    | ADDSPn  --Add immediate to SP
    | INCrr   --Increment (Register,Register)
    | INCSP   --Increment SP
    | DECrr   --Decrement (Register,Register)
    | DECSP   --Decrement SP
 --Misc.
    | SWAPr   --Swap upper & lower nibles of register
    | SWAPHL  --Swap upper & lower nibles of (HL)
    | DAA     --Decimal adjust A
    | CPL     --Complement A
    | CCF     --Complement carry flag
    | SCF     --Set carry flag
    | NOP     --No operation
    | HALT    --Power down CPU until interupt
    | STOP    --Halt CPU & LCD until button pressed
    | DI      --Disable interrupts
    | EI      --Enable interrupts
 --Rotates & Shifts
    | RLCA    --Rotate A left. Old bit 7 to carry flag
    | RLA     --Rotate A left through carry flag
    | RRCA    --Rotate A right. Old bit 0 to carry flag
    | RRA     --Rotate A right through carry flag
  
  
 ld :: Emulator m => Register -> Either Register Word8 -> m ()
 ld reg1 (Left reg2) = do
   a <- load $ OneRegister reg2
   store (OneRegister reg1) a
 ld reg1 (Right word) = do
   store (OneRegister reg1) (MemVal8 word)

 
 executeInstruction :: Emulator m => Word8 -> m ()
 executeInstruction instr = case instr of
   0x06 -> ld Register 
            
   
