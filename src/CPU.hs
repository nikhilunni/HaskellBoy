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
      LDrn Register Word8    --Load immediate into register
    | LDrr Register Register     --Load register into register
    | LDrHL Register    --Load (HL) into register
    | LDrBC Register    --Load (BC) into register
    | LDrDE Register    --Load (DE) into register
    | LDrnn Register Word16    --Load (immediate) into register
    | LDHLr Register    --Load register into (HL)
    | LDBCr Register    --Load register into (BC)
    | LDDEr Register    --Load register into (DE)
    | LDHLn Word8    --Load immediate into (HL)
    | LDnnr Word16 Register    --Load register into (immediate)
    | LDrr'     --Load (FF00 + register) into register
    | LDrr''   --Load register into (FF00 + register)
    | LDDrHL   --Load (HL) into register, decrement HL
    | LDDHLr   --Load register into (HL), decrement HL
    | LDIrHL   --Load (HL) into register, increment HL
    | LDIHLr   --Load register into (HL), increment HL
    | LDnr     --Load register into (FF00 + immediate)
    | LDrn'    --Load (FF00 + immediate) into register
 --16-bit loads
    | LDrrnn   --Load immediate into (register,register)
    | LDSPHL   --Load HL into SP
    | LDHLSPn  --Load SP+n into (HL)
    | LDnnSP   --Load SP into (nn)
    | PUSHnn   --Push (Register,Register) onto Stack, decrement SP twice
    | POPnn   --Pop two bytes off stack into (Register,Register), increment SP twice
 --8-bit ALU
    | ADDr     --Add register to A
    | ADDn     --Add immediate to A
    | ADDHL    --Add (HL) to 
    | ADCr     --Add (register+carry flag) to A
    | ADCn     --Add (immediate+carry flag) to A
    | ADCHL    --Add ((HL)+carry flag) to A
    | SUBr     --Sub register from A
    | SUBn     --Sub immediate from A
    | SUBHL    --Sub (HL) from A
    | SBCr     --Sub (register+carry flag) from A 
    | SBCn     --Sub (immediate+carry flag) from A 
    | SBCHL    --Sub ((HL)+carry flag) from A 
    | ANDr     --AND register with A
    | ANDn     --AND immediate with A
    | ANDHL    --AND (HL) with A
    | ORr      --OR register with A
    | ORn      --OR immediate with A
    | ORHL     --OR (HL) with A
    | XORr     --XOR register with A
    | XORn     --XOR immediate with A
    | XORHL    --XOR (HL) with A
    | CPr      --Compare register with A
    | CPn      --Compare immediate with A
    | CPHL     --Compare (HL) with A
    | INCr     --Increment register
    | INCHL    --Increment (HL)
    | DECr     --Decrement register
    | DECHL    --Decrement (HL)
 --16-bit ALU
    | ADDHLrr  --Add (Register,Register) to HL
    | ADDSPn   --Add immediate to SP
    | INCrr    --Increment (Register,Register)
    | INCSP    --Increment SP
    | DECrr    --Decrement (Register,Register)
    | DECSP    --Decrement SP
 --Misc.
    | SWAPr    --Swap upper & lower nibles of register
    | SWAPHL   --Swap upper & lower nibles of (HL)
    | DAA      --Decimal adjust A
    | CPL      --Complement A
    | CCF      --Complement carry flag
    | SCF      --Set carry flag
    | NOP      --No operation
    | HALT     --Power down CPU until interupt
    | STOP     --Halt CPU & LCD until button pressed
    | DI       --Disable interrupts
    | EI       --Enable interrupts
 --Rotates & Shifts
    | RLCr     --Rotate register left. Old bit 7 to carry flag
    | RLCHL    --Rotate (HL) left. Old bit 7 to carry flag
    | RLr      --Rotate register left through carry flag
    | RLHL     --Rotate (HL) left through carry flag
    | RRCr     --Rotate register right. Old bit 0 to carry flag
    | RRCHL    --Rotate (HL) right. Old bit 0 to carry flag
    | RRr      --Rotate register right through carry flag
    | RRHL     --Rotate (HL) right through carry flag
    | SLAr     --Shift register left into carry. LSB set to 0
    | SLAHL    --Shift (HL) left into carry. LSB set to 0
    | SRAr     --Shift register right into carry. MSB doesn't change.
    | SRAHL    --Shift (HL) right into carry. MSB doesn't change.
    | SRLr     --Shift register right into carry. MSB set to 0
    | SRLHL    --Shift (HL) right into carry. MSB set to 0
 --Bit Opcodes
    | BITnr    --Test immediate in register
    | BITnHL   --Test immediate in (HL)
    | SETnr    --Set bit b in register
    | SETnHL   --Set bit b in (HL)
    | RESnr    --Reset bit b in register
    | RESnHL   --Reset bit b in (HL)
 --Jumps
    | JPnn     --Jump to address nn
    | JPccnn   --Jump to nn if cc conditions are true (see specs)
    | JPHL     --Jump to address (HL)
    | JPn      --Jump to (current address + n)
    | JRccn    --Jump to (current address + n) if cc conditions are true (see specs)
 --Calls
    | CALLnn   --Push address of next instruction onto stack, and jump to address nn
    | CALLccnn --Call address nn if cc conditions are true (see specs)
 --Restarts
    | RSTn     --Push current address on stack, jump to address n
 --Returns
    | RET      --Pop two bytes from stack, and jump to that address
    | RETcc    --RET if cc conditions are true (see specs)
    | RETI     --RET, then enable interrupts

  
  
 ld :: Emulator m => Register -> Either Register Word8 -> m ()
 ld reg1 (Left reg2) = do
   a <- load $ OneRegister reg2
   store (OneRegister reg1) a
 ld reg1 (Right word) = do
   store (OneRegister reg1) (MemVal8 word)

 
-- executeInstruction :: Emulator m => Word8 -> m ()
-- executeInstruction instr = case instr of
--   0x06 -> ld Register 
            
   
