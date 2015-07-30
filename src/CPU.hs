module CPU
       (--execute
       ) where

 import Data.Word
 import Data.Either
 
 import Memory
 import Monad

 data Instruction =   LDrn  --Load immediate into register
                    | LDrr  --Load register into register
                    | LDrHL --Load (HL) into register
                    | LDrBC --Load (BC) into register
                    | LDrDE --Load (DE) into register
                    | LDHLr --Load register into (HL)
                    | LDBCr --Load register into (BC)
                    | LDDEr --Load register into (DE)
                    | LDHLn --Load immediate into (HL)
  
  
 ld :: Emulator m => Register -> Either Register Word8 -> m ()
 ld reg1 (Left reg2) = do
   a <- load $ OneRegister reg2
   store (OneRegister reg1) a
 ld reg1 (Right word) = do
   store (OneRegister reg1) (MemVal8 word)

 
 executeInstruction :: Emulator m => Word8 -> m ()
 executeInstruction instr = case instr of
   0x06 -> ld Register 
            
   
