module Register 
       ( Register
       , Registers
       , makeRegisters
       )
where
       
import Control.Applicative
import Control.Monad.ST
import Data.Word
import Data.STRef

type Register s = STRef s Word32

-- See : http://problemkaputt.de/gbatek.htm#armcpuregisterset
data Registers s = Registers {
      r0 :: (Register s)
    , r1 :: (Register s)
    , r2 :: (Register s)
    , r3 :: (Register s)
    , r4 :: (Register s)
    , r5 :: (Register s)
    , r6 :: (Register s)
    , r7 :: (Register s)
    , r8 :: (Register s)
    , r8_fiq :: (Register s)
    , r9 :: (Register s)
    , r9_fiq :: (Register s)
    , r10 :: (Register s)
    , r10_fiq :: (Register s)
    , r11 :: (Register s)
    , r11_fiq :: (Register s)
    , r12 :: (Register s)
    , r12_fiq :: (Register s)
    , r13 :: (Register s)
    , r13_fiq :: (Register s)
    , r13_svc :: (Register s)
    , r13_abt :: (Register s)
    , r13_irq :: (Register s)
    , r13_und :: (Register s)
    , r14 :: (Register s)
    , r14_fiq :: (Register s)
    , r14_svc :: (Register s)
    , r14_abt :: (Register s)
    , r14_irq :: (Register s)
    , r14_und :: (Register s)
    , r15 :: (Register s)
    , cspr :: (Register s)
    , spsr_fiq :: (Register s)
    , spsr_svc :: (Register s)
    , spsr_abt :: (Register s)
    , spsr_irq :: (Register s)
    , spsr_und :: (Register s)
}

makeRegisters :: ST s (Registers s)
makeRegisters = let k = newSTRef 0 in Registers
    <$> k <*> k <*> k <*> k <*> k -- 5
    <*> k <*> k <*> k <*> k <*> k -- 10
    <*> k <*> k <*> k <*> k <*> k -- 15
    <*> k <*> k <*> k <*> k <*> k -- 20
    <*> k <*> k <*> k <*> k <*> k -- 25
    <*> k <*> k <*> k <*> k <*> k -- 30
    <*> k <*> k <*> k <*> k <*> k -- 35
    <*> k <*> k                   -- 37 total registers

