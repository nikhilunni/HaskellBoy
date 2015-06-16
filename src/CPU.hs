module CPU where

import Data.Word
import Data.Int
import Data.Bits
import Register
import Data.STRef
import Control.Monad.ST
import Control.Applicative

data CPUState = 
       ARM 
     | THUMB

data CPUMode = 
       User
     | FIQ
     | Supervisor
     | Abort
     | IRQ
     | Undefined

type State s = STRef s CPUState
type Mode s = STRef s CPUMode

cpuInit :: 
