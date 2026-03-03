{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module JIT.Pointers
  ( withMemoryPtr
  , withRegistersPtr
  ) where

import Data.Word
import Data.Array.Base (STUArray(..))
import Foreign (Ptr)
import GHC.Exts (Ptr(..), mutableByteArrayContents#, touch#)
import GHC.IO (IO(..))
import Control.Monad.ST (RealWorld)

import Types (Memory(..))

-- | Get a Ptr to the underlying MutableByteArray# of an unboxed array.
-- The pointer is valid only during the callback. The array is kept alive
-- via touch# at the end.
-- Safe when used with unsafe FFI calls (GC won't run during the call).
withSTUArrayPtr :: STUArray RealWorld i Word8 -> (Ptr Word8 -> IO a) -> IO a
withSTUArrayPtr arr@(STUArray _ _ _ marr#) action = do
  let ptr = Ptr (mutableByteArrayContents# marr#)
  result <- action ptr
  -- Keep the array alive past the action using touch#
  IO $ \s -> case touch# arr s of s' -> (# s', () #)
  return result

-- | Extract pointer to the main 64K memory array
withMemoryPtr :: Memory RealWorld -> (Ptr Word8 -> IO a) -> IO a
withMemoryPtr mem = withSTUArrayPtr (memory mem)

-- | Extract pointer to the 8-byte register array
withRegistersPtr :: Memory RealWorld -> (Ptr Word8 -> IO a) -> IO a
withRegistersPtr mem = withSTUArrayPtr (registers mem)
