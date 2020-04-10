{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}

module Lazy (unpackAppendCString#, unpackCString#) where

import GHC.Exts

unpackAppendCString'# :: [Char] -> Addr# -> [Char]
{-# INLINE unpackAppendCString'# #-}
unpackAppendCString'# rest0 addr0 = go addr0
  where
    go :: Addr# -> [Char]
    go addr
      | isTrue# (ch `eqChar#` '\0'#) = rest0
      | True =
          let rest = go (addr `plusAddr#` 1#)
          in C# ch : rest
        where
          -- See Note [unpackCString# iterating over addr]
          !ch = indexCharOffAddr# addr 0#

unpackAppendCString# :: Addr# -> [Char] -> [Char]
{-# NOINLINE unpackAppendCString# #-}
unpackAppendCString# addr rest = unpackAppendCString'# rest addr

unpackCString# :: Addr# -> [Char]
{-# NOINLINE CONLIKE unpackCString# #-}
     -- See the NOINLINE note on unpackCString#
unpackCString# addr = unpackAppendCString'# [] addr

