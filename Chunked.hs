{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -O1 #-}

module Chunked (
  unpackAppendCString32#, unpackCString32#
) where

import GHC.Exts

unpackAppendCString'# :: Int# -> [Char] -> Addr# -> [Char]
{-# INLINE unpackAppendCString'# #-}
unpackAppendCString'# unpackChunkLen rest0 addr0 = goStrict addr0 unpackChunkLen
  where
    unpackChunk :: Addr# -> [Char]
    unpackChunk addr = goStrict addr unpackChunkLen

    goStrict :: Addr# -> Int# -> [Char]
    goStrict addr n
      | isTrue# (ch `eqChar#` '\0'#) = rest0
      | isTrue# (n ==# 0#) =
          -- We've reached the end of our chunk, lazily unpack the next chunk
          let rest = unpackChunk (addr `plusAddr#` 1#)
          in C# ch : rest
      | True =
          let !rest = goStrict (addr `plusAddr#` 1#) (n -# 1#)
          in C# ch : rest
        where
          -- See Note [unpackCString# iterating over addr]
          !ch = indexCharOffAddr# addr 0#

unpackAppendCString32# :: Addr# -> [Char] -> [Char]
{-# NOINLINE unpackAppendCString32# #-}
unpackAppendCString32# addr rest = unpackAppendCString'# 32# rest addr

unpackCString32# :: Addr# -> [Char]
{-# NOINLINE CONLIKE unpackCString32# #-}
unpackCString32# addr = unpackAppendCString'# 32# [] addr

