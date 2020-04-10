{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}

module ChunkedAddr (
  unpackAppendCString32#, unpackCString32#
) where

import GHC.Exts

unpackAppendCString'# :: Int# -> [Char] -> Addr# -> [Char]
{-# INLINE unpackAppendCString'# #-}
unpackAppendCString'# unpackChunkMask rest0 addr0 = goStrict addr0
  where
    unpackChunk :: Addr# -> [Char]
    unpackChunk addr = goStrict addr

    goStrict :: Addr# -> [Char]
    goStrict addr
      | isTrue# (ch `eqChar#` '\0'#) = rest0
      | isTrue# (addr2Int# addr `andI#` unpackChunkMask ==# 0#) =
          -- We've reached the end of our chunk, lazily unpack the next chunk
          let rest = unpackChunk (addr `plusAddr#` 1#)
          in C# ch : rest
      | True =
          let !rest = goStrict (addr `plusAddr#` 1#)
          in C# ch : rest
        where
          -- See Note [unpackCString# iterating over addr]
          !ch = indexCharOffAddr# addr 0#

unpackAppendCString32# :: Addr# -> [Char] -> [Char]
{-# NOINLINE unpackAppendCString32# #-}
unpackAppendCString32# addr rest = unpackAppendCString'# 31# rest addr

unpackCString32# :: Addr# -> [Char]
{-# NOINLINE CONLIKE unpackCString32# #-}
unpackCString32# addr = unpackAppendCString'# 31# [] addr

