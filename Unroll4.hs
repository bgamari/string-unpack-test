{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -O1 #-}

module Unroll4 (
  unpackAppendCString4#, unpackCString4#,
  unpackAppendCString8#, unpackCString8#,
) where

import GHC.Exts

unpackChar# :: Int# -> Addr# -> Maybe Char
unpackChar# off addr
  | isTrue# (ch `eqChar#` '\0'#) = Nothing
  | True = Just (C# ch)
  where !ch = indexCharOffAddr# addr off
{-# INLINE unpackChar# #-}

----
unpackAppendCString4# :: Addr# -> [Char] -> [Char]
{-# NOINLINE unpackAppendCString4# #-}
unpackAppendCString4# addr rest = unpackAppendCString4'# rest addr

unpackCString4# :: Addr# -> [Char]
{-# NOINLINE CONLIKE unpackCString4# #-}
unpackCString4# addr = unpackAppendCString4'# [] addr
----
unpackAppendCString8# :: Addr# -> [Char] -> [Char]
{-# NOINLINE unpackAppendCString8# #-}
unpackAppendCString8# addr rest = unpackAppendCString8'# rest addr

unpackCString8# :: Addr# -> [Char]
{-# NOINLINE CONLIKE unpackCString8# #-}
unpackCString8# addr = unpackAppendCString8'# [] addr
----

unpackAppendCString4'# :: [Char] -> Addr# -> [Char]
{-# INLINE unpackAppendCString4'# #-}
unpackAppendCString4'# rest0 addr0 = goStrict addr0
  where
    goStrict :: Addr# -> [Char]
    goStrict addr =
          unpackOne 0#
        $ unpackOne 1#
        $ unpackOne 2#
        $ unpackOne 3#
        $ goStrict (addr `plusAddr#` 4#)
      where
        unpackOne :: Int# -> [Char] -> [Char]
        unpackOne off rest =
          case unpackChar# off addr of
            Nothing -> rest0
            Just c  -> c : rest
        {-# INLINE unpackOne #-}

unpackAppendCString8'# :: [Char] -> Addr# -> [Char]
{-# INLINE unpackAppendCString8'# #-}
unpackAppendCString8'# rest0 addr0 = goStrict addr0
  where
    goStrict :: Addr# -> [Char]
    goStrict addr =
          unpackOne 0#
        $ unpackOne 1#
        $ unpackOne 2#
        $ unpackOne 3#
        $ unpackOne 4#
        $ unpackOne 5#
        $ unpackOne 6#
        $ unpackOne 7#
        $ goStrict (addr `plusAddr#` 8#)
      where
        unpackOne :: Int# -> [Char] -> [Char]
        unpackOne off rest =
          case unpackChar# off addr of
            Nothing -> rest0
            Just c  -> c : rest
        {-# INLINE unpackOne #-}
