{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -O1 #-}

module Unrolled (
  unpackAppendCString4#, unpackCString4#,
  unpackAppendCString8#, unpackCString8#,
  unpackAppendCString16#, unpackCString16#,
  unpackAppendCString32#, unpackCString32#
) where

import GHC.Exts
import Unroll

unpackChar# :: Int# -> Addr# -> Maybe Char
unpackChar# off addr
  | isTrue# (ch `eqChar#` '\0'#) = Nothing
  | True = Just (C# ch)
  where !ch = indexCharOffAddr# addr off
{-# INLINE unpackChar# #-}

type DiffList a = [a] -> [a]
orEnd :: DiffList a -> Maybe a -> DiffList a
orEnd dl Nothing = dl
orEnd dl (Just x) = dl . (x:)
{-# INLINE orEnd #-}

----
unpackAppendCString4# :: Addr# -> [Char] -> [Char]
{-# NOINLINE unpackAppendCString4# #-}
unpackAppendCString4# addr rest = unpackAppendCString'# @N4 rest addr

unpackCString4# :: Addr# -> [Char]
{-# NOINLINE CONLIKE unpackCString4# #-}
unpackCString4# addr = unpackAppendCString'# @N4 [] addr
----
unpackAppendCString8# :: Addr# -> [Char] -> [Char]
{-# NOINLINE unpackAppendCString8# #-}
unpackAppendCString8# addr rest = unpackAppendCString'# @N8 rest addr

unpackCString8# :: Addr# -> [Char]
{-# NOINLINE CONLIKE unpackCString8# #-}
unpackCString8# addr = unpackAppendCString'# @N8 [] addr
----
unpackAppendCString16# :: Addr# -> [Char] -> [Char]
{-# NOINLINE unpackAppendCString16# #-}
unpackAppendCString16# addr rest = unpackAppendCString'# @N16 rest addr

unpackCString16# :: Addr# -> [Char]
{-# NOINLINE CONLIKE unpackCString16# #-}
unpackCString16# addr = unpackAppendCString'# @N16 [] addr
----
unpackAppendCString32# :: Addr# -> [Char] -> [Char]
{-# NOINLINE unpackAppendCString32# #-}
unpackAppendCString32# addr rest = unpackAppendCString'# @N32 rest addr

unpackCString32# :: Addr# -> [Char]
{-# NOINLINE CONLIKE unpackCString32# #-}
unpackCString32# addr = unpackAppendCString'# @N32 [] addr
----

unpackAppendCString'# :: forall (n::Nat). (IsNatural n, Unroll n)
                      => [Char] -> Addr# -> [Char]
{-# INLINE unpackAppendCString'# #-}
unpackAppendCString'# rest0 addr0 = goStrict addr0
  where
    goStrict :: Addr# -> [Char]
    goStrict addr =
        --case unpackChar# 0# addr of
        --  Nothing -> rest0
        --  Just c  -> c : (...)
        unroll @n unpackOne (goStrict (addr `plusAddr#` chunkSize))
      where
        unpackOne :: Int -> [Char] -> [Char]
        unpackOne off rest =
          case unpackChar# off# addr of
            Nothing  -> rest0
            Just c   -> c : rest
          where
            I# off# = off
        {-# INLINE unpackOne #-}

    I# chunkSize = natValue @n

