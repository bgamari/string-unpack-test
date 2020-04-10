{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -O1 #-}

module Unroll4 (
  unpackAppendCString#,
  unpackCString#
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

unpackAppendCString# :: Addr# -> [Char] -> [Char]
{-# NOINLINE unpackAppendCString# #-}
unpackAppendCString# addr rest = unpackAppendCString'# rest addr

unpackCString# :: Addr# -> [Char]
{-# NOINLINE CONLIKE unpackCString# #-}
unpackCString# addr = unpackAppendCString'# [] addr

unpackAppendCString'# :: [Char] -> Addr# -> [Char]
{-# INLINE unpackAppendCString'# #-}
unpackAppendCString'# rest0 addr0 = goStrict addr0
  where
    goStrict :: Addr# -> [Char]
    goStrict addr =
      --case unpackChar# 0# addr of
      --  Nothing -> rest0
      --  Just c  -> c : (...)
      snd $ unroll @(Pred N32) (unpackOne rest0) (Addr addr, goStrict (addr `plusAddr#` 32#))

data Addr = Addr Addr#

-- TODO: This is wrong; it will be backwards
unpackOne :: [Char] -> (Addr, [Char]) -> (Addr, [Char])
unpackOne rest0 (Addr addr, rest) =
  case unpackChar# 0# addr of
    Nothing  -> (Addr addr, rest0)
    Just c   -> (Addr (addr `plusAddr#` 1#), c : rest)

