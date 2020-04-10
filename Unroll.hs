{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}

module Unroll where

data Nat = Zero | Succ Nat

class IsNatural n => Unroll (n :: Nat) where
  unroll :: (Int -> a -> a) -> (a -> a)

instance Unroll n => Unroll ('Succ n) where
  unroll f = unroll @n f . f (natValue @('Succ n))
  {-# INLINE unroll #-}

instance Unroll 'Zero where
  unroll f = f 0
  {-# INLINE unroll #-}

type N0 = Zero
type N1 = Succ N0
type N2 = Succ N1
type N3 = Succ N2
type N4 = Succ N3

type family (+) (n :: Nat) (m :: Nat) :: Nat where
  n + Zero   = n
  n + Succ m = Succ (n + m)

type family (:*) (n :: Nat) (m :: Nat) :: Nat where
  n :* Zero   = Zero
  n :* Succ m = n + (n :* m)

type family Pred (n :: Nat) where
  Pred ('Succ m) = m

type N8  = N4 + N4
type N16 = N8 + N8
type N32 = N16 + N16

class IsNatural (n :: Nat) where
  natValue :: Int

instance IsNatural 'Zero where
  natValue = 0

instance IsNatural n => IsNatural ('Succ n) where
  natValue = 1 + natValue @n

-- Testing
four :: Int
four = plusFour 0

plusFour :: Int -> Int
plusFour = unroll @N3 (const succ)

ints :: [Int]
ints = unroll @N3 (:) []
