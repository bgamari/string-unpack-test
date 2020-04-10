{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Unroll where

data Nat = Zero | Succ Nat

class Unroll (n :: Nat) where
  unroll :: (a -> a) -> (a -> a)

instance Unroll n => Unroll ('Succ n) where
  unroll f = f . unroll @n f
  {-# INLINE unroll #-}

instance Unroll 'Zero where
  unroll = id
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

type N8 = N4 + N4
type N16 = N8 + N8
type N32 = N16 + N16

-- Testing
five :: Int
five = plusFive 0

plusFive :: Int -> Int
plusFive = unroll @N4 succ
