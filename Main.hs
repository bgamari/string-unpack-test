{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}

module Main(main) where

import Criterion
import Criterion.Main
import qualified Lazy
import qualified Chunked
import qualified ChunkedO2
import qualified ChunkedAddr
import qualified Unrolled
import qualified Unroll4

import Data.Foldable
import Data.Char
import GHC.Exts

data Lit = Lit Addr#

lit :: Lit
lit = Lit str
  where
    str :: Addr#
    str = "hello  worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello world"#

{-# INLINE withLit #-}
withLit :: (Addr# -> a) -> Lit -> a
withLit f (Lit s) = (f s)

hash :: String -> Int
hash = length -- foldl' (\hash c -> hash + ord c) 0

main :: IO ()
main = do
  print $ withLit Lazy.unpackCString# lit
  print $ withLit Unrolled.unpackCString4# lit
  defaultMain
    [ bench "lazy" $ whnf (length . withLit Lazy.unpackCString#) lit
    , bench "chunked-32" $ nf (length . withLit Chunked.unpackCString32#) lit
    , bench "chunked-O2-32" $ nf (length . withLit ChunkedO2.unpackCString32#) lit
    , bench "chunked-addr-32" $ nf (length . withLit ChunkedAddr.unpackCString32#) lit
    , bgroup "unrolled"
      [ bench "unrolled-32" $ nf (length . withLit Unrolled.unpackCString32#) lit
      , bench "unrolled-16" $ nf (length . withLit Unrolled.unpackCString16#) lit
      , bench "unrolled-8"  $ nf (length . withLit Unrolled.unpackCString8#) lit
      , bench "unrolled-4"  $ nf (length . withLit Unrolled.unpackCString4#) lit
      ]
    , bgroup "manually-unrolled"
      [ bench "unrolled-4"  $ nf (length . withLit Unroll4.unpackCString4#) lit
      , bench "unrolled-8"  $ nf (length . withLit Unroll4.unpackCString8#) lit
      ]
    ]

