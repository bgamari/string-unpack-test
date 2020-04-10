{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}

import Criterion
import Criterion.Main
import qualified Lazy
import qualified Chunked
import qualified ChunkedO2
import qualified ChunkedAddr
import qualified Unroll4
import GHC.Exts

data Lit = Lit Addr#

lit :: Lit
lit = Lit str
  where
    str :: Addr#
    str = "hello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello worldhello world"#

{-# INLINE withLit #-}
withLit :: (Addr# -> a) -> Lit -> a
withLit f (Lit s) = f s

main :: IO ()
main = do
  print $ withLit Unroll4.unpackCString8# lit
  print $ withLit Unroll4.unpackCString32# lit
  defaultMain
    [ bench "lazy" $ nf (withLit Lazy.unpackCString#) lit
    , bench "chunked-32" $ nf (withLit Chunked.unpackCString32#) lit
    , bench "chunked-O2-32" $ nf (withLit ChunkedO2.unpackCString32#) lit
    , bench "chunked-addr-32" $ nf (withLit ChunkedAddr.unpackCString32#) lit
    , bgroup "unrolled"
      [ bench "unrolled-32" $ nf (withLit Unroll4.unpackCString32#) lit
      , bench "unrolled-16" $ nf (withLit Unroll4.unpackCString16#) lit
      , bench "unrolled-8" $ nf (withLit Unroll4.unpackCString8#) lit
      , bench "unrolled-4" $ nf (withLit Unroll4.unpackCString4#) lit
      ]
    ]

