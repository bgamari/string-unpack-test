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
  print $ withLit Unroll4.unpackCString# lit
  defaultMain
    [ bench "lazy" $ nf (withLit Lazy.unpackCString#) lit
    , bench "chunked-32" $ nf (withLit Chunked.unpackCString32#) lit
    , bench "chunked-O2-32" $ nf (withLit ChunkedO2.unpackCString32#) lit
    , bench "unroll4" $ nf (withLit Unroll4.unpackCString#) lit
    , bench "chunked-addr-32" $ nf (withLit ChunkedAddr.unpackCString32#) lit
    ]

