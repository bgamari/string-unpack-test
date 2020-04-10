{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}

import Criterion
import Criterion.Main
import qualified Lazy
import qualified Chunked
import qualified ChunkedO2
import qualified ChunkedAddr
import GHC.Exts

data Lit = Lit Addr#

lit :: Lit
lit = Lit str
  where
    str :: Addr#
    str = "hello world this is a string haoh;flakhg; oiahwer;gliah;seofgiya ;o94ty htg;skjhd;khaf;khasd;fkha;sdfgha;kjsdghalkjhglsdhfglkajhdgkjuasdhglkjahsdflkajsdhdlkjhsflkjashdflkjhasdjlfkhals"#

{-# INLINE withLit #-}
withLit :: (Addr# -> a) -> Lit -> a
withLit f (Lit s) = f s

main :: IO ()
main = do
  defaultMain
    [ bench "lazy" $ nf (withLit Lazy.unpackAppendCString#) lit
    , bench "chunked-32" $ nf (withLit Chunked.unpackAppendCString32#) lit
    , bench "chunked-O2-32" $ nf (withLit ChunkedO2.unpackAppendCString32#) lit
    -- , bench "unroll4" $ nf (withLit Unroll4.unpackAppendCString#) lit
    , bench "chunked-addr-32" $ nf (withLit ChunkedAddr.unpackAppendCString32#) lit
    ]

