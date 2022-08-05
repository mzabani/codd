{-# LANGUAGE NumericUnderscores #-}
module Main where

import Codd.Parsing (SqlPiece (..), PureStream (..), SqlMigration (..), ParsedSql (..), parseSqlMigration)
import Criterion.Main (defaultMain, bgroup, bench, nfIO)
import Data.Text (Text)
import Streaming (Of)
import Streaming.Prelude (Stream)
import qualified Streaming.Prelude as Streaming

manySelect1s :: Monad m => Int -> Stream (Of Text) m ()
manySelect1s size = Streaming.replicate size "SELECT 1;"

veryLargeCopy :: Monad m => Int -> Stream (Of Text) m ()
veryLargeCopy size = Streaming.yield "COPY sometable FROM STDIN WITH (FORMAT csv);\n" >> Streaming.replicate size "4,Some name,2020-01-01\n" >> Streaming.yield "\\.\n"

parseMig :: Stream (Of Text) IO () -> IO (Stream (Of SqlPiece) IO ())
parseMig contents = do
    Right (SqlMigration { migrationSql } :: SqlMigration IO) <- parseSqlMigration @IO "any-name.sql" (PureStream contents)
    case migrationSql of
        UnparsedSql _ -> error "Error parsing SQL"
        WellParsedSql s -> pure s

main :: IO ()
main = defaultMain [
        bgroup "Streaming parser with 'SELECT 1;'" [
            bench "10_000" $ nfIO $ parseMig (manySelect1s 10_000) >>= Streaming.any_ (== CommentPiece "Not in the stream")
            , bench "100_000" $ nfIO $ parseMig (manySelect1s 100_000) >>= Streaming.any_ (== CommentPiece "Not in the stream")
            , bench "1_000_000" $ nfIO $ parseMig (manySelect1s 1_000_000) >>= Streaming.any_ (== CommentPiece "Not in the stream")
        ]
        , bgroup "Streaming parser with COPY statement" [
            bench "10_000" $ nfIO $ parseMig (veryLargeCopy 10_000) >>= Streaming.any_ (== CommentPiece "Not in the stream")
            , bench "100_000" $ nfIO $ parseMig (veryLargeCopy 100_000) >>= Streaming.any_ (== CommentPiece "Not in the stream")
            , bench "1_000_000" $ nfIO $ parseMig (veryLargeCopy 1_000_000) >>= Streaming.any_ (== CommentPiece "Not in the stream")
        ]
    ]
