module BatchingSpec where

import           Codd                           ( CheckHashes(..)
                                                , applyMigrations
                                                , applyMigrationsNoCheck
                                                )
import           Codd.Analysis                  ( MigrationCheck(..)
                                                , checkMigration
                                                )
import           Codd.Environment               ( CoddSettings(..) )
import           Codd.Hashing                   ( readHashesFromDatabaseWithSettings
                                                )
import           Codd.Hashing.Types             ( DbHashes(..)
                                                , ObjHash(..)
                                                )
import           Codd.Internal                  ( baseApplyMigsBlock
                                                , beginCommitTxnBracket
                                                , collectAndApplyMigrations
                                                , withConnection
                                                )
import           Codd.MultiQueryStatement
                                                ( SqlStatementException )
import Codd.MultiQueryStatement (BatchedSqlStatements(..), batchCopyRows)
import           Codd.Parsing                   ( AddedSqlMigration(..)
                                                , SqlMigration(..)
                                                , SqlPiece(..)
                                                , parseSqlPiecesStreaming
                                                , sqlPieceText
                                                )
import           Codd.Query                     ( unsafeQuery1 )
import           Codd.Types                     ( RetryBackoffPolicy(..)
                                                , RetryPolicy(..)
                                                , TxnIsolationLvl(..)
                                                )
import           Control.Monad                  ( forM_
                                                , void
                                                , when
                                                )
import           Control.Monad.Logger           ( LogStr
                                                , LoggingT(runLoggingT)
                                                , fromLogStr
                                                , runStdoutLoggingT
                                                )
import           Control.Monad.Trans            ( lift )
import           Control.Monad.Trans.Resource   ( MonadThrow )
import qualified Data.List                     as List
import qualified Data.Map.Strict               as Map
import           Data.Text                      ( Text
                                                , unpack
                                                )
import qualified Data.Text                     as Text
import           Data.Text.Encoding             ( decodeUtf8 )
import qualified Data.Text.IO                  as Text
import           Data.Time                      ( UTCTime
                                                , diffUTCTime
                                                , secondsToDiffTime
                                                , secondsToNominalDiffTime
                                                )
import qualified Database.PostgreSQL.Simple    as DB
import           Database.PostgreSQL.Simple     ( ConnectInfo(..) )
import           DbUtils                        ( aroundFreshDatabase
                                                , createTestUserMig
                                                , createTestUserMigPol
                                                , finallyDrop
                                                , fixMigsOrder
                                                , getIncreasingTimestamp
                                                , mkValidSql
                                                , shouldBeStrictlySortedOn
                                                , testCoddSettings
                                                , testConnInfo
                                                , testConnTimeout
                                                )
import GHC.Generics (Generic)
import qualified Streaming.Prelude as Streaming
import           Test.Hspec
import           Test.Hspec.Expectations
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import           Test.QuickCheck
import qualified Test.QuickCheck               as QC
import           UnliftIO                       ( MonadIO
                                                , liftIO
                                                , stdout
                                                )
import           UnliftIO.Concurrent            ( MVar
                                                , modifyMVar_
                                                , newMVar
                                                , readMVar
                                                )



spec :: Spec
spec =
    describe "Statement batching tests" $ do
        modifyMaxSuccess (const 1000) $ it "COPY row batching works" $ property $ \CopyRows { minBatchSize, rows } -> do
            let
                copyStatement =
                    [ "COPY x (col) FROM STDIN WITH (FORMAT CSV);\n" ]
                    ++ rows
                    ++ [ "\\.\n" ]
            
            batchedStms <- Streaming.toList_ (batchCopyRows minBatchSize $ parseSqlPiecesStreaming $ Streaming.each copyStatement)
            let rowBatchesOnly = [ r | BatchedCopyRows r <- batchedStms ]
            -- print $ CopyRows {..}

            -- 1. Make sure only row contents are batched
            Text.concat rowBatchesOnly `shouldBe` Text.concat rows

            -- 2. Batch sizes must be >= minBatchSize, except for the last batch.
            drop 1 (reverse rowBatchesOnly) `shouldSatisfy` all ((>= minBatchSize) . Text.length)

            -- 3. A stronger requirement: full content matching
            Text.concat (map (\case
                StandaloneSqlPiece p -> sqlPieceText p
                BatchedCopyRows r -> r) batchedStms) `shouldBe` Text.concat copyStatement

-- | Used to generate rows for a single-column COPY statement that are all close
-- to in size to `minBatchSize`, sometimes all having the exact size, sometimes
-- having slightly under or above it, to stress test our batching implementation.
data CopyRows = CopyRows {
    minBatchSize :: Int
    , rows :: [Text]
}
    deriving stock (Generic, Show)

instance Arbitrary CopyRows where
    arbitrary = do
        -- No need for large batch sizes; exploring the different code paths
        -- depends only on whether the length of contents is a multiple of
        -- minBatchSize.
        minBatchSize <- QC.chooseBoundedIntegral (1, 10)
        rows <- QC.listOf $ QC.oneof $ 
            [ exactSizeRowGen minBatchSize, allLargerRowGen minBatchSize, mixedRowGen minBatchSize ]
            ++ if minBatchSize <= 1 then [] else [ allSmallerRowGen minBatchSize ]

        pure $ CopyRows minBatchSize rows
        where
            arbchar = arbitrary @Char `suchThat` (\c -> c /= '\n' && c /= '\\')
            exactSizeRowGen bs = do
                c <- arbchar
                pure $ (Text.concat $ map Text.singleton $ take (bs - 1) $ repeat c) <> "\n"
            allSmallerRowGen bs = do
                c <- arbchar
                n <- QC.chooseBoundedIntegral (0, bs)
                pure $ (Text.concat $ map Text.singleton $ take n $ repeat c) <> "\n"
            allLargerRowGen bs = do
                c <- arbchar
                n <- QC.chooseBoundedIntegral (bs, bs + 1000)
                pure $ (Text.concat $ map Text.singleton $ take n $ repeat c) <> "\n"
            mixedRowGen bs = do
                c <- arbchar
                n <- QC.chooseBoundedIntegral (0, bs + 100)
                pure $ (Text.concat $ map Text.singleton $ take n $ repeat c) <> "\n"
    -- shrink v = filter ((> 0) . minBatchSize) $ QC.genericShrink v