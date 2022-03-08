module Codd.AppCommands
    ( timestampAndMoveMigrationFile
    ) where

import           Codd.Parsing                   ( toMigrationTimestamp )
import           Codd.Types                     ( SqlFilePath(..) )
import qualified Data.Text                     as Text
import           Data.Time.Clock                ( getCurrentTime )
import           Data.Time.Format.ISO8601       ( iso8601Show )
import           System.FilePath                ( (</>)
                                                , takeFileName
                                                )
import           UnliftIO                       ( MonadIO(..) )
import           UnliftIO.Directory             ( copyFile
                                                , removeFile
                                                )

timestampAndMoveMigrationFile
    :: MonadIO m => SqlFilePath -> FilePath -> m FilePath
timestampAndMoveMigrationFile (unSqlFilePath -> migrationPath) folderToMoveTo =
    do
    -- The only important invariants for naming SQL migrations are:
    --   1. Migrations added by the same developer consecutively are such that the first is alphabetically lesser than the second.
    --   2. Chance of naming conflicts with migrations added by other developers is small.
    -- One desirable property, however, is that filenames are human-readable
    -- and convey more or less an idea of when they were added.
        (migTimestamp, _) <- toMigrationTimestamp <$> liftIO getCurrentTime
        let finalName =
                folderToMoveTo
                    </> nicerTimestampFormat (iso8601Show migTimestamp)
                    ++  "-"
                    ++  takeFileName migrationPath
        copyFile migrationPath finalName
        removeFile migrationPath
        return finalName
  where
        -- Replaces 'T' and colons by a dash and removes 'Z' from UTC timestamps.
        -- This makes them compatible with NTFS and nicer to work with in bash too.
    nicerTimestampFormat =
        Text.unpack
            . Text.replace ":" "-"
            . Text.replace "T" "-"
            . Text.replace "Z" ""
            . Text.pack
