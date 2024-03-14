module Codd.AppCommands
    ( timestampAndCopyMigrationFile
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
import           UnliftIO.Directory             ( copyFile )

timestampAndCopyMigrationFile
    :: MonadIO m => SqlFilePath -> FilePath -> m FilePath
timestampAndCopyMigrationFile (unSqlFilePath -> migrationPath) folderToCopyTo =
    do
    -- The only important invariants for naming SQL migrations are:
    --   1. Migrations added by a developer is such that it should come after all existing migrations on disk
    --   2. Chance of naming conflicts with migrations added by other developers is small.
    -- One desirable property, however, is that filenames are human-readable
    -- and convey more or less an idea of when they were added.
        (migTimestamp, _) <- toMigrationTimestamp <$> liftIO getCurrentTime
        let finalName =
                folderToCopyTo
                    </> nicerTimestampFormat (iso8601Show migTimestamp)
                    ++  "-"
                    ++  takeFileName migrationPath
        copyFile migrationPath finalName
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
