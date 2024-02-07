{-# LANGUAGE TemplateHaskell #-}

module DB.Migration where

import RIO
import qualified Hasql.Migration as M
import qualified Hasql.Migration.Util as Util
import qualified Hasql.Pool as P
import qualified Hasql.Transaction as T
import qualified Data.FileEmbed as FE
import DB.Transaction (runTransactionWithPool)

scripts :: [(FilePath, ByteString)]
scripts = $(FE.embedDir "sql")

migrationCommands :: [M.MigrationCommand]
migrationCommands = map (uncurry M.MigrationScript) scripts


initializeMigrationSchema :: T.Transaction ()
initializeMigrationSchema = do
  let tableName = "schema_migrations"
  exists <- Util.existsTable tableName
  unless exists
    $ T.sql
    $ mconcat
    [ "CREATE TABLE IF NOT EXISTS " <> encodeUtf8 tableName
    , "( filename VARCHAR(512) NOT NULL"
    , ", checksum VARCHAR(32) NOT NULL"
    , ", executed_at TIMESTAMP WITHOUT TIME ZONE NOT NULL DEFAULT NOW()"
    , ")"
    ]

runMigrations :: [M.MigrationCommand] -> T.Transaction (Maybe M.MigrationError)
runMigrations [] = pure Nothing;
runMigrations (x : xs) = do
  err <- M.runMigration x
  case err of
    Nothing -> runMigrations xs
    Just _ -> return err

autoMigrate :: MonadIO m => P.Pool -> m (Maybe M.MigrationError)
autoMigrate pool = do
  runTransactionWithPool pool initializeMigrationSchema
  runTransactionWithPool pool $ runMigrations migrationCommands
