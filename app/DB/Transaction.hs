{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE InstanceSigs #-}
module DB.Transaction where

import RIO
import qualified Hasql.Connection as C
import qualified Hasql.Pool as P
import qualified Hasql.Transaction.Sessions as TS
import qualified Hasql.Transaction as T
import qualified Hasql.Session as S
import qualified Hasql.Statement as STMT
import Types (HasDbPool (..))
import RIO.List (intercalate)
import RIO.Time (secondsToDiffTime)


loadPool :: C.Settings  -> Int -> IO P.Pool
loadPool connectionString poolSize =
    let
      minute = 60
      acquisitionTimeout = secondsToDiffTime 10
      maxLifetime = secondsToDiffTime $ 30 * minute
      maxIdleLifetime = secondsToDiffTime $ 30 * minute
    in P.acquire
      poolSize
      acquisitionTimeout
      maxLifetime
      maxIdleLifetime
      connectionString

runTransactionWithConnection :: MonadIO m => C.Connection -> T.Transaction b -> m b
runTransactionWithConnection conn transaction = do
    e <- liftIO $ S.run (TS.transaction TS.Serializable TS.Write transaction) conn
    either throwIO pure e


runTransactionWithPool :: MonadIO m => P.Pool -> T.Transaction b -> m b
runTransactionWithPool pool transaction = do
  result <- liftIO $ P.use pool (TS.transaction TS.Serializable TS.Write transaction)
  case result of
    Right e -> pure e
    Left (P.ConnectionUsageError e) -> error $ "Failed to connect to database, error: " ++  show e
    Left (P.SessionUsageError e) -> throwIO e
    Left P.AcquisitionTimeoutUsageError -> error "Timeout"

runStmt :: STMT.Statement () a -> T.Transaction a
runStmt = T.statement ()

runTransaction :: (Types.HasDbPool env, MonadReader env m, MonadIO m) => T.Transaction a -> m a
-- runTransaction :: T.Transaction a -> RIO AppConfig a
runTransaction transaction = do
  pool <- view Types.dbPoolL
  runTransactionWithPool pool transaction


executeStmt :: HasDbPool env => STMT.Statement () a -> RIO env a
executeStmt = runTransaction . runStmt

truncateTables :: [Text] -> T.Transaction ()
truncateTables tables  =
  let sqlTransaction =
        [ "TRUNCATE "
        , fromString $ intercalate ", " (map show tables)
        , " RESTART IDENTITY;"
        ]
  in T.sql $ mconcat sqlTransaction


