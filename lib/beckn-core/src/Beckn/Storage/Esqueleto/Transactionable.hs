module Beckn.Storage.Esqueleto.Transactionable where

import Beckn.Prelude
import Beckn.Storage.Esqueleto.Config
import Beckn.Storage.Esqueleto.Logger
import Beckn.Storage.Esqueleto.SqlDB
import Beckn.Types.Time (MonadTime, getCurrentTime)
import Beckn.Utils.IOLogging
import Database.Esqueleto.Experimental (SqlBackend, runSqlPool)

class Transactionable m where
  runTransaction :: SqlDB a -> m a

instance {-# OVERLAPPING #-} Transactionable (ReaderT SqlDBEnv (ReaderT SqlBackend LoggerIO)) where
  runTransaction = identity

type HasEsqEnv r m = (MonadReader r m, HasLog r, HasField "esqDBEnv" r EsqDBEnv, MonadTime m, MonadIO m)

instance {-# INCOHERENT #-} HasEsqEnv r m => Transactionable m where
  runTransaction = runTransactionImpl

runTransactionImpl ::
  (HasEsqEnv r m) =>
  SqlDB a ->
  m a
runTransactionImpl run = do
  logEnv <- asks (.loggerEnv)
  dbEnv <- asks (.esqDBEnv)
  now <- getCurrentTime
  let sqlDBEnv =
        SqlDBEnv
          { currentTime = now
          }
  liftIO . runLoggerIO logEnv $ runSqlPool (runReaderT run sqlDBEnv) dbEnv.connPool
