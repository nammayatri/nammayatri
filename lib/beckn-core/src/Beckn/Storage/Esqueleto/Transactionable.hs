module Beckn.Storage.Esqueleto.Transactionable where

import Beckn.Prelude
import Beckn.Storage.Esqueleto.Config
import Beckn.Storage.Esqueleto.Logger
import Beckn.Storage.Esqueleto.SqlDB
import Beckn.Types.Time (getCurrentTime)
import Database.Esqueleto.Experimental (SqlBackend, runSqlPool)

class Transactionable m where
  runTransaction :: SqlDB a -> m a

instance {-# OVERLAPPING #-} Transactionable (ReaderT SqlDBEnv (ReaderT SqlBackend LoggerIO)) where
  runTransaction = identity

instance {-# INCOHERENT #-} EsqDBFlow m r => Transactionable m where
  runTransaction = runTransactionImpl

runTransactionImpl ::
  (EsqDBFlow m r) =>
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
