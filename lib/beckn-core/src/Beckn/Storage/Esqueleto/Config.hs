module Beckn.Storage.Esqueleto.Config where

import Beckn.Storage.Esqueleto.Logger (runLoggerIO)
import Beckn.Types.App (HasFlowEnv)
import Beckn.Types.Field ((:::))
import Beckn.Types.Logging (LoggerEnv)
import Beckn.Utils.Dhall (FromDhall)
import Data.Pool (Pool)
import Database.Persist.Postgresql
import Database.PostgreSQL.Simple (execute_)
import Database.PostgreSQL.Simple.Types (Query (Query))
import EulerHS.Prelude

data EsqDBConfig = EsqDBConfig
  { connectHost :: Text,
    connectPort :: Word16,
    connectUser :: Text,
    connectPassword :: Text,
    connectDatabase :: Text,
    connectSchemaName :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, FromDhall)

newtype EsqDBEnv = EsqDBEnv
  { connPool :: Pool SqlBackend
  }
  deriving (Generic)

prepareEsqDBEnv :: EsqDBConfig -> LoggerEnv -> IO EsqDBEnv
prepareEsqDBEnv cfg logEnv = do
  let connStr = makeConnString cfg
      modifyConnString = encodeUtf8 cfg.connectSchemaName
  pool <- liftIO . runLoggerIO logEnv $ createPostgresqlPoolModified (modifyConn modifyConnString) connStr 10
  return $ EsqDBEnv pool
  where
    makeConnString dbConfig =
      encodeUtf8 $
        "host=" <> dbConfig.connectHost
          <> " dbname="
          <> dbConfig.connectDatabase
          <> " user="
          <> dbConfig.connectUser
          <> " password="
          <> dbConfig.connectPassword
          <> " port="
          <> show dbConfig.connectPort
    modifyConn schemaName conn =
      void . execute_ conn . Query $ "set search_path to " <> schemaName <> ", public; "

type EsqDBFlow m r = HasFlowEnv m r '["esqDBEnv" ::: EsqDBEnv]
