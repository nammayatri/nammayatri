-- {-# LANGUAGE DuplicateRecordFields #-}
-- {-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE PartialTypeSignatures #-}

module Config.Config where

import Config.Env as Env
import Database.Beam.MySQL (MySQLM)
import Database.Beam.Postgres as BP
import Euler.Types.Errors (internalError)
import EulerHS.Extra.EulerDB as Extra
import EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Types as T
import Types.Config

defaultDBSyncConfig :: DBSyncConfig
defaultDBSyncConfig =
  DBSyncConfig
    { _emptyRetry = 50,
      _rateLimitN = 200,
      _rateLimitWindow = 100,
      _streamReadCount = 1000
    }

getDatabase :: IO DatabaseConfig
getDatabase = do
  _host <- Env.getDatabaseHost
  _port <- Env.getDatabasePort
  _db <- Env.getDatabaseName
  _user <- Env.getDatabaseUser
  _password <- Env.getDatabasePassword
  return DatabaseConfig {..}

getDBPoolDetails :: IO DBPoolDetails
getDBPoolDetails = do
  _pool <- Env.getDbPool
  _maxIdleTime <- Env.getDbMaxIdleTimeout
  _maxConnections <- Env.getDbMaxConnections
  return DBPoolDetails {..}

getRedis :: IO RedisConfig
getRedis = do
  _host <- Env.getRedisHost
  _port <- Env.getRedisPort
  _db <- Env.getRedisDbValue
  _password <- Env.getRedisPassword
  _socket_keep_alive <- Env.getRedisSocketKeepAlive
  return RedisConfig {..}

mkConfigFromEnv :: IO Config
mkConfigFromEnv = do
  _database <- getDatabase
  _dbPoolDetails <- getDBPoolDetails
  _redis <- getRedis
  _isMaskingEnabled <- Env.getMaskingEnabled
  return Config {..}

config' :: IO Config
config' = mkConfigFromEnv

getEulerDbConf :: (L.MonadFlow m) => m (T.DBConfig MySQLM)
getEulerDbConf = Extra.getEulerDbConf internalError

getEulerPgDbConf :: (L.MonadFlow m) => m (T.DBConfig BP.Pg)
getEulerPgDbConf = Extra.getEulerPsqlDbConf internalError
