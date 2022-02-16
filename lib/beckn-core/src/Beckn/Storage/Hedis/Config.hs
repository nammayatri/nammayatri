module Beckn.Storage.Hedis.Config where

import Beckn.Prelude
import Beckn.Types.Logging
import Beckn.Utils.Dhall (FromDhall)
import qualified Control.Monad.Catch as C
import Data.Binary (Word16)
import Database.Redis
import GHC.Records.Extra
import Network.Socket (HostName)

type HedisFlow m env =
  (MonadReader env m, HasField "hedisEnv" env HedisEnv, MonadIO m, C.MonadThrow m, Log m)

type KeyModifierFunc = (Text -> Text)

data HedisCfg = HedisCfg
  { connectHost :: HostName,
    connectPort :: Word16,
    connectAuth :: Maybe Text,
    connectDatabase :: Integer,
    connectMaxConnections :: Int,
    connectMaxIdleTime :: NominalDiffTime,
    connectTimeout :: Maybe NominalDiffTime
  }
  deriving (Generic, Show, FromDhall)

data HedisEnv = HedisEnv
  { hedisConnection :: Connection,
    keyModifier :: KeyModifierFunc
  }
  deriving (Generic)

defaultHedisCfg :: HedisCfg
defaultHedisCfg =
  HedisCfg
    { connectHost = "localhost",
      connectPort = 6379,
      connectAuth = Nothing,
      connectDatabase = 0,
      connectMaxConnections = 50,
      connectMaxIdleTime = 30,
      connectTimeout = Nothing
    }

withHedisEnv :: HedisCfg -> KeyModifierFunc -> (HedisEnv -> IO a) -> IO a
withHedisEnv cfg keyModifier = C.bracket (connectHedis cfg keyModifier) disconnectHedis

connectHedis :: HedisCfg -> KeyModifierFunc -> IO HedisEnv
connectHedis cfg keyModifier = do
  conn <- checkedConnect connectInfo
  return $
    HedisEnv
      { hedisConnection = conn,
        keyModifier = keyModifier
      }
  where
    connectInfo :: ConnectInfo
    connectInfo =
      defaultConnectInfo
        { connectHost = cfg.connectHost,
          connectPort = PortNumber $ toEnum $ fromEnum cfg.connectPort,
          connectAuth = encodeUtf8 <$> cfg.connectAuth,
          connectDatabase = cfg.connectDatabase,
          connectMaxConnections = cfg.connectMaxConnections,
          connectMaxIdleTime = cfg.connectMaxIdleTime
        }

disconnectHedis :: HedisEnv -> IO ()
disconnectHedis HedisEnv {..} =
  disconnect hedisConnection
