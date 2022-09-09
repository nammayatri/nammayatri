module Tools.DataServer where

import Beckn.Prelude
import Beckn.Types.Common
import Beckn.Types.Error
import Beckn.Utils.Common
import Beckn.Utils.Dhall (FromDhall)

data ServerName = APP_BACKEND | BECKN_TRANSPORT | DRIVER_OFFER_BPP
  deriving (Generic, FromDhall, Eq, Show)

data DataServer = DataServer
  { name :: ServerName,
    url :: BaseUrl,
    token :: Text
  }
  deriving (Generic, FromDhall)

-- FIXME Can we change types so as not to throw error here?
-- FIXME use Map instead of List
getDataServer ::
  HasFlowEnv m r '["dataServers" ::: [DataServer]] =>
  ServerName ->
  m DataServer
getDataServer serverName = do
  dataServers <- asks (.dataServers)
  case filter (\server -> server.name == serverName) dataServers of
    [dataServer] -> pure dataServer
    [] -> throwError (InternalError $ "Unknown data server: " <> show serverName)
    _ -> throwError (InternalError $ "Should be exactly one data server with name " <> show serverName)
