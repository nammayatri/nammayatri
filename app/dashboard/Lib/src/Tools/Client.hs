{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Tools.Client (DataServer (..), CallServerAPI (..)) where

import Beckn.Prelude
import Beckn.Types.Error
import Beckn.Utils.Common hiding (Error, callAPI)
import Beckn.Utils.Dhall (FromDhall)
import qualified Domain.Types.ServerName as DSN
import qualified EulerHS.Types as Euler
import Tools.Error
import Tools.Metrics

data DataServer = DataServer
  { name :: DSN.ServerName,
    url :: BaseUrl,
    token :: Text
  }
  deriving (Generic, FromDhall)

getDataServer ::
  HasFlowEnv m r '["dataServers" ::: [DataServer]] =>
  DSN.ServerName ->
  m DataServer
getDataServer serverName = do
  dataServers <- asks (.dataServers)
  case filter (\server -> server.name == serverName) dataServers of
    [dataServer] -> pure dataServer
    [] -> throwError (InternalError $ "Unknown data server: " <> show serverName)
    _ -> throwError (InternalError $ "Should be exactly one data server with name " <> show serverName)

class
  ( CoreMetrics m,
    HasFlowEnv m r '["dataServers" ::: [DataServer]]
  ) =>
  CallServerAPI apis m r b c
    | m b -> c
  where
  callServerAPI :: DSN.ServerName -> (Text -> apis) -> Text -> (apis -> b) -> c

instance
  ( CoreMetrics m,
    HasFlowEnv m r '["dataServers" ::: [DataServer]],
    ToJSON d,
    FromJSON d
  ) =>
  CallServerAPI apis m r (Euler.EulerClient d) (m d)
  where
  callServerAPI serverName mkAPIs descr f = do
    dataServer <- getDataServer serverName
    let driverOfferAPIs = mkAPIs dataServer.token
    callApiUnwrappingApiError (identity @Error) Nothing Nothing dataServer.url (f driverOfferAPIs) descr

instance
  ( CoreMetrics m,
    HasFlowEnv m k '["dataServers" ::: [DataServer]],
    MonadFlow m,
    CallServerAPI apis m k d r1,
    r ~ (c -> r1)
  ) =>
  CallServerAPI apis m k (c -> d) r
  where
  callServerAPI serverName mkAPIs descr f c =
    callServerAPI @_ @m serverName mkAPIs descr (`f` c)
