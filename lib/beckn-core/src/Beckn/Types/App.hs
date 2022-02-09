{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Beckn.Types.App
  ( module Beckn.Types.App,
    Servant.BaseUrl,
    Aeson.Value,
  )
where

import Beckn.Types.Field (HasFields)
import Beckn.Types.Forkable
import Beckn.Types.Logging
import Beckn.Types.MonadGuid
import Beckn.Types.Time
import Beckn.Utils.Dhall (FromDhall)
import Control.Lens.Operators
import Data.Aeson as Aeson (Value)
import Data.OpenApi
import Database.Beam.Backend
import Database.Beam.Postgres
import Database.Beam.Query
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Runtime as R
import Servant
import qualified Servant.Client.Core as Servant

data EnvR r = EnvR
  { flowRuntime :: R.FlowRuntime,
    appEnv :: r
  }
  deriving (Generic)

type MonadFlow m =
  ( Monad m,
    MonadIO m,
    L.MonadFlow m,
    Forkable m,
    Log m,
    MonadGuid m,
    MonadTime m,
    MonadClock m,
    MonadThrow m
  )

-- | Require monad to be Flow-based and have specified fields in Reader env.
type HasFlowEnv m r fields =
  ( MonadFlow m,
    MonadReader r m,
    HasFields r fields
  )

type FlowHandlerR r = ReaderT (EnvR r) IO

type FlowServerR r api = ServerT api (FlowHandlerR r)

type MandatoryQueryParam name a = QueryParam' '[Required, Strict] name a

type Limit = Int

type Offset = Int

type RegToken = Text

-- FIXME: remove this
type AuthHeader = Header' '[Required, Strict] "token" RegToken

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Servant.BaseUrl where
  sqlValueSyntax = sqlValueSyntax . Servant.showBaseUrl

instance FromBackendRow Postgres Servant.BaseUrl where
  fromBackendRow =
    either (fail . show) pure . Servant.parseBaseUrl =<< fromBackendRow

instance HasSqlEqualityCheck Postgres Servant.BaseUrl

instance ToSchema Servant.BaseUrl where
  declareNamedSchema _ = do
    aSchema <- declareSchema (Proxy :: Proxy Text)
    return $
      NamedSchema (Just "BaseUrl") aSchema

instance ToSchema Value where
  declareNamedSchema _ = do
    aSchema <- declareSchema (Proxy :: Proxy Text)
    return $
      NamedSchema (Just "Value") $
        aSchema
          & description
            ?~ "Some JSON."

newtype RegistrySecrets = RegistrySecrets
  { encryptionPrivKeyB16 :: Text
  }
  deriving (Generic, FromDhall)
