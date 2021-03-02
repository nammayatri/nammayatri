{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Beckn.Types.App
  ( module Beckn.Types.App,
    Servant.BaseUrl,
  )
where

import Beckn.Storage.DB.Config (HasDbCfg)
import Beckn.Types.Common (FlowR)
import Beckn.Types.Error
import Beckn.Types.ID
import Beckn.Types.Storage.Case (Case)
import Beckn.Types.Storage.Person (Person)
import Beckn.Utils.Logging (HasLogContext (..))
import Beckn.Utils.TH
import Control.Lens ((?~))
import qualified Data.Swagger as S
import Database.Beam.Backend
import Database.Beam.Postgres
import Database.Beam.Query
import Dhall (FromDhall)
import EulerHS.Prelude
import qualified EulerHS.Runtime as R
import GHC.Records (HasField (..))
import Servant
import qualified Servant.Client.Core as Servant

data EnvR r = EnvR
  { runTime :: R.FlowRuntime,
    appEnv :: r
  }

type FlowHandlerR r = ReaderT (EnvR r) (ExceptT ServerError IO)

type FlowServerR r api = ServerT api (FlowHandlerR r)

type MandatoryQueryParam name a = QueryParam' '[Required, Strict] name a

newtype ProductsId = ProductsId
  { _getProductsId :: Text
  }
  deriving (Generic, Show)

deriveIdentifierInstances ''ProductsId

newtype OrganizationId = OrganizationId
  { _getOrganizationId :: Text
  }
  deriving (Generic, Show)

deriveIdentifierInstances ''OrganizationId

newtype ShortOrganizationId = ShortOrganizationId
  { _getShortOrganizationId :: Text
  }
  deriving (Generic, Show)

deriveIdentifierInstances ''ShortOrganizationId

newtype LocationId = LocationId
  { _getLocationId :: Text
  }
  deriving (Generic, Show)

deriveIdentifierInstances ''LocationId

newtype VehicleId = VehicleId
  { _getVehicleId :: Text
  }
  deriving (Generic, Show)

deriveIdentifierInstances ''VehicleId

newtype InventoryId = InventoryId
  { _getInventoryId :: Text
  }
  deriving (Generic, Show)

deriveIdentifierInstances ''InventoryId

newtype IssueId = IssueId
  { _getIssueId :: Text
  }
  deriving (Generic, Show)

deriveIdentifierInstances ''IssueId

newtype RatingId = RatingId
  { _getRatingId :: Text
  }
  deriving (Generic, Show)

deriveIdentifierInstances ''RatingId

type Limit = Int

type Offset = Int

type RegToken = Text

type APIKey = Text

type CronAuthKey = Text

-- FIXME: remove this
type AuthHeader = Header' '[Required, Strict] "token" RegToken

type DomainResult a = Either DomainError a

-- TODO: Add this later if required

-- | ByOrganizationId OrganizationId
data ListById
  = ByApplicationId (ID Case)
  | ById ProductsId
  | ByCustomerId (ID Person)

-- | Requests / responses track condiguration flag
data TraceFlag = TRACE_INCOMING | TRACE_OUTGOING | TRACE_ALL | TRACE_NOTHING
  deriving (Generic, FromDhall)

type HasTraceFlag r = HasField "traceFlag" r TraceFlag

type FlowWithTraceFlag r a =
  ( HasTraceFlag r,
    HasDbCfg r,
    HasLogContext r
  ) =>
  FlowR r a

instance S.ToSchema Servant.BaseUrl where
  declareNamedSchema _ = do
    aSchema <- S.declareSchema (Proxy :: Proxy Text)
    return $
      S.NamedSchema (Just "url") $
        aSchema
          & S.description ?~ "A valid URL"

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Servant.BaseUrl where
  sqlValueSyntax = sqlValueSyntax . Servant.showBaseUrl

instance FromBackendRow Postgres Servant.BaseUrl where
  fromBackendRow =
    either (fail . show) pure . Servant.parseBaseUrl =<< fromBackendRow

instance HasSqlEqualityCheck Postgres Servant.BaseUrl
