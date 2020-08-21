{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Beckn.Types.App where

import Beckn.Types.Error
import Beckn.Utils.TH
import EulerHS.Prelude
import qualified EulerHS.Runtime as R
import Servant

data EnvR r = EnvR
  { runTime :: R.FlowRuntime,
    appEnv :: r
  }

type FlowHandlerR r = ReaderT (EnvR r) (ExceptT ServerError IO)

type FlowServerR r api = ServerT api (FlowHandlerR r)

type MandatoryQueryParam name a = QueryParam' '[Required, Strict] name a

newtype CaseId = CaseId
  { _getCaseId :: Text
  }
  deriving (Generic, Show)

deriveIdentifierInstances ''CaseId

newtype ProductsId = ProductsId
  { _getProductsId :: Text
  }
  deriving (Generic, Show)

deriveIdentifierInstances ''ProductsId

newtype PersonId = PersonId
  { _getPersonId :: Text
  }
  deriving (Generic, Show)

deriveIdentifierInstances ''PersonId

newtype OrganizationId = OrganizationId
  { _getOrganizationId :: Text
  }
  deriving (Generic, Show)

deriveIdentifierInstances ''OrganizationId

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

newtype ProductInstanceId = ProductInstanceId
  { _getProductInstanceId :: Text
  }
  deriving (Generic, Show)

deriveIdentifierInstances ''ProductInstanceId

newtype InventoryId = InventoryId
  { _getInventoryId :: Text
  }
  deriving (Generic, Show)

deriveIdentifierInstances ''InventoryId

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
  = ByApplicationId CaseId
  | ById ProductsId
  | ByCustomerId PersonId
