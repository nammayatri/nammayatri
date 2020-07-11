{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Beckn.Types.App where

import Beckn.Utils.TH
import EulerHS.Prelude
import qualified EulerHS.Runtime as R
import Servant

-- App Types
data EnvR r = EnvR
  { runTime :: R.FlowRuntime,
    appEnv :: r
  }

type Env = EnvR ()

type FlowHandlerR r = ReaderT (EnvR r) (ExceptT ServerError IO)

type FlowHandler = FlowHandlerR ()

type FlowServerR r api = ServerT api (FlowHandlerR r)

type FlowServer api = FlowServerR () api

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
  { _getVechicleId :: Text
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

type CronAuthKey = Text

type AuthHeader = Header' '[Required, Strict] "token" RegToken
