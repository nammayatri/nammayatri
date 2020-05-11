{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Beckn.Types.App where

import Beckn.Utils.TH
import EulerHS.Prelude
import Servant

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

newtype CaseProductId = CaseProductId
  { _getCaseProductId :: Text
  }
  deriving (Generic, Show)

deriveIdentifierInstances ''CaseProductId

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
