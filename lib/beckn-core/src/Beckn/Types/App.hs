{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}

module Beckn.Types.App where

import           EulerHS.Prelude
import           Beckn.Utils.TH

newtype CaseId =
  CaseId
    { _getCaseId :: Text
    }
  deriving  (Generic, Show)

deriveIdentifierInstances ''CaseId

newtype ProductsId =
  ProductsId
    { _getProductsId :: Text
    }
  deriving  (Generic, Show)

deriveIdentifierInstances ''ProductsId

newtype CaseProductId =
  CaseProductId
    { _getCaseProductId :: Text
    }
  deriving  (Generic, Show)

deriveIdentifierInstances ''CaseProductId

newtype OrganizationId =
  OrganizationId
    { _getOrganizationId :: Text
    }
  deriving  (Generic, Show)

deriveIdentifierInstances ''OrganizationId