{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}

module Beckn.Types.App where

import           Beckn.Utils.TH
import           Database.Beam.Backend.SQL (FromBackendRow, HasSqlValueSyntax)
import           Database.Beam.MySQL       (MySQL, MysqlValueSyntax)
import           EulerHS.Prelude
import qualified EulerHS.Runtime           as R
import           Servant

-- App Types
data Env =
  Env
    { runTime :: R.FlowRuntime
    }

type FlowHandler = ReaderT Env (ExceptT ServerError IO)

newtype CustomerId = CustomerId Text
  deriving stock (Show)

deriveIdentifierInstances ''CustomerId

newtype OrganizationId = OrganizationId Text
  deriving stock (Show)

deriveIdentifierInstances ''OrganizationId

newtype BusinessAddressId = BusinessAddressId Text
  deriving stock (Show)

deriveIdentifierInstances ''BusinessAddressId

newtype PassApplicationId = PassApplicationId Text
  deriving stock (Show)

deriveIdentifierInstances ''PassApplicationId

newtype FromLocationId = FromLocationId Text
  deriving stock (Show)

deriveIdentifierInstances ''FromLocationId

newtype ToLocationId = ToLocationId Text
  deriving stock (Show)

deriveIdentifierInstances ''ToLocationId

newtype AssignedTo = AssignedTo Text
  deriving stock (Show)

deriveIdentifierInstances ''AssignedTo

newtype LocationId = LocationId Text
  deriving stock (Show)

deriveIdentifierInstances ''LocationId

newtype QuotaId = QuotaId Text
  deriving stock (Show)

deriveIdentifierInstances ''QuotaId

newtype BlacklistedBy = BlacklistedBy Text
  deriving stock (Show)

deriveIdentifierInstances ''BlacklistedBy
