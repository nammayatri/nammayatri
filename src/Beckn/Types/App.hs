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
import           Data.Swagger
import           Servant.Swagger

-- App Types
data Env =
  Env
    { runTime :: R.FlowRuntime
    }

type FlowHandler = ReaderT Env (ExceptT ServerError IO)

type FlowServer api = ServerT api (ReaderT Env (ExceptT ServerError IO))

newtype CustomerId = CustomerId { _getCustomerId :: Text }
  deriving stock (Generic, Show)

instance ToSchema CustomerId
deriveIdentifierInstances ''CustomerId

newtype OrganizationId = OrganizationId { _getOrganizationId :: Text }
  deriving stock (Generic, Show)

instance ToSchema OrganizationId
deriveIdentifierInstances ''OrganizationId

newtype BusinessAddressId = BusinessAddressId { _getBusinessAddressId :: Text }
  deriving stock (Generic, Show)

instance ToSchema BusinessAddressId
deriveIdentifierInstances ''BusinessAddressId

newtype PassApplicationId = PassApplicationId { _getPassApplicationId :: Text }
  deriving stock (Generic, Show)

instance ToSchema PassApplicationId
deriveIdentifierInstances ''PassApplicationId

newtype FromLocationId = FromLocationId { _getFromLocationId :: Text }
  deriving stock (Generic, Show)

instance ToSchema FromLocationId
deriveIdentifierInstances ''FromLocationId

newtype ToLocationId = ToLocationId { _getToLocationId :: Text }
  deriving stock (Generic, Show)

instance ToSchema ToLocationId
deriveIdentifierInstances ''ToLocationId

newtype AssignedTo = AssignedTo { _getAssignedTo :: Text }
  deriving stock (Generic, Show)

instance ToSchema AssignedTo
deriveIdentifierInstances ''AssignedTo

newtype LocationId = LocationId { _getLocationId :: Text }
  deriving stock (Generic, Show)

instance ToSchema LocationId
deriveIdentifierInstances ''LocationId

newtype QuotaId = QuotaId { _getQuotaId :: Text }
  deriving stock (Generic, Show)

instance ToSchema QuotaId
deriveIdentifierInstances ''QuotaId

newtype BlacklistedBy = BlacklistedBy { _getBlacklistedBy :: Text }
  deriving stock (Generic, Show)

instance ToSchema BlacklistedBy
deriveIdentifierInstances ''BlacklistedBy

newtype CustomerDetailsId = CustomerDetailsId { _getCustomerDetailsId :: Text }
  deriving stock (Generic, Show)

instance ToSchema CustomerDetailsId
deriveIdentifierInstances ''CustomerDetailsId

newtype PassId = PassId { _getPassId :: Text }
  deriving stock (Generic, Show)

instance ToSchema PassId
deriveIdentifierInstances ''PassId

newtype UserId = UserId { _getUserId :: Text }
  deriving stock (Generic, Show)

instance ToSchema UserId
deriveIdentifierInstances ''UserId

newtype RegistrationTokenId = RegistrationTokenId { _getRegistrationTokenId :: Text }
  deriving stock (Generic, Show)

instance ToSchema RegistrationTokenId
deriveIdentifierInstances ''RegistrationTokenId

newtype LocationBlacklistId = LocationBlacklistId { _getLocationBlacklistId :: Text }
  deriving stock (Generic, Show)

instance ToSchema LocationBlacklistId
deriveIdentifierInstances ''LocationBlacklistId

newtype AllocatedQuotaId = AllocatedQuotaId { _getAllocatedQuotaId :: Text }
  deriving stock (Generic, Show)

instance ToSchema AllocatedQuotaId
deriveIdentifierInstances ''AllocatedQuotaId

type RegistrationToken = Text

type Limit = Int

type Offset = Int
