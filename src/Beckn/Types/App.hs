{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}

module Beckn.Types.App where

import           Beckn.Utils.TH
import           Database.Beam.Backend.SQL (FromBackendRow, HasSqlValueSyntax)
import           Database.Beam.MySQL       (MySQL, MysqlValueSyntax)
import qualified EulerHS.Interpreters      as I
import qualified EulerHS.Language          as L
import           EulerHS.Prelude
import qualified EulerHS.Runtime           as R
import           Servant

-- App Types
data Env =
  Env
    { runTime :: R.FlowRuntime
    }

type FlowHandler = ReaderT Env (ExceptT ServerError IO)

type FlowServer api = ServerT api (ReaderT Env (ExceptT ServerError IO))

withFlowHandler :: L.Flow a -> FlowHandler a
withFlowHandler flow = do
  Env flowRt <- ask
  lift . lift . I.runFlow flowRt $ flow

newtype CustomerId = CustomerId { _getCustomerId :: Text }
  deriving stock (Show)

deriveIdentifierInstances ''CustomerId

newtype OrganizationId = OrganizationId { _getOrganizationId :: Text }
  deriving stock (Show)

deriveIdentifierInstances ''OrganizationId

newtype BusinessAddressId = BusinessAddressId { _getBusinessAddressId :: Text }
  deriving stock (Show)

deriveIdentifierInstances ''BusinessAddressId

newtype PassApplicationId = PassApplicationId { _getPassApplicationId :: Text }
  deriving stock (Show)

deriveIdentifierInstances ''PassApplicationId

newtype FromLocationId = FromLocationId { _getFromLocationId :: Text }
  deriving stock (Show)

deriveIdentifierInstances ''FromLocationId

newtype ToLocationId = ToLocationId { _getToLocationId :: Text }
  deriving stock (Show)

deriveIdentifierInstances ''ToLocationId

newtype AssignedTo = AssignedTo { _getAssignedTo :: Text }
  deriving stock (Show)

deriveIdentifierInstances ''AssignedTo

newtype LocationId = LocationId { _getLocationId :: Text }
  deriving stock (Show)

deriveIdentifierInstances ''LocationId

newtype QuotaId = QuotaId { _getQuotaId :: Text }
  deriving stock (Show)

deriveIdentifierInstances ''QuotaId

newtype BlacklistedBy = BlacklistedBy { _getBlacklistedBy :: Text }
  deriving stock (Show)

deriveIdentifierInstances ''BlacklistedBy

newtype CustomerDetailsId = CustomerDetailsId { _getCustomerDetailsId :: Text }
  deriving stock (Show)

deriveIdentifierInstances ''CustomerDetailsId

newtype PassId = PassId { _getPassId :: Text }
  deriving stock (Show)

deriveIdentifierInstances ''PassId

newtype UserId = UserId { _getUserId :: Text }
  deriving stock (Show)

deriveIdentifierInstances ''UserId

newtype RegistrationTokenId = RegistrationTokenId { _getRegistrationTokenId :: Text }
  deriving stock (Show)

deriveIdentifierInstances ''RegistrationTokenId

newtype LocationBlacklistId = LocationBlacklistId { _getLocationBlacklistId :: Text }
  deriving stock (Show)

deriveIdentifierInstances ''LocationBlacklistId

newtype AllocatedQuotaId = AllocatedQuotaId { _getAllocatedQuotaId :: Text }
  deriving stock (Show)

deriveIdentifierInstances ''AllocatedQuotaId

type RegistrationToken = Text

type Limit = Int

type Offset = Int
