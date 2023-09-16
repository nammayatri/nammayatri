{-# LANGUAGE DerivingVia #-}

module Domain.Types.Notification where

import Data.Aeson
import Domain.Types.DriverFee (DriverFee)
import Domain.Types.Mandate (Mandate)
import qualified Kernel.External.Payment.Juspay.Types as Payment
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)
import Kernel.Types.Id

data Notification = Notification
  { id :: Id Notification,
    shortId :: Text,
    sourceAmount :: HighPrecMoney,
    mandateId :: Id Mandate,
    driverFeeId :: Id DriverFee,
    txnDate :: UTCTime,
    juspayProvidedId :: Text,
    providerName :: Text,
    notificationType :: Text,
    description :: Text,
    status :: Payment.NotificationStatus,
    dateCreated :: UTCTime,
    lastUpdated :: UTCTime,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Eq, Show, FromJSON, ToJSON, ToSchema)

---- add migration for this change ----
