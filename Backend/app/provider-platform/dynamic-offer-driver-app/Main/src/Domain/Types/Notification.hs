{-# LANGUAGE DerivingVia #-}

module Domain.Types.Notification where

import Data.Aeson
import qualified Kernel.External.Payment.Juspay.Types as Payment
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)
import Kernel.Types.Id

data Notification = Notification
  { id :: Id Notification,
    sourceAmount :: HighPrecMoney,
    mandateId :: Text,
    driverFeeId :: Text,
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
