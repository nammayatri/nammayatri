{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.TDSDistributionRecord where

import Data.Aeson
import qualified Domain.Types.EmailDelivery
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.TDSDistributionBatch
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data TDSDistributionRecord = TDSDistributionRecord
  { assessmentYear :: Kernel.Prelude.Text,
    attemptCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    batchId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.TDSDistributionBatch.TDSDistributionBatch),
    createdAt :: Kernel.Prelude.UTCTime,
    deliveredAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    driverId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    emailAddress :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    failureReason :: Kernel.Prelude.Maybe Domain.Types.TDSDistributionRecord.TDSFailureReason,
    fileName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    financialYear :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.TDSDistributionRecord.TDSDistributionRecord,
    lastAttemptAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    latestEmailDeliveryId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.EmailDelivery.EmailDelivery),
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    quarter :: Kernel.Prelude.Text,
    retryCount :: Kernel.Prelude.Int,
    status :: Domain.Types.TDSDistributionRecord.TDSDistributionStatus,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data TDSDistributionStatus
  = PENDING
  | SENDING
  | SENT
  | DELIVERED
  | FAILED
  | MISSING_FILE
  | MISSING_MANIFEST
  | MISMATCH
  deriving (Show, (Eq), (Ord), (Read), (Generic), (ToJSON), (FromJSON), (ToSchema))

data TDSFailureReason
  = MISSING_EMAIL
  | ADDRESS_NOT_FOUND
  | MAILBOX_FULL
  | ATTACHMENT_TOO_LARGE
  | REJECTED
  | SUPPRESSED
  | TIMEOUT
  | SEND_ERROR
  deriving (Show, (Eq), (Ord), (Read), (Generic), (ToJSON), (FromJSON), (ToSchema))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnum (''TDSDistributionStatus))

$(Kernel.Utils.TH.mkFromHttpInstanceForEnum (''TDSDistributionStatus))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnum (''TDSFailureReason))
