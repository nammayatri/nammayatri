{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.TowingAuditLog where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.TowingRequest
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data TowingAuditLog = TowingAuditLog
  { action :: Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.TowingAuditLog.TowingAuditLog,
    ipAddress :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    lat :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    lon :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    metadata :: Kernel.Prelude.Maybe Data.Aeson.Value,
    newStatus :: Kernel.Prelude.Maybe Domain.Types.TowingRequest.TowingStatus,
    performedBy :: Kernel.Prelude.Text,
    performedByName :: Kernel.Prelude.Text,
    performedByRole :: Kernel.Prelude.Text,
    previousStatus :: Kernel.Prelude.Maybe Domain.Types.TowingRequest.TowingStatus,
    towingRequestId :: Kernel.Types.Id.Id Domain.Types.TowingRequest.TowingRequest,
    userAgent :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
