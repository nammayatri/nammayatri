{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.DriverOffer where

import Data.Aeson
import qualified Domain.Types.Estimate
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data DriverOffer = DriverOffer
  { bppQuoteId :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    distanceToPickup :: Kernel.Prelude.Maybe Kernel.Types.Common.Distance,
    distanceUnit :: Kernel.Types.Common.DistanceUnit,
    driverName :: Kernel.Prelude.Text,
    durationToPickup :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    estimateId :: Kernel.Types.Id.Id Domain.Types.Estimate.Estimate,
    fulfillmentId :: Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.DriverOffer.DriverOffer,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    rating :: Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal,
    status :: Domain.Types.DriverOffer.DriverOfferStatus,
    updatedAt :: Kernel.Prelude.UTCTime,
    validTill :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data BPPQuote = BPPQuote {} deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data DriverOfferStatus = ACTIVE | INACTIVE deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''DriverOfferStatus)
