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
  { id :: Kernel.Types.Id.Id Domain.Types.DriverOffer.DriverOffer,
    estimateId :: Kernel.Types.Id.Id Domain.Types.Estimate.Estimate,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    driverName :: Kernel.Prelude.Text,
    durationToPickup :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    distanceToPickup :: Kernel.Prelude.Maybe Kernel.Types.Common.Distance,
    distanceUnit :: Kernel.Types.Common.DistanceUnit,
    validTill :: Kernel.Prelude.UTCTime,
    bppQuoteId :: Kernel.Prelude.Text,
    rating :: Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal,
    status :: Domain.Types.DriverOffer.DriverOfferStatus,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data BPPQuote = BPPQuote {} deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data DriverOfferStatus = ACTIVE | INACTIVE deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''DriverOfferStatus)
