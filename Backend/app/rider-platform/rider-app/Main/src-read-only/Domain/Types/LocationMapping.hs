{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.LocationMapping where

import Data.Aeson
import qualified Domain.Types.Location
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data LocationMapping = LocationMapping
  { id :: Kernel.Types.Id.Id Domain.Types.LocationMapping.LocationMapping,
    tag :: Domain.Types.LocationMapping.LocationMappingTags,
    locationId :: Kernel.Types.Id.Id Domain.Types.Location.Location,
    entityId :: Kernel.Prelude.Text,
    order :: Kernel.Prelude.Int,
    version :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity)
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data LocationMappingTags = BOOKING | SEARCH_REQUEST | RIDE | BOOKING_UPDATE_REQUEST deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''LocationMappingTags)
