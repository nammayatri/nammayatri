{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.TicketPlace where

import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Id
import Tools.Beam.UtilsTH

data TicketPlace = TicketPlace
  { closeTimings :: Kernel.Prelude.Maybe Kernel.Prelude.TimeOfDay,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    gallery :: [Kernel.Prelude.Text],
    iconUrl :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace,
    lat :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    lon :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    mapImageUrl :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    name :: Kernel.Prelude.Text,
    openTimings :: Kernel.Prelude.Maybe Kernel.Prelude.TimeOfDay,
    placeType :: Domain.Types.TicketPlace.PlaceType,
    shortDesc :: Kernel.Prelude.Text,
    termsAndConditions :: [Kernel.Prelude.Text],
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data PlaceType = Museum | ThemePark | AmusementPark | WaterPark | WildLifeSanctuary | ArtGallery | HeritageSite | ReligiousSite | Other
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(mkBeamInstancesForEnum ''PlaceType)
