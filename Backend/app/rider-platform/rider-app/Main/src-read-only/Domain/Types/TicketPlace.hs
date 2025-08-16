{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.TicketPlace where

import Data.Aeson
import qualified Data.Time
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified SharedLogic.TicketRule.Core
import qualified Tools.Beam.UtilsTH

data TicketPlace = TicketPlace
  { allowSameDayBooking :: Kernel.Prelude.Bool,
    assignTicketToBpp :: Kernel.Prelude.Bool,
    closeTimings :: Kernel.Prelude.Maybe Kernel.Prelude.TimeOfDay,
    customTabs :: Kernel.Prelude.Maybe [Domain.Types.TicketPlace.CustomTab],
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    endDate :: Kernel.Prelude.Maybe Data.Time.Day,
    enforcedAsSubPlace :: Kernel.Prelude.Bool,
    faqs :: Kernel.Prelude.Maybe [Domain.Types.TicketPlace.Faq],
    gallery :: [Kernel.Prelude.Text],
    iconUrl :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace,
    isClosed :: Kernel.Prelude.Bool,
    isRecurring :: Kernel.Prelude.Bool,
    lat :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    lon :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    mapImageUrl :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    metadata :: Kernel.Prelude.Maybe [Domain.Types.TicketPlace.Metadata],
    name :: Kernel.Prelude.Text,
    openTimings :: Kernel.Prelude.Maybe Kernel.Prelude.TimeOfDay,
    placeType :: Domain.Types.TicketPlace.PlaceType,
    platformFee :: Kernel.Prelude.Maybe Domain.Types.TicketPlace.Fee,
    platformFeeVendor :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    pricingOnwards :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    priority :: Kernel.Prelude.Int,
    recommend :: Kernel.Prelude.Bool,
    rules :: Kernel.Prelude.Maybe [SharedLogic.TicketRule.Core.Rule],
    shortDesc :: Kernel.Prelude.Text,
    startDate :: Kernel.Prelude.Maybe Data.Time.Day,
    status :: Domain.Types.TicketPlace.PlaceStatus,
    termsAndConditions :: [Kernel.Prelude.Text],
    termsAndConditionsUrl :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    ticketMerchantId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    venue :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data CustomTab = CustomTab {body :: Kernel.Prelude.Text, header :: Kernel.Prelude.Text} deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Eq, Ord)

data Faq = Faq {answer :: Kernel.Prelude.Text, question :: Kernel.Prelude.Text} deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Eq, Ord)

data Fee = Percentage Kernel.Prelude.Int | Flat Kernel.Prelude.Double deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data Metadata = Metadata {icon :: Kernel.Prelude.Maybe Kernel.Prelude.Text, key :: Kernel.Prelude.Text, value :: Kernel.Prelude.Text} deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Eq, Ord)

data PlaceStatus = Active | Inactive | ComingSoon | Ended | Unpublished deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data PlaceType
  = Museum
  | ThemePark
  | AmusementPark
  | WaterPark
  | WildLifeSanctuary
  | ArtGallery
  | HeritageSite
  | ReligiousSite
  | Boating
  | Other
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''Fee)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''PlaceStatus)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''PlaceType)
