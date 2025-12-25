{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.TicketDashboard where

import Data.Aeson
import qualified Data.Time as Time
import qualified Domain.Types.BusinessHour as DBusinessHour
import qualified Domain.Types.ServiceCategory as DServiceCategory
import qualified Domain.Types.ServicePeopleCategory as DServicePeopleCategory
import qualified Domain.Types.SpecialOccasion as DSpecialOccasion
import qualified Domain.Types.TicketPlace as DTicketPlace
import qualified Domain.Types.TicketService as DTicketService
import qualified Domain.Types.TicketSubPlace as DTicketSubPlace
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Types.TimeBound (TimeBound (..))
import SharedLogic.TicketRule.Core (Rule)
import qualified Tools.Payment as Payment

data TicketPlaceDashboardDetails = TicketPlaceDashboardDetails
  { id :: Id DTicketPlace.TicketPlace,
    name :: Text,
    description :: Maybe Text,
    shortDesc :: Text,
    address :: Maybe Text,
    latitude :: Maybe Double,
    longitude :: Maybe Double,
    status :: DTicketPlace.PlaceStatus,
    priority :: Int,
    placeType :: DTicketPlace.PlaceType,
    allowSameDayBooking :: Bool,
    gallery :: [Text],
    iconUrl :: Maybe Text,
    mapImageUrl :: Maybe Text,
    termsAndConditions :: [Text],
    termsAndConditionsUrl :: Maybe Text,
    openTimings :: Maybe TimeOfDay,
    closeTimings :: Maybe TimeOfDay,
    services :: [TicketServiceDetails],
    businessHours :: [BusinessHourDetails],
    serviceCategories :: [ServiceCategoryDetails],
    servicePeopleCategories :: [ServicePeopleCategoryDetails],
    specialOccasions :: [SpecialOccasionDetails],
    faqs :: Maybe [DTicketPlace.Faq],
    metadata :: Maybe [DTicketPlace.Metadata],
    isRecurring :: Maybe Bool,
    platformFee :: Maybe DTicketPlace.Fee,
    platformFeeVendor :: Maybe Text,
    pricingOnwards :: Maybe Int,
    startDate :: Maybe Time.Day,
    endDate :: Maybe Time.Day,
    venue :: Maybe Text,
    rules :: Maybe [Rule],
    assignTicketToBpp :: Maybe Bool,
    customTabs :: Maybe [DTicketPlace.CustomTab],
    recommend :: Maybe Bool,
    enforcedAsSubPlace :: Maybe Bool,
    merchantOperatingCityId :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data TicketServiceDetails = TicketServiceDetails
  { id :: Id DTicketService.TicketService,
    service :: Text,
    shortDesc :: Maybe Text,
    operationalDays :: [Text],
    operationalDate :: Maybe DTicketService.OperationalDate,
    maxVerification :: Int,
    allowFutureBooking :: Bool,
    allowCancellation :: Bool,
    serviceDetails :: Maybe [Text],
    subPlaceId :: Maybe (Id DTicketSubPlace.TicketSubPlace),
    expiry :: DTicketService.ExpiryType,
    businessHours :: [Id DBusinessHour.BusinessHour],
    rules :: Maybe [Rule],
    maxSelection :: Maybe Int,
    note :: Maybe Text,
    priority :: Maybe Int
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data BusinessHourDetails = BusinessHourDetails
  { id :: Id DBusinessHour.BusinessHour,
    name :: Text,
    btype :: DBusinessHour.BusinessHourType,
    categoryId :: [Id DServiceCategory.ServiceCategory],
    bookingClosingTime :: Maybe TimeOfDay
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data ServiceCategoryDetails = ServiceCategoryDetails
  { id :: Id DServiceCategory.ServiceCategory,
    name :: Text,
    description :: Text,
    allowedSeats :: Maybe Int,
    availableSeats :: Maybe Int,
    inclusionPoints :: Maybe [Text],
    peopleCategory :: [Id DServicePeopleCategory.ServicePeopleCategory],
    rules :: Maybe [Rule],
    maxSelection :: Maybe Int
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data ServicePeopleCategoryDetails = ServicePeopleCategoryDetails
  { id :: Id DServicePeopleCategory.ServicePeopleCategory,
    name :: Text,
    description :: Text,
    pricingType :: DServicePeopleCategory.PricingType,
    priceAmount :: HighPrecMoney,
    priceCurrency :: Currency,
    timeBounds :: TimeBound,
    vendorSplitDetails :: Maybe [Payment.VendorSplitDetails],
    rules :: Maybe [Rule],
    iconUrl :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data SpecialOccasionDetails = SpecialOccasionDetails
  { id :: Id DSpecialOccasion.SpecialOccasion,
    entityId :: Text,
    date :: Maybe Time.Day,
    dayOfWeek :: Maybe Text,
    specialDayType :: DSpecialOccasion.SpecialDayType,
    description :: Maybe Text,
    businessHours :: [Id DBusinessHour.BusinessHour],
    placeId :: Maybe Text,
    name :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data TicketPlaceDashboardDetailsUpdateResp = TicketPlaceDashboardDetailsUpdateResp
  { success :: Bool,
    message :: Maybe Text,
    placeId :: Id DTicketPlace.TicketPlace
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
