{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.EventManagement where

import Data.Aeson
import qualified Data.Time as Time
import qualified Domain.Types.BusinessHour as DBusinessHour
import qualified Domain.Types.ServiceCategory as DServiceCategory
import qualified Domain.Types.ServicePeopleCategory as DServicePeopleCategory
import qualified Domain.Types.SpecialOccasion as DSpecialOccasion
import qualified Domain.Types.TicketPlace as DTicketPlace
import qualified Domain.Types.TicketService as DTicketService
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Types.TimeBound (TimeBound (..))
import SharedLogic.TicketRule.Core (Rule)
import qualified Tools.Payment as Payment

data TicketPlaceDef = TicketPlaceDef
  { id :: Id DTicketPlace.TicketPlace,
    basicInformation :: BasicInformation,
    services :: [TicketServiceDef],
    serviceCategories :: [ServiceCategoryDef],
    servicePeopleCategories :: [ServicePeopleCategoryDef],
    isDraft :: Bool
    --rules :: Maybe [Rule]
    --specialOccasions :: [SpecialOccasionDetails]
  }
  deriving (Generic, Show, Eq, Ord, ToJSON, FromJSON, ToSchema)

data BasicInformation = BasicInformation
  { name :: Text,
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
    customTabs :: Maybe [DTicketPlace.CustomTab],
    rules :: Maybe [Rule]
  }
  deriving (Generic, Show, Ord, Eq, ToJSON, FromJSON, ToSchema)

data TicketServiceDef = TicketServiceDef
  { id :: Id DTicketService.TicketService,
    service :: Text,
    shortDesc :: Maybe Text,
    operationalDays :: [Text],
    operationalDate :: Maybe DTicketService.OperationalDate,
    maxVerification :: Int,
    allowFutureBooking :: Bool,
    allowCancellation :: Bool,
    expiry :: DTicketService.ExpiryType,
    serviceCategoryId :: [Id DServiceCategory.ServiceCategory],
    rules :: Maybe [Rule]
  }
  deriving (Generic, Show, Eq, Ord, ToJSON, FromJSON, ToSchema)

data BusinessHourDef = BusinessHourDef
  { btype :: DBusinessHour.BusinessHourType,
    bookingClosingTime :: Maybe TimeOfDay
  }
  deriving (Generic, Show, Eq, Ord, ToJSON, FromJSON, ToSchema)

data ServiceCategoryDef = ServiceCategoryDef
  { id :: Id DServiceCategory.ServiceCategory,
    name :: Text,
    description :: Text,
    allowedSeats :: Maybe Int,
    businessHours :: [BusinessHourDef],
    peopleCategory :: [Id DServicePeopleCategory.ServicePeopleCategory],
    rules :: Maybe [Rule]
  }
  deriving (Generic, Show, Eq, Ord, ToJSON, FromJSON, ToSchema)

data ServicePeopleCategoryDef = ServicePeopleCategoryDef
  { id :: Id DServicePeopleCategory.ServicePeopleCategory,
    name :: Text,
    description :: Text,
    pricingType :: DServicePeopleCategory.PricingType,
    priceAmount :: HighPrecMoney,
    priceCurrency :: Currency,
    rules :: Maybe [Rule]
  }
  deriving (Generic, Show, Eq, Ord, ToJSON, FromJSON, ToSchema)

-- TODO: Add these back in
-- data SpecialOccasionDetails = SpecialOccasionDetails
--   { id :: Id DSpecialOccasion.SpecialOccasion,
--     entityId :: Text,
--     date :: Maybe Time.Day,
--     dayOfWeek :: Maybe Text,
--     specialDayType :: DSpecialOccasion.SpecialDayType,
--     description :: Maybe Text,
--     businessHours :: [Id DBusinessHour.BusinessHour],
--     placeId :: Maybe Text,
--     name :: Text
--   }
--   deriving (Generic, Show, Eq, Ord, ToJSON, FromJSON, ToSchema)
