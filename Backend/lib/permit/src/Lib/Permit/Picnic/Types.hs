-- | Details of a picnic journey application (Assam SOP "Picnic Journeys by Road").
-- Only what the SOP requires is collected.
module Lib.Permit.Picnic.Types where

import Data.Time (Day)
import Kernel.Prelude

data PicnicVehicleCategory
  = CAR
  | SUV_VAN
  | TRAVELLER
  | BUS
  | GOODS
  | TRUCK
  | TRACTOR
  | TRAILER
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)

data PicnicVehicleUsage = PRIVATE_USE | HIRED
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)

data PicnicVehicle = PicnicVehicle
  { registrationNo :: Text,
    category :: PicnicVehicleCategory,
    usage :: PicnicVehicleUsage,
    seatingCapacity :: Int,
    ownerName :: Text,
    ownerMobileNumber :: Text,
    fitnessRequired :: Bool,
    permitRequired :: Bool,
    speedLimiterRequired :: Bool
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data PicnicChallans = PicnicChallans
  { onRecord :: Int,
    unpaid :: Int,
    unpaidAmount :: Int
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

-- | Licence validity is not repeated here: it is the valid-until date of the
-- driving licence document.
data PicnicDriver = PicnicDriver
  { name :: Text,
    mobileNumber :: Text,
    licenceNo :: Text,
    licenceClass :: Text,
    licenceClassCovers :: Bool,
    suspended :: Bool,
    recentConviction :: Bool,
    medicallyFit :: Bool,
    knowsRoute :: Bool
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

-- | 'departureTime' and 'returnTime' are "HH:MM" in Indian Standard Time.
data PicnicDetails = PicnicDetails
  { leaderName :: Text,
    travelDate :: Day,
    departureTime :: Text,
    returnTime :: Text,
    startPoint :: Text,
    destinationDistrict :: Text,
    spot :: Text,
    routeOut :: Text,
    routeBack :: Text,
    driveMinutes :: Int,
    vehicle :: PicnicVehicle,
    challans :: PicnicChallans,
    driver :: PicnicDriver,
    handymanName :: Maybe Text,
    passengerNames :: [Text]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)
