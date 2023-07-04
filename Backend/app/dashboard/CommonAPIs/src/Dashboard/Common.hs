{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Dashboard.Common where

import Data.Aeson
import Data.OpenApi hiding (description)
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id (Id)

data Customer

data Driver

data User

data Image

data Ride

data Message

data File

data Receiver

data Booking

data IssueReport

data IssueCategory

data FarePolicy

data Merchant

data SpecialLocation

data Variant = SEDAN | SUV | HATCHBACK | AUTO_RICKSHAW | TAXI | TAXI_PLUS
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FlowType
  = RIDE_OTP
  | NORMAL
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data Area
  = Pickup (Id SpecialLocation)
  | Drop (Id SpecialLocation)
  | Default
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- | Hide secrets before storing request (or response) to DB.
--
-- By default considered that request type has no secrets.
-- So you need to provide manual type instance to hide secrets, if there are.
class ToJSON (ReqWithoutSecrets req) => HideSecrets req where
  type ReqWithoutSecrets req
  hideSecrets :: req -> ReqWithoutSecrets req
  type ReqWithoutSecrets req = req

-- FIXME next default implementation is not working
-- default hideSecrets :: req -> req
-- hideSecrets = identity

instance HideSecrets () where
  hideSecrets = identity

data Summary = Summary
  { totalCount :: Int, --TODO add db indexes
    count :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ListItemResult = SuccessItem | FailItem Text
  deriving stock (Show, Generic)

instance ToJSON ListItemResult where
  toJSON = genericToJSON listItemOptions

instance FromJSON ListItemResult where
  parseJSON = genericParseJSON listItemOptions

instance ToSchema ListItemResult where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions listItemOptions

listItemOptions :: Options
listItemOptions =
  defaultOptions
    { sumEncoding = listItemTaggedObject
    }

listItemTaggedObject :: SumEncoding
listItemTaggedObject =
  TaggedObject
    { tagFieldName = "result",
      contentsFieldName = "errorMessage"
    }

-- is it correct to show every error?
listItemErrHandler :: Monad m => SomeException -> m ListItemResult
listItemErrHandler = pure . FailItem . show @Text @SomeException

data UsageSafety = Safe | Unsafe

data DriverExtraFeeBounds = DriverExtraFeeBounds
  { startDistance :: Meters,
    minFee :: Money,
    maxFee :: Money
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FarePolicyD (s :: UsageSafety) = FarePolicy
  { id :: Id FarePolicyN,
    driverExtraFeeBounds :: Maybe (NonEmpty DriverExtraFeeBounds),
    serviceCharge :: Maybe Money,
    nightShiftBounds :: Maybe NightShiftBounds,
    allowedTripDistanceBounds :: Maybe AllowedTripDistanceBounds,
    govtCharges :: Maybe Double,
    farePolicyDetails :: FarePolicyDetailsD s,
    description :: Maybe Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type FarePolicyN = FarePolicyD 'Safe

data FarePolicyDetailsD (s :: UsageSafety) = ProgressiveDetails (FPProgressiveDetailsD s) | SlabsDetails (FPSlabsDetailsD s)
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type FarePolicyDetails = FarePolicyDetailsD 'Safe

newtype FPSlabsDetailsD (s :: UsageSafety) = FPSlabsDetails
  { slabs :: NonEmpty (FPSlabsDetailsSlabD s)
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type FPSlabsDetails = FPSlabsDetailsD 'Safe

data FPSlabsDetailsSlabD (s :: UsageSafety) = FPSlabsDetailsSlab
  { startDistance :: Meters,
    baseFare :: Money,
    waitingChargeInfo :: Maybe WaitingChargeInfo,
    platformFeeInfo :: Maybe PlatformFeeInfo,
    nightShiftCharge :: Maybe NightShiftCharge
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type FPSlabsDetailsSlab = FPSlabsDetailsSlabD 'Safe

data PlatformFeeCharge = ProgressivePlatformFee HighPrecMoney | ConstantPlatformFee Money
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PlatformFeeInfo = PlatformFeeInfo
  { platformFeeCharge :: PlatformFeeCharge,
    cgst :: Double,
    sgst :: Double
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data NightShiftBounds = NightShiftBounds
  { nightShiftStart :: TimeOfDay,
    nightShiftEnd :: TimeOfDay
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AllowedTripDistanceBounds = AllowedTripDistanceBounds
  { maxAllowedTripDistance :: Meters,
    minAllowedTripDistance :: Meters
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FarePolicyType = Progressive | Slabs deriving stock (Show, Read)

data FPProgressiveDetailsD (s :: UsageSafety) = FPProgressiveDetails
  { baseFare :: Money,
    baseDistance :: Meters,
    perExtraKmRateSections :: NonEmpty (FPProgressiveDetailsPerExtraKmRateSectionD s),
    deadKmFare :: Money,
    waitingChargeInfo :: Maybe WaitingChargeInfo,
    nightShiftCharge :: Maybe NightShiftCharge
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type FPProgressiveDetails = FPProgressiveDetailsD 'Safe

data WaitingChargeInfo = WaitingChargeInfo
  { freeWaitingTime :: Minutes,
    waitingCharge :: WaitingCharge
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data WaitingCharge = PerMinuteWaitingCharge HighPrecMoney | ConstantWaitingCharge Money
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data NightShiftCharge = ProgressiveNightShiftCharge Float | ConstantNightShiftCharge Money
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FPProgressiveDetailsPerExtraKmRateSectionD (s :: UsageSafety) = FPProgressiveDetailsPerExtraKmRateSection
  { startDistance :: Meters,
    perExtraKmRate :: HighPrecMoney
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type FPProgressiveDetailsPerExtraKmRateSection = FPProgressiveDetailsPerExtraKmRateSectionD 'Safe

data FullFarePolicyD (s :: UsageSafety) = FullFarePolicy
  { id :: Id FarePolicyN,
    merchantId :: Id Merchant,
    driverExtraFeeBounds :: Maybe (NonEmpty DriverExtraFeeBounds),
    serviceCharge :: Maybe Money,
    nightShiftBounds :: Maybe NightShiftBounds,
    allowedTripDistanceBounds :: Maybe AllowedTripDistanceBounds,
    govtCharges :: Maybe Double,
    farePolicyDetails :: FarePolicyDetailsD s,
    description :: Maybe Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type FullFarePolicyUpdateReq = FullFarePolicyD 'Safe

instance HideSecrets FullFarePolicyUpdateReq where
  hideSecrets = identity
