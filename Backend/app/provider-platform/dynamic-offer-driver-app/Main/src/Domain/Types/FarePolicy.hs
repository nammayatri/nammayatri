{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.FarePolicy (module Reexport, module Domain.Types.FarePolicy) where

import Control.Lens.Combinators
import Control.Lens.Fold
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Merchant as DPM
import qualified Data.Aeson as DA
import Data.Aeson.Key as DAK
import qualified Data.Aeson.KeyMap as DAKM
import Data.Aeson.Lens
import Data.Aeson.Types
import Data.List.NonEmpty
import Data.Text as Text
import qualified Data.Vector as DV
import qualified Domain.Types.Common as DTC
import Domain.Types.FarePolicy.DriverExtraFeeBounds as Reexport
import Domain.Types.FarePolicy.FarePolicyProgressiveDetails as Reexport
import Domain.Types.FarePolicy.FarePolicyRentalDetails as Reexport
import Domain.Types.FarePolicy.FarePolicySlabsDetails as Reexport
import Domain.Types.Merchant
import Domain.Types.Vehicle.Variant
import Kernel.Prelude as KP
import Kernel.Types.Cac
import Kernel.Types.Common
import Kernel.Types.Id as KTI
import Tools.Beam.UtilsTH (mkBeamInstancesForEnum)

data FarePolicyD (s :: DTC.UsageSafety) = FarePolicy
  { id :: Id FarePolicy,
    driverExtraFeeBounds :: Maybe (NonEmpty DriverExtraFeeBounds),
    serviceCharge :: Maybe Money,
    nightShiftBounds :: Maybe DPM.NightShiftBounds,
    allowedTripDistanceBounds :: Maybe DPM.AllowedTripDistanceBounds,
    govtCharges :: Maybe Double,
    perMinuteRideExtraTimeCharge :: Maybe HighPrecMoney,
    farePolicyDetails :: FarePolicyDetailsD s,
    description :: Maybe Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show)

jsonToDriverExtraFeeBounds :: String -> String -> Maybe (NonEmpty DriverExtraFeeBounds)
jsonToDriverExtraFeeBounds config key' =
  let res' =
        config
          ^@.. _Value
            . _Object
            . reindexed
              (dropPrefixFromConfig "farePolicyDriverExtraFeeBounds:")
              ( itraversed
                  . indices
                    ( Text.isPrefixOf
                        "farePolicyDriverExtraFeeBounds:"
                        . DAK.toText
                    )
              )
      res'' =
        fromMaybe
          (DA.Array (DV.fromList []))
          (DAKM.lookup (DAK.fromText (Text.pack key')) (DAKM.fromList res'))
      res = res'' ^? _JSON :: Maybe [DriverExtraFeeBounds]
   in nonEmpty $ fromMaybe [] res

farePolicyMiddleWare :: DAKM.KeyMap Value -> String -> String -> DAKM.KeyMap Value
farePolicyMiddleWare configMap config key' = do
  let nightShiftStart = DAKM.lookup "nightShiftStart" configMap >>= fromJSONHelper
      nightShiftEnd = DAKM.lookup "nightShiftEnd" configMap >>= fromJSONHelper
      maxAllowedTripDistance = DAKM.lookup "maxAllowedTripDistance" configMap >>= fromJSONHelper
      minAllowedTripDistance = DAKM.lookup "minAllowedTripDistance" configMap >>= fromJSONHelper
      dEFB = jsonToDriverExtraFeeBounds config key'
      nightShiftBounds = DPM.NightShiftBounds <$> nightShiftStart <*> nightShiftEnd
      allowedTripDistanceBounds = DPM.AllowedTripDistanceBounds <$> maxAllowedTripDistance <*> minAllowedTripDistance
      configMap' = KP.foldr DAKM.delete configMap ["nightShiftStart", "nightShiftEnd", "maxAllowedTripDistance", "minAllowedTripDistance"]
      configMap'' = case DAKM.lookup "farePolicyType" configMap' of
        Just (String "Progressive") -> toJSON $ ProgressiveDetails <$> jsonToFPProgressiveDetails config key'
        Just (String "Slabs") -> toJSON $ SlabsDetails <$> getFPSlabDetailsSlab config key'
        Just (String "Rental") -> toJSON $ RentalDetails <$> jsonToFPRentalDetails config key'
        _ -> toJSON (Nothing :: Maybe FarePolicyDetails)
  KP.foldr (\(k, v) acc -> DAKM.insert k v acc) configMap' [("nightShiftBounds", DA.toJSON nightShiftBounds), ("allowedTripDistanceBounds", DA.toJSON allowedTripDistanceBounds), ("driverExtraFeeBounds", DA.toJSON dEFB), ("farePolicyDetails", configMap'')]

jsonToFarePolicy :: String -> String -> Maybe FarePolicy
jsonToFarePolicy config key' = do
  let res' =
        config
          ^@.. _Value
            . _Object
            . reindexed
              (dropPrefixFromConfig "farePolicy:")
              ( itraversed
                  . indices
                    ( Text.isPrefixOf
                        "farePolicy:"
                        . DAK.toText
                    )
              )
      res'' = farePolicyMiddleWare (DAKM.fromList res') config key'
      res = Object res'' ^? _JSON :: (Maybe FarePolicy)
  res

type FarePolicy = FarePolicyD 'DTC.Safe

instance FromJSON (FarePolicyD 'DTC.Unsafe)

instance ToJSON (FarePolicyD 'DTC.Unsafe)

instance FromJSON FarePolicy

instance ToJSON FarePolicy

data FarePolicyDetailsD (s :: DTC.UsageSafety) = ProgressiveDetails (FPProgressiveDetailsD s) | SlabsDetails (FPSlabsDetailsD s) | RentalDetails (FPRentalDetailsD s)
  deriving (Generic, Show)

type FarePolicyDetails = FarePolicyDetailsD 'DTC.Safe

instance FromJSON (FarePolicyDetailsD 'DTC.Unsafe)

instance ToJSON (FarePolicyDetailsD 'DTC.Unsafe)

instance FromJSON (FarePolicyDetailsD 'DTC.Safe)

instance ToJSON (FarePolicyDetailsD 'DTC.Safe)

data FarePolicyType = Progressive | Slabs | Rental
  deriving stock (Show, Eq, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

$(mkBeamInstancesForEnum ''FarePolicyType)

data FullFarePolicyD (s :: DTC.UsageSafety) = FullFarePolicy
  { id :: Id FarePolicy,
    merchantId :: Id Merchant,
    vehicleVariant :: Variant,
    tripCategory :: DTC.TripCategory,
    driverExtraFeeBounds :: Maybe (NonEmpty DriverExtraFeeBounds),
    serviceCharge :: Maybe Money,
    nightShiftBounds :: Maybe DPM.NightShiftBounds,
    allowedTripDistanceBounds :: Maybe DPM.AllowedTripDistanceBounds,
    govtCharges :: Maybe Double,
    perMinuteRideExtraTimeCharge :: Maybe HighPrecMoney,
    farePolicyDetails :: FarePolicyDetailsD s,
    description :: Maybe Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show)

type FullFarePolicy = FullFarePolicyD 'DTC.Safe

instance FromJSON (FullFarePolicyD 'DTC.Unsafe)

instance ToJSON (FullFarePolicyD 'DTC.Unsafe)

instance FromJSON FullFarePolicy

instance ToJSON FullFarePolicy

type FullDriverExtraFeeBounds = (Id FarePolicy, DriverExtraFeeBounds)

type FullFarePolicyProgressiveDetails = (Id FarePolicy, FPProgressiveDetails)

type FullFarePolicyRentalDetails = (Id FarePolicy, FPRentalDetails)

farePolicyToFullFarePolicy :: Id Merchant -> Variant -> DTC.TripCategory -> FarePolicy -> FullFarePolicy
farePolicyToFullFarePolicy merchantId vehicleVariant tripCategory FarePolicy {..} =
  FullFarePolicy {..}

fullFarePolicyToFarePolicy :: FullFarePolicy -> FarePolicy
fullFarePolicyToFarePolicy FullFarePolicy {..} =
  FarePolicy {..}

getFarePolicyType :: FarePolicy -> FarePolicyType
getFarePolicyType farePolicy = case farePolicy.farePolicyDetails of
  ProgressiveDetails _ -> Progressive
  SlabsDetails _ -> Slabs
  RentalDetails _ -> Rental
