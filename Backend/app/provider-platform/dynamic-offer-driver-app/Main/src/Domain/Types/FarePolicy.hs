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

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Merchant as DPM
-- import Kernel.Prelude as KP
-- import Data.Aeson as DA
import Data.Aeson.Key as DAK
import Data.Aeson.Types
import Data.List.NonEmpty
import Data.Text as Text
import Domain.Types.Common
import Domain.Types.FarePolicy.DriverExtraFeeBounds as Reexport hiding (replaceSingleQuotes)
import Domain.Types.FarePolicy.FarePolicyProgressiveDetails as Reexport hiding (listToType, readWithInfo, replaceSingleQuotes)
import Domain.Types.FarePolicy.FarePolicySlabsDetails as Reexport hiding (listToType, readWithInfo, replaceSingleQuotes)
import Domain.Types.Merchant
import Domain.Types.Vehicle.Variant
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id as KTI
import Tools.Beam.UtilsTH (mkBeamInstancesForEnum)

data FarePolicyD (s :: UsageSafety) = FarePolicy
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
  deriving (Generic)

jsonToFullDriverExtraFeeBounds :: String -> Object -> Parser (Id FarePolicy, [DriverExtraFeeBounds])
jsonToFullDriverExtraFeeBounds fpId v =
  (,) <$> (pure (Id (Text.pack fpId))) <*> (jsonToDriverExtraFeeBounds v fpId)

jsontToNightShiftBounds :: Object -> Parser (Maybe DPM.NightShiftBounds)
jsontToNightShiftBounds k = do
  nightShiftStart <- (readWithInfo' "farePolicy:nightShiftStart" <$> (k .: DAK.fromText (Text.pack "farePolicy:nightShiftStart"))) :: Parser (Maybe TimeOfDay)
  nightShiftEnd <- (readWithInfo' "farePolicy:nightShiftEnd" <$> (k .: DAK.fromText (Text.pack "farePolicy:nightShiftEnd"))) :: Parser (Maybe TimeOfDay)
  pure $ DPM.NightShiftBounds <$> nightShiftStart <*> nightShiftEnd

jsonToAllowedTripDistanceBounds :: Object -> Parser (Maybe DPM.AllowedTripDistanceBounds)
jsonToAllowedTripDistanceBounds k = do
  maxAllowedTripDistance <- (readWithInfo' "farePolicy:maxAllowedTripDistance" <$> (k .: DAK.fromText (Text.pack "farePolicy:maxAllowedTripDistance"))) :: Parser (Maybe Meters)
  minAllowedTripDistance <- (readWithInfo' "farePolicy:minAllowedTripDistance" <$> (k .: DAK.fromText (Text.pack "farePolicy:minAllowedTripDistance"))) :: Parser (Maybe Meters)
  pure $ DPM.AllowedTripDistanceBounds <$> maxAllowedTripDistance <*> minAllowedTripDistance

jsonToFarePolicy :: Object -> String -> Parser (Maybe FarePolicy)
jsonToFarePolicy k key = do
  -- fullDEFB <- jsonToFullDriverExtraFeeBounds key k
  fDEFB <- jsonToDriverExtraFeeBounds k key
  id <- (readWithInfo "farePolicy:id" <$> (k .: DAK.fromText (Text.pack "farePolicy:id"))) :: Parser (Id FarePolicy)
  serviceCharge <- (readWithInfo' "farePolicy:serviceCharge" <$> (k .: DAK.fromText (Text.pack "farePolicy:serviceCharge"))) :: Parser (Maybe Money)
  farePolicyType <- (readWithInfo "farePolicy:farePolicyType" <$> (k .: DAK.fromText (Text.pack "farePolicy:farePolicyType"))) :: Parser FarePolicyType
  nightShiftBounds <- jsontToNightShiftBounds k
  allowedTripDistanceBounds <- jsonToAllowedTripDistanceBounds k
  govtCharges <- (readWithInfo' "farePolicy:govtCharges" <$> (k .: DAK.fromText (Text.pack "farePolicy:govtCharges"))) :: Parser (Maybe Double)
  perMinuteRideExtraTimeCharge <- (readWithInfo' "farePolicy:perMinuteRideExtraTimeCharge" <$> (k .: DAK.fromText (Text.pack "farePolicy:perMinuteRideExtraTimeCharge"))) :: Parser (Maybe HighPrecMoney)
  description <- (readWithInfo' "farePolicy:description" <$> (k .: DAK.fromText (Text.pack "farePolicy:description"))) :: Parser (Maybe Text)
  createdAt <- (readWithInfo "farePolicy:createdAt" <$> (k .: DAK.fromText (Text.pack "farePolicy:createdAt"))) :: Parser UTCTime
  updatedAt <- (readWithInfo "farePolicy:updatedAt" <$> (k .: DAK.fromText (Text.pack "farePolicy:updatedAt"))) :: Parser UTCTime
  mFarePolicyDetails <- case farePolicyType of
    Progressive -> do
      mFPPD <- jsonToFPProgressiveDetails key k
      pure $ Just (ProgressiveDetails mFPPD)
    Slabs -> do
      val <- (makeFPSlabsDetails' k key)
      case val of
        Just fpsd -> pure $ Just (SlabsDetails fpsd)
        Nothing -> pure Nothing

  case mFarePolicyDetails of
    Just farePolicyDetails -> do
      return . Just $
        FarePolicy
          { id,
            serviceCharge,
            nightShiftBounds,
            allowedTripDistanceBounds,
            govtCharges,
            driverExtraFeeBounds = nonEmpty fDEFB,
            farePolicyDetails,
            perMinuteRideExtraTimeCharge,
            description,
            createdAt,
            updatedAt
          }
    Nothing -> do
      _ <- error "FarePolicyDetails not found"
      pure $ Nothing

type FarePolicy = FarePolicyD 'Safe

instance FromJSON (FarePolicyD 'Unsafe)

instance ToJSON (FarePolicyD 'Unsafe)

data FarePolicyDetailsD (s :: UsageSafety) = ProgressiveDetails (FPProgressiveDetailsD s) | SlabsDetails (FPSlabsDetailsD s)
  deriving (Generic, Show)

type FarePolicyDetails = FarePolicyDetailsD 'Safe

instance FromJSON (FarePolicyDetailsD 'Unsafe)

instance ToJSON (FarePolicyDetailsD 'Unsafe)

-- readWithInfo' :: (Read a, Show a) => String -> Value -> Maybe a
-- readWithInfo' msg s = case s of
--   String str -> case KP.readMaybe (Text.unpack str) of
--     Just val -> Just val
--     Nothing -> Nothing
--   Number scientific -> case KP.readMaybe (show scientific) of
--     Just val -> Just val
--     Nothing -> Nothing
--   _ -> error . Text.pack $ "Failed to parse: for key: mes " <>  msg <> " and value: " ++ show s

-- jsonToFarePolicy :: Value -> Parser FarePolicy
-- jsonToFarePolicy v =
--   FarePolicy
--     <$> (Id <$> (v .: DAK.fromText (Text.pack "farePolicy:id")))
--     <*> ((readWithInfo' "driverExtraFeeBounds" <$> (v .: DAK.fromText (Text.pack "farePolicy:driverExtraFeeBounds"))) :: (Parser (Maybe (NonEmpty DriverExtraFeeBounds))))
--     <*> ((readWithInfo' "serviceCharge" <$> (v .: DAK.fromText (Text.pack "farePolicy:serviceCharge"))) :: (Parser (Maybe Money)))
--     <*> ((readWithInfo' "nightShiftBounds" <$> (v .: DAK.fromText (Text.pack "farePolicy:nightShiftBounds"))) :: (Parser (Maybe DPM.NightShiftBounds)))
--     <*> ((readWithInfo' "allowedTripDistanceBounds" <$> (v .: DAK.fromText (Text.pack "farePolicy:allowedTripDistanceBounds"))) :: (Parser (Maybe DPM.AllowedTripDistanceBounds)))
--     <*> ((readWithInfo' "govtCharges" <$> (v .: DAK.fromText (Text.pack "farePolicy:govtCharges"))) :: (Parser (Maybe Double)))
--     <*> ((readWithInfo' "perMinuteRideExtraTimeCharge" <$> (v .: DAK.fromText (Text.pack "farePolicy:perMinuteRideExtraTimeCharge"))) :: (Parser (Maybe HighPrecMoney)))
--     <*> ((v .: DAK.fromText (Text.pack "farePolicy:farePolicyDetails")) >>= jsonToFarePolicyDetails)

data FarePolicyType = Progressive | Slabs
  deriving stock (Show, Eq, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

$(mkBeamInstancesForEnum ''FarePolicyType)

getFarePolicyType :: FarePolicy -> FarePolicyType
getFarePolicyType farePolicy = case farePolicy.farePolicyDetails of
  ProgressiveDetails _ -> Progressive
  SlabsDetails _ -> Slabs

data FullFarePolicy = FullFarePolicy
  { id :: Id FarePolicy,
    merchantId :: Id Merchant,
    vehicleVariant :: Variant,
    driverExtraFeeBounds :: Maybe (NonEmpty DriverExtraFeeBounds),
    serviceCharge :: Maybe Money,
    nightShiftBounds :: Maybe DPM.NightShiftBounds,
    allowedTripDistanceBounds :: Maybe DPM.AllowedTripDistanceBounds,
    govtCharges :: Maybe Double,
    perMinuteRideExtraTimeCharge :: Maybe HighPrecMoney,
    farePolicyDetails :: FarePolicyDetails,
    description :: Maybe Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show)

farePolicyToFullFarePolicy :: Id Merchant -> Variant -> FarePolicy -> FullFarePolicy
farePolicyToFullFarePolicy merchantId vehicleVariant FarePolicy {..} =
  FullFarePolicy {..}

type FullDriverExtraFeeBounds = (Id FarePolicy, DriverExtraFeeBounds)

type FullFarePolicyProgressiveDetails = (Id FarePolicy, FPProgressiveDetails)
