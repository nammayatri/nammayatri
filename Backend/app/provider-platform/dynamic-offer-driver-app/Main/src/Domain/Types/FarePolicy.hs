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

-- import Data.Aeson as DA

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
import Debug.Trace as T
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

-- import Kernel.Utils.Logging
-- import Kernel.Utils.Common (CacheFlow)

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

-- jsonToFullDriverExtraFeeBounds :: String -> Object -> Parser (Id FarePolicy, [DriverExtraFeeBounds])
-- jsonToFullDriverExtraFeeBounds fpId v =
--   (,) <$> (pure (Id (Text.pack fpId))) <*> (jsonToDriverExtraFeeBounds v fpId)

-- jsontToNightShiftBounds :: Object -> Parser (Maybe DPM.NightShiftBounds)
-- jsontToNightShiftBounds k = do
--   nightShiftStart <- (readWithInfo' "farePolicy:nightShiftStart" <$> (k .: DAK.fromText (Text.pack "farePolicy:nightShiftStart"))) :: Parser (Maybe TimeOfDay)
--   nightShiftEnd <- (readWithInfo' "farePolicy:nightShiftEnd" <$> (k .: DAK.fromText (Text.pack "farePolicy:nightShiftEnd"))) :: Parser (Maybe TimeOfDay)
--   pure $ DPM.NightShiftBounds <$> nightShiftStart <*> nightShiftEnd

-- jsonToAllowedTripDistanceBounds :: Object -> Parser (Maybe DPM.AllowedTripDistanceBounds)
-- jsonToAllowedTripDistanceBounds k = do
--   maxAllowedTripDistance <- (readWithInfo' "farePolicy:maxAllowedTripDistance" <$> (k .: DAK.fromText (Text.pack "farePolicy:maxAllowedTripDistance"))) :: Parser (Maybe Meters)
--   minAllowedTripDistance <- (readWithInfo' "farePolicy:minAllowedTripDistance" <$> (k .: DAK.fromText (Text.pack "farePolicy:minAllowedTripDistance"))) :: Parser (Maybe Meters)
--   pure $ DPM.AllowedTripDistanceBounds <$> maxAllowedTripDistance <*> minAllowedTripDistance

jsonToDriverExtraFeeBounds :: String -> String -> Maybe (NonEmpty DriverExtraFeeBounds)
jsonToDriverExtraFeeBounds config key' = do
  let res' = (config ^@.. _Value . _Object . reindexed (dropPrefixFromConfig "farePolicyDriverExtraFeeBounds:") (itraversed . indices (\k -> Text.isPrefixOf "farePolicyDriverExtraFeeBounds:" (DAK.toText k))))
      res'' = fromMaybe (DA.Array (DV.fromList [])) (DAKM.lookup (DAK.fromText (Text.pack key')) (DAKM.fromList res'))
      res = res'' ^? _JSON :: (Maybe [DriverExtraFeeBounds])
  nonEmpty $ fromMaybe [] res

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
      -- Object (fromList [(\\\"contents\\\",Number 1.0),(\\\"tag\\\",String \\\"PerMinuteWaitingCharge\\\")]))
      configMap'' = case DAKM.lookup "farePolicyType" configMap' of
        Just (String "Progressive") -> toJSON $ ProgressiveDetails <$> (jsonToFPProgressiveDetails config key')
        Just (String "Slabs") -> toJSON $ SlabsDetails <$> (getFPSlabDetailsSlab config key')
        Just (String "Rental") -> toJSON $ RentalDetails <$> (jsonToFPRentalDetails config key')
        _ -> (toJSON (Nothing :: Maybe FarePolicyDetails))
  KP.foldr (\(k, v) acc -> DAKM.insert k v acc) configMap' [("nightShiftBounds", DA.toJSON nightShiftBounds), ("allowedTripDistanceBounds", DA.toJSON allowedTripDistanceBounds), ("driverExtraFeeBounds", DA.toJSON dEFB), ("farePolicyDetails", configMap'')]

jsonToFarePolicy :: String -> String -> Maybe FarePolicy
jsonToFarePolicy config key' = do
  let res' = (config ^@.. _Value . _Object . reindexed (dropPrefixFromConfig "farePolicy:") (itraversed . indices (\k -> Text.isPrefixOf "farePolicy:" (DAK.toText k))))
      -- res'' = T.trace ("farepolicy key value here " <> show res') $ fromMaybe (DA.Array (DV.fromList [] )) (DAKM.lookup (DAK.fromText ( Text.pack key')) (DAKM.fromList res'))
      res'' = T.trace ("farepolicy key value here " <> show res') $ farePolicyMiddleWare (DAKM.fromList res') config key'
      res = T.trace ("farepolicy the list i'm getting " <> show res'') $ (Object res'') ^? _JSON :: (Maybe FarePolicy)
  T.trace ("farePolicyParsed" <> show res <> "and " <> show (fromJSON (Object res'') :: Result FarePolicy)) $ res

-- jsonToFarePolicy :: Object -> String -> Parser (Maybe FarePolicy)
-- jsonToFarePolicy k key' = do
--   -- fullDEFB <- jsonToFullDriverExtraFeeBounds key k
--   fDEFB <- jsonToDriverExtraFeeBounds k key'
--   id <- pure (Id (Text.pack key'))
--   serviceCharge <- (readWithInfo' "farePolicy:serviceCharge" <$> (k .: DAK.fromText (Text.pack "farePolicy:serviceCharge")))
--   farePolicyType <- (readWithInfo "farePolicy:farePolicyType" <$> (k .: DAK.fromText (Text.pack "farePolicy:farePolicyType")))
--   nightShiftBounds <- jsontToNightShiftBounds k
--   allowedTripDistanceBounds <- jsonToAllowedTripDistanceBounds k
--   govtCharges <- (readWithInfo' "farePolicy:govtCharges" <$> (k .: DAK.fromText (Text.pack "farePolicy:govtCharges")))
--   perMinuteRideExtraTimeCharge <- (readWithInfo' "farePolicy:perMinuteRideExtraTimeCharge" <$> (k .: DAK.fromText (Text.pack "farePolicy:perMinuteRideExtraTimeCharge"))) :: Parser (Maybe HighPrecMoney)
--   description <- (readWithInfo' "farePolicy:description" <$> (k .: DAK.fromText (Text.pack "farePolicy:description")))
--   createdAt <- (readWithInfo "farePolicy:createdAt" <$> (k .: DAK.fromText (Text.pack "farePolicy:createdAt")))
--   updatedAt <- (readWithInfo "farePolicy:updatedAt" <$> (k .: DAK.fromText (Text.pack "farePolicy:updatedAt")))
--   mFarePolicyDetails <- case farePolicyType of
--     Progressive -> do
--       mFPPD <- jsonToFPProgressiveDetails key' k
--       pure $ Just (ProgressiveDetails mFPPD)
--     Slabs -> do
--       val <- (makeFPSlabsDetails' k key')
--       case val of
--         Just fpsd -> pure $ Just (SlabsDetails fpsd)
--         Nothing -> pure Nothing
--     Rental -> do
--       mFPRD <- jsonToFPRentalDetails key' k
--       pure $ Just (RentalDetails mFPRD)
--   case mFarePolicyDetails of
--     Just farePolicyDetails -> do
--       return . Just $
--         FarePolicy
--           { id,
--             serviceCharge,
--             nightShiftBounds,
--             allowedTripDistanceBounds,
--             govtCharges,
--             driverExtraFeeBounds = nonEmpty fDEFB,
--             farePolicyDetails,
--             perMinuteRideExtraTimeCharge,
--             description,
--             createdAt,
--             updatedAt
--           }
--     Nothing -> do
--       _ <- error "FarePolicyDetails not found"
--       pure $ Nothing

type FarePolicy = FarePolicyD 'DTC.Safe

instance FromJSON (FarePolicyD 'DTC.Unsafe)

instance ToJSON (FarePolicyD 'DTC.Unsafe)

instance FromJSON (FarePolicyD 'DTC.Safe)

instance ToJSON (FarePolicyD 'DTC.Safe)

data FarePolicyDetailsD (s :: DTC.UsageSafety) = ProgressiveDetails (FPProgressiveDetailsD s) | SlabsDetails (FPSlabsDetailsD s) | RentalDetails (FPRentalDetailsD s)
  deriving (Generic, Show)

type FarePolicyDetails = FarePolicyDetailsD 'DTC.Safe

instance FromJSON (FarePolicyDetailsD 'DTC.Unsafe)

instance ToJSON (FarePolicyDetailsD 'DTC.Unsafe)

instance FromJSON (FarePolicyDetailsD 'DTC.Safe)

instance ToJSON (FarePolicyDetailsD 'DTC.Safe)

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

instance FromJSON (FullFarePolicyD 'DTC.Safe)

instance ToJSON (FullFarePolicyD 'DTC.Safe)

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
