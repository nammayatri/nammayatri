{-
 Copyright 2022-23, Juspay India Pvt Ltd
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.FarePolicy.FarePolicyRentalDetails
  ( module Reexport,
    module Domain.Types.FarePolicy.FarePolicyRentalDetails,
  )
where

import "dashboard-helper-api" Dashboard.ProviderPlatform.Merchant
import Data.Aeson as DA
import qualified Data.Aeson.Key as DAK
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as BL
import Data.List.NonEmpty as NE
import qualified Data.Text as Text
import qualified Data.Text.Encoding as DTE
import Domain.Types.Common
import Domain.Types.FarePolicy.FarePolicyRentalDetails.FarePolicyRentalDetailsDistanceBuffer as Reexport
import Kernel.Prelude
import qualified Kernel.Prelude as KP
import Kernel.Types.Common

data FPRentalDetailsD (s :: UsageSafety) = FPRentalDetails
  { baseFare :: Money,
    perHourCharge :: Money,
    distanceBuffers :: NonEmpty (FPRentalDetailsDistanceBuffersD s),
    perExtraKmRate :: Money,
    perExtraMinRate :: Money,
    includedKmPerHr :: Kilometers,
    plannedPerKmRate :: Money,
    maxAdditionalKmsLimit :: Kilometers,
    totalAdditionalKmsLimit :: Kilometers,
    nightShiftCharge :: Maybe NightShiftCharge
  }
  deriving (Generic, Show)

type FPRentalDetails = FPRentalDetailsD 'Safe

instance FromJSON (FPRentalDetailsD 'Unsafe)

instance ToJSON (FPRentalDetailsD 'Unsafe)

readWithInfo :: (Read a, Show a) => String -> Value -> a
readWithInfo mes s = case s of
  String str -> case KP.readMaybe (Text.unpack str) of
    Just val -> val
    Nothing -> error . Text.pack $ "Failed to parse: for key: mes " <> mes <> " and value: " ++ Text.unpack str
  Number scientific -> case KP.readMaybe (show scientific) of
    Just val -> val
    Nothing -> error . Text.pack $ "Failed to parse: for key: mes " <> mes <> " and value: " ++ show scientific
  _ -> error $ "Not able to parse value" <> show s

readWithInfo' :: (Read a, Show a) => String -> Value -> Maybe a
readWithInfo' msg s = case s of
  String str -> case KP.readMaybe (Text.unpack str) of
    Just val -> Just val
    Nothing -> Nothing
  Number scientific -> case KP.readMaybe (show scientific) of
    Just val -> Just val
    Nothing -> Nothing
  Null -> Nothing
  _ -> error . Text.pack $ "Failed to parse: for key: mes " <> msg <> " and value: " ++ show s

valueToType :: FromJSON a => Value -> Maybe a
valueToType value = case value of
  String str -> DA.decode (BL.fromStrict (DTE.encodeUtf8 (replaceSingleQuotes str)))
  _ -> error $ "Not able to parse value" <> show value

jsonToFPRentalDetails :: String -> Object -> Parser FPRentalDetails
jsonToFPRentalDetails id obj = do
  fPRDDB' <- jsonToFPRentalDetailsDistanceBuffers id obj
  let fPRDDB = fromMaybe (error "fare policy distance buffers not found") (nonEmpty fPRDDB')
  baseFare <- (readWithInfo "farePolicyRentalDetails:baseFare" <$> (obj .: DAK.fromText (Text.pack ("farePolicyRentalDetails:baseFare")))) :: Parser Money
  perHourCharge <- (readWithInfo "farePolicyRentalDetails:perHourCharge" <$> (obj .: DAK.fromText (Text.pack ("farePolicyRentalDetails:perHourCharge")))) :: Parser Money
  perExtraKmRate <- (readWithInfo "farePolicyRentalDetails:perExtraKmRate" <$> (obj .: DAK.fromText (Text.pack ("farePolicyRentalDetails:perExtraKmRate")))) :: Parser Money
  perExtraMinRate <- (readWithInfo "farePolicyRentalDetails:perExtraMinRate" <$> (obj .: DAK.fromText (Text.pack ("farePolicyRentalDetails:perExtraMinRate")))) :: Parser Money
  includedKmPerHr <- (readWithInfo "farePolicyRentalDetails:includedKmPerHr" <$> (obj .: DAK.fromText (Text.pack ("farePolicyRentalDetails:includedKmPerHr")))) :: Parser Kilometers
  plannedPerKmRate <- (readWithInfo "farePolicyRentalDetails:plannedPerKmRate" <$> (obj .: DAK.fromText (Text.pack ("farePolicyRentalDetails:plannedPerKmRate")))) :: Parser Money
  maxAdditionalKmsLimit <- (readWithInfo "farePolicyRentalDetails:maxAdditionalKmsLimit" <$> (obj .: DAK.fromText (Text.pack ("farePolicyRentalDetails:maxAdditionalKmsLimit")))) :: Parser Kilometers
  totalAdditionalKmsLimit <- (readWithInfo "farePolicyRentalDetails:totalAdditionalKmsLimit" <$> (obj .: DAK.fromText (Text.pack ("farePolicyRentalDetails:totalAdditionalKmsLimit")))) :: Parser Kilometers
  nightShiftCharge <- (valueToType <$> (obj .: DAK.fromText (Text.pack ("farePolicyRentalDetails:nightShiftCharge")))) :: Parser (Maybe NightShiftCharge)
  return $ FPRentalDetails baseFare perHourCharge fPRDDB perExtraKmRate perExtraMinRate includedKmPerHr plannedPerKmRate maxAdditionalKmsLimit totalAdditionalKmsLimit nightShiftCharge
