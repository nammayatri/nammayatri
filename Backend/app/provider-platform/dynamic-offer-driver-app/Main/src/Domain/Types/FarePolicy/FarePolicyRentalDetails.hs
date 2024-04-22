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

import Control.Lens.Combinators
import Control.Lens.Fold
import "dashboard-helper-api" Dashboard.ProviderPlatform.Merchant
import Data.Aeson as DA
import qualified Data.Aeson.Key as DAK
import qualified Data.Aeson.KeyMap as DAKM
import Data.Aeson.Lens
import Data.List.NonEmpty as NE
import qualified Data.Text as Text
import Domain.Types.Common
import Domain.Types.FarePolicy.FarePolicyRentalDetails.FarePolicyRentalDetailsDistanceBuffer as Reexport
import Kernel.Prelude
import Kernel.Types.Cac
import Kernel.Types.Common
import Kernel.Utils.Logging

data FPRentalDetailsD (s :: UsageSafety) = FPRentalDetails
  { baseFare :: HighPrecMoney,
    perHourCharge :: HighPrecMoney,
    distanceBuffers :: NonEmpty (FPRentalDetailsDistanceBuffersD s),
    perExtraKmRate :: HighPrecMoney,
    perExtraMinRate :: HighPrecMoney,
    includedKmPerHr :: Kilometers,
    plannedPerKmRate :: HighPrecMoney,
    currency :: Currency,
    maxAdditionalKmsLimit :: Kilometers,
    totalAdditionalKmsLimit :: Kilometers,
    nightShiftCharge :: Maybe NightShiftCharge
  }
  deriving (Generic, Show)

type FPRentalDetails = FPRentalDetailsD 'Safe

instance FromJSON (FPRentalDetailsD 'Unsafe)

instance ToJSON (FPRentalDetailsD 'Unsafe)

instance FromJSON (FPRentalDetailsD 'Safe)

instance ToJSON (FPRentalDetailsD 'Safe)

parsingMiddlewareForRental :: String -> DAKM.KeyMap Value -> String -> DAKM.KeyMap Value
parsingMiddlewareForRental config configMap key' =
  let fPRDDB = nonEmpty $ jsonToFPRentalDetailsDistanceBuffers config key'
   in DAKM.insert "distanceBuffers" (DA.toJSON fPRDDB) configMap

jsonToFPRentalDetails :: MonadFlow m => String -> String -> m (Maybe FPRentalDetails)
jsonToFPRentalDetails config key' = do
  let fPRD' = config ^@.. _Value . _Object . reindexed (dropPrefixFromConfig "farePolicyRentalDetails:") (itraversed . indices (Text.isPrefixOf "farePolicyRentalDetails:" . DAK.toText))
      fpRD'' = parsingMiddlewareForRental config (DAKM.fromList fPRD') key'
      res = Object fpRD'' ^? _JSON :: (Maybe FPRentalDetails)
  when (isNothing res) do
    logDebug $ "FarePolicyRentalDetails from CAC Not Parsable: " <> show fPRD' <> " after middle parsing" <> show fpRD'' <> " for key: " <> Text.pack key'
  pure res
