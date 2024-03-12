{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.FarePolicy.FarePolicyProgressiveDetails
  ( module Reexport,
    module Domain.Types.FarePolicy.FarePolicyProgressiveDetails,
  )
where

import Control.Lens.Combinators
import Control.Lens.Fold
import "dashboard-helper-api" Dashboard.ProviderPlatform.Merchant
import qualified Data.Aeson as DA
import Data.Aeson.Key as DAK
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Lens
import Data.Aeson.Types
import Data.List.NonEmpty
import Data.Text as Text
import qualified Data.Vector as DV
import Domain.Types.Common
import Domain.Types.FarePolicy.FarePolicyProgressiveDetails.FarePolicyProgressiveDetailsPerExtraKmRateSection as Reexport
import Kernel.Prelude as KP
import Kernel.Types.Cac
import Kernel.Types.Common

data FPProgressiveDetailsD (s :: UsageSafety) = FPProgressiveDetails
  { baseFare :: Money,
    baseDistance :: Meters,
    perExtraKmRateSections :: NonEmpty (FPProgressiveDetailsPerExtraKmRateSectionD s),
    deadKmFare :: Money,
    waitingChargeInfo :: Maybe WaitingChargeInfo,
    nightShiftCharge :: Maybe NightShiftCharge
  }
  deriving (Generic, Show)

type FPProgressiveDetails = FPProgressiveDetailsD 'Safe

instance FromJSON (FPProgressiveDetailsD 'Unsafe)

instance ToJSON (FPProgressiveDetailsD 'Unsafe)

instance FromJSON (FPProgressiveDetailsD 'Safe)

instance ToJSON (FPProgressiveDetailsD 'Safe)

-----------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------APIEntity--------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------

data FPProgressiveDetailsAPIEntity = FPProgressiveDetailsAPIEntity
  { baseFare :: Money,
    baseDistance :: Meters,
    perExtraKmRateSections :: NonEmpty FPProgressiveDetailsPerExtraKmRateSectionAPIEntity,
    deadKmFare :: Money,
    waitingChargeInfo :: Maybe WaitingChargeInfo,
    nightShiftCharge :: Maybe NightShiftCharge
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

jsonToFPProgressiveDetailsPerExtraKmRateSection :: String -> String -> [FPProgressiveDetailsPerExtraKmRateSection]
jsonToFPProgressiveDetailsPerExtraKmRateSection config key' = do
  let res' =
        config
          ^@.. _Value
            . _Object
            . reindexed
              ( dropPrefixFromConfig
                  "farePolicyProgressiveDetailsPerExtraKmRateSection:"
              )
              ( itraversed
                  . indices
                    ( Text.isPrefixOf
                        "farePolicyProgressiveDetailsPerExtraKmRateSection:"
                        . DAK.toText
                    )
              )
      res'' = fromMaybe (DA.Array (DV.fromList [])) (KM.lookup (DAK.fromText (Text.pack key')) (KM.fromList res'))
      res = res'' ^? _JSON :: (Maybe [FPProgressiveDetailsPerExtraKmRateSection])
  fromMaybe [] res

parsingMiddleware :: KM.KeyMap Value -> String -> String -> KM.KeyMap Value
parsingMiddleware config configS key' =
  let perExtraKmRateSections = jsonToFPProgressiveDetailsPerExtraKmRateSection configS key'
      waitingCharge = KM.lookup "waitingCharge" config >>= fromJSONHelper
      freeWaitingTime = KM.lookup "freeWatingTime" config >>= fromJSONHelper
      waitingChargeInfo = WaitingChargeInfo <$> waitingCharge <*> freeWaitingTime
   in KP.foldr (\(k, v) acc -> KM.insert k v acc) config [("perExtraKmRateSections", toJSON perExtraKmRateSections), ("waitingChargeInfo", DA.toJSON waitingChargeInfo)]

jsonToFPProgressiveDetails :: String -> String -> Maybe FPProgressiveDetails
jsonToFPProgressiveDetails config key' =
  let res' = (config ^@.. _Value . _Object . reindexed (dropPrefixFromConfig "farePolicyProgressiveDetails:") (itraversed . indices (Text.isPrefixOf "farePolicyProgressiveDetails:" . DAK.toText)))
      res'' = parsingMiddleware (KM.fromList res') config key'
      res = DA.Object res'' ^? _JSON :: (Maybe FPProgressiveDetails)
   in res

makeFPProgressiveDetailsAPIEntity :: FPProgressiveDetails -> FPProgressiveDetailsAPIEntity
makeFPProgressiveDetailsAPIEntity FPProgressiveDetails {..} =
  FPProgressiveDetailsAPIEntity
    { perExtraKmRateSections = makeFPProgressiveDetailsPerExtraKmRateSectionAPIEntity <$> perExtraKmRateSections,
      ..
    }
