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

module Domain.Types.FarePolicy.FarePolicySlabsDetails.FarePolicySlabsDetailsSlab where

import Control.Lens.Fold
import "dashboard-helper-api" Dashboard.ProviderPlatform.Merchant
import Data.Aeson as DA
import Data.Aeson.Key as DAK
import qualified Data.Aeson.KeyMap as DAKM
import Data.Aeson.Lens
import Data.Text as Text
import qualified Data.Vector as DV
import Domain.Types.Common
import Kernel.Prelude as KP
import Kernel.Types.Cac
import Kernel.Types.Common
import Tools.Beam.UtilsTH (mkBeamInstancesForJSON)

-- import Data.Maybe

data FPSlabsDetailsSlabD (s :: UsageSafety) = FPSlabsDetailsSlab
  { startDistance :: Meters,
    baseFare :: Money,
    waitingChargeInfo :: Maybe WaitingChargeInfo,
    platformFeeInfo :: Maybe PlatformFeeInfo,
    nightShiftCharge :: Maybe NightShiftCharge
  }
  deriving (Generic, Show, Eq, ToSchema)

type FPSlabsDetailsSlab = FPSlabsDetailsSlabD 'Safe

instance FromJSON (FPSlabsDetailsSlabD 'Unsafe)

instance ToJSON (FPSlabsDetailsSlabD 'Unsafe)

instance FromJSON (FPSlabsDetailsSlabD 'Safe)

instance ToJSON (FPSlabsDetailsSlabD 'Safe)

data PlatformFeeCharge = ProgressivePlatformFee HighPrecMoney | ConstantPlatformFee HighPrecMoney
  deriving stock (Show, Eq, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data PlatformFeeInfo = PlatformFeeInfo
  { platformFeeCharge :: PlatformFeeCharge,
    cgst :: Double,
    sgst :: Double
  }
  deriving (Generic, Eq, Show, ToJSON, FromJSON, ToSchema)

-----------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------APIEntity--------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------

parseFromCACMiddleware :: Value -> Maybe FPSlabsDetailsSlab
parseFromCACMiddleware k1 = do
  case k1 of
    Object config -> do
      let waitingCharge = DAKM.lookup "waitingCharge" config >>= fromJSONHelper
          freeWaitingTime = DAKM.lookup "freeWatingTime" config >>= fromJSONHelper
          waitingChargeInfo = WaitingChargeInfo <$> waitingCharge <*> freeWaitingTime
          platformFeeCharge = DAKM.lookup "platformFeeCharge" config >>= fromJSONHelper
          platformFeeCgst = DAKM.lookup "platformFeeCgst" config >>= fromJSONHelper
          platformFeeSgst = DAKM.lookup "platformFeeSgst" config >>= fromJSONHelper
          platformFeeInfo = PlatformFeeInfo <$> platformFeeCharge <*> platformFeeCgst <*> platformFeeSgst
          newKeyMap = KP.foldr (\(k, v) acc -> DAKM.insert k v acc) config [("waitingChargeInfo", DA.toJSON waitingChargeInfo), ("platformFeeInfo", DA.toJSON platformFeeInfo)]
      Object newKeyMap ^? _JSON :: Maybe FPSlabsDetailsSlab
    _ -> Nothing

jsonToFPSlabsDetailsSlab :: DAKM.KeyMap Value -> String -> [FPSlabsDetailsSlab]
jsonToFPSlabsDetailsSlab config key' = do
  let res' = fromMaybe (DA.Array (DV.fromList [])) (DAKM.lookup (DAK.fromText (Text.pack key')) config)
      res = case res' of
        DA.Array k -> KP.map parseFromCACMiddleware (DV.toList k)
        _ -> []

  catMaybes res

data FPSlabsDetailsSlabAPIEntity = FPSlabsDetailsSlabAPIEntity
  { startDistance :: Meters,
    baseFare :: Money,
    waitingChargeInfo :: Maybe WaitingChargeInfo,
    platformFeeInfo :: Maybe PlatformFeeInfo,
    nightShiftCharge :: Maybe NightShiftCharge
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

makeFPSlabsDetailsSlabAPIEntity :: FPSlabsDetailsSlab -> FPSlabsDetailsSlabAPIEntity
makeFPSlabsDetailsSlabAPIEntity FPSlabsDetailsSlab {..} =
  FPSlabsDetailsSlabAPIEntity
    { ..
    }

$(mkBeamInstancesForJSON ''PlatformFeeCharge)
