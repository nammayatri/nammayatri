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

import "dashboard-helper-api" Dashboard.ProviderPlatform.Merchant
import Data.Aeson as DA
import Data.Aeson.Key as DAK
import Data.Aeson.Types
import Data.ByteString.Lazy as BL
import Data.Text as Text
import Data.Text.Encoding as DTE
import Domain.Types.Common
import Kernel.Prelude
import Kernel.Prelude as KP
import Kernel.Types.Common
import Tools.Beam.UtilsTH (mkBeamInstancesForJSON)

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

replaceSingleQuotes :: Text -> Text
replaceSingleQuotes = Text.replace "'" "\""

readWithInfo :: (Read a, Show a) => String -> Value -> a
readWithInfo mes s = case s of
  String str -> case KP.readMaybe (Text.unpack str) of
    Just val -> val
    Nothing -> error . Text.pack $ "Failed to parse: for key: mes " <> mes <> " and value: " ++ Text.unpack str
  Number scientific -> case KP.readMaybe (show scientific) of
    Just val -> val
    Nothing -> error . Text.pack $ "Failed to parse: for key: mes " <> mes <> " and value: " ++ show scientific
  _ -> error $ "Not able to parse value" <> show s

listToType :: FromJSON a => Value -> [a]
listToType value =
  case value of
    String str ->
      let val = replaceSingleQuotes $ str
       in case DA.decode (BL.fromStrict (DTE.encodeUtf8 val)) of
            Just a -> a
            Nothing -> error $ "Not able to parse value" <> show val
    _ -> error $ "Not able to parse value" <> show value

jsonToFPSlabsDetailsSlab :: Object -> String -> Parser [FPSlabsDetailsSlab]
jsonToFPSlabsDetailsSlab k key = do
  val <- ((listToType <$> (k .: DAK.fromText (Text.pack ("farePolicySlabsDetailsSlab:" <> key)))) :: Parser [FPSlabsDetailsSlabAPIEntity])
  pure $ makeFPSlabsDetailsSlabList val

makeFPSlabsDetailsSlabList :: [FPSlabsDetailsSlabAPIEntity] -> [FPSlabsDetailsSlab]
makeFPSlabsDetailsSlabList = fmap makeFPSlabsDetailsSlab

makeFPSlabsDetailsSlab :: FPSlabsDetailsSlabAPIEntity -> FPSlabsDetailsSlab
makeFPSlabsDetailsSlab FPSlabsDetailsSlabAPIEntity {..} =
  FPSlabsDetailsSlab
    { ..
    }

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
