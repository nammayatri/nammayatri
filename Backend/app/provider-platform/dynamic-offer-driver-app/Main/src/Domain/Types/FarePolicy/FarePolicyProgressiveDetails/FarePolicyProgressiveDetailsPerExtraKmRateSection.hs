{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.FarePolicy.FarePolicyProgressiveDetails.FarePolicyProgressiveDetailsPerExtraKmRateSection where

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

data FPProgressiveDetailsPerExtraKmRateSectionD (s :: UsageSafety) = FPProgressiveDetailsPerExtraKmRateSection
  { startDistance :: Meters,
    perExtraKmRate :: HighPrecMoney
  }
  deriving (Generic, Show, Eq, ToSchema)

type FPProgressiveDetailsPerExtraKmRateSection = FPProgressiveDetailsPerExtraKmRateSectionD 'Safe

instance FromJSON (FPProgressiveDetailsPerExtraKmRateSectionD 'Unsafe)

instance ToJSON (FPProgressiveDetailsPerExtraKmRateSectionD 'Unsafe)

-----------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------APIEntity--------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------

data FPProgressiveDetailsPerExtraKmRateSectionAPIEntity = FPProgressiveDetailsPerExtraKmRateSectionAPIEntity
  { startDistance :: Meters,
    perExtraKmRate :: HighPrecMoney
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

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
            Nothing -> error $ "Not able to parse value" <> show value
    _ -> error $ "Not able to parse value" <> show value

jsonToFPProgressiveDetailsPerExtraKmRateSection :: Object -> String -> Parser [FPProgressiveDetailsPerExtraKmRateSection]
jsonToFPProgressiveDetailsPerExtraKmRateSection k key = do
  val <- ((listToType <$> (k .: DAK.fromText (Text.pack ("farePolicyProgressiveDetailsPerExtraKmRateSection:" <> key)))) :: Parser [FPProgressiveDetailsPerExtraKmRateSectionAPIEntity])
  pure $ makeFPProgressiveDetailsPerExtraKmRateSection val

makeFPProgressiveDetailsPerExtraKmRateSection :: [FPProgressiveDetailsPerExtraKmRateSectionAPIEntity] -> [FPProgressiveDetailsPerExtraKmRateSection]
makeFPProgressiveDetailsPerExtraKmRateSection = fmap makeFPProgressiveDetailsPerExtraKmRateSectionEntity

makeFPProgressiveDetailsPerExtraKmRateSectionEntity :: FPProgressiveDetailsPerExtraKmRateSectionAPIEntity -> FPProgressiveDetailsPerExtraKmRateSection
makeFPProgressiveDetailsPerExtraKmRateSectionEntity FPProgressiveDetailsPerExtraKmRateSectionAPIEntity {..} =
  FPProgressiveDetailsPerExtraKmRateSection
    { ..
    }

makeFPProgressiveDetailsPerExtraKmRateSectionAPIEntity :: FPProgressiveDetailsPerExtraKmRateSection -> FPProgressiveDetailsPerExtraKmRateSectionAPIEntity
makeFPProgressiveDetailsPerExtraKmRateSectionAPIEntity FPProgressiveDetailsPerExtraKmRateSection {..} =
  FPProgressiveDetailsPerExtraKmRateSectionAPIEntity
    { ..
    }
