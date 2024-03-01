{-
 Copyright 2022-23, Juspay India Pvt Ltd
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.FarePolicy.FarePolicyRentalDetails.FarePolicyRentalDetailsDistanceBuffer where

import Data.Aeson as DA
import Data.Aeson.Key as DAK
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as BL
import qualified Data.List.NonEmpty as NE
import Data.Ord (comparing)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as DTE
import Domain.Types.Common
import Kernel.Prelude
import Kernel.Utils.Common (Seconds)

data FPRentalDetailsDistanceBuffersD (s :: UsageSafety) = FPRentalDetailsDistanceBuffers
  { rideDuration :: Seconds,
    bufferKms :: Int
  }
  deriving (Generic, Show, Eq, ToSchema)

type FPRentalDetailsDistanceBuffers = FPRentalDetailsDistanceBuffersD 'Safe

instance FromJSON (FPRentalDetailsDistanceBuffersD 'Unsafe)

instance ToJSON (FPRentalDetailsDistanceBuffersD 'Unsafe)

instance FromJSON (FPRentalDetailsDistanceBuffersD 'Safe)

instance ToJSON (FPRentalDetailsDistanceBuffersD 'Safe)

findFPRentalDetailsByDuration :: Int -> NonEmpty (FPRentalDetailsDistanceBuffersD s) -> FPRentalDetailsDistanceBuffersD s
findFPRentalDetailsByDuration duration slabList = do
  case NE.filter (\slab -> slab.rideDuration.getSeconds <= duration) $ NE.sortBy (comparing (.rideDuration)) slabList of
    [] -> error $ "Slab for duration = " <> show duration <> " not found. Non-emptiness supposed to be guaranteed by app logic."
    a -> last a

-----------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------APIEntity--------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------

data FPRentalDetailsDistanceBuffersAPIEntity = FPRentalDetailsDistanceBuffersAPIEntity
  { rideDuration :: Seconds,
    bufferKms :: Int
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

replaceSingleQuotes :: Text -> Text
replaceSingleQuotes = Text.replace "'" "\""

listToType :: FromJSON a => Value -> [a]
listToType value =
  case value of
    String str ->
      let val = replaceSingleQuotes $ str
       in case DA.decode (BL.fromStrict (DTE.encodeUtf8 val)) of
            Just a -> a
            Nothing -> error $ "Not able to parse value" <> show val
    _ -> error $ "Not able to parse value" <> show value

jsonToFPRentalDetailsDistanceBuffers :: String -> Object -> Parser [FPRentalDetailsDistanceBuffers]
jsonToFPRentalDetailsDistanceBuffers id k = do
  apiData <- ((listToType <$> (k .: DAK.fromText (Text.pack ("farePolicyRentalDetailsDistanceBuffers:" <> id)))) :: Parser [FPRentalDetailsDistanceBuffersAPIEntity])
  pure $ makeFPRentalDetailsDistanceBuffersList apiData

makeFPRentalDetailsDistanceBuffersList :: [FPRentalDetailsDistanceBuffersAPIEntity] -> [FPRentalDetailsDistanceBuffers]
makeFPRentalDetailsDistanceBuffersList = fmap makeFPRentalDetailsDistanceBuffers

makeFPRentalDetailsDistanceBuffersAPIEntity :: FPRentalDetailsDistanceBuffers -> FPRentalDetailsDistanceBuffersAPIEntity
makeFPRentalDetailsDistanceBuffersAPIEntity FPRentalDetailsDistanceBuffers {..} =
  FPRentalDetailsDistanceBuffersAPIEntity
    { rideDuration = rideDuration,
      bufferKms = bufferKms
    }

makeFPRentalDetailsDistanceBuffers :: FPRentalDetailsDistanceBuffersAPIEntity -> FPRentalDetailsDistanceBuffers
makeFPRentalDetailsDistanceBuffers FPRentalDetailsDistanceBuffersAPIEntity {..} =
  FPRentalDetailsDistanceBuffers
    { rideDuration = rideDuration,
      bufferKms = bufferKms
    }
