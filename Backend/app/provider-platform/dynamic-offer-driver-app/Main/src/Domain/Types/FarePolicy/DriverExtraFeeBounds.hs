{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.FarePolicy.DriverExtraFeeBounds where

import Data.Aeson as DA
-- import Data.Aeson.Key as DAK
-- import Data.Aeson.Types
-- import Data.ByteString.Lazy as BL hiding (last)
import qualified Data.List.NonEmpty as NE
import Data.Ord
-- import Data.Text as Text hiding (last)
-- import Data.Text.Encoding as DTE
import Kernel.Prelude
-- import Kernel.Prelude as KP hiding (last)
import Kernel.Types.Common

data DriverExtraFeeBounds = DriverExtraFeeBounds
  { startDistance :: Meters,
    minFee :: Money,
    maxFee :: Money
  }
  deriving (Generic, Eq, Show, ToJSON, FromJSON, ToSchema, Read)

-- replaceSingleQuotes :: Text -> Text
-- replaceSingleQuotes = Text.replace "'" "\""

-- readWithInfo :: (Read a, Show a) => String -> Value -> a
-- readWithInfo mes s = case s of
--   String str -> case KP.readMaybe (Text.unpack str) of
--     Just val -> val
--     Nothing -> error . Text.pack $ "Failed to parse: for key: " <> mes <> " and value: " ++ Text.unpack str
--   Number scientific -> case KP.readMaybe (show scientific) of
--     Just val -> val
--     Nothing -> error . Text.pack $ "Failed to parse: for key: " <> mes <> " and value: " ++ show scientific
--   _ -> error $ "Not able to parse value" <> show s

-- listToType :: FromJSON a => Maybe Value -> [a]
-- listToType val' =
--   case val' of
--     Nothing -> []
--     Just value ->
--       case value of
--         String str ->
--           let val = replaceSingleQuotes $ str
--            in case DA.decode (BL.fromStrict (DTE.encodeUtf8 val)) of
--                 Just a -> a
--                 Nothing -> error $ "Not able to parse value" <> show val
--         _ -> error $ "Not able to parse value" <> show value

-- jsonToDriverExtraFeeBounds :: Object -> String -> Parser ([DriverExtraFeeBounds])
-- jsonToDriverExtraFeeBounds k key = listToType <$> (k .:? DAK.fromText (Text.pack ("farePolicyDriverExtraFeeBounds:" <> key)))

findDriverExtraFeeBoundsByDistance :: Meters -> NonEmpty DriverExtraFeeBounds -> DriverExtraFeeBounds
findDriverExtraFeeBoundsByDistance dist driverExtraFeeBoundsList = do
  case NE.filter (\driverExtraFeeBounds -> driverExtraFeeBounds.startDistance <= dist) $ NE.sortBy (comparing (.startDistance)) driverExtraFeeBoundsList of
    [] -> DriverExtraFeeBounds 0 0 0 -- error $ "DriverExtraFeeBounds for dist = " <> show dist <> " not found. Non-emptiness supposed to be guaranteed by app logic."
    a -> last a
