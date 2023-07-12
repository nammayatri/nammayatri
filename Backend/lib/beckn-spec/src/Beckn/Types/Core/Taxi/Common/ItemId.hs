{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.Common.ItemId
  ( module Beckn.Types.Core.Taxi.Common.ItemId,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.Vehicle as Reexport
import Data.Aeson
import Data.OpenApi
import qualified Data.Text as T
import GHC.Show (show)
import Kernel.Prelude hiding (show)

data ItemId = ItemId
  { providerName :: Text,
    vehicleVariant :: VehicleVariant
  }
  deriving (Generic, Eq)

instance Show ItemId where
  show (ItemId providerName vehVar) = T.unpack providerName <> " | " <> show vehVar

instance Read ItemId where
  readsPrec _ str =
    case T.splitOn " | " $ T.pack str of
      (providerNameStr : vehicleVariant : _) ->
        case reads $ T.unpack vehicleVariant of
          [(vehVar, "")] -> [(ItemId providerNameStr vehVar, "")]
          _ -> []
      _ -> []

instance ToJSON ItemId where
  toJSON = String . T.pack . show

instance FromJSON ItemId where
  parseJSON = withText "ItemId" $ \s -> do
    case readMaybe $ T.unpack s of
      Nothing -> fail "Unable to parse ItemId"
      Just ic -> return ic

instance ToSchema ItemId where
  declareNamedSchema _ = declareNamedSchema (Proxy @Text)
