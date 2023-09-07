{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}

module Domain.Types.Vehicle.Variant where

import Data.Aeson
import Data.Bifunctor
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Kernel.Prelude hiding (first)
import Kernel.Utils.GenericPretty
import Servant.API
import Tools.Beam.UtilsTH (mkBeamInstancesForEnum)

data Variant = SEDAN | SUV | HATCHBACK | AUTO_RICKSHAW | TAXI | TAXI_PLUS
  deriving
    ( Show,
      Eq,
      Ord,
      Read,
      Generic,
      ToJSON,
      FromJSON,
      ToSchema,
      ToParamSchema,
      Enum,
      Bounded
    )
  deriving (PrettyShow) via Showable Variant

$(mkBeamInstancesForEnum ''Variant)

instance FromHttpApiData Variant where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict
