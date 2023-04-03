{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}

module Domain.Types.SearchStep where

import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import qualified Domain.Types.Estimate as DEst
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.Vehicle.Variant as Variant
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.GenericPretty
import Servant hiding (throwError)

data SearchStep = SearchStep
  { id :: Id SearchStep,
    requestId :: Id DSR.SearchRequest,
    estimateId :: Id DEst.Estimate,
    messageId :: Text,
    startTime :: UTCTime,
    validTill :: UTCTime,
    vehicleVariant :: Variant.Variant,
    status :: SearchStepStatus,
    searchRepeatCounter :: Int,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, PrettyShow, Show)

data SearchStepStatus = ACTIVE | CANCELLED | REPEAT
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)
  deriving (PrettyShow) via Showable SearchStepStatus

instance FromHttpApiData SearchStepStatus where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = left T.pack . eitherDecode . BSL.fromStrict

instance ToHttpApiData SearchStepStatus where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode
