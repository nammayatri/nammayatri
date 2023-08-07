{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.DriverPlan where

import Data.Aeson
import qualified Data.Bifunctor as BF
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import qualified Data.Text.Encoding as Dt
import qualified Domain.Types.Person as DP
import Domain.Types.PlanDetails (PaymentMode)
import qualified Domain.Types.PlanDetails as DPlan
import Kernel.Prelude
import Kernel.Types.Id
import Servant.API

data MandateStatus = CREATED | ACTIVE | PAUSED | REVOKED | FAILURE | EXPIRED deriving (Read, Show, Eq, Generic, FromJSON, ToJSON, ToSchema, ToParamSchema)

data PlanStatus = ACTIVE_PLAN | INACTIVE_PLAN | PENDING_PLAN deriving (Read, Show, Eq, Generic, FromJSON, ToJSON, ToSchema, ToParamSchema)

data DriverPlan = DriverPlan
  { driverId :: Id DP.Person,
    planId :: Id DPlan.PlanDetails,
    planType :: PaymentMode,
    mandateId :: Maybe Text,
    mandateStatus :: Maybe MandateStatus,
    planStatus :: PlanStatus,
    activatedAt :: Maybe UTCTime,
    endAt :: Maybe UTCTime,
    resumeDate :: Maybe UTCTime,
    maxAmount :: Int,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show)

instance FromHttpApiData MandateStatus where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = BF.first T.pack . eitherDecode . BSL.fromStrict

instance ToHttpApiData MandateStatus where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode

instance FromHttpApiData PlanStatus where
  parseUrlPiece = parseHeader . Dt.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = BF.first T.pack . eitherDecode . BSL.fromStrict

instance ToHttpApiData PlanStatus where
  toUrlPiece = Dt.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode
