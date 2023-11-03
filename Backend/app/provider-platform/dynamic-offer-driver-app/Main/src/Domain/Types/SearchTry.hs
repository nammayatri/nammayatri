{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}

module Domain.Types.SearchTry where

import Data.Aeson
import qualified Domain.Types.Estimate as DEst
import Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.Vehicle.Variant as Variant
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.GenericPretty
import Kernel.Utils.TH (mkHttpInstancesForEnum)
import Tools.Beam.UtilsTH (mkBeamInstancesForEnum)

data SearchTry = SearchTry
  { id :: Id SearchTry,
    requestId :: Id DSR.SearchRequest,
    tag :: DSR.SearchRequestTag,
    estimateId :: Maybe (Id DEst.Estimate), -- not required for RENTAL
    merchantId :: Maybe (Id DM.Merchant),
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity,
    messageId :: Text,
    startTime :: UTCTime,
    validTill :: UTCTime,
    vehicleVariant :: Variant.Variant,
    baseFare :: Money,
    customerExtraFee :: Maybe Money,
    status :: SearchTryStatus,
    searchRepeatCounter :: Int,
    searchRepeatType :: SearchRepeatType,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, PrettyShow, Show)

data SearchTryStatus = ACTIVE | CANCELLED | COMPLETED
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)
  deriving (PrettyShow) via Showable SearchTryStatus

data SearchRepeatType = INITIAL | RETRIED | REALLOCATION | CANCELLED_AND_RETRIED
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)
  deriving (PrettyShow) via Showable SearchRepeatType

$(mkBeamInstancesForEnum ''SearchTryStatus)

$(mkBeamInstancesForEnum ''SearchRepeatType)

$(mkHttpInstancesForEnum ''SearchTryStatus)
