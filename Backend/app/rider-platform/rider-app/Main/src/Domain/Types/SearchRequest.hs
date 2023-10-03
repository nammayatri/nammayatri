{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.SearchRequest
  ( module Domain.Types.SearchRequest,
    module Reexport,
  )
where

import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.SearchRequest.SearchReqLocation as DLoc
import Domain.Types.SearchRequest.SearchType as Reexport
import qualified Kernel.External.Maps as Maps
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMeters, Money, Seconds)
import Kernel.Types.Id
import Kernel.Types.Version

data SearchRequestStatus = NEW | INPROGRESS | CONFIRMED | COMPLETED | CLOSED
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data SearchRequest = SearchRequest
  { id :: Id SearchRequest,
    startTime :: UTCTime,
    validTill :: UTCTime,
    riderId :: Id DP.Person,
    fromLocation :: DLoc.SearchReqLocation,
    toLocation :: Maybe DLoc.SearchReqLocation,
    searchTypes :: [SearchType],
    distance :: Maybe HighPrecMeters,
    maxDistance :: Maybe HighPrecMeters,
    estimatedRideDuration :: Maybe Seconds,
    device :: Maybe Text,
    merchantId :: Id DMerchant.Merchant, -- remove when searchRequest will not be used in CustomerSupport
    bundleVersion :: Maybe Version,
    clientVersion :: Maybe Version,
    language :: Maybe Maps.Language,
    disabilityTag :: Maybe Text,
    customerExtraFee :: Maybe Money,
    autoAssignEnabled :: Maybe Bool,
    autoAssignEnabledV2 :: Maybe Bool,
    availablePaymentMethods :: [Id DMPM.MerchantPaymentMethod],
    selectedPaymentMethodId :: Maybe (Id DMPM.MerchantPaymentMethod),
    createdAt :: UTCTime
  }
  deriving (Generic, Show)
