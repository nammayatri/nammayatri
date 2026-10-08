{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Dashboard.ProviderPlatform.Management.Ride
  ( module Dashboard.ProviderPlatform.Management.Ride,
    module Reexport,
  )
where

import API.Types.ProviderPlatform.Management.Endpoints.Ride as Reexport
import Dashboard.Common as Reexport
import Dashboard.Common.Booking as Reexport (CancellationReasonCode (..))
import Dashboard.Common.Ride as Reexport
import Data.Aeson
import qualified Data.Bifunctor as BF
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TEnc
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Predicate (UniqueField (UniqueField))
import Kernel.Utils.JSON (constructorsWithLowerCase)
import Kernel.Utils.TH (mkHttpInstancesForEnum)
import Kernel.Utils.Validation
import Servant (FromHttpApiData (..), ToHttpApiData (..))

---------------------------------------------------------
-- ride list --------------------------------------------

derivePersistField "BookingStatus"

$(mkHttpInstancesForEnum ''BookingStatus)

$(mkHttpInstancesForEnum ''RideStatus)

$(mkHttpInstancesForEnum ''PaymentMode)

$(mkHttpInstancesForEnum ''PaymentCollector)

$(mkHttpInstancesForEnum ''RideDetailGroup)

-- `QueryParam "detailGroups" [RideDetailGroup]` needs explicit list instances; the list is sent as JSON, e.g. ["SAFETY","TAX"].
instance FromHttpApiData [RideDetailGroup] where
  parseUrlPiece = parseHeader . TEnc.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader bs = BF.first T.pack . eitherDecode . LBS.fromStrict $ bs

instance ToHttpApiData [RideDetailGroup] where
  toUrlPiece = TEnc.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = LBS.toStrict . encode

---------------------------------------------------------
-- multiple ride end ------------------------------

validateMultipleRideEndReq :: Validate MultipleRideEndReq
validateMultipleRideEndReq MultipleRideEndReq {..} = do
  validateField "rides" rides $ UniqueField @"rideId"

---------------------------------------------------------
-- multiple ride cancel ---------------------------

validateMultipleRideCancelReq :: Validate MultipleRideCancelReq
validateMultipleRideCancelReq MultipleRideCancelReq {..} = do
  validateField "rides" rides $ UniqueField @"rideId"

-- ticket ride list --------------------------------------------

instance HideSecrets TicketRideListRes where
  hideSecrets = identity

deriving anyclass instance FromJSON TicketRideListRes

deriving anyclass instance ToJSON TicketRideListRes

instance FromJSON RideInfo where
  parseJSON = genericParseJSON constructorsWithLowerCase

instance ToJSON RideInfo where
  toJSON = genericToJSON constructorsWithLowerCase
