{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Beckn.ACL.OnCancel where

import qualified Beckn.Types.Core.Taxi.Common.Tags as Tags
import qualified Beckn.Types.Core.Taxi.OnCancel as OnCancel
import Data.Text
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.Ride as R
import qualified Domain.Types.SearchRequest as DSR
import EulerHS.Prelude hiding (pack)
import Kernel.External.Encryption
import Kernel.Storage.Esqueleto
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Error
import Storage.Queries.Estimate as QE
import Storage.Queries.SearchRequest as QS
import Tools.Error

data OnCancelBuildReq = OnCancelBuildReq
  { booking :: DRB.Booking,
    mbRideId :: Maybe (Id R.Ride),
    cancellationSource :: SBCR.CancellationSource
  }

newtype OnSearchCancelBuildReq = OnSearchCancelBuildReq {searchReqId :: Id DSR.SearchRequest}

buildOnCancelMessage :: (EsqDBFlow m r, EncFlow m r) => OnCancelBuildReq -> m OnCancel.OnCancelMessage
buildOnCancelMessage OnCancelBuildReq {..} = do
  fId <- getFulFillmentId booking mbRideId
  let tagGroups =
        Tags.TG
          [ Tags.TagGroup
              { display = True,
                code = "booking_cancelled_info",
                name = "Booking Cancelled Info",
                list = [Tags.Tag (Just True) (Just "booking_cancelled") (Just "Booking Cancelled") (Just $ show True)]
              },
            Tags.TagGroup
              { display = True,
                code = "cancellation_source_info",
                name = "Cancellation Source Info",
                list = [Tags.Tag (Just True) (Just "cancellation_source") (Just $ show cancellationSource) (Just $ show cancellationSource)]
              }
          ]
  return $
    OnCancel.OnCancelMessage $
      OnCancel.Order
        { id = Just booking.id.getId,
          state = "CANCELLED",
          fulfillment =
            OnCancel.FulfillmentInfo
              { id = fId,
                state =
                  Just
                    OnCancel.FulfillmentState
                      { descriptor =
                          OnCancel.Descriptor
                            { short_desc = Nothing,
                              code = Just "CANCELLED"
                            }
                      },
                tags = Just tagGroups
              }
        }

getFulFillmentId :: (EsqDBFlow m r, EncFlow m r) => DRB.Booking -> Maybe (Id R.Ride) -> m Text
getFulFillmentId booking mbRideId = do
  case booking.specialLocationTag of
    Just _ -> pure booking.quoteId
    Nothing -> getFId booking mbRideId
  where
    getFId :: (EsqDBFlow m r, EncFlow m r) => DRB.Booking -> Maybe (Id R.Ride) -> m Text
    getFId booking_ mbRideId_ =
      case mbRideId_ of
        Just rideId -> pure rideId.getId
        Nothing -> getEstimateId booking_

getEstimateId :: (EsqDBFlow m r, EncFlow m r) => DRB.Booking -> m Text
getEstimateId booking = do
  searchReqId <- QS.findByTransactionId booking.transactionId >>= fromMaybeM (SearchRequestDoesNotExist booking.transactionId)
  estimate <- QE.findBySearchReqId searchReqId >>= fromMaybeM (EstimateDoesNotExist searchReqId.getId)
  pure estimate.id.getId

buildOnSearchCancelMessage :: (EsqDBFlow m r, EncFlow m r) => OnSearchCancelBuildReq -> m OnCancel.OnCancelMessage
buildOnSearchCancelMessage OnSearchCancelBuildReq {..} = do
  fId <- getEstimateIdFromSearchReqId searchReqId
  let tagGroups =
        Tags.TG
          [ Tags.TagGroup
              { display = True,
                code = "search_cancelled_info",
                name = "Search Cancelled Info",
                list = [Tags.Tag (Just True) (Just "search_cancelled") (Just "Search Cancelled") (Just $ show True)]
              }
          ]
  return $
    OnCancel.OnCancelMessage $
      OnCancel.Order
        { id = Nothing,
          state = "CANCELLED",
          fulfillment =
            OnCancel.FulfillmentInfo
              { id = fId,
                state = Nothing,
                tags = Just tagGroups
              }
        }

getEstimateIdFromSearchReqId :: (EsqDBFlow m r, EncFlow m r) => Id DSR.SearchRequest -> m Text
getEstimateIdFromSearchReqId searchReqId = do
  estimate <- QE.findBySearchReqId searchReqId >>= fromMaybeM (EstimateDoesNotExist searchReqId.getId)
  pure estimate.id.getId
