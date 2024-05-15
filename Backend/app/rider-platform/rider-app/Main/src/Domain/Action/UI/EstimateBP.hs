{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.EstimateBP where

import API.Types.UI.EstimateBP as DTEst
import qualified API.Types.UI.EstimateBP
import qualified "dashboard-helper-api" Dashboard.RiderPlatform.Ride
import Data.OpenApi (ToSchema)
import Domain.Types.Estimate
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as Ride
import qualified Domain.Types.SearchRequest as DSearch
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions as B
import Kernel.Prelude (head)
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id as Id
import Kernel.Types.Price (PriceAPIEntity (..))
import Kernel.Utils.Common
import Servant
import qualified Storage.Clickhouse.EstimateBreakup as CH
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Estimate as QEst
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.SearchRequest as QSearch
import Tools.Auth

getRideEstimateBreakup :: ((Kernel.Prelude.Maybe (Id.Id Person.Person), Id.Id Merchant.Merchant) -> Id.Id Ride.Ride -> Environment.Flow API.Types.UI.EstimateBP.EstimateDetailsRes)
getRideEstimateBreakup (_, _) rideId_ = do
  ride <- B.runInReplica $ QRide.findById rideId_ >>= fromMaybeM (RideDoesNotExist rideId_.getId)
  booking <- B.runInReplica $ QRB.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  case booking.quoteId of
    Just quoteId -> do
      mbQuote <- B.runInReplica $ QQuote.findById quoteId
      case mbQuote of
        Just quote -> do
          mbSearchReq <- B.runInReplica $ QSearch.findById quote.requestId
          case mbSearchReq of
            Just searchreq -> do
              mbEstimate <- B.runInReplica $ QEst.findBySRIdAndStatus COMPLETED searchreq.id
              case mbEstimate of
                Just estimate -> do
                  breakup <- CH.findAllByEstimateIdT (Id.cast estimate.id)
                  return $ EstimateDetailsRes (transformEstimate <$> breakup)
                Nothing -> pure (EstimateDetailsRes [])
            Nothing -> pure (EstimateDetailsRes [])
        Nothing -> pure (EstimateDetailsRes [])
    Nothing -> pure (EstimateDetailsRes [])

transformEstimate :: CH.EstimateBreakup -> API.Types.UI.EstimateBP.EstimateBreakup
transformEstimate estimate =
  API.Types.UI.EstimateBP.EstimateBreakup
    { price =
        API.Types.UI.EstimateBP.EstimateBreakupPrice
          { value =
              Kernel.Types.Price.PriceAPIEntity
                { amount = CH.priceValue estimate,
                  currency = CH.priceCurrency estimate
                }
          },
      title = CH.title estimate
    }
