module Product.Confirm (confirm) where

import App.Types
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common hiding (id)
import qualified Beckn.Types.Core.ReqTypes as Common
import qualified Beckn.Types.Core.Taxi.Confirm.Req as ReqConfirm
import Beckn.Types.Id
import EulerHS.Prelude hiding (id)
import qualified ExternalAPI.Flow as ExternalAPI
import qualified Storage.Queries.Organization as OQ
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.RideBooking as QRideB
import qualified Storage.Queries.SearchRequest as QSearchRequest
import qualified Types.API.Confirm as API
import Types.Error
import qualified Types.Storage.Person as Person
import qualified Types.Storage.Quote as SQuote
import qualified Types.Storage.RideBooking as SRB
import qualified Types.Storage.SearchRequest as SearchRequest
import Utils.Common

confirm :: Id Person.Person -> Id SearchRequest.SearchRequest -> Id SQuote.Quote -> FlowHandler API.ConfirmRes
confirm personId searchRequestId quoteId = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  lt <- getCurrentTime
  searchRequest <- QSearchRequest.findByPersonId personId searchRequestId >>= fromMaybeM SearchRequestDoesNotExist
  when ((searchRequest.validTill) < lt) $
    throwError SearchRequestExpired
  quote <- QQuote.findById quoteId >>= fromMaybeM QuoteDoesNotExist
  organization <-
    OQ.findOrganizationById (quote.providerId)
      >>= fromMaybeM OrgNotFound
  bapURIs <- asks (.bapSelfURIs)
  bppURI <- organization.callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
  context <- buildTaxiContext (getId searchRequestId) bapURIs.cabs (Just bppURI)
  let order =
        ReqConfirm.Order
          { items = [ReqConfirm.OrderItem {id = quote.bppQuoteId.getId}]
          }
  res <- ExternalAPI.confirm bppURI (Common.BecknReq context $ ReqConfirm.ConfirmReqMessage order)

  let bppRideBookingId = Id res.order.id
  now <- getCurrentTime
  rideBooking <- buildRideBooking searchRequest quote bppRideBookingId now
  DB.runSqlDBTransaction $
    QRideB.create rideBooking
  return $ API.ConfirmRes rideBooking.id
  where
    buildRideBooking searchRequest quote bppRideBookingId now = do
      id <- generateGUID
      return $
        SRB.RideBooking
          { id = Id id,
            bppBookingId = bppRideBookingId,
            requestId = searchRequest.id,
            quoteId = quote.id,
            status = SRB.CONFIRMED,
            providerId = quote.providerId,
            providerName = quote.providerName,
            providerMobileNumber = quote.providerMobileNumber,
            startTime = searchRequest.startTime,
            requestorId = searchRequest.requestorId,
            fromLocationId = searchRequest.fromLocationId,
            toLocationId = searchRequest.toLocationId,
            estimatedFare = quote.estimatedFare,
            discount = quote.discount,
            estimatedTotalFare = quote.estimatedTotalFare,
            distance = searchRequest.distance,
            vehicleVariant = quote.vehicleVariant,
            createdAt = now,
            updatedAt = now
          }
