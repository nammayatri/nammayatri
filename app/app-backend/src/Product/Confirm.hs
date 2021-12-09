module Product.Confirm (confirm, onConfirm) where

import App.Types
import Beckn.Product.Validation.Context (validateContext)
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common hiding (id)
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Core.Cabs.API.OnConfirm as OnConfirm
import qualified Beckn.Types.Core.Cabs.Confirm as Confirm
import qualified Beckn.Types.Core.ReqTypes as Common
import Beckn.Types.Id
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
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
  now <- getCurrentTime
  rideBooking <- buildRideBooking searchRequest quote now
  DB.runSqlDBTransaction $
    QRideB.create rideBooking
  bapURIs <- asks (.bapSelfURIs)
  bppURI <- organization.callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
  context <- buildCabsContext (getId searchRequestId) bapURIs.cabs (Just bppURI)
  let order =
        Confirm.Order
          { items = [Confirm.OrderItem {id = quote.bppQuoteId.getId}]
          }
  ExternalAPI.confirm bppURI (Common.BecknReq context $ Confirm.ConfirmMessage order)
  return $ API.ConfirmRes rideBooking.id
  where
    buildRideBooking searchRequest quote now = do
      id <- generateGUID
      return $
        SRB.RideBooking
          { id = Id id,
            bppBookingId = Nothing,
            requestId = searchRequest.id,
            quoteId = quote.id,
            status = SRB.CONFIRMED,
            providerId = quote.providerId,
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

onConfirm ::
  SignatureAuthResult ->
  OnConfirm.OnConfirmReq ->
  FlowHandler AckResponse
onConfirm _org req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    -- TODO: Verify api key here
    logTagInfo "on_confirm req" (show req)
    validateContext req.context
    case req.contents of
      Left err -> logTagError "on_confirm req" $ "on_confirm error: " <> show err
      Right msg -> do
        bppQuoteId <- (Id . (.id) <$> listToMaybe msg.order.items) & fromMaybeM (InternalError "Empty items list.")
        let bppRideBookingId = Id msg.order.id
        quote <- QQuote.findByBPPQuoteId bppQuoteId >>= fromMaybeM QuoteDoesNotExist
        rb <- QRideB.findByQuoteId quote.id >>= fromMaybeM RideBookingNotFound
        DB.runSqlDBTransaction $
          QRideB.updateBPPBookingId rb.id bppRideBookingId
    return Ack
