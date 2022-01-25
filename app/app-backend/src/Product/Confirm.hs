module Product.Confirm (confirm, onConfirm) where

import App.Types
import Beckn.External.Encryption (decrypt)
import Beckn.Product.Validation.Context (validateContext)
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common hiding (id)
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Core.ReqTypes as Common
import qualified Beckn.Types.Core.Taxi.API.OnConfirm as OnConfirm
import qualified Beckn.Types.Core.Taxi.Common.Context as Context
import qualified Beckn.Types.Core.Taxi.Confirm as Confirm
import Beckn.Types.Id
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import EulerHS.Prelude hiding (id)
import qualified ExternalAPI.Flow as ExternalAPI
import qualified Storage.Queries.Person as QPerson
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
  now <- getCurrentTime
  rideBooking <- buildRideBooking searchRequest quote now
  DB.runSqlDBTransaction $
    QRideB.create rideBooking
  bapURIs <- asks (.bapSelfURIs)
  bapIDs <- asks (.bapSelfIds)
  context <- buildTaxiContext Context.CONFIRM (getId searchRequestId) bapIDs.cabs bapURIs.cabs Nothing Nothing
  person <- QPerson.findById personId >>= fromMaybeM PersonDoesNotExist
  customerMobileNumber <- decrypt person.mobileNumber >>= fromMaybeM (PersonFieldNotPresent "mobileNumber")
  customerMobileCountryCode <- person.mobileCountryCode & fromMaybeM (PersonFieldNotPresent "mobileCountryCode")
  let order =
        Confirm.Order
          { items =
              [ Confirm.OrderItem
                  { id = quote.bppQuoteId.getId
                  }
              ],
            fulfillment =
              Confirm.Fulfillment $
                Confirm.Customer {mobile_number = customerMobileCountryCode <> customerMobileNumber}
          }
  void $ ExternalAPI.confirm quote.providerUrl (Common.BecknReq context $ Confirm.ConfirmMessage order)
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
            status = SRB.NEW,
            providerUrl = quote.providerUrl,
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

onConfirm ::
  SignatureAuthResult ->
  OnConfirm.OnConfirmReq ->
  FlowHandler AckResponse
onConfirm _org req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    logTagInfo "on_confirm req" (show req)
    validateContext req.context
    case req.contents of
      Left err -> logTagError "on_confirm req" $ "on_confirm error: " <> show err
      Right msg -> do
        bppQuoteId <- (Id . (.id) <$> listToMaybe msg.order.items) & fromMaybeM (InternalError "Empty items list.")
        let bppRideBookingId = Id msg.order.id
        quote <- QQuote.findByBPPQuoteId bppQuoteId >>= fromMaybeM QuoteDoesNotExist
        rideBooking <- QRideB.findByQuoteId quote.id >>= fromMaybeM RideBookingNotFound
        DB.runSqlDBTransaction $ do
          QRideB.updateBPPBookingId rideBooking.id bppRideBookingId
          QRideB.updateStatus rideBooking.id SRB.CONFIRMED
    return Ack
