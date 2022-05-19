module Product.Confirm (confirm, onConfirm) where

import App.Types
import Beckn.External.Encryption (decrypt)
import Beckn.Product.Validation.Context (validateContext)
import qualified Beckn.Storage.Esqueleto as DB
import qualified Beckn.Storage.Queries.BecknRequest as QBR
import Beckn.Types.Common hiding (id)
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Core.Context as Context
import qualified Beckn.Types.Core.ReqTypes as Common
import qualified Beckn.Types.Core.Taxi.API.OnConfirm as OnConfirm
import qualified Beckn.Types.Core.Taxi.Confirm as Confirm
import Beckn.Types.Id
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import Data.Aeson (encode)
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Quote as SQuote
import qualified Domain.Types.RideBooking as SRB
import qualified Domain.Types.SearchRequest as SearchRequest
import EulerHS.Prelude hiding (id)
import qualified ExternalAPI.Flow as ExternalAPI
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.RideBooking as QRideB
import qualified Storage.Queries.SearchRequest as QSearchRequest
import qualified Types.API.Confirm as API
import Types.Error
import Utils.Common

confirm :: Id Person.Person -> Id SearchRequest.SearchRequest -> Id SQuote.Quote -> FlowHandler API.ConfirmRes
confirm personId searchRequestId quoteId = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  lt <- getCurrentTime
  searchRequest <- QSearchRequest.findByPersonId personId searchRequestId >>= fromMaybeM (SearchRequestDoesNotExist personId.getId)
  when ((searchRequest.validTill) < lt) $
    throwError SearchRequestExpired
  quote <- QQuote.findById quoteId >>= fromMaybeM (QuoteDoesNotExist quoteId.getId)
  now <- getCurrentTime
  rideBooking <- buildRideBooking searchRequest quote now
  DB.runTransaction $
    QRideB.create rideBooking
  bapURIs <- asks (.bapSelfURIs)
  bapIDs <- asks (.bapSelfIds)
  msgId <- generateGUID
  txnId <- generateGUID
  context <- buildTaxiContext Context.CONFIRM msgId (Just txnId) bapIDs.cabs bapURIs.cabs (Just quote.providerId) (Just quote.providerUrl)
  person <- QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  customerMobileNumber <- mapM decrypt person.mobileNumber >>= fromMaybeM (PersonFieldNotPresent "mobileNumber")
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
                Confirm.Customer $
                  Confirm.Contact $
                    Confirm.Phone
                      { country_code = customerMobileCountryCode,
                        number = customerMobileNumber
                      }
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
            status = SRB.NEW,
            providerId = quote.providerId,
            providerUrl = quote.providerUrl,
            providerName = quote.providerName,
            providerMobileNumber = quote.providerMobileNumber,
            startTime = searchRequest.startTime,
            riderId = searchRequest.riderId,
            fromLocationId = undefined,
            toLocationId = undefined,
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
onConfirm (SignatureAuthResult signPayload _) req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    logTagInfo "on_confirm req" (show req)
    validateContext Context.ON_CONFIRM req.context
    case req.contents of
      Left err -> logTagError "on_confirm req" $ "on_confirm error: " <> show err
      Right msg -> do
        let bppRideBookingId = Id msg.order.id
        DB.runTransaction $ do
          QRideB.updateBPPBookingId undefined bppRideBookingId
          QRideB.updateStatus undefined SRB.CONFIRMED
          QBR.logBecknRequest (show $ encode req) (show $ signPayload.signature)
    return Ack
