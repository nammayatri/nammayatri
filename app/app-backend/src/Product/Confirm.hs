module Product.Confirm (confirm, onConfirm) where

import App.Types
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common hiding (id)
import qualified Beckn.Types.Core.API.Confirm as BecknAPI
import Beckn.Types.Core.Ack
import Beckn.Types.Id
import qualified Beckn.Types.Mobility.Order as BO
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import EulerHS.Prelude hiding (id)
import qualified ExternalAPI.Flow as ExternalAPI
import qualified Storage.Queries.Organization as OQ
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.RideBooking as QRideB
import qualified Storage.Queries.SearchRequest as QSearchRequest
import qualified Types.API.Confirm as API
import Types.Error
import qualified Types.Storage.Organization as Organization
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
  context <- buildContext "confirm" (getId searchRequestId) Nothing Nothing
  baseUrl <- organization.callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
  let order = mkOrder quote now
  ExternalAPI.confirm baseUrl (BecknAPI.ConfirmReq context $ BecknAPI.ConfirmOrder order)
  return $ API.ConfirmRes rideBooking.id
  where
    mkOrder quote now = do
      BO.Order
        { id = getId $ quote.id,
          state = Nothing,
          created_at = now,
          updated_at = now,
          items = [],
          billing = Nothing,
          payment = Nothing,
          trip = Nothing,
          cancellation_reason_id = Nothing,
          cancellation_reasons = [],
          cancellation_policy = Nothing
        }
    buildRideBooking searchRequest quote now = do
      id <- generateGUID
      return $
        SRB.RideBooking
          { id = Id id,
            requestId = searchRequest.id,
            quoteId = quote.id,
            status = SRB.CONFIRMED,
            providerId = quote.providerId,
            providerMobileNumber = quote.providerMobileNumber,
            startTime = searchRequest.startTime,
            requestorId = searchRequest.requestorId,
            fromLocationId = searchRequest.fromLocationId,
            toLocationId = searchRequest.toLocationId,
            price = quote.price,
            discount = quote.discount,
            estimatedTotalFare = quote.estimatedTotalFare,
            distance = searchRequest.distance,
            vehicleVariant = quote.vehicleVariant,
            createdAt = now,
            updatedAt = now
          }

onConfirm ::
  SignatureAuthResult Organization.Organization ->
  BecknAPI.OnConfirmReq ->
  FlowHandler AckResponse
onConfirm _org req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    -- TODO: Verify api key here
    logTagInfo "on_confirm req" (show req)
    validateContext "on_confirm" $ req.context
    case req.contents of
      Right _ -> do
        return ()
      Left err -> logTagError "on_confirm req" $ "on_confirm error: " <> show err
    return Ack
