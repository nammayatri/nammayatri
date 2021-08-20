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
import qualified Types.ProductInfo as Products
import qualified Types.Storage.Organization as Organization
import qualified Types.Storage.Person as Person
import qualified Types.Storage.Quote as SQuote
import qualified Types.Storage.RideBooking as SRB
import qualified Types.Storage.SearchRequest as SearchRequest
import Utils.Common

confirm :: Id Person.Person -> Id SearchRequest.SearchRequest -> Id SQuote.Quote -> FlowHandler API.ConfirmRes
confirm personId searchRequestId rideBookingId = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  lt <- getCurrentTime
  searchRequest <- QSearchRequest.findByPersonId personId searchRequestId >>= fromMaybeM SearchRequestDoesNotExist
  when ((searchRequest.validTill) < lt) $
    throwError SearchRequestExpired
  quote <- QQuote.findById rideBookingId >>= fromMaybeM QuoteDoesNotExist
  organization <-
    OQ.findOrganizationById (quote.providerId)
      >>= fromMaybeM OrgNotFound
  now <- getCurrentTime
  rideBooking <- buildRideBooking searchRequest quote organization now
  DB.runSqlDBTransaction $
    QRideB.create rideBooking
  context <- buildContext "confirm" (getId searchRequestId) Nothing Nothing
  baseUrl <- organization.callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
  let order = mkOrder rideBooking now
  ExternalAPI.confirm baseUrl (BecknAPI.ConfirmReq context $ BecknAPI.ConfirmOrder order)
  return $ API.ConfirmRes rideBooking.id
  where
    mkOrder rideBooking now = do
      BO.Order
        { id = getId $ rideBooking.id,
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
    buildRideBooking searchRequest quote provider now = do
      id <- generateGUID
      return $
        SRB.RideBooking
          { id = Id id,
            requestId = searchRequest.id,
            quoteId = quote.id,
            status = SRB.CONFIRMED,
            providerId = provider.id,
            providerMobileNumber = fromMaybe "" provider.mobileNumber,
            startTime = searchRequest.startTime,
            requestorId = searchRequest.requestorId,
            fromLocationId = searchRequest.fromLocationId,
            toLocationId = searchRequest.toLocationId,
            price = quote.price,
            distance = searchRequest.distance,
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
      Right msg -> do
        let trip = fromBeckn <$> msg.order.trip
            quoteId = Id $ msg.order.id
            tracker = flip Products.Tracker Nothing <$> trip
        quote <- QQuote.findById quoteId >>= fromMaybeM QuoteDoesNotExist
        -- TODO: update tracking prodInfo in.info
        let mprdInfo = decodeFromText =<< (quote.info)
        let uInfo = (\info -> info {Products.tracker = tracker}) <$> mprdInfo
        let uQuote =
              quote{info = encodeToText <$> uInfo,
                    udf4 = (.id) <$> trip,
                    status = SQuote.CONFIRMED
                   }
        SQuote.validateStatusTransition quote.status SQuote.CONFIRMED & fromEitherM QuoteInvalidStatus
        DB.runSqlDBTransaction $ do
          QQuote.updateMultiple quoteId uQuote
      Left err -> logTagError "on_confirm req" $ "on_confirm error: " <> show err
    return Ack
