module Product.Confirm (confirm, onConfirm) where

import App.Types
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common hiding (id)
import qualified Beckn.Types.Core.API.Confirm as BecknAPI
import Beckn.Types.Core.Ack
import Beckn.Types.Id
import qualified Beckn.Types.Mobility.Order as BO
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import qualified Data.Text as T
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified ExternalAPI.Flow as ExternalAPI
import qualified Storage.Queries.Organization as OQ
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.SearchRequest as QSearchRequest
import qualified Test.RandomStrings as RS
import qualified Types.API.Confirm as API
import Types.Error
import qualified Types.ProductInfo as Products
import qualified Types.Storage.Organization as Organization
import qualified Types.Storage.Person as Person
import qualified Types.Storage.Quote as SQuote
import qualified Types.Storage.Ride as SRide
import qualified Types.Storage.SearchRequest as SearchRequest
import Utils.Common
import qualified Utils.Metrics as Metrics

confirm :: Id Person.Person -> Id SearchRequest.SearchRequest -> Id SQuote.Quote -> FlowHandler API.ConfirmRes
confirm personId searchRequestId rideBookingId = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  lt <- getCurrentTime
  searchRequest <- QSearchRequest.findByPersonId personId searchRequestId >>= fromMaybeM SearchRequestDoesNotExist
  when ((searchRequest.validTill) < lt) $
    throwError SearchRequestExpired
  quote <- QQuote.findById rideBookingId >>= fromMaybeM QuoteDoesNotExist
  organization <-
    OQ.findOrganizationById (quote.organizationId)
      >>= fromMaybeM OrgNotFound
  Metrics.incrementSearchRequestCount SearchRequest.INPROGRESS
  ride <- mkRide (searchRequest.id) quote
  DB.runSqlDBTransaction $ do
    QRide.create ride
  context <- buildContext "confirm" (getId searchRequestId) Nothing Nothing
  baseUrl <- organization.callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
  order <- mkOrder quote
  ExternalAPI.confirm baseUrl (BecknAPI.ConfirmReq context $ BecknAPI.ConfirmOrder order)
  return $ API.ConfirmRes ride.id
  where
    mkOrder quote = do
      now <- getCurrentTime
      return $
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
        Metrics.incrementSearchRequestCount SearchRequest.COMPLETED
        SQuote.validateStatusTransition quote.status SQuote.CONFIRMED & fromEitherM QuoteInvalidStatus
        DB.runSqlDBTransaction $ do
          QQuote.updateMultiple quoteId uQuote
      Left err -> logTagError "on_confirm req" $ "on_confirm error: " <> show err
    return Ack

mkRide :: MonadFlow m => Id SearchRequest.SearchRequest -> SQuote.Quote -> m SRide.Ride
mkRide searchRequestId quote@SQuote.Quote {..} = do
  now <- getCurrentTime
  quoteId <- generateGUID
  shortId' <- T.pack <$> L.runIO (RS.randomString (RS.onlyAlphaNum RS.randomASCII) 16)
  return
    SRide.Ride
      { id = Id quoteId,
        requestId = searchRequestId,
        entityType = SRide.VEHICLE,
        entityId = Nothing,
        shortId = ShortId shortId',
        quantity = 1,
        quoteId = quote.id,
        status = SRide.INSTOCK,
        createdAt = now,
        updatedAt = now,
        ..
      }
