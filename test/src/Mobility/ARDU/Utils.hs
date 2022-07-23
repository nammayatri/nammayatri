module Mobility.ARDU.Utils (module Mobility.ARDU.Utils, module Reexport) where

import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.APISuccess (APISuccess)
import Beckn.Types.Id
import Beckn.Types.MapSearch
import Common
import "driver-offer-bpp" Domain.Types.Person as TPerson
import qualified "app-backend" Domain.Types.Quote as AppQuote
import qualified "driver-offer-bpp" Domain.Types.Ride as TRide
import qualified "app-backend" Domain.Types.RideBooking as AppRB
import qualified "driver-offer-bpp" Domain.Types.RideBooking as TRB
import qualified "app-backend" Domain.Types.SearchRequest as AppSearchReq
import qualified "driver-offer-bpp" Domain.Types.SearchRequest as ArduSReq
import "driver-offer-bpp" Domain.Types.SearchRequestForDriver
import qualified "app-backend" Domain.Types.SelectedQuote as AppSelQuote
import EulerHS.Prelude
import HSpec
import qualified Mobility.ARDU.APICalls as API
import Mobility.ARDU.Fixtures
import Mobility.AppBackend.APICalls
import Mobility.AppBackend.Fixtures
import Mobility.Utils as Reexport
import Servant.Client
import qualified "driver-offer-bpp" Storage.Queries.DriverInformation as DriverInfo
import "driver-offer-bpp" Storage.Queries.DriverLocation
import qualified "driver-offer-bpp" Storage.Queries.Ride as TQRide
import qualified "app-backend" Storage.Queries.RideBooking as BQRB
import qualified "driver-offer-bpp" Storage.Queries.RideBooking as TQRB
import qualified "driver-offer-bpp" Types.API.Driver as TDriver
import "app-backend" Types.API.Quote (OfferRes (OnDemandCab))
import qualified "app-backend" Types.API.Search as AppSearch
import Utils

setupDriver :: DriverTestData -> LatLong -> ClientsM ()
setupDriver driver initialPoint = do
  void . callBPP $ API.setDriverOnline driver.token True
  -- Moves driver to the pickup point
  preUpdate <- liftIO $ API.buildUpdateLocationRequest $ initialPoint :| []
  void . callBPP $
    API.updateLocation driver.token preUpdate

resetDriver :: DriverTestData -> IO ()
resetDriver driver = runARDUFlow "" $
  Esq.runTransaction $ do
    DriverInfo.updateActivity (cast driver.driverId) False
    DriverInfo.updateOnRide (cast driver.driverId) False

--
getBAPRideBooking ::
  Id AppRB.RideBooking ->
  ClientsM AppRB.RideBooking
getBAPRideBooking bapRBId = do
  mbBRB <- liftIO $ runAppFlow "" $ BQRB.findById bapRBId
  mbBRB `shouldSatisfy` isJust
  let Just bRB = mbBRB
  return bRB

search :: Text -> AppSearch.SearchReq -> ClientsM (Id AppSearchReq.SearchRequest)
search token searchReq_ = callBAP $ searchServices token searchReq_ <&> (.searchId)

getOnSearchTaxiQuotesByTransporterName ::
  Text ->
  Id AppSearchReq.SearchRequest ->
  Text ->
  ClientsM (NonEmpty AppQuote.QuoteAPIEntity)
getOnSearchTaxiQuotesByTransporterName appToken searchId transporterName =
  pollFilteredList
    "get on_search quotes"
    (\p -> p.agencyName == transporterName)
    $ callBAP (getQuotes searchId appToken)
      <&> (.quotes)
      <&> mapMaybe \case
        OnDemandCab p -> Just p
        _ -> Nothing

select :: Text -> Id AppQuote.Quote -> ClientsM ()
select bapToken quoteId = void $ callBAP $ selectQuote bapToken quoteId

getNearbySearchRequestForDriver :: DriverTestData -> Id AppQuote.Quote -> ClientsM (NonEmpty SearchRequestForDriverAPIEntity)
getNearbySearchRequestForDriver driver quoteId =
  pollFilteredList
    "get at least one nearby search request for driver"
    (\p -> p.messageId == quoteId.getId)
    ((.searchRequests) <$> callBPP (API.getNearbySearchRequests driver.token))

offerQuote_ :: DriverTestData -> Double -> Id ArduSReq.SearchRequest -> ClientsM ()
offerQuote_ driver fare bppSearchRequestId =
  void $ callBPP $ API.offerQuote driver.token $ TDriver.DriverOfferReq (Just fare) bppSearchRequestId

offerQuoteEither :: DriverTestData -> Double -> Id ArduSReq.SearchRequest -> ClientsM (Either ClientError APISuccess)
offerQuoteEither driver fare bppSearchRequestId =
  callBppEither $ API.offerQuote driver.token $ TDriver.DriverOfferReq (Just fare) bppSearchRequestId

getSelectedQuotesByQuoteId :: Text -> Id AppQuote.Quote -> ClientsM (NonEmpty AppSelQuote.SelectedQuoteAPIEntity)
getSelectedQuotesByQuoteId appToken quoteId =
  pollFilteredList
    "get at least one on_select quote"
    (\p -> p.providerName == bapTransporterName)
    ((.selectedQuotes) <$> callBAP (selectList appToken quoteId))

initWithCheck :: Text -> Id AppSelQuote.SelectedQuote -> ClientsM (Id AppRB.RideBooking)
initWithCheck appToken selectedQuoteId = do
  bRideBookingId <- fmap (.bookingId) $ callBAP $ appInitRide appToken $ mkAppInitReqSelected selectedQuoteId
  void . pollDesc "init result" $ do
    initRB <- getBAPRideBooking bRideBookingId
    initRB.bppBookingId `shouldSatisfy` isJust
    return $ Just ()
  pure bRideBookingId

getBPPRideBooking ::
  Id AppRB.RideBooking ->
  ClientsM TRB.RideBooking
getBPPRideBooking bapRBId = do
  bRB <- getBAPRideBooking bapRBId
  bRB.bppBookingId `shouldSatisfy` isJust
  let Just bppBookingId = bRB.bppBookingId
  mbTRB <- liftIO $ runARDUFlow "" $ TQRB.findById $ cast bppBookingId
  mbTRB $> () `shouldSatisfy` isJust
  let Just tRB = mbTRB
  return tRB

getBPPRide ::
  Id TRB.RideBooking ->
  ClientsM TRide.Ride
getBPPRide rideBookingId = do
  mbRide <- liftIO $ runARDUFlow "" $ TQRide.findActiveByRBId rideBookingId
  mbRide `shouldSatisfy` isJust
  return $ fromJust mbRide

getBPPRideById ::
  Id TRide.Ride ->
  ClientsM TRide.Ride
getBPPRideById rideId = do
  mbRide <- liftIO $ runARDUFlow "" $ TQRide.findById rideId
  mbRide `shouldSatisfy` isJust
  return $ fromJust mbRide

getBPPDriverLocation ::
  Id TPerson.Person ->
  ClientsM LatLong
getBPPDriverLocation driverId = do
  mbRes <- liftIO $ runARDUFlow "" $ findById driverId
  mbRes `shouldSatisfy` isJust
  let res = fromJust mbRes
  pure $
    LatLong
      { lat = res.lat,
        lon = res.lon
      }

confirmWithCheck :: Text -> Id AppRB.RideBooking -> ClientsM (TRB.RideBooking, TRide.Ride)
confirmWithCheck appToken bRideBookingId = do
  void . callBAP $
    appConfirmRide appToken $ mkAppConfirmReq bRideBookingId

  void . pollDesc "ride confirmed and assigned" $
    callBAP (appRideBookingStatus bRideBookingId appRegistrationToken)
      <&> (.status)
      >>= (`shouldBe` AppRB.TRIP_ASSIGNED)
      <&> Just

  tRideBooking <- pollDesc "trip assigned" $ do
    trb <- getBPPRideBooking bRideBookingId
    trb.status `shouldBe` TRB.TRIP_ASSIGNED
    return $ Just trb

  tRide <- pollDesc "new ride" $ do
    tRide <- getBPPRide tRideBooking.id
    tRide.status `shouldBe` TRide.NEW
    return $ Just tRide

  pure (tRideBooking, tRide)
