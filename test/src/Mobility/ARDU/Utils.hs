module Mobility.ARDU.Utils (module Mobility.ARDU.Utils) where

import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.APISuccess (APISuccess)
import Beckn.Types.Id
import Beckn.Types.MapSearch
import Common
import qualified "app-backend" Domain.Action.UI.Cancel as AppCancel
import qualified "app-backend" Domain.Types.CancellationReason as AppCR
import qualified "driver-offer-bpp" Domain.Types.CancellationReason as SCR
import qualified "driver-offer-bpp" Domain.Types.DriverInformation as TDrInfo
import "driver-offer-bpp" Domain.Types.Person as TPerson
import qualified "app-backend" Domain.Types.Quote as AppQuote
import qualified "app-backend" Domain.Types.Ride as BRide
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
import Mobility.AppBackend.APICalls as BapAPI
import Mobility.AppBackend.Fixtures
import Servant.Client
import qualified "driver-offer-bpp" Storage.Queries.DriverInformation as DriverInfo
import qualified "driver-offer-bpp" Storage.Queries.DriverInformation as QTDrInfo
import "driver-offer-bpp" Storage.Queries.DriverLocation
import qualified "driver-offer-bpp" Storage.Queries.Ride as TQRide
import qualified "app-backend" Storage.Queries.RideBooking as BQRB
import qualified "driver-offer-bpp" Storage.Queries.RideBooking as TQRB
import qualified "driver-offer-bpp" Types.API.Driver as TDriver
import "app-backend" Types.API.Quote (OfferRes (OnDemandCab))
import qualified "driver-offer-bpp" Types.API.Ride as RideAPI
import qualified "app-backend" Types.API.Search as AppSearch
import Utils

-- database calls
getBAPRideBooking ::
  Id AppRB.RideBooking ->
  ClientsM AppRB.RideBooking
getBAPRideBooking bapRBId = do
  mbBRB <- liftIO $ runAppFlow "" $ BQRB.findById bapRBId
  mbBRB `shouldSatisfy` isJust
  let Just bRB = mbBRB
  return bRB

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

getBPPDriverInformation ::
  Id TPerson.Person ->
  ClientsM TDrInfo.DriverInformation
getBPPDriverInformation driverId =
  liftIO $ poll $ runARDUFlow "" $ QTDrInfo.findById $ cast driverId

-- driver setup/reset
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

-- flow primitives
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

offerQuote :: DriverTestData -> Double -> Id ArduSReq.SearchRequest -> ClientsM ()
offerQuote driver fare bppSearchRequestId =
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

startRide :: DriverTestData -> LatLong -> TRide.Ride -> Id AppRB.RideBooking -> ClientsM ()
startRide driver origin tRide bRideBookingId = do
  void . callBPP $
    API.rideStart driver.token tRide.id $
      API.buildStartRideReq tRide.otp origin

  void . pollDesc "trip started" $ do
    inprogressRBStatusResult <- callBAP (appRideBookingStatus bRideBookingId appRegistrationToken)
    inprogressRBStatusResult.rideList `shouldSatisfy` not . null
    inprogressRBStatusResult.status `shouldBe` AppRB.TRIP_ASSIGNED
    let [inprogressRide] = inprogressRBStatusResult.rideList
    inprogressRide.status `shouldBe` BRide.INPROGRESS
    return $ Just ()

endRide ::
  DriverTestData ->
  LatLong ->
  TRide.Ride ->
  Id AppRB.RideBooking ->
  ClientsM ()
endRide driver destination tRide bRideBookingId = do
  void . callBPP $ API.rideEnd driver.token tRide.id $ RideAPI.EndRideReq destination
  void $
    pollDesc "ride completed" $ do
      completedRBStatusResult <- callBAP (appRideBookingStatus bRideBookingId appRegistrationToken)
      completedRBStatusResult.rideList `shouldSatisfy` not . null
      completedRBStatusResult.status `shouldBe` AppRB.COMPLETED
      let [completedRide] = completedRBStatusResult.rideList
      completedRide.status `shouldBe` BRide.COMPLETED
      return $ Just completedRide.id

cancellationChecks :: Id AppRB.RideBooking -> DriverTestData -> ClientsM ()
cancellationChecks bapRideBookingId driver =
  void $
    pollDesc "ride cancelled by driver" $ do
      bapRideBooking <- getBAPRideBooking bapRideBookingId
      bapRideBooking.status `shouldBe` AppRB.CANCELLED
      bppRideBooking <- getBPPRideBooking bapRideBookingId
      bppRideBooking.status `shouldBe` TRB.CANCELLED
      driverInfo <- getBPPDriverInformation $ cast driver.driverId
      driverInfo.onRide `shouldBe` False

      pure $ Just ()

cancelRideByDriver :: DriverTestData -> Id AppRB.RideBooking -> TRide.Ride -> ClientsM ()
cancelRideByDriver driver bapRideBookingId tRide = do
  void . callBPP $
    API.rideCancel driver.token tRide.id $
      RideAPI.CancelRideReq (SCR.CancellationReasonCode "OTHER") Nothing
  cancellationChecks bapRideBookingId driver

cancelRideByApp :: Text -> DriverTestData -> Id AppRB.RideBooking -> ClientsM ()
cancelRideByApp appToken driver bapRideBookingId = do
  void . callBAP $
    BapAPI.cancelRide bapRideBookingId appToken $
      AppCancel.CancelReq
        { reasonCode = AppCR.CancellationReasonCode "",
          reasonStage = AppCR.OnAssign,
          additionalInfo = Nothing
        }
  cancellationChecks bapRideBookingId driver

-- aggregate functions

-- from search to select (inclusive)
search'Select :: Text -> AppSearch.SearchReq -> ClientsM (Id AppQuote.Quote)
search'Select appToken searchReq' = do
  appSearchId <- search appToken searchReq'
  (bapQuoteAPIEntity :| _) <- getOnSearchTaxiQuotesByTransporterName appToken appSearchId bapTransporterName
  let quoteId = bapQuoteAPIEntity.id
  select appToken quoteId
  pure quoteId

data SearchConfirmResult = SearchConfirmResult
  { bapRideBookingId :: Id AppRB.RideBooking,
    bppRideBooking :: TRB.RideBooking,
    ride :: TRide.Ride
  }

search'Confirm :: Text -> DriverTestData -> AppSearch.SearchReq -> ClientsM SearchConfirmResult
search'Confirm appToken driver searchReq' = do
  quoteId <- search'Select appToken searchReq'

  (searchReqForDriver :| _) <- getNearbySearchRequestForDriver driver quoteId

  let firstDriverFare = 30.5 -- this doesn't matter i think and can be hardcoded
  offerQuote driver firstDriverFare searchReqForDriver.searchRequestId

  (selectedQuoteAPIEntity :| _) <- getSelectedQuotesByQuoteId appToken quoteId
  let selectedQuoteId = selectedQuoteAPIEntity.id

  bapRideBookingId <- initWithCheck appToken selectedQuoteId
  (bppRideBooking, ride) <- confirmWithCheck appToken bapRideBookingId
  pure $
    SearchConfirmResult
      { bapRideBookingId,
        bppRideBooking,
        ride
      }
