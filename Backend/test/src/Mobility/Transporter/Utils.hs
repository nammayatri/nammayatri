{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Mobility.Transporter.Utils where

import qualified "static-offer-driver-app" API.UI.Booking as BookingAPI
import "static-offer-driver-app" API.UI.Location as LocationAPI
import "rider-app" API.UI.Quote
import qualified "static-offer-driver-app" API.UI.Ride as RideAPI
import qualified "rider-app" API.UI.Search as AppSearch
import Common
import qualified "static-offer-driver-app" Domain.Action.UI.Booking as DUB
import qualified "rider-app" Domain.Types.Booking as BRB
import qualified "static-offer-driver-app" Domain.Types.Booking as TBooking
import qualified "static-offer-driver-app" Domain.Types.Booking as TRB
import qualified "rider-app" Domain.Types.CancellationReason as AppCR
import qualified "static-offer-driver-app" Domain.Types.CancellationReason as SCR
import qualified "static-offer-driver-app" Domain.Types.Merchant.MerchantServiceConfig as TDMSC
import "static-offer-driver-app" Domain.Types.Person as TPerson
import qualified "rider-app" Domain.Types.Quote as AppQuote
import qualified "rider-app" Domain.Types.Ride as BRide
import qualified "static-offer-driver-app" Domain.Types.Ride as TRide
import qualified "rider-app" Domain.Types.SearchRequest as AppSearchReq
import HSpec
import qualified Kernel.External.Maps as Maps
import Kernel.External.Maps.Types
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Types.Time
import Kernel.Utils.Common
import qualified Mobility.AppBackend.APICalls as API
import Mobility.AppBackend.Fixtures
import qualified Mobility.Transporter.APICalls as API
import Mobility.Transporter.Fixtures as Fixtures
import qualified "static-offer-driver-app" Storage.CachedQueries.Merchant.MerchantServiceConfig as TCQMSC
import qualified "rider-app" Storage.Queries.Booking as BQRB
import qualified "static-offer-driver-app" Storage.Queries.Booking as TQBooking
import qualified "static-offer-driver-app" Storage.Queries.Booking as TQRB
import qualified "static-offer-driver-app" Storage.Queries.DriverInformation as DriverInfo
import "static-offer-driver-app" Storage.Queries.DriverLocation
import qualified "static-offer-driver-app" Storage.Queries.NotificationStatus as QNS
import qualified "static-offer-driver-app" Storage.Queries.Ride as TQRide
import Utils

getFutureTime :: IO UTCTime
getFutureTime =
  -- Generate a time 2 hours in to the future else booking will fail
  addUTCTime 7200 <$> getCurrentTime

-- db calls
getBAPBooking ::
  Id BRB.Booking ->
  ClientsM BRB.Booking
getBAPBooking bapRBId = do
  mbBRB <- liftIO $ runAppFlow "" $ BQRB.findById bapRBId (Proxy @RiderPlatformFlow)
  mbBRB `shouldSatisfy` isJust
  let Just bRB = mbBRB
  return bRB

getBPPBooking ::
  Id BRB.Booking ->
  ClientsM TRB.Booking
getBPPBooking bapRBId = do
  bRB <- getBAPBooking bapRBId
  bRB.bppBookingId `shouldSatisfy` isJust
  let Just bppBookingId = bRB.bppBookingId
  mbTRB <- liftIO $ runTransporterFlow "" $ TQRB.findById (cast bppBookingId) (Proxy @StaticDriverAppFlow)
  mbTRB $> () `shouldSatisfy` isJust
  let Just tRB = mbTRB
  return tRB

getBPPRide ::
  Id TRB.Booking ->
  ClientsM TRide.Ride
getBPPRide rideBookingId = do
  mbRide <- liftIO $ runTransporterFlow "" $ TQRide.findActiveByRBId rideBookingId (Proxy @StaticDriverAppFlow)
  mbRide `shouldSatisfy` isJust
  return $ fromJust mbRide

getBPPRideById ::
  Id TRide.Ride ->
  ClientsM TRide.Ride
getBPPRideById rideId = do
  mbRide <- liftIO $ runTransporterFlow "" $ TQRide.findById rideId (Proxy @StaticDriverAppFlow)
  mbRide `shouldSatisfy` isJust
  return $ fromJust mbRide

getBPPDriverLocation ::
  Id TPerson.Person ->
  ClientsM LatLong
getBPPDriverLocation driverId = do
  mbRes <- liftIO $ runTransporterFlow "" $ findById driverId (Proxy @StaticDriverAppFlow)
  mbRes `shouldSatisfy` isJust
  let res = fromJust mbRes
  pure $
    LatLong
      { lat = res.lat,
        lon = res.lon
      }

-- driver setup/reset
setupDriver :: DriverTestData -> LatLong -> ClientsM ()
setupDriver driver initialPoint = do
  void . callBPP $ API.setDriverOnline driver.token True
  -- Moves driver to the pickup point
  preUpdate <- liftIO $ API.buildUpdateLocationRequest $ initialPoint :| []
  void . callBPP $
    API.updateLocation driver.token preUpdate

resetDriver :: DriverTestData -> IO ()
resetDriver driver = runTransporterFlow "" $ do
  mbActiveRide <- TQRide.getActiveByDriverId (cast driver.driverId) (Proxy @StaticDriverAppFlow)
  Esq.runTransaction $ do
    QNS.deleteByPersonId @StaticDriverAppFlow $ cast driver.driverId
    whenJust mbActiveRide $ \activeRide -> do
      TQRide.updateStatus activeRide.id TRide.CANCELLED
      TQBooking.updateStatus activeRide.bookingId TBooking.CANCELLED
    DriverInfo.updateActivity (cast driver.driverId) False
    DriverInfo.updateOnRide (cast driver.driverId) False

-- flow primitives
search :: Text -> AppSearch.SearchReq -> ClientsM (Id AppSearchReq.SearchRequest)
search token searchReq_ = callBAP $ searchServices token searchReq_ (Just defaultVersion) (Just defaultVersion) <&> (.searchId)

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

confirmWithCheck :: Text -> Id AppQuote.Quote -> ClientsM (Id BRB.Booking, TRB.Booking)
confirmWithCheck appToken quoteId = do
  bBookingId <- fmap (.bookingId) $ callBAP $ API.appConfirmRide appToken quoteId
  void . pollDesc "confirm result" $ do
    initRB <- getBAPBooking bBookingId
    initRB.bppBookingId `shouldSatisfy` isJust
    return $ Just ()

  void . pollDesc "ride confirmed" $
    callBAP (API.appBookingStatus bBookingId appRegistrationToken)
      <&> (.status)
      >>= (`shouldBe` BRB.CONFIRMED)
      <&> Just

  tBooking <- pollDesc "ride booking id should exist and should be confirmed" $ do
    trb <- getBPPBooking bBookingId
    trb.status `shouldBe` TRB.CONFIRMED
    return $ Just trb
  pure (bBookingId, tBooking)

acceptRide :: DriverTestData -> TRB.Booking -> ClientsM TRide.Ride
acceptRide driver tBooking = do
  void . callBPP $
    API.rideRespond tBooking.id driver.token $
      BookingAPI.SetDriverAcceptanceReq BookingAPI.ACCEPT

  pollDesc ("ride with booking id=" <> tBooking.id.getId <> " should exist and should have status=NEW") $ do
    tRide <- getBPPRide tBooking.id
    tRide.status `shouldBe` TRide.NEW
    return $ Just tRide

rejectRide :: DriverTestData -> TRB.Booking -> ClientsM ()
rejectRide driver tBooking = do
  void . callBPP $
    API.rideRespond tBooking.id driver.token $
      BookingAPI.SetDriverAcceptanceReq BookingAPI.REJECT

cancelRideByApp :: Text -> Id BRB.Booking -> ClientsM ()
cancelRideByApp appToken bBookingId = do
  void . callBAP $ API.cancelRide bBookingId appRegistrationToken (API.mkAppCancelReq AppCR.OnConfirm)
  checkBookingBapStatus appToken bBookingId BRB.CANCELLED

cancelRideByDriver :: DriverTestData -> TRide.Ride -> ClientsM ()
cancelRideByDriver driver tRide = do
  void . callBPP $
    API.rideCancel driver.token tRide.id $
      RideAPI.CancelRideReq (SCR.CancellationReasonCode "OTHER") Nothing

  void $
    pollDesc ("cancelled ride by driver id=" <> driver.driverId.getId) $ do
      tRide' <- getBPPRideById tRide.id
      tRide'.status `shouldBe` TRide.CANCELLED
      pure $ Just tRide'

checkBookingBapStatus :: Text -> Id BRB.Booking -> BRB.BookingStatus -> ClientsM ()
checkBookingBapStatus appToken bBookingId status =
  void . pollDesc ("bap booking with id=" <> bBookingId.getId <> " should exist and should have status=" <> show status) $
    callBAP (API.appBookingStatus bBookingId appToken)
      <&> (.status)
      >>= (`shouldBe` status)
      <&> Just

getRideInfo :: DriverTestData -> Id TRB.Booking -> ClientsM DUB.RideInfo
getRideInfo driver bppBookingId = do
  rideInfo2 <-
    pollDesc ("poll for rideInfo for driver with token:" <> show driver.token) . callBPP $
      API.getNotificationInfo bppBookingId driver.token
        <&> (.rideRequest)
  rideInfo2.bookingId `shouldBe` bppBookingId
  pure rideInfo2

checkEmptyRideInfo :: DriverTestData -> Id TRB.Booking -> ClientsM ()
checkEmptyRideInfo driver bppBookingId = do
  rideInfo2 :: Maybe () <-
    pollDesc ("poll for empty rideInfo for driver with token:" <> show driver.token) . callBPP $ do
      res <- API.getNotificationInfo bppBookingId driver.token
      let mbRideRequest = res.rideRequest
      case mbRideRequest of
        Nothing -> return $ Just Nothing -- sorry, i don't want to refactor poll for this case only :)
        Just _ -> return Nothing
  rideInfo2 `shouldBe` Nothing

startRide :: Text -> DriverTestData -> LatLong -> TRide.Ride -> Id BRB.Booking -> ClientsM ()
startRide appToken driver origin tRide bBookingId = do
  void . callBPP $
    API.rideStart driver.token tRide.id $
      API.buildDriverStartRideReq tRide.otp origin

  void . pollDesc "trip started" $ do
    inprogressRBStatusResult <- callBAP (API.appBookingStatus bBookingId appToken)
    inprogressRBStatusResult.rideList `shouldSatisfy` not . null
    inprogressRBStatusResult.status `shouldBe` BRB.TRIP_ASSIGNED
    let [inprogressRide] = inprogressRBStatusResult.rideList
    inprogressRide.status `shouldBe` BRide.INPROGRESS
    return $ Just ()

updateLocation ::
  DriverTestData ->
  NonEmpty LocationAPI.Waypoint ->
  ClientsM ()
updateLocation driver updatesList = do
  void . callBPP $
    API.updateLocation driver.token updatesList

endRide ::
  Text ->
  DriverTestData ->
  LatLong ->
  TRide.Ride ->
  Id BRB.Booking ->
  ClientsM (Id BRide.Ride)
endRide appToken driver destination tRide bBookingId = do
  void . callBPP $ API.rideEnd driver.token tRide.id $ RideAPI.EndRideReq destination
  pollDesc "ride completed" $ do
    completedRBStatusResult <- callBAP (API.appBookingStatus bBookingId appToken)
    completedRBStatusResult.rideList `shouldSatisfy` not . null
    completedRBStatusResult.status `shouldBe` BRB.COMPLETED
    let [completedRide] = completedRBStatusResult.rideList
    completedRide.status `shouldBe` BRide.COMPLETED
    return $ Just completedRide.id

data SearchConfirmResult = SearchConfirmResult
  { bapBookingId :: Id BRB.Booking,
    bppBooking :: TRB.Booking
  }

search'Confirm :: Text -> AppSearch.SearchReq -> (SearchConfirmResult -> ClientsM ()) -> ClientsM ()
search'Confirm appToken searchReq' f = do
  appSearchId <- search appToken searchReq'
  (bapQuoteAPIEntity :| _) <- getOnSearchTaxiQuotesByTransporterName appToken appSearchId bapTransporterName
  let quoteId = bapQuoteAPIEntity.id

  (bapBookingId, bppBooking) <- confirmWithCheck appToken quoteId
  flip onException (cancelRideByApp appToken bapBookingId) $
    f
      SearchConfirmResult
        { bapBookingId,
          bppBooking
        }

changeCachedMapsConfig :: Maps.MapsServiceConfig -> IO ()
changeCachedMapsConfig googleCfg = runTransporterFlow "change cached maps config" $ do
  let serviceConfig = TDMSC.MapsServiceConfig googleCfg
  yatriPartnerServiceConfig <- TDMSC.buildMerchantServiceConfig Fixtures.yatriPartnerMerchantId serviceConfig
  otherMerchantServiceConfig <- TDMSC.buildMerchantServiceConfig Fixtures.otherMerchantId serviceConfig
  TCQMSC.cacheMerchantServiceConfig yatriPartnerServiceConfig
  TCQMSC.cacheMerchantServiceConfig otherMerchantServiceConfig

clearCachedMapsConfig :: IO ()
clearCachedMapsConfig = runTransporterFlow "clear cached maps config" $ do
  TCQMSC.clearCache Fixtures.yatriPartnerMerchantId (TDMSC.MapsService Maps.Google)
  TCQMSC.clearCache Fixtures.otherMerchantId (TDMSC.MapsService Maps.Google)
