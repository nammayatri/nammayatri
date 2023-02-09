module Mobility.ARDU.SyncRide where

import Common (getAppBaseUrl)
import qualified "dynamic-offer-driver-app" Domain.Types.Ride as DDriverOfferRide
import qualified "rider-app" Domain.Types.Ride as DAppBackendRide
import EulerHS.Prelude
import HSpec
import Mobility.ARDU.APICalls (getDriverOfferBppBaseUrl)
import Mobility.ARDU.Fixtures (arduDriver1, nammaYatriPartnerMerchantShortId)
import Mobility.ARDU.Utils as Utils
import Mobility.AppBackend.Fixtures
import Mobility.Fixtures.Routes
import Utils

spec :: Spec
spec = do
  clients <- runIO $ mkMobilityClients getAppBaseUrl getDriverOfferBppBaseUrl
  beforeAndAfter_
    ( do
        Utils.resetDriver arduDriver1
        Utils.resetCustomer appRegistrationToken
    )
    $ do
      it "Testing API flow for cancelled ride sync" $
        withBecknClients clients cancelSyncFlow

cancelSyncFlow :: ClientsM ()
cancelSyncFlow = do
  let (origin, _, searchReq') = karnatakaSearchReq
  Utils.setupDriver arduDriver1 origin
  scRes <- search'Confirm appRegistrationToken arduDriver1 searchReq'

  -- lets simulate situation, when bpp send update to bap, but this update wasn't accepted by bap
  badCancelRideByDriver arduDriver1 scRes

  let providerPlatformRideId = scRes.ride.id
  bppRide <- getBPPRideById providerPlatformRideId
  bppRide.status `shouldBeDesc` DDriverOfferRide.CANCELLED $
    "providerPlatformRideId: " <> show providerPlatformRideId.getId <> "; bpp ride status:"
  bapRide <- getBAPRide providerPlatformRideId
  bapRide.status `shouldBeDesc` DAppBackendRide.NEW $
    "providerPlatformRideId: " <> show providerPlatformRideId.getId <> "; bap ride status:"

  rideSync nammaYatriPartnerMerchantShortId providerPlatformRideId

  syncRide <- getBPPRideById providerPlatformRideId
  syncRide.status `shouldBeDesc` DDriverOfferRide.CANCELLED $
    "providerPlatformRideId: " <> show providerPlatformRideId.getId <> "; bpp sync ride status:"
  bapSyncRide <- getBAPRide providerPlatformRideId
  bapSyncRide.status `shouldBeDesc` DAppBackendRide.CANCELLED $
    "providerPlatformRideId: " <> show providerPlatformRideId.getId <> "; bap sync ride status:"
  where
    shouldBeDesc a1 a2 desc =
      if a1 == a2
        then pure ()
        else expectationFailure (desc <> "\n  expected: " <> show a2 <> "\n   but got: " <> show a1)
