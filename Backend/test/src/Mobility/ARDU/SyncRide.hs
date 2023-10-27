{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Mobility.ARDU.SyncRide where

import Common (getAppBaseUrl)
import qualified "dynamic-offer-driver-app" Domain.Types.Ride as DDriverOfferRide
import qualified "rider-app" Domain.Types.Ride as DAppBackendRide
import EulerHS.Prelude
import HSpec
import Mobility.ARDU.APICalls (getDriverOfferBppBaseUrl)
import Mobility.ARDU.Fixtures (arduDriver1, nammaYatriDefaultOperatingCity, nammaYatriPartnerMerchantShortId)
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

  rideSync nammaYatriPartnerMerchantShortId nammaYatriDefaultOperatingCity providerPlatformRideId

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
