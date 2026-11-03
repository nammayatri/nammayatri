{-
 Copyright 2022-23, Juspay India Pvt Ltd
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Shared-cab driver-facing actions: a pass-through that rejects unless the variant is SHARED_CAB and forwards to rider-app, which
-- checks under the plate lock that the driver owns the cab's live session and (for booking actions) that the booking is on that plate.
-- selectSharedCabRoute sets sharedCabSessionActive True BEFORE the BAP call (fail-closed); endSharedCabRoute clears it only after a
-- successful route END. Both go through SharedLogic.SharedCab.Flag; the reconciler is otherwise the only clearer.
module Domain.Action.UI.SharedCab
  ( getPublicTransportData,
    postSharedCabRoutesDemand,
    postSharedCabRouteOperations,
    getSharedCabSession,
    postSharedCabSessionOperations,
    postSharedCabBookingOperations,
    postSharedCabBookingCall,
    postSharedCabBookingMessage,
    getSharedCabCallStatus,
    sharedCabFeedVersion,
    BookingAction (..),
    actionReason,
    bookingActionOf,
  )
where

import API.Types.UI.SharedCab
import qualified Domain.Action.UI.Call as DCall
import qualified Domain.Types.CallStatus as SCS
import qualified Domain.Types.Common as DCommon
import Domain.Types.IntegratedBPPConfig (PlatformType (..))
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person as SP
import Domain.Types.SharedCabSession
import qualified Domain.Types.Vehicle as DVehicle
import qualified Domain.Types.VehicleVariant as DV
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption (decrypt)
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Storage.InMem as IM
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.Allocator (SharedCabReconcilerJobData (..))
import SharedLogic.CallBAPInternal (AppBackendBapInternal)
import qualified SharedLogic.DriverPool as DP
import SharedLogic.IntegratedBPPConfig (findIntegratedBPPConfig)
import SharedLogic.Ride (offerQuoteLockKeyWithCoolDown)
import qualified SharedLogic.SharedCab.Flag as SharedCabFlag
import qualified SharedLogic.SharedCab.ReconcilerSeed as SharedCabSeed
import qualified SharedLogic.SharedCab.RiderClient as SharedCabBAP
import qualified Storage.Queries.CallStatus as QCallStatus
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Vehicle as QVehicle
import qualified Tools.Call as TCall
import Tools.Error

type DriverAuthInfo = (Id SP.Person, Id Merchant, Id MerchantOperatingCity)

type TokenAuthInfo = (Maybe (Id SP.Person), Id Merchant, Id MerchantOperatingCity)

getPublicTransportData :: TokenAuthInfo -> Flow SharedCabRouteDataResp
getPublicTransportData auth = requireDriver auth >>= routeData

postSharedCabRoutesDemand :: TokenAuthInfo -> SharedCabDemandReq -> Flow SharedCabDemandResp
postSharedCabRoutesDemand auth req = requireDriver auth >>= (`routesDemand` req)

postSharedCabRouteOperations :: TokenAuthInfo -> SharedCabRouteOperationReq -> Flow SelectRouteResp
postSharedCabRouteOperations auth req = requireDriver auth >>= (`routeOperation` req)

getSharedCabSession :: TokenAuthInfo -> Flow (Maybe SharedCabSession)
getSharedCabSession auth = requireDriver auth >>= currentSession

postSharedCabSessionOperations :: TokenAuthInfo -> SharedCabSessionOperationReq -> Flow SharedCabSession
postSharedCabSessionOperations auth req = requireDriver auth >>= (`sessionOperation` req)

postSharedCabBookingOperations :: TokenAuthInfo -> Text -> SharedCabBookingOperationReq -> Flow SharedCabSession
postSharedCabBookingOperations auth bookingId req = requireDriver auth >>= \driver -> bookingOperation driver bookingId req

postSharedCabBookingCall :: TokenAuthInfo -> Text -> Flow DCall.CallRes
postSharedCabBookingCall auth bookingId = requireDriver auth >>= (`callRider` bookingId)

postSharedCabBookingMessage :: TokenAuthInfo -> Text -> SharedCabMessageReq -> Flow APISuccess
postSharedCabBookingMessage auth bookingId req = requireDriver auth >>= \driver -> sendMessage driver bookingId req

getSharedCabCallStatus :: TokenAuthInfo -> Id SCS.CallStatus -> Flow DCall.GetCallStatusRes
getSharedCabCallStatus _ = DCall.getCallStatus

requireDriver :: TokenAuthInfo -> Flow DriverAuthInfo
requireDriver (mbPersonId, merchantId, merchantOpCityId) = do
  personId <- fromMaybeM (PersonNotFound "No person found") mbPersonId
  pure (personId, merchantId, merchantOpCityId)

sharedCabDefaultCapacity :: Int
sharedCabDefaultCapacity = 6

sharedCabVehicleCategory :: Text
sharedCabVehicleCategory = "SHARED_CAB"

sharedCabServiceTierType :: Text
sharedCabServiceTierType = show DCommon.SHARED_CAB

validateSharedCabDriver :: Id SP.Person -> Flow DVehicle.Vehicle
validateSharedCabDriver personId = do
  driver <- QPerson.findById personId >>= fromMaybeM (DriverNotFound personId.getId)
  unless (driver.role == SP.DRIVER) $ throwError (InvalidRequest ("Person " <> personId.getId <> " is not a driver"))
  vehicle <- QVehicle.findById personId >>= fromMaybeM (DriverWithoutVehicle personId.getId)
  unless (vehicle.variant == DV.SHARED_CAB) $
    throwError $
      InvalidRequest ("Vehicle variant " <> show vehicle.variant <> " is not a shared cab")
  pure vehicle

sharedCabBPPConfig :: Id MerchantOperatingCity -> Flow DIBC.IntegratedBPPConfig
sharedCabBPPConfig merchantOpCityId =
  findIntegratedBPPConfig Nothing merchantOpCityId sharedCabVehicleCategory APPLICATION

bapInternal :: Flow AppBackendBapInternal
bapInternal = asks (.appBackendBapInternal)

routeData :: DriverAuthInfo -> Flow SharedCabRouteDataResp
routeData (personId, _merchantId, merchantOpCityId) = do
  _ <- validateSharedCabDriver personId
  integratedBPPConfig <- sharedCabBPPConfig merchantOpCityId
  bap <- bapInternal
  SharedCabBAP.getSharedCabRouteData bap.apiKey bap.url integratedBPPConfig.agencyKey

-- | Rider-app owns the feed, so a demand read with no routes fetches its version. A failure only leaves the app on its cached routes.
sharedCabFeedVersion :: Id MerchantOperatingCity -> Maybe DV.VehicleVariant -> Flow (Maybe Text)
sharedCabFeedVersion merchantOpCityId variant
  | variant /= Just DV.SHARED_CAB = pure Nothing
  | otherwise =
    either (const Nothing) identity
      <$> withTryCatch
        "sharedCabFeedVersion"
        ( do
            integratedBPPConfig <- sharedCabBPPConfig merchantOpCityId
            bap <- bapInternal
            IM.withInMemCache ["SharedCabFeedVersion", integratedBPPConfig.agencyKey] 300 $
              (.publicTransportVersion)
                <$> SharedCabBAP.postSharedCabRoutesDemand bap.apiKey bap.url SharedCabBAP.BAPDemandReq {SharedCabBAP.agencyId = integratedBPPConfig.agencyKey, SharedCabBAP.routeCodes = []}
        )

routesDemand :: DriverAuthInfo -> SharedCabDemandReq -> Flow SharedCabDemandResp
routesDemand (personId, _merchantId, merchantOpCityId) req = do
  _ <- validateSharedCabDriver personId
  integratedBPPConfig <- sharedCabBPPConfig merchantOpCityId
  bap <- bapInternal
  SharedCabBAP.postSharedCabRoutesDemand bap.apiKey bap.url SharedCabBAP.BAPDemandReq {SharedCabBAP.agencyId = integratedBPPConfig.agencyKey, SharedCabBAP.routeCodes = req.routeCodes}

-- set the exclusion flag BEFORE the BAP call (fail-closed: a failed call leaves the driver excluded until the reconciler clears it),
-- under the offer-quote lock so a silent assign cannot read the pool data between the two
setSharedCabSessionActiveBeforeSelect :: Id SP.Person -> Flow ()
setSharedCabSessionActiveBeforeSelect driverId = do
  quoteRespondCoolDown <- asks (.quoteRespondCoolDown)
  Redis.whenWithLockRedisAndReturnValue (offerQuoteLockKeyWithCoolDown driverId) quoteRespondCoolDown (SharedCabFlag.setSharedCabSessionActive driverId)
    >>= either (const $ throwError (InvalidRequest "A ride is being assigned to you; try again shortly")) pure

routeOperation :: DriverAuthInfo -> SharedCabRouteOperationReq -> Flow SelectRouteResp
routeOperation auth@(personId, _, _) req = SharedCabFlag.withSharedCabFlagLock personId $ case req.operation of
  SELECT_ROUTE -> do
    routeCode <- fromMaybeM (InvalidRequest "routeCode is required to select a route") req.routeCode
    walkupCount <- fromMaybeM (InvalidRequest "walkupCount is required to select a route") req.walkupCount
    selectSharedCabRoute auth req routeCode walkupCount
  _ -> endSharedCabRoute auth req

selectSharedCabRoute :: DriverAuthInfo -> SharedCabRouteOperationReq -> Text -> Int -> Flow SelectRouteResp
selectSharedCabRoute (personId, merchantId, merchantOpCityId) req routeCode walkupCount = do
  vehicle <- validateSharedCabDriver personId
  driver <- QPerson.findById personId >>= fromMaybeM (DriverNotFound personId.getId)
  integratedBPPConfig <- sharedCabBPPConfig merchantOpCityId
  bap <- bapInternal
  setSharedCabSessionActiveBeforeSelect personId
  -- dead-chain recovery: a select is the city's hottest driver action, so re-run the idempotent chain seeder (a cheap no-op
  -- while the marker is alive). Fire-and-forget: a seed failure is logged, never propagated into the driver's select.
  SharedCabSeed.trySeedSharedCabReconcilerChain (Just merchantId) merchantOpCityId (SharedCabReconcilerJobData merchantId merchantOpCityId)
  SharedCabBAP.postRouteOperation bap.apiKey bap.url $
    SharedCabBAP.BAPRouteOperationReq
      { SharedCabBAP.operation = req.operation,
        SharedCabBAP.driverId = personId.getId,
        SharedCabBAP.vehicleNumber = vehicle.registrationNo,
        SharedCabBAP.agencyId = Just integratedBPPConfig.agencyKey,
        SharedCabBAP.serviceTierType = Just sharedCabServiceTierType,
        SharedCabBAP.capacity = Just (fromMaybe sharedCabDefaultCapacity vehicle.capacity),
        SharedCabBAP.routeCode = Just routeCode,
        SharedCabBAP.walkupCount = Just walkupCount,
        SharedCabBAP.mode = req.mode,
        SharedCabBAP.driverName = Just (unwords (catMaybes [Just driver.firstName, driver.lastName])),
        SharedCabBAP.vehicleModel = Just vehicle.model,
        SharedCabBAP.atLastStop = Nothing,
        SharedCabBAP.forceEnd = Nothing
      }

currentSession :: DriverAuthInfo -> Flow (Maybe SharedCabSession)
currentSession (personId, _merchantId, _merchantOpCityId) = do
  vehicle <- validateSharedCabDriver personId
  bap <- bapInternal
  SharedCabBAP.getSharedCabSession bap.apiKey bap.url personId.getId vehicle.registrationNo

endSharedCabRoute :: DriverAuthInfo -> SharedCabRouteOperationReq -> Flow SelectRouteResp
endSharedCabRoute (personId, _merchantId, _merchantOpCityId) req = do
  vehicle <- validateSharedCabDriver personId
  bap <- bapInternal
  resp <-
    SharedCabBAP.postRouteOperation bap.apiKey bap.url $
      SharedCabBAP.BAPRouteOperationReq
        { SharedCabBAP.operation = req.operation,
          SharedCabBAP.driverId = personId.getId,
          SharedCabBAP.vehicleNumber = vehicle.registrationNo,
          SharedCabBAP.agencyId = Nothing,
          SharedCabBAP.serviceTierType = Nothing,
          SharedCabBAP.capacity = Nothing,
          SharedCabBAP.routeCode = Nothing,
          SharedCabBAP.walkupCount = Nothing,
          SharedCabBAP.mode = Nothing,
          SharedCabBAP.driverName = Nothing,
          SharedCabBAP.vehicleModel = Nothing,
          SharedCabBAP.atLastStop = req.atLastStop,
          SharedCabBAP.forceEnd = req.forceEnd
        }
  -- the flag clears ONLY after a successful route END whose response carries no session; a session payload (RETURN/CHANGE) keeps
  -- the driver excluded
  when (isNothing resp.session) $
    SharedCabFlag.clearSharedCabSessionActive personId
  pure resp

sessionOperation :: DriverAuthInfo -> SharedCabSessionOperationReq -> Flow SharedCabSession
sessionOperation (personId, _merchantId, _merchantOpCityId) req = do
  vehicle <- validateSharedCabDriver personId
  bap <- bapInternal
  SharedCabBAP.postSessionOperation bap.apiKey bap.url $
    SharedCabBAP.BAPSessionOperationReq
      { SharedCabBAP.operation = req.operation,
        SharedCabBAP.driverId = personId.getId,
        SharedCabBAP.vehicleNumber = vehicle.registrationNo,
        SharedCabBAP.version = req.version,
        SharedCabBAP.walkupCount = req.walkupCount
      }

data BookingAction = Cancel Text | BoardedWithoutCode | Dropped | Accept | Decline
  deriving (Show, Eq)

bookingActionOf :: SharedCabBookingOperationReq -> Maybe BookingAction
bookingActionOf req = case req.operation of
  ACCEPT -> Just Accept
  DECLINE -> Just Decline
  DROPPED -> Just Dropped
  BOARDED_WITHOUT_CODE -> Just BoardedWithoutCode
  CANCEL -> Cancel <$> req.reason

bookingOperation :: DriverAuthInfo -> Text -> SharedCabBookingOperationReq -> Flow SharedCabSession
bookingOperation (personId, _merchantId, merchantOpCityId) bookingId req = do
  action <- fromMaybeM (InvalidRequest "reason is required to cancel a booking") (bookingActionOf req)
  vehicle <- validateSharedCabDriver personId
  bap <- bapInternal
  session <-
    SharedCabBAP.postBookingOperation bap.apiKey bap.url bookingId $
      SharedCabBAP.BAPBookingOperationReq {operation = req.operation, driverId = personId.getId, vehicleNumber = vehicle.registrationNo, reason = actionReason action}
  -- the offer's answer goes into the driver's quote-response counters, like a ride request's
  whenJust (offerResponse action) $ DP.recordQuoteResponseCounters merchantOpCityId personId
  pure session

offerResponse :: BookingAction -> Maybe DCommon.SearchRequestForDriverResponse
offerResponse = \case
  Accept -> Just DCommon.Accept
  Decline -> Just DCommon.Reject
  _ -> Nothing

actionReason :: BookingAction -> Maybe Text
actionReason = \case
  Cancel reason -> Just reason
  _ -> Nothing

-- | Driver calls the rider of a shared-cab booking through Exotel, so each side sees only the ExoPhone. Same shape as
-- Domain.Action.UI.Call.initiateCallToCustomer, but keyed by the FRFS booking id: rider-app owns the booking and the rider's phone,
-- and decides whether the booking is inside the call window.
callRider :: DriverAuthInfo -> Text -> Flow DCall.CallRes
callRider (personId, merchantId, merchantOpCityId) bookingId = do
  vehicle <- validateSharedCabDriver personId
  bap <- bapInternal
  rider <- SharedCabBAP.getRiderCallNumber bap.apiKey bap.url bookingId personId.getId vehicle.registrationNo
  driverPhone <- getDriverPhone personId
  callStatusId <- generateGUID
  let entityId = DCall.sharedCabCallEntity bookingId
  exotelResponse <- TCall.initiateCall merchantId merchantOpCityId (DCall.driverCallReq driverPhone rider.riderNumber callStatusId entityId)
  logTagInfo ("BookingId: " <> bookingId) "Call initiated from shared-cab driver to rider."
  QCallStatus.create =<< DCall.buildDriverCallStatus callStatusId entityId merchantId merchantOpCityId exotelResponse
  pure $ DCall.CallRes callStatusId

getDriverPhone :: Id SP.Person -> Flow Text
getDriverPhone personId = do
  driver <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  decrypted <- mapM decrypt driver.mobileNumber
  fromMaybeM (InternalError "Driver has no phone number.") $ (<>) <$> driver.mobileCountryCode <*> decrypted

-- | The booking-keyed twin of POST /onMessage: rider-app checks the booking is on this driver's live cab and sends the rider the CHAT_MESSAGE push.
sendMessage :: DriverAuthInfo -> Text -> SharedCabMessageReq -> Flow APISuccess
sendMessage (personId, _, _) bookingId req = do
  vehicle <- validateSharedCabDriver personId
  bap <- bapInternal
  SharedCabBAP.postRiderMessage bap.apiKey bap.url bookingId SharedCabBAP.BAPMessageReq {driverId = personId.getId, vehicleNumber = vehicle.registrationNo, message = req.message}
