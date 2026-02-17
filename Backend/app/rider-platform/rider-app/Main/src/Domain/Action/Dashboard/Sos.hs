{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.Sos (getSosTracking, callExternalSOS, postSosCallExternalSOS) where

import qualified API.Types.RiderPlatform.Management.Sos
import qualified API.Types.UI.Sos as UISos
import qualified Dashboard.Common
import Data.OpenApi (ToSchema)
import qualified Domain.Action.UI.Profile as DP
import qualified Domain.Action.UI.Sos as Sos
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DMSC
import qualified Domain.Types.RiderConfig as DRC
import qualified Domain.Types.Sos
import qualified Domain.Types.Sos as DSos
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.SOS as PoliceSOS
import qualified Kernel.External.SOS.Interface.Types as SOSInterface
import qualified Kernel.External.SOS.Types as SOS
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant hiding (throwError)
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as QMSC
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.Sos as QSos
import Tools.Auth
import Tools.Error

getSosTracking :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Sos -> Environment.Flow API.Types.RiderPlatform.Management.Sos.SosTrackingRes)
getSosTracking _merchantShortId _opCity sosId = do
  let sosId' = Kernel.Types.Id.cast @Dashboard.Common.Sos @Domain.Types.Sos.Sos sosId
  res <- Sos.getSosTracking sosId'
  pure $ convertToApiRes res

-- Convert from rider-app SosTrackingRes to dashboard API SosTrackingRes
convertToApiRes :: UISos.SosTrackingRes -> API.Types.RiderPlatform.Management.Sos.SosTrackingRes
convertToApiRes r =
  API.Types.RiderPlatform.Management.Sos.SosTrackingRes
    { currentLocation = convertLocation <$> r.currentLocation,
      sosState = convertSosState <$> r.sosState,
      status = convertSosStatus r.status
    }

convertLocation :: UISos.SosLocationRes -> API.Types.RiderPlatform.Management.Sos.SosLocationRes
convertLocation loc =
  API.Types.RiderPlatform.Management.Sos.SosLocationRes
    { lat = loc.lat,
      lon = loc.lon,
      accuracy = loc.accuracy
    }

convertSosState :: Domain.Types.Sos.SosState -> API.Types.RiderPlatform.Management.Sos.SosState
convertSosState Domain.Types.Sos.LiveTracking = API.Types.RiderPlatform.Management.Sos.LiveTracking
convertSosState Domain.Types.Sos.SosActive = API.Types.RiderPlatform.Management.Sos.SosActive

convertSosStatus :: Domain.Types.Sos.SosStatus -> API.Types.RiderPlatform.Management.Sos.SosStatus
convertSosStatus Domain.Types.Sos.Resolved = API.Types.RiderPlatform.Management.Sos.Resolved
convertSosStatus Domain.Types.Sos.NotResolved = API.Types.RiderPlatform.Management.Sos.NotResolved
convertSosStatus Domain.Types.Sos.Pending = API.Types.RiderPlatform.Management.Sos.Pending
convertSosStatus Domain.Types.Sos.MockPending = API.Types.RiderPlatform.Management.Sos.MockPending
convertSosStatus Domain.Types.Sos.MockResolved = API.Types.RiderPlatform.Management.Sos.MockResolved

-- | Called from the dashboard when triggerSource is DASHBOARD.
--   Fetches the SOS record and dispatches the external SOS API call.
callExternalSOS :: Kernel.Types.Id.Id DSos.Sos -> Environment.Flow ()
callExternalSOS sosId = do
  sos <- QSos.findById sosId >>= fromMaybeM (InvalidRequest "SOS record not found")
  merchantOpCityId <- sos.merchantOperatingCityId & fromMaybeM (InvalidRequest "SOS record missing merchantOperatingCityId")
  merchantId <- sos.merchantId & fromMaybeM (InvalidRequest "SOS record missing merchantId")
  person <- QP.findById sos.personId >>= fromMaybeM (PersonDoesNotExist sos.personId.getId)
  riderConfig <- QRC.findByMerchantOperatingCityId merchantOpCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist merchantOpCityId.getId)
  case riderConfig.externalSOSConfig of
    Nothing -> throwError $ InvalidRequest "External SOS config not configured for this city"
    Just sosConfig -> do
      when (sosConfig.triggerSource /= DRC.DASHBOARD) $
        throwError $ InvalidRequest "External SOS trigger source is not DASHBOARD for this city"
      let sosServiceType = flowToSOSService sosConfig.flow
      merchantSvcCfg <-
        QMSC.findByMerchantOpCityIdAndService merchantId merchantOpCityId (DMSC.SOSService sosServiceType)
          >>= fromMaybeM (MerchantServiceConfigNotFound merchantOpCityId.getId "SOS" (show sosServiceType))
      case merchantSvcCfg.serviceConfig of
        DMSC.SOSServiceConfig specificConfig -> do
          mbRide <- QRide.findById sos.rideId
          emergencyContacts <- DP.getDefaultEmergencyNumbers (sos.personId, merchantId)
          merchantOpCity <- CQMOC.findById merchantOpCityId >>= fromMaybeM (MerchantOperatingCityNotFound merchantOpCityId.getId)
          externalSOSDetails <- Sos.buildExternalSOSDetails (mkSosReq sos) person sosConfig mbRide emergencyContacts.defaultEmergencyNumbers merchantOpCity riderConfig
          response <- PoliceSOS.sendInitialSOS specificConfig externalSOSDetails
          when (response.success && sos.entityType /= Just DSos.NonRide) $ do
            whenJust response.trackingId $ \trackingId ->
              void $ QSos.updateExternalReferenceId (Just trackingId) sosId
        _ -> throwError $ InternalError "Invalid SOS Service Config for provider"
  where
    mkSosReq :: DSos.Sos -> UISos.SosReq
    mkSosReq sos =
      UISos.SosReq
        { flow = sos.flow,
          rideId = Just sos.rideId,
          isRideEnded = Nothing,
          sendPNOnPostRideSOS = Nothing,
          notifyAllContacts = Nothing,
          customerLocation = Nothing
        }

flowToSOSService :: DRC.ExternalSOSFlow -> SOS.SOSService
flowToSOSService DRC.ERSS = SOS.ERSS
flowToSOSService DRC.GJ112 = SOS.GJ112

postSosCallExternalSOS :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Sos -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postSosCallExternalSOS _merchantShortId _opCity sosId = do
  let sosId' = Kernel.Types.Id.cast @Dashboard.Common.Sos @Domain.Types.Sos.Sos sosId
  callExternalSOS sosId'
  pure Kernel.Types.APISuccess.Success
