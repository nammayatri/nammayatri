{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.Sos (getSosTracking, getSosDetails, callExternalSOS, postSosCallExternalSOS) where

import qualified API.Types.RiderPlatform.Management.Sos
import qualified API.Types.UI.Sos as UISos
import qualified Dashboard.Common
import Data.OpenApi (ToSchema)
import qualified Domain.Action.UI.Profile as DP
import qualified Domain.Action.UI.Sos as Sos
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DMSC
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RiderConfig as DRC
import qualified Domain.Types.Sos
import qualified Domain.Types.Sos as DSos
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions as B
import Kernel.External.Maps.Types (LatLong (..))
import qualified Kernel.External.SOS as PoliceSOS
import qualified Kernel.External.SOS.Interface.Types as SOSInterface
import qualified Kernel.External.SOS.Types as SOS
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import qualified Safety.Domain.Types.Sos as SafetyDSos
import Kernel.Utils.Common
import Servant hiding (throwError)
import qualified SharedLogic.Ride as SRide
import qualified SharedLogic.SosLocationTracking as SOSLocation
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
  let sosId' = Kernel.Types.Id.cast @Dashboard.Common.Sos @SafetyDSos.Sos sosId
  res <- Sos.getSosTracking sosId'
  pure $ convertToApiRes res

getSosDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Sos -> Environment.Flow API.Types.RiderPlatform.Management.Sos.SosDetailsMaybeRes)
getSosDetails _merchantShortId _opCity sosId = do
  let sosId' = Kernel.Types.Id.cast @Dashboard.Common.Sos @Domain.Types.Sos.Sos sosId
  mbSos <- B.runInReplica $ QSos.findById sosId'
  pure $ API.Types.RiderPlatform.Management.Sos.SosDetailsMaybeRes {API.Types.RiderPlatform.Management.Sos.details = convertToSosDetailsRes <$> mbSos}

convertToSosDetailsRes :: DSos.Sos -> API.Types.RiderPlatform.Management.Sos.SosDetailsRes
convertToSosDetailsRes s =
  API.Types.RiderPlatform.Management.Sos.SosDetailsRes
    { id = Kernel.Types.Id.cast @Domain.Types.Sos.Sos @Dashboard.Common.Sos s.id,
      personId = Kernel.Types.Id.cast @DPerson.Person @Dashboard.Common.Customer s.personId,
      rideId = Kernel.Types.Id.cast @DRide.Ride @Dashboard.Common.Ride s.rideId,
      flow = convertSosType s.flow,
      status = convertSosStatus s.status,
      ticketId = s.ticketId,
      mediaFiles = Kernel.Types.Id.getId <$> s.mediaFiles,
      trackingExpiresAt = s.trackingExpiresAt,
      sosState = convertSosState <$> s.sosState,
      entityType = convertSosEntityType <$> s.entityType,
      externalReferenceId = s.externalReferenceId,
      externalReferenceStatus = s.externalReferenceStatus,
      externalStatusHistory = s.externalStatusHistory,
      merchantId = Kernel.Types.Id.cast @Domain.Types.Merchant.Merchant @Dashboard.Common.Merchant <$> s.merchantId,
      merchantOperatingCityId = Kernel.Types.Id.cast @DMOC.MerchantOperatingCity @Dashboard.Common.MerchantOperatingCity <$> s.merchantOperatingCityId,
      createdAt = s.createdAt,
      updatedAt = s.updatedAt
    }

convertSosType :: Domain.Types.Sos.SosType -> API.Types.RiderPlatform.Management.Sos.SosType
convertSosType Domain.Types.Sos.Police = API.Types.RiderPlatform.Management.Sos.Police
convertSosType Domain.Types.Sos.CustomerCare = API.Types.RiderPlatform.Management.Sos.CustomerCare
convertSosType (Domain.Types.Sos.EmergencyContact _) = API.Types.RiderPlatform.Management.Sos.SafetyFlow
convertSosType Domain.Types.Sos.SafetyFlow = API.Types.RiderPlatform.Management.Sos.SafetyFlow
convertSosType Domain.Types.Sos.CSAlertSosTicket = API.Types.RiderPlatform.Management.Sos.CSAlertSosTicket
convertSosType Domain.Types.Sos.AudioRecording = API.Types.RiderPlatform.Management.Sos.AudioRecording
convertSosType Domain.Types.Sos.KaptureDashboard = API.Types.RiderPlatform.Management.Sos.KaptureDashboard

convertSosEntityType :: Domain.Types.Sos.SosEntityType -> API.Types.RiderPlatform.Management.Sos.SosEntityType
convertSosEntityType Domain.Types.Sos.Ride = API.Types.RiderPlatform.Management.Sos.Ride
convertSosEntityType Domain.Types.Sos.NonRide = API.Types.RiderPlatform.Management.Sos.NonRide

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

convertSosState :: SafetyDSos.SosState -> API.Types.RiderPlatform.Management.Sos.SosState
convertSosState SafetyDSos.LiveTracking = API.Types.RiderPlatform.Management.Sos.LiveTracking
convertSosState SafetyDSos.SosActive = API.Types.RiderPlatform.Management.Sos.SosActive

convertSosStatus :: SafetyDSos.SosStatus -> API.Types.RiderPlatform.Management.Sos.SosStatus
convertSosStatus SafetyDSos.Resolved = API.Types.RiderPlatform.Management.Sos.Resolved
convertSosStatus SafetyDSos.NotResolved = API.Types.RiderPlatform.Management.Sos.NotResolved
convertSosStatus SafetyDSos.Pending = API.Types.RiderPlatform.Management.Sos.Pending
convertSosStatus SafetyDSos.MockPending = API.Types.RiderPlatform.Management.Sos.MockPending
convertSosStatus SafetyDSos.MockResolved = API.Types.RiderPlatform.Management.Sos.MockResolved
