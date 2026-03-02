module Domain.Action.Internal.ProdLoopStatus where
-- import Domain.Types.External.LiveEKD
import qualified Data.HashMap.Strict as HM
import qualified Domain.Action.UI.CallFeedbackFCM as CallFeedbackFCM
import Kernel.Beam.Functions (runInReplica)
import Kernel.External.Types (Language (ENGLISH))
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, HasFlowEnv, HasRequestId, fromMaybeM, type (:::))
import Kernel.Utils.Error.Throwing hiding (fromMaybeM)
import Kernel.Utils.Logging
import SharedLogic.CallBAPInternal as CallBAPInternal
import qualified Storage.Queries.CallStatus as CallStatus
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QR
import Tools.Metrics (CoreMetrics)
import qualified Tools.Notifications as TN

data ProdLoopStatusReq = ProdLoopStatusReq
  { callId :: Text,
    userType :: Text,
    additionalChargesRequested :: Bool
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

prodLoopStatus ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    CoreMetrics m,
    HasFlowEnv m r '["appBackendBapInternal" ::: AppBackendBapInternal],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasRequestId r
  ) =>
  ProdLoopStatusReq ->
  m APISuccess
prodLoopStatus (ProdLoopStatusReq {..}) = do
  callStatus <- CallStatus.findByCallId callId >>= fromMaybeM (CallStatusDoesNotExist)
  if not additionalChargesRequested
    then do
      pure Success
    else do
      case callStatus.merchantOperatingCityId of
        Just operatingCityId -> case callStatus.entityId of
          Just rideId -> do
            ride <- runInReplica $ QR.findById (Id rideId) >>= fromMaybeM (RideDoesNotExist rideId)
            let personId = ride.driverId.getId
            person <- runInReplica $ QP.findById (Id personId) >>= fromMaybeM (PersonDoesNotExist personId)
            let deviceToken = person.deviceToken
            let language = fromMaybe ENGLISH person.language
            entityData <- CallFeedbackFCM.makeCallFeedbackEntity (Id callId) "EKD_LIVE_CALL_FEEDBACK" operatingCityId language
            logInfo $ "entityData: " <> show entityData
            void $ TN.sendDriverEKDLiveFCM operatingCityId (Id personId) deviceToken language entityData

            -- Notify Rider App via BAP Internal
            appBackendBapInternal <- asks (.appBackendBapInternal)
            let feedbackReq = CallBAPInternal.EKDLiveCallFeedbackReq {rideId = ride.id.getId}
            void $ CallBAPInternal.ekdLiveCallFeedback appBackendBapInternal.apiKey appBackendBapInternal.url feedbackReq

            pure Success
          Nothing -> throwError (InvalidRequest "Ride does not exist")
        Nothing -> throwError (InvalidRequest "Merchant operating city does not exist")
