{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneKindSignatures #-}
module API.Types.Dashboard.AppManagement.Endpoints.Penalty where
import EulerHS.Prelude hiding (id, state)
import Servant
import Data.OpenApi (ToSchema)
import Servant.Client
import Kernel.Types.Common
import qualified Dashboard.Common
import qualified Kernel.Types.APISuccess
import qualified EulerHS.Types
import qualified Data.Singletons.TH
import qualified Data.Aeson



type API = ("penalty" :> PostPenaltyTriggerJobCancellationPenaltyServiceName)
type PostPenaltyTriggerJobCancellationPenaltyServiceName = ("trigger" :> "job" :> "cancellationPenalty" :> "serviceName" :> Capture "serviceName" Dashboard.Common.ServiceNames :> Post ('[JSON])
                                                                                                                                                                                        Kernel.Types.APISuccess.APISuccess)
newtype PenaltyAPIs = PenaltyAPIs {postPenaltyTriggerJobCancellationPenaltyServiceName :: (Dashboard.Common.ServiceNames -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess)}
mkPenaltyAPIs :: (Client EulerHS.Types.EulerClient API -> PenaltyAPIs)
mkPenaltyAPIs penaltyClient = (PenaltyAPIs {..})
                  where postPenaltyTriggerJobCancellationPenaltyServiceName = penaltyClient
data PenaltyUserActionType
    = POST_PENALTY_TRIGGER_JOB_CANCELLATION_PENALTY_SERVICE_NAME
    deriving stock (Show, Read, Generic, Eq, Ord)
    deriving anyclass ToSchema
instance ToJSON PenaltyUserActionType
    where toJSON (POST_PENALTY_TRIGGER_JOB_CANCELLATION_PENALTY_SERVICE_NAME) = Data.Aeson.String "POST_PENALTY_TRIGGER_JOB_CANCELLATION_PENALTY_SERVICE_NAME"
instance FromJSON PenaltyUserActionType
    where parseJSON (Data.Aeson.String "POST_PENALTY_TRIGGER_JOB_CANCELLATION_PENALTY_SERVICE_NAME") = pure POST_PENALTY_TRIGGER_JOB_CANCELLATION_PENALTY_SERVICE_NAME
          parseJSON _ = fail "POST_PENALTY_TRIGGER_JOB_CANCELLATION_PENALTY_SERVICE_NAME expected"

$(Data.Singletons.TH.genSingletons [(''PenaltyUserActionType)])

