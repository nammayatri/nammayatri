{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.AppManagement.Endpoints.Penalty where

import qualified Dashboard.Common
import qualified Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import Servant
import Servant.Client

type API = ("penalty" :> PostPenaltyTriggerJobCancellationPenaltyServiceName)

type PostPenaltyTriggerJobCancellationPenaltyServiceName =
  ( "trigger" :> "job" :> "cancellationPenalty" :> "serviceName" :> Capture "serviceName" Dashboard.Common.ServiceNames
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

newtype PenaltyAPIs = PenaltyAPIs {postPenaltyTriggerJobCancellationPenaltyServiceName :: (Dashboard.Common.ServiceNames -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess)}

mkPenaltyAPIs :: (Client EulerHS.Types.EulerClient API -> PenaltyAPIs)
mkPenaltyAPIs penaltyClient = (PenaltyAPIs {..})
  where
    postPenaltyTriggerJobCancellationPenaltyServiceName = penaltyClient

data PenaltyUserActionType
  = POST_PENALTY_TRIGGER_JOB_CANCELLATION_PENALTY_SERVICE_NAME
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToSchema)

instance ToJSON PenaltyUserActionType where
  toJSON (POST_PENALTY_TRIGGER_JOB_CANCELLATION_PENALTY_SERVICE_NAME) = Data.Aeson.String "POST_PENALTY_TRIGGER_JOB_CANCELLATION_PENALTY_SERVICE_NAME"

instance FromJSON PenaltyUserActionType where
  parseJSON (Data.Aeson.String "POST_PENALTY_TRIGGER_JOB_CANCELLATION_PENALTY_SERVICE_NAME") = pure POST_PENALTY_TRIGGER_JOB_CANCELLATION_PENALTY_SERVICE_NAME
  parseJSON _ = fail "POST_PENALTY_TRIGGER_JOB_CANCELLATION_PENALTY_SERVICE_NAME expected"

$(Data.Singletons.TH.genSingletons [(''PenaltyUserActionType)])
