{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Endpoints.DriverGroupInsurance where

import qualified Dashboard.Common
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import Servant.Client

type API = ("driverGroupInsurance" :> (PostDriverGroupInsuranceSecondBotCheck :<|> PostDriverGroupInsuranceEnable))

type PostDriverGroupInsuranceSecondBotCheck =
  ( Capture "insuranceId" (Kernel.Types.Id.Id Dashboard.Common.DriverGroupInsurance) :> "secondBotCheck"
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

type PostDriverGroupInsuranceEnable = (Capture "insuranceId" (Kernel.Types.Id.Id Dashboard.Common.DriverGroupInsurance) :> "enable" :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)

data DriverGroupInsuranceAPIs = DriverGroupInsuranceAPIs
  { postDriverGroupInsuranceSecondBotCheck :: (Kernel.Types.Id.Id Dashboard.Common.DriverGroupInsurance -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    postDriverGroupInsuranceEnable :: (Kernel.Types.Id.Id Dashboard.Common.DriverGroupInsurance -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess)
  }

mkDriverGroupInsuranceAPIs :: (Client EulerHS.Types.EulerClient API -> DriverGroupInsuranceAPIs)
mkDriverGroupInsuranceAPIs driverGroupInsuranceClient = (DriverGroupInsuranceAPIs {..})
  where
    postDriverGroupInsuranceSecondBotCheck :<|> postDriverGroupInsuranceEnable = driverGroupInsuranceClient

data DriverGroupInsuranceUserActionType
  = POST_DRIVER_GROUP_INSURANCE_SECOND_BOT_CHECK
  | POST_DRIVER_GROUP_INSURANCE_ENABLE
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [(''DriverGroupInsuranceUserActionType)])
