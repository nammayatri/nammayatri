{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.AppManagement.Endpoints.EDCMachine where

import qualified Data.Bool
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Data.Text
import qualified "this" Domain.Types.EDCMachineMapping
import qualified "this" Domain.Types.Person
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import Servant
import Servant.Client

data AssignEDCMachineReq = AssignEDCMachineReq
  { channelId :: Data.Text.Text,
    clientId :: Data.Text.Text,
    machineName :: Kernel.Prelude.Maybe Data.Text.Text,
    merchantKey :: Data.Text.Text,
    paytmMid :: Data.Text.Text,
    paytmTid :: Data.Text.Text,
    personId :: Kernel.Types.Id.Id Domain.Types.Person.Person
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets AssignEDCMachineReq where
  hideSecrets = Kernel.Prelude.identity

data AssignEDCMachineResp = AssignEDCMachineResp
  { id :: Kernel.Types.Id.Id Domain.Types.EDCMachineMapping.EDCMachineMapping,
    isActive :: Data.Bool.Bool,
    machineName :: Kernel.Prelude.Maybe Data.Text.Text,
    paytmTid :: Data.Text.Text,
    personId :: Kernel.Types.Id.Id Domain.Types.Person.Person
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data EDCMachineMappingListItem = EDCMachineMappingListItem
  { createdAt :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.EDCMachineMapping.EDCMachineMapping,
    isActive :: Data.Bool.Bool,
    machineName :: Kernel.Prelude.Maybe Data.Text.Text,
    personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    terminalId :: Data.Text.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data EDCMachineMappingListResp = EDCMachineMappingListResp {mappings :: [EDCMachineMappingListItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UpdateEDCMachineReq = UpdateEDCMachineReq
  { channelId :: Kernel.Prelude.Maybe Data.Text.Text,
    clientId :: Kernel.Prelude.Maybe Data.Text.Text,
    isActive :: Kernel.Prelude.Maybe Data.Bool.Bool,
    machineName :: Kernel.Prelude.Maybe Data.Text.Text,
    merchantKey :: Kernel.Prelude.Maybe Data.Text.Text,
    paytmMid :: Kernel.Prelude.Maybe Data.Text.Text,
    paytmTid :: Kernel.Prelude.Maybe Data.Text.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpdateEDCMachineReq where
  hideSecrets = Kernel.Prelude.identity

type API = ("edcMachine" :> (AssignEDCMachine :<|> ListEDCMachine :<|> UpdateEDCMachine :<|> DeleteEDCMachine))

type AssignEDCMachine = ("assign" :> ReqBody ('[JSON]) AssignEDCMachineReq :> Post ('[JSON]) AssignEDCMachineResp)

type ListEDCMachine = ("list" :> QueryParam "isActive" Data.Bool.Bool :> QueryParam "personId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> Get ('[JSON]) EDCMachineMappingListResp)

type UpdateEDCMachine =
  ( Capture "mappingId" (Kernel.Types.Id.Id Domain.Types.EDCMachineMapping.EDCMachineMapping) :> "update" :> ReqBody ('[JSON]) UpdateEDCMachineReq
      :> Put
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

type DeleteEDCMachine = (Capture "mappingId" (Kernel.Types.Id.Id Domain.Types.EDCMachineMapping.EDCMachineMapping) :> "delete" :> Delete ('[JSON]) Kernel.Types.APISuccess.APISuccess)

data EDCMachineAPIs = EDCMachineAPIs
  { assignEDCMachine :: (AssignEDCMachineReq -> EulerHS.Types.EulerClient AssignEDCMachineResp),
    listEDCMachine :: (Kernel.Prelude.Maybe (Data.Bool.Bool) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) -> EulerHS.Types.EulerClient EDCMachineMappingListResp),
    updateEDCMachine :: (Kernel.Types.Id.Id Domain.Types.EDCMachineMapping.EDCMachineMapping -> UpdateEDCMachineReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    deleteEDCMachine :: (Kernel.Types.Id.Id Domain.Types.EDCMachineMapping.EDCMachineMapping -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess)
  }

mkEDCMachineAPIs :: (Client EulerHS.Types.EulerClient API -> EDCMachineAPIs)
mkEDCMachineAPIs eDCMachineClient = (EDCMachineAPIs {..})
  where
    assignEDCMachine :<|> listEDCMachine :<|> updateEDCMachine :<|> deleteEDCMachine = eDCMachineClient

data EDCMachineUserActionType
  = ASSIGN_EDC_MACHINE
  | LIST_EDC_MACHINE
  | UPDATE_EDC_MACHINE
  | DELETE_EDC_MACHINE
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [(''EDCMachineUserActionType)])
