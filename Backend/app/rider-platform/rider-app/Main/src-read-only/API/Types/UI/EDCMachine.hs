{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.EDCMachine where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.EDCMachineMapping
import qualified Domain.Types.Person
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

data AssignEDCMachineReq = AssignEDCMachineReq
  { channelId :: Kernel.Prelude.Text,
    clientId :: Kernel.Prelude.Text,
    machineName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantKey :: Kernel.Prelude.Text,
    paytmMid :: Kernel.Prelude.Text,
    paytmTid :: Kernel.Prelude.Text,
    personId :: Kernel.Types.Id.Id Domain.Types.Person.Person
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AssignEDCMachineResp = AssignEDCMachineResp
  { id :: Kernel.Types.Id.Id Domain.Types.EDCMachineMapping.EDCMachineMapping,
    isActive :: Kernel.Prelude.Bool,
    machineName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    paytmTid :: Kernel.Prelude.Text,
    personId :: Kernel.Types.Id.Id Domain.Types.Person.Person
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data EDCMachineMappingListItem = EDCMachineMappingListItem
  { createdAt :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.EDCMachineMapping.EDCMachineMapping,
    isActive :: Kernel.Prelude.Bool,
    machineName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    terminalId :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data EDCMachineMappingListResp = EDCMachineMappingListResp {mappings :: [EDCMachineMappingListItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UpdateEDCMachineReq = UpdateEDCMachineReq
  { channelId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    clientId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    isActive :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    machineName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantKey :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    paytmMid :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    paytmTid :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
