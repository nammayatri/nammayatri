{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.Operator.Fleet
  ( postFleetOperatorFleetRegister,
    postFleetOperatorFleetLink,
    postFleetOperatorFleetUnlink,
  )
where

import qualified API.Types.ProviderPlatform.Operator.Fleet
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

postFleetOperatorFleetRegister :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Operator.Fleet.FleetOwnerRegisterReq -> Environment.Flow API.Types.ProviderPlatform.Operator.Fleet.FleetOwnerRegisterRes)
postFleetOperatorFleetRegister _merchantShortId _opCity requestorId req = do error "Logic yet to be decided" requestorId req

postFleetOperatorFleetLink :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postFleetOperatorFleetLink _merchantShortId _opCity fleetOwnerId requestorId = do error "Logic yet to be decided" fleetOwnerId requestorId

postFleetOperatorFleetUnlink :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postFleetOperatorFleetUnlink _merchantShortId _opCity fleetOwnerId requestorId = do error "Logic yet to be decided" fleetOwnerId requestorId
