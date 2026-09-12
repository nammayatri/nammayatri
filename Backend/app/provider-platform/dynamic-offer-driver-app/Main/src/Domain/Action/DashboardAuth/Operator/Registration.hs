{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Hand-written handlers for direct-dashboard routes whose request needs more
-- than the verified operator's id or name: fleet-owner resolution, fleet-owner
-- verification, dashboard-database writes after the call, and similar.
--
-- provider-dashboard did this work in its own hand-written
-- @Domain.Action.ProviderPlatform.*@ layer before forwarding the call. The
-- generated @API.Action.DashboardAuth@ handler calls these functions instead of
-- the domain handler for every endpoint marked @appServerHandler: custom@ in
-- the spec, so this logic lives here and is never overwritten by the generator.
module Domain.Action.DashboardAuth.Operator.Registration
  ( postOperatorRegister,
    postRegistrationDashboardRegister,
  )
where

import qualified API.Types.ProviderPlatform.Operator.Registration
import qualified Domain.Action.Dashboard.Operator.Registration
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth
import qualified Tools.Auth.DashboardRegistration
import Tools.Auth.DashboardUserAuth

postOperatorRegister :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Operator.Registration.OperatorRegisterReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postOperatorRegister a4 a3 _a2 a1 = do
  Tools.Auth.DashboardRegistration.assertOperatorRegistrable a4.getShortId a1.email a1.mobileNumber a1.mobileCountryCode Kernel.Prelude.Nothing
  res <- Domain.Action.Dashboard.Operator.Registration.postOperatorRegister a4 a3 a1
  Tools.Auth.DashboardRegistration.registerOperatorDashboardPerson a4.getShortId a3 a1.email a1.mobileNumber a1.mobileCountryCode a1.firstName a1.lastName Kernel.Prelude.Nothing res.personId.getId Kernel.Prelude.Nothing
  Kernel.Prelude.pure Kernel.Types.APISuccess.Success

postRegistrationDashboardRegister :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Operator.Registration.CreateDashboardOperatorReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postRegistrationDashboardRegister a4 a3 _a2 a1 = do
  Tools.Auth.DashboardRegistration.assertOperatorRegistrable a4.getShortId (Kernel.Prelude.Just a1.email) a1.mobileNumber a1.mobileCountryCode (Kernel.Prelude.Just a1.roleId)
  res <- Domain.Action.Dashboard.Operator.Registration.postRegistrationDashboardRegister a4 a3 a1
  Tools.Auth.DashboardRegistration.registerOperatorDashboardPerson a4.getShortId a3 (Kernel.Prelude.Just a1.email) a1.mobileNumber a1.mobileCountryCode a1.firstName a1.lastName (Kernel.Prelude.Just a1.password) res.personId.getId (Kernel.Prelude.Just a1.roleId)
  Kernel.Prelude.pure Kernel.Types.APISuccess.Success
