{-# LANGUAGE TypeApplications #-}

module Tools.Auth.Dashboard (DashboardAuth, verifyDashboardAction, module Reexport) where

import Beckn.Prelude
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Utils.Common
import Beckn.Utils.Monitoring.Prometheus.Servant
import Beckn.Utils.Servant.HeaderAuth
import Data.Singletons.TH
import qualified Domain.Types.Person as DP
import Domain.Types.Role as Reexport (DashboardAccessType (..))
import qualified Domain.Types.Role as DRole
import Servant hiding (throwError)
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Role as QRole
import qualified Tools.Auth.Common as Common
import Tools.Error
import Tools.Servant.HeaderAuth

instance
  SanitizedUrl (sub :: Type) =>
  SanitizedUrl (DashboardAuth r :> sub)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy sub)

-- | Performs token verification with checking api access level.
type DashboardAuth at = HeaderAuthWithPayload "token" VerifyDashboard (DashboardPayload at)

data VerifyDashboard

data DashboardPayload (at :: DRole.DashboardAccessType)

instance VerificationMethod VerifyDashboard where
  type VerificationResult VerifyDashboard = Id DP.Person
  verificationDescription =
    "Checks whether token is registered and checks person dashboard access.\
    \If you don't have a token, use registration endpoints."

instance VerificationMethodWithPayload VerifyDashboard where
  type VerificationPayloadType VerifyDashboard = DRole.DashboardAccessType

verifyDashboardAction ::
  Common.AuthFlow m r =>
  VerificationActionWithPayload VerifyDashboard m
verifyDashboardAction = VerificationActionWithPayload verifyDashboard

verifyDashboard ::
  Common.AuthFlow m r =>
  DRole.DashboardAccessType ->
  RegToken ->
  m (Id DP.Person)
verifyDashboard requiredAccessType token = do
  (personId, _serverName) <- Common.verifyPerson token
  verifyDashboardAccess requiredAccessType personId

instance
  forall (at :: DRole.DashboardAccessType).
  SingI at =>
  (VerificationPayload DRole.DashboardAccessType) (DashboardPayload at)
  where
  toPayloadType _ = fromSing (sing @at)

verifyDashboardAccess :: EsqDBFlow m r => DRole.DashboardAccessType -> Id DP.Person -> m (Id DP.Person)
verifyDashboardAccess requiredAccessType personId = do
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  case requiredAccessType of
    DRole.DASHBOARD_ADMIN -> do
      role <- QRole.findById person.roleId >>= fromMaybeM (RoleNotFound person.roleId.getId)
      if role.dashboardAccessType == DRole.DASHBOARD_ADMIN
        then pure person.id
        else throwError AccessDenied
    DRole.DASHBOARD_USER ->
      pure person.id
