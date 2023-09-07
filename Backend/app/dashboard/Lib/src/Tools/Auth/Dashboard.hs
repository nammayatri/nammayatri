{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Auth.Dashboard (DashboardAuth, verifyDashboardAction, TokenInfo (..), module Reexport) where

import Data.Singletons.TH
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.Person as DP
import Domain.Types.Role as Reexport (DashboardAccessType (..))
import qualified Domain.Types.Role as DRole
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Monitoring.Prometheus.Servant
import Kernel.Utils.Servant.HeaderAuth
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

data TokenInfo = TokenInfo
  { personId :: Id DP.Person,
    merchantId :: Id DMerchant.Merchant
  }

instance VerificationMethod VerifyDashboard where
  type VerificationResult VerifyDashboard = TokenInfo
  verificationDescription =
    "Checks whether token is registered and checks person dashboard access. \
    \If you don't have a token, use registration endpoints."

instance VerificationMethodWithPayload VerifyDashboard where
  type VerificationPayloadType VerifyDashboard = DRole.DashboardAccessType

verifyDashboardAction ::
  (Common.AuthFlow m r, Redis.HedisFlow m r) =>
  VerificationActionWithPayload VerifyDashboard m
verifyDashboardAction = VerificationActionWithPayload verifyDashboard

verifyDashboard ::
  (Common.AuthFlow m r, Redis.HedisFlow m r) =>
  DRole.DashboardAccessType ->
  RegToken ->
  m TokenInfo
verifyDashboard requiredAccessType token = do
  (personId, merchantId) <- Common.verifyPerson token
  void $ verifyDashboardAccess requiredAccessType personId
  pure TokenInfo {personId, merchantId}

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
    DRole.FLEET_OWNER -> do
      role <- QRole.findById person.roleId >>= fromMaybeM (RoleNotFound person.roleId.getId)
      if role.dashboardAccessType == DRole.FLEET_OWNER
        then pure person.id
        else throwError AccessDenied
    DRole.DASHBOARD_USER ->
      pure person.id
