{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.BackgroundVerification where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DMSC
import qualified Kernel.External.BackgroundVerification.Interface as BackgroundVerification
import qualified Kernel.External.BackgroundVerification.Types as BackgroundVerification
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Cac.MerchantServiceUsageConfig as QOMC
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC

createCandidate ::
  ServiceFlow m r =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  BackgroundVerification.CreateCandidateReq ->
  m BackgroundVerification.CreateCandidateResp
createCandidate = runWithServiceConfig BackgroundVerification.createCandidate

createInvitation ::
  ServiceFlow m r =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  BackgroundVerification.CreateInvitationReqI ->
  m BackgroundVerification.CreateInvitationResp
createInvitation = runWithServiceConfig BackgroundVerification.createInvitation

getInvitation ::
  ServiceFlow m r =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Text ->
  m BackgroundVerification.GetInvitationResp
getInvitation = runWithServiceConfig BackgroundVerification.getInvitation

getReport ::
  ServiceFlow m r =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Text ->
  m BackgroundVerification.GetReportResp
getReport = runWithServiceConfig BackgroundVerification.getReport

runWithServiceConfig ::
  ServiceFlow m r =>
  (BackgroundVerification.BackgroundVerificationServiceConfig -> req -> m resp) ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  req ->
  m resp
runWithServiceConfig func _merchantId merchantOpCityId req = do
  orgBackgroundVerificationsConfig <- QOMC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOpCityId.getId)
  orgBackgroundVerificationServiceConfig <-
    CQMSC.findByServiceAndCity (DMSC.BackgroundVerificationService orgBackgroundVerificationsConfig.backgroundVerification) merchantOpCityId
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantOpCityId.getId "BackgroundVerifications" (show $ orgBackgroundVerificationsConfig.backgroundVerification))
  case orgBackgroundVerificationServiceConfig.serviceConfig of
    DMSC.BackgroundVerificationServiceConfig msc -> func msc req
    _ -> throwError $ InternalError "Unknown Service Config"
