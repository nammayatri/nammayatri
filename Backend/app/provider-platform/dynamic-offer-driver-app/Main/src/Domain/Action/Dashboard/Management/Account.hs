module Domain.Action.Dashboard.Management.Account
  ( getAccountFetchUnverifiedAccounts,
    postAccountVerifyAccount,
    putAccountUpdateRole,
  )
where

import qualified API.Types.ProviderPlatform.Management.Account as Common
import qualified Dashboard.Common
import qualified Domain.Action.Dashboard.Fleet.RegistrationV2 as DRegistrationV2
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person as DP
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error (GenericError (InternalError), PersonError (PersonDoesNotExist))
import qualified Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM, throwError)
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.Queries.FleetOwnerInformation as QFOI
import Storage.Queries.Person ()
import qualified Storage.Queries.Person as QP
import Storage.Queries.PersonExtra (updatePersonRole)
import Tools.Error (TransporterError (TransporterConfigNotFound))

-- This function will not be called.
getAccountFetchUnverifiedAccounts ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Maybe Kernel.Prelude.UTCTime ->
  Kernel.Prelude.Maybe Kernel.Prelude.UTCTime ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Kernel.Prelude.Maybe Common.FleetOwnerStatus ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Environment.Flow Common.UnverifiedAccountsResp
getAccountFetchUnverifiedAccounts _merchantShortId _opCity _mbFromDate _mbToDate _mbMobileNumber _mbStatus _mbLimit _mbOffset = throwError . InternalError $ "This function should not be called"

postAccountVerifyAccount ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.VerifyAccountReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postAccountVerifyAccount _merchantShortId _opCity Common.VerifyAccountReq {..} = do
  let enabled = case status of
        Common.Approved -> True
        _ -> False
  let fleetOwnerId' = Kernel.Types.Id.cast fleetOwnerId
  mbFleetOwnerInfo <- QFOI.findByPrimaryKey fleetOwnerId'
  let wasDisabled = maybe True (not . (.enabled)) mbFleetOwnerInfo
  QFOI.updateFleetOwnerEnabledStatus enabled fleetOwnerId'
  when (enabled && wasDisabled) $ do
    person <- QP.findById fleetOwnerId' >>= fromMaybeM (PersonDoesNotExist fleetOwnerId'.getId)
    DRegistrationV2.sendFleetOnboardingSms fleetOwnerId' person.merchantOperatingCityId
  pure Kernel.Types.APISuccess.Success

putAccountUpdateRole ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id Dashboard.Common.Person ->
  Common.DashboardAccessType ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
putAccountUpdateRole _merchantShortId _opCity personId' accessType = do
  let personId = Kernel.Types.Id.cast personId'
  person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  mbFleetOwnerInfo <- QFOI.findByPrimaryKey personId
  when (accessType == Common.FLEET_OWNER && isNothing mbFleetOwnerInfo) $ do
    transporterConfig <- SCTC.findByMerchantOpCityId person.merchantOperatingCityId Nothing >>= fromMaybeM (TransporterConfigNotFound person.merchantOperatingCityId.getId)
    DRegistrationV2.createFleetOwnerInfo personId person.merchantId (Just False) (Just person.merchantOperatingCityId) transporterConfig.taxConfig.defaultTdsRate
  updatePersonRole personId =<< castRole accessType
  pure Kernel.Types.APISuccess.Success
  where
    castRole role = case role of
      Common.FLEET_OWNER -> pure DP.FLEET_OWNER
      Common.DASHBOARD_OPERATOR -> pure DP.OPERATOR
      _ -> throwError . InternalError $ "This role will not be able to set: " <> show role
