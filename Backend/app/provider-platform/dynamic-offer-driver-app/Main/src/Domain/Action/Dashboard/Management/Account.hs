module Domain.Action.Dashboard.Management.Account
  ( getAccountFetchUnverifiedAccounts,
    postAccountVerifyAccount,
    putAccountUpdateRole,
  )
where

import qualified API.Types.ProviderPlatform.Management.Account as Common
import qualified Dashboard.Common
import qualified Domain.Action.Dashboard.Fleet.RegistrationV2 as DRegistrationV2
import qualified Domain.Types.DocsVerificationStatus as DDVS
import qualified Domain.Types.DriverInformation as DI
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
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified SharedLogic.DriverOnboarding.OnboardingFlags.Flow as SFlags
import qualified SharedLogic.DriverOnboarding.OnboardingFlags.Guard as SGuard
import qualified Storage.CachedQueries.Merchant as QMerchant
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.FleetOwnerInformation as QFOI
import Storage.Queries.Person ()
import qualified Storage.Queries.Person as QP
import Storage.Queries.PersonExtra (updatePersonRole)
import Tools.Error (FleetOwnerNotFoundError (FleetOwnerNotFound), MerchantError (MerchantNotFound), TransporterError (TransporterConfigNotFound))

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
postAccountVerifyAccount merchantShortId opCity Common.VerifyAccountReq {..} = do
  let enabled = case status of
        Common.Approved -> True
        _ -> False
  let fleetOwnerId' = Kernel.Types.Id.cast fleetOwnerId
  fleetOwnerInfo <- QFOI.findByPrimaryKey fleetOwnerId' >>= fromMaybeM (FleetOwnerNotFound fleetOwnerId'.getId)
  let wasDisabled = not fleetOwnerInfo.enabled
  fleetOwnerPerson <- QP.findById fleetOwnerId' >>= fromMaybeM (PersonDoesNotExist fleetOwnerId'.getId)
  merchant <- QMerchant.findByShortId merchantShortId >>= fromMaybeM (MerchantNotFound merchantShortId.getShortId)
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  tc <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  if enabled
    then do
      SGuard.withOnboardingAction tc SGuard.None SGuard.Enable (SGuard.TargetFleetOwner fleetOwnerId') $
        SFlags.markDisabledFlags (tc.unifiedOnboardingFlagsRecompute == Just True) fleetOwnerPerson SFlags.AdminEnable
      when wasDisabled $ do
        DRegistrationV2.sendFleetOnboardingSms fleetOwnerId' merchantOpCityId
    else
      SGuard.withOnboardingAction tc SGuard.None SGuard.Disable (SGuard.TargetFleetOwner fleetOwnerId') $
        SFlags.markDisabledFlags (tc.unifiedOnboardingFlagsRecompute == Just True) fleetOwnerPerson (SFlags.AdminDisable DI.AdminDisabled)
  pure Kernel.Types.APISuccess.Success

putAccountUpdateRole ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id Dashboard.Common.Person ->
  Common.DashboardAccessType ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
putAccountUpdateRole merchantShortId opCity personId' accessType = do
  let personId = Kernel.Types.Id.cast personId'
  person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  mbFleetOwnerInfo <- QFOI.findByPrimaryKey personId
  when (accessType == Common.FLEET_OWNER && isNothing mbFleetOwnerInfo) $ do
    merchant <- QMerchant.findByShortId merchantShortId >>= fromMaybeM (MerchantNotFound merchantShortId.getShortId)
    merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
    transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
    let defaultDocsVerificationStatus =
          if transporterConfig.enableManualDocumentStatusCheck == Just True
            then Just DDVS.ADMIN_PENDING
            else Nothing
    DRegistrationV2.createFleetOwnerInfo personId person.merchantId (Just False) person.merchantOperatingCityId ((.rate) <$> transporterConfig.taxConfig.defaultTdsRate) defaultDocsVerificationStatus
  updatePersonRole personId =<< castRole accessType
  pure Kernel.Types.APISuccess.Success
  where
    castRole role = case role of
      Common.FLEET_OWNER -> pure DP.FLEET_OWNER
      Common.DASHBOARD_OPERATOR -> pure DP.OPERATOR
      _ -> throwError . InternalError $ "This role will not be able to set: " <> show role
