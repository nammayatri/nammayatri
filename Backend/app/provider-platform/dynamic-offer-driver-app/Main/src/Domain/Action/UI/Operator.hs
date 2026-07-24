module Domain.Action.UI.Operator (postOperatorConsent) where

import Data.Maybe
import qualified Data.Text as T
import qualified Domain.Action.Internal.DriverMode as DDriverMode
import qualified Domain.Action.UI.DriverOnboarding.Referral as DOR
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Domain.Types.Person
import qualified Domain.Types.SubscriptionPurchase as DSP
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import SharedLogic.Analytics as Analytics
import SharedLogic.AnalyticsExtra as AnalyticsExtra
import qualified SharedLogic.DriverFleetOperatorAssociation as SA
import qualified SharedLogic.DriverOnboarding.Status as SStatus
import Storage.Beam.SchedulerJob ()
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantPushNotification as CPN
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.DriverInformation.Internal as QDriverInfoInternal
import qualified Storage.Queries.DriverOperatorAssociation as QDriverOperatorAssociation
import qualified Storage.Queries.DriverRCAssociation as QRCAssociation
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.SubscriptionPurchaseExtra as QSubscriptionPurchaseExtra
import Tools.Error
import qualified Tools.Notifications as TN

postOperatorConsent ::
  ( ( Maybe (Id Person),
      Id Merchant,
      Id MerchantOperatingCity
    ) ->
    Flow APISuccess
  )
postOperatorConsent (mbDriverId, merchantId, merchantOperatingCityId) = do
  driverId <- fromMaybeM DriverNotFoundWithId mbDriverId
  driver <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  driverOperatorAssociation <- QDriverOperatorAssociation.findByDriverId driverId False >>= fromMaybeM (InactiveOperatorDriverAssociationNotFound driverId.getId)
  let mbOnboardingVehicleCategory = driverOperatorAssociation.onboardingVehicleCategory
  operator <- QPerson.findById (Id driverOperatorAssociation.operatorId) >>= fromMaybeM (OperatorNotFound driverOperatorAssociation.operatorId)
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOperatingCityId.getId}) (Just (SCTC.findByMerchantOpCityId merchantOperatingCityId Nothing)) >>= fromMaybeM (TransporterConfigNotFound merchantOperatingCityId.getId)

  SA.endDriverAssociationsIfAllowed merchant merchantOperatingCityId transporterConfig driver
  when (merchant.overwriteAssociation == Just True) $
    QRCAssociation.endAllRCAssociationsForDriver driverId

  DOR.makeDriverReferredByOperator merchantOperatingCityId driverId operator.id

  QDriverOperatorAssociation.updateByPrimaryKey driverOperatorAssociation{isActive = True}
  Analytics.handleDriverAnalyticsAndFlowStatus
    transporterConfig
    driverOperatorAssociation.driverId
    Nothing
    ( \driverInfo -> do
        activeSubCount <- QSubscriptionPurchaseExtra.countActiveSubscriptionsForOwner driverOperatorAssociation.driverId.getId DSP.DRIVER
        AnalyticsExtra.adjustOperatorDriverAssociationAnalytics transporterConfig operator.id.getId 1 activeSubCount driverInfo.enabled
    )
    ( \driverInfo -> do
        DDriverMode.incrementFleetOperatorStatusKeyForDriver OPERATOR driverOperatorAssociation.operatorId driverInfo.driverFlowStatus
    )
  QDriverInfoInternal.updateOnboardingVehicleCategory mbOnboardingVehicleCategory driver.id

  void $ SStatus.runRefreshOnboardingFlagsDriver Nothing (Just transporterConfig) (cast driverId)
  mbMerchantPN <- CPN.findMatchingMerchantPN merchantOperatingCityId "OPERATOR_CONSENT" Nothing Nothing driver.language Nothing
  whenJust mbMerchantPN $ \merchantPN -> do
    let title = T.replace "{#operatorName#}" operator.firstName merchantPN.title
    let body = T.replace "{#operatorName#}" operator.firstName merchantPN.body
    TN.notifyDriver merchantOperatingCityId merchantPN.fcmNotificationType title body driver driver.deviceToken
  pure Success
