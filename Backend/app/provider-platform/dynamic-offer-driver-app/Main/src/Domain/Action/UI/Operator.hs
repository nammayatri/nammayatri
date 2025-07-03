module Domain.Action.UI.Operator (postOperatorConsent) where

import Data.Maybe
import qualified Data.Text as T
import qualified Domain.Action.UI.DriverOnboarding.Referral as DOR
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Domain.Types.Person
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.DriverFleetOperatorAssociation as SA
import Storage.Beam.SchedulerJob ()
import qualified Storage.Cac.TransporterConfig as SCT
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantPushNotification as CPN
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverInformation.Internal as QDriverInfoInternal
import qualified Storage.Queries.DriverOperatorAssociation as QDriverOperatorAssociation
import qualified Storage.Queries.Person as QPerson
import Tools.Error
import qualified Tools.Notifications as TN
import Utils.Common.Cac.KeyNameConstants

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

  SA.endDriverAssociationsIfAllowed merchant merchantOperatingCityId driver

  DOR.makeDriverReferredByOperator merchantOperatingCityId driverId operator.id

  QDriverOperatorAssociation.updateByPrimaryKey driverOperatorAssociation{isActive = True}
  QDriverInfoInternal.updateOnboardingVehicleCategory mbOnboardingVehicleCategory driver.id

  transporterConfig <- SCT.findByMerchantOpCityId merchantOperatingCityId (Just (DriverId (cast driverId))) >>= fromMaybeM (TransporterConfigNotFound merchantOperatingCityId.getId)
  unless (transporterConfig.requiresOnboardingInspection == Just True) $
    QDI.updateEnabledVerifiedState driverId True (Just True)
  mbMerchantPN <- CPN.findMatchingMerchantPN merchantOperatingCityId "OPERATOR_CONSENT" Nothing Nothing driver.language Nothing
  whenJust mbMerchantPN $ \merchantPN -> do
    let title = T.replace "{#operatorName#}" operator.firstName merchantPN.title
    let body = T.replace "{#operatorName#}" operator.firstName merchantPN.body
    TN.notifyDriver merchantOperatingCityId merchantPN.fcmNotificationType title body driver driver.deviceToken
  pure Success
