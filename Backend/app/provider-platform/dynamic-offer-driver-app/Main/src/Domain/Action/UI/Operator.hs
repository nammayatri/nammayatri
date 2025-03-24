module Domain.Action.UI.Operator (postOperatorConsent) where

import Data.Maybe
import qualified Data.Text as T
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Domain.Types.Person
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Beam.SchedulerJob ()
import qualified Storage.CachedQueries.Merchant.MerchantPushNotification as CPN
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverInformation.Internal as QDriverInfoInternal
import qualified Storage.Queries.DriverOperatorAssociation as QDriverOperatorAssociation
import qualified Storage.Queries.Person as QPerson
import Tools.Error
import qualified Tools.Notifications as TN

postOperatorConsent ::
  ( ( Maybe (Id Person),
      Id Merchant,
      Id MerchantOperatingCity
    ) ->
    Flow APISuccess
  )
postOperatorConsent (mbDriverId, _, merchantOperatingCityId) = do
  driverId <- fromMaybeM (DriverNotFoundWithId) mbDriverId
  driver <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  driverOperatorAssociation <- QDriverOperatorAssociation.findByDriverId driverId False >>= fromMaybeM (InactiveOperatorDriverAssociationNotFound driverId.getId)
  onboardingVehicleCategory <- driverOperatorAssociation.onboardingVehicleCategory & fromMaybeM DriverOnboardingVehicleCategoryNotFound
  operator <- QPerson.findById (Id driverOperatorAssociation.operatorId) >>= fromMaybeM (OperatorNotFound driverOperatorAssociation.operatorId)
  QDriverOperatorAssociation.updateByPrimaryKey driverOperatorAssociation{isActive = True}
  QDriverInfoInternal.updateOnboardingVehicleCategory (Just onboardingVehicleCategory) driver.id
  QDI.updateEnabledVerifiedState driverId True (Just True)
  mbMerchantPN <- CPN.findMatchingMerchantPN merchantOperatingCityId "OPERATOR_CONSENT" Nothing Nothing driver.language Nothing
  whenJust mbMerchantPN $ \merchantPN -> do
    let title = T.replace "{#operatorName#}" operator.firstName merchantPN.title
    let body = T.replace "{#operatorName#}" operator.firstName merchantPN.body
    TN.notifyDriver merchantOperatingCityId merchantPN.fcmNotificationType title body driver driver.deviceToken
  pure Success
