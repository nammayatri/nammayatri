module Domain.Action.UI.DriverDocument (postDriverDocumentRegister, getDriverDocumentGet) where

import qualified API.Types.ProviderPlatform.Management.DriverRegistration as Common
import qualified Domain.Action.Dashboard.Management.DriverRegistration as DR
import qualified Domain.Types.DocumentVerificationConfig as DVC
import qualified Domain.Types.DriverPanCard as DPan
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess
import Kernel.Types.Id (cast)
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.DriverOnboarding as SDO
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Tools.Error

postDriverDocumentRegister ::
  ( Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
  ) ->
  Common.DocumentRegisterReq ->
  Environment.Flow APISuccess
postDriverDocumentRegister (mbPersonId, merchantId, merchantOpCityId) req = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  moc <- CQMOC.findById merchantOpCityId >>= fromMaybeM (MerchantOperatingCityNotFound merchantOpCityId.getId)
  DR.postDriverRegistrationDocumentRegisterWithVerifiedBy DPan.FRONTEND_SDK merchant.shortId moc.city (cast personId) req

getDriverDocumentGet ::
  ( Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
  ) ->
  Text ->
  Maybe DVC.DocumentType ->
  Maybe Common.EntityType ->
  Environment.Flow Common.GetDocumentResponse
getDriverDocumentGet (mbPersonId, merchantId, merchantOpCityId) entityId mbDocType mbEntityType = do
  void $ mbPersonId & fromMaybeM (PersonNotFound "No person found")
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  moc <- CQMOC.findById merchantOpCityId >>= fromMaybeM (MerchantOperatingCityNotFound merchantOpCityId.getId)
  DR.getDriverRegistrationGetDocument merchant.shortId moc.city entityId (SDO.castDocumentType <$> mbDocType) mbEntityType
