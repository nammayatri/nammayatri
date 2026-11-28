module Domain.Action.UI.UnserviceableCityLead (postUnserviceableCityLead) where

import qualified API.Types.UI.UnserviceableCityLead
import Data.Char (isDigit)
import qualified Data.Text as T
import qualified Domain.Types.UnserviceableCityLead as DUCL
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption (encrypt)
import qualified Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.UnserviceableCityLead as QUCL
import Tools.Error

postUnserviceableCityLead ::
  API.Types.UI.UnserviceableCityLead.UnserviceableCityLeadReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postUnserviceableCityLead req = do
  merchant <- CQM.findByShortId req.merchantShortId >>= fromMaybeM (MerchantDoesNotExist req.merchantShortId.getShortId)
  let name = T.strip req.name
      mobileNumber = T.strip req.mobileNumber
  when (T.null name) $ throwError $ InvalidRequest "name cannot be empty"
  unless (T.length mobileNumber >= 6 && T.all isDigit mobileNumber) $ throwError $ InvalidRequest "invalid mobileNumber"
  encMobileNumber <- encrypt mobileNumber
  now <- getCurrentTime
  leadId <- generateGUID
  QUCL.create
    DUCL.UnserviceableCityLead
      { id = Id leadId,
        merchantId = merchant.id,
        deviceToken = req.deviceToken,
        name = name,
        mobileNumber = encMobileNumber,
        mobileCountryCode = fromMaybe "+91" req.mobileCountryCode,
        vehicleCategory = req.vehicleCategory,
        lat = req.lat,
        lon = req.lon,
        createdAt = now,
        updatedAt = now
      }
  pure Kernel.Types.APISuccess.Success
