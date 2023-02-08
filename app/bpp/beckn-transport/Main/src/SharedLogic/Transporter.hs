module SharedLogic.Transporter where

import qualified Domain.Types.Merchant as DM
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant as CQM
import Tools.Error

findTransporter :: Id DM.Merchant -> Flow DM.Merchant
findTransporter transporterId = do
  transporter <- CQM.findById transporterId >>= fromMaybeM (MerchantDoesNotExist transporterId.getId)
  unless transporter.enabled $ throwError AgencyDisabled
  pure transporter

findMerchantByShortId :: ShortId DM.Merchant -> Flow DM.Merchant
findMerchantByShortId merchantShortId = do
  CQM.findByShortId merchantShortId
    >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
