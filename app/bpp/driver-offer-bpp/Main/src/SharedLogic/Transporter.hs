module SharedLogic.Transporter where

import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant as CQM
import Tools.Error

findMerchantByShortId :: ShortId DM.Merchant -> Flow DM.Merchant
findMerchantByShortId merchantShortId = do
  CQM.findByShortId merchantShortId
    >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
