module SharedLogic.Transporter where

import Beckn.Prelude
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Domain.Types.Merchant as DM
import Environment
import qualified Storage.CachedQueries.Merchant as CQM
import Tools.Error

findMerchantByShortId :: ShortId DM.Merchant -> Flow DM.Merchant
findMerchantByShortId merchantShortId = do
  CQM.findByShortId merchantShortId
    >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
