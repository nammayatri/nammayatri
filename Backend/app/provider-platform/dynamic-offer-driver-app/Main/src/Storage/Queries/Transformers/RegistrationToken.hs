{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Transformers.RegistrationToken where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC

getMerchantOperatingCityId :: KvDbFlow m r => (Maybe Kernel.Prelude.Text -> Text -> m (Kernel.Prelude.Text))
getMerchantOperatingCityId merchantOperatingCityId merchantId = do
  case merchantOperatingCityId of
    Just opCityId -> return opCityId
    Nothing -> do
      merchant <- CQM.findById (Id merchantId) >>= fromMaybeM (MerchantNotFound merchantId)
      mOpCId <- CQMOC.getMerchantOpCityId Nothing merchant Nothing
      return $ mOpCId.getId
