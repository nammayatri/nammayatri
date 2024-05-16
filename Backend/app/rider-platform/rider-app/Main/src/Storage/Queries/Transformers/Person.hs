{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Transformers.Person where

import Data.Text
import Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Storage.CachedQueries.Merchant as CQM

backfillCityAndMOCId :: KvDbFlow m r => Kernel.Prelude.Maybe Kernel.Types.Beckn.Context.City -> Maybe Text -> Text -> m (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity, Kernel.Types.Beckn.Context.City)
backfillCityAndMOCId mbCity mbMerchantOperatingCityId merchantId =
  case mbMerchantOperatingCityId of
    Just mocId -> do
      city <- backfillCity mbCity merchantId
      pure (Kernel.Types.Id.Id mocId, city)
    Nothing -> do
      moc <- CQM.getDefaultMerchantOperatingCity (Kernel.Types.Id.Id merchantId)
      pure (moc.id, moc.city)

backfillCity :: KvDbFlow m r => Kernel.Prelude.Maybe Kernel.Types.Beckn.Context.City -> Text -> m Kernel.Types.Beckn.Context.City
backfillCity city merchantId = case city of
  Just city' -> pure city'
  Nothing -> CQM.findById (Kernel.Types.Id.Id merchantId) >>= fmap (.defaultCity) <$> fromMaybeM (MerchantNotFound merchantId)
