module SharedLogic.Cac where

import qualified Client.Main as CM
import qualified Data.Aeson as DA
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Domain.Types.Merchant.MerchantOperatingCity
import Kernel.Prelude
--import Kernel.Types.App

import Kernel.Types.CacheFlow
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Error.Throwing
import Kernel.Utils.Logging
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as QMerchantOpCity
import Tools.Error

getFrontendConfigs :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Log m) => Id MerchantOperatingCity -> Maybe Int -> m (Maybe DA.Object)
getFrontendConfigs merchantOpCityId mbToss = do
  city <- (QMerchantOpCity.findById merchantOpCityId >>= fromMaybeM (MerchantOperatingCityNotFound merchantOpCityId.getId)) <&> (.city)
  ghcCond <- liftIO $ CM.hashMapToString $ HM.fromList [(T.pack "city", (DA.String . T.pack . show) city)]
  contextValue <- case mbToss of
    Just toss -> liftIO $ CM.evalExperiment "atlas_driver_ui" ghcCond toss
    Nothing -> liftIO $ CM.evalCtx "atlas_driver_ui" ghcCond
  case contextValue of
    Left err -> do
      logError $ "Error in getting frontend configs: " <> show err
      return Nothing
    Right cfgs -> return $ Just cfgs
