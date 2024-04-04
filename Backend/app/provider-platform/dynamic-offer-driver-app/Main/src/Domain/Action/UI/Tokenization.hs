{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.Tokenization where

import qualified API.Types.UI.Tokenization
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.Merchant.MerchantServiceConfig as DomainMSC
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Tokenize as Tokenize
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import Tools.Auth
import Tools.Error

getDriverSdkToken ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Int ->
    Tokenize.TokenizationService ->
    Environment.Flow API.Types.UI.Tokenization.GetTokenRes
  )
getDriverSdkToken (_, merchantId, merchantOperatingCityId) expirySec svc = do
  svcfg <- (CQMSC.findByMerchantIdAndServiceWithCity merchantId (DomainMSC.TokenizationService svc) merchantOperatingCityId >>= fromMaybeM (MerchantServiceConfigNotFound merchantId.getId "Tokenization" (show svc))) <&> (.serviceConfig)
  hvsc <- case svcfg of
    DomainMSC.TokenizationServiceConfig sc -> return sc
    _ -> throwError $ ServiceConfigError "Service Config is not Tokenization service config !!!!"
  makeResponse <$> Tokenize.tokenize hvsc (makeTokenizeReq expirySec)
  where
    makeTokenizeReq :: Int -> Tokenize.TokenizationReq
    makeTokenizeReq expiry = Tokenize.TokenizationReq {..}

    makeResponse :: Tokenize.TokenizationResp -> API.Types.UI.Tokenization.GetTokenRes
    makeResponse Tokenize.TokenizationResp {..} = API.Types.UI.Tokenization.GetTokenRes {..}
