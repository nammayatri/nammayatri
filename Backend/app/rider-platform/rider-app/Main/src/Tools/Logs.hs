module Tools.Logs where

import Data.Text
import qualified Domain.Types.Extra.MerchantServiceConfig as DMSC
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.External.ConversionEvent.Interface as Conversion
import Kernel.External.ConversionEvent.Interface.Types as ConversionType
import Kernel.External.ConversionEvent.Meta.Types as MetaType
import Kernel.External.ConversionEvent.Types as Type2
import qualified Kernel.External.ConversionEvent.Types as ConversionType
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EncFlow, EsqDBFlow, MonadFlow, fromMaybeM, logDebug, throwError)
import Servant.Client.Core.BaseUrl
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import Tools.Error
import Prelude

logEvent :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r) => ConversionType.ConversionEventService -> Id Merchant.Merchant -> Id DMOC.MerchantOperatingCity -> Text -> m ()
logEvent service merchantId merchantOpCityId eventName = do
  metaConfig <- CQMSC.findByMerchantOpCityIdAndService merchantId merchantOpCityId (DMSC.ConversionEventService service) >>= fromMaybeM (InternalError $ "No Log Config for the merchant, merchantId:" <> merchantId.getId)

  let metaConfig1 =
        ConversionType.MetaConfig $
          MetaType.MetaConfig
            { MetaType.url = BaseUrl Https "graph.facebook.com" 443 "/",
              MetaType.apiVersion = "v23.0",
              MetaType.pixelId = "923945098916169",
              MetaType.accessToken = "EAANIUrtpsUkBO78kZCwUoORwtFzjZATPZCaGNCYRoQ2Qt0QcdLKi6CP9Td3kWoqMjKf9inLaJ5RoplzGTC3ooYZBJJoWbDjghtpmO4OsU61yOsV5Ogy0n4dAWMTMD3fIjScBwA0FUZBVqic2GcCfANkQGiiwVcQYTqwbAPQzpfcCoIAGczD5lx3UgPahLgNZAmfkFt9LL4UpTg1iuJKZBhGEwUKniBNJyLokOm5zwZDZD"
            }
  let conversionReq =
        Type2.ConversionReqType
          { Type2.eventName = eventName
          }

  case metaConfig.serviceConfig of
    DMSC.ConversionEventServiceConfig cfg -> Conversion.conversionEvent cfg conversionReq
    _ -> throwError $ InternalError "Unknown Service Config"

  logDebug $ "run logevent to kernel"
  _ <- Conversion.conversionEvent metaConfig1 conversionReq
  return ()
