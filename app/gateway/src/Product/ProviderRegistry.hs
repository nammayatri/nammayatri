module Product.ProviderRegistry
  ( lookup,
  )
where

import Beckn.Types.Common
import Beckn.Types.Registry as Registry
import qualified Beckn.Types.Registry.API as Registry
import qualified Beckn.Types.Registry.Domain as Registry
import qualified Beckn.Utils.Registry as Registry
import EulerHS.Prelude
import Tools.Metrics
import qualified Types.Beckn.Context as B
import qualified Types.Beckn.Domain as B

lookup ::
  ( MonadReader r m,
    MonadFlow m,
    Registry m,
    CoreMetrics m,
    HasField "registryUrl" r BaseUrl
  ) =>
  B.Context ->
  m [Registry.Subscriber]
lookup context = do
  case context.domain of
    B.MOBILITY -> listDomainProviders Registry.MOBILITY
    B.LOCAL_RETAIL -> listDomainProviders Registry.LOCAL_RETAIL
    B.FOOD_AND_BEVERAGE -> listDomainProviders Registry.FOOD_AND_BEVERAGE
    B.HEALTHCARE -> listDomainProviders Registry.HEALTHCARE
    B.METRO -> listDomainProviders Registry.METRO
    B.PARKING -> listDomainProviders Registry.PARKING
    B.LOGISTICS -> listDomainProviders Registry.LOGISTICS
    B.PUBLIC_TRANSPORT -> listDomainProviders Registry.PUBLIC_TRANSPORT
    B.UNKNOWN_DOMAIN _ -> pure []
  where
    listDomainProviders domain =
      Registry.registryFetch
        Registry.emptyLookupRequest{_type = Just Registry.BPP,
                                    domain = Just domain
                                   }
