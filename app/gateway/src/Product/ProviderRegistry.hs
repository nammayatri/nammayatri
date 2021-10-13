module Product.ProviderRegistry
  ( lookup,
  )
where

import Beckn.Types.Common
import EulerHS.Prelude
import qualified Storage.Queries.Provider as Provider
import qualified Types.Beckn.Context as B
import qualified Types.Beckn.Domain as B
import qualified Types.Storage.Organization as Org

lookup :: DBFlow m r => B.Context -> m [Org.Organization]
lookup context = do
  providers <- case context.domain of
    B.MOBILITY -> listDomainProviders Org.MOBILITY
    B.FINAL_MILE_DELIVERY -> listDomainProviders Org.FINAL_MILE_DELIVERY
    B.LOCAL_RETAIL -> listDomainProviders Org.LOCAL_RETAIL
    B.FOOD_AND_BEVERAGE -> listDomainProviders Org.FOOD_AND_BEVERAGE
    B.HEALTHCARE -> listDomainProviders Org.HEALTHCARE
    B.METRO -> listDomainProviders Org.METRO
    B.UNKNOWN_DOMAIN _ -> pure []
  pure $ filter (isJust . Org.callbackUrl) providers
  where
    listDomainProviders = Provider.listProviders Org.PROVIDER
