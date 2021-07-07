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
  let orgDomain = domainToOrgType (context.domain)
  filter (isJust . Org.callbackUrl)
    <$> Provider.listProviders Org.PROVIDER orgDomain
  where
    domainToOrgType :: B.Domain -> Org.OrganizationDomain
    domainToOrgType domain = case domain of
      B.MOBILITY -> Org.MOBILITY
      B.FINAL_MILE_DELIVERY -> Org.FINAL_MILE_DELIVERY
      B.LOCAL_RETAIL -> Org.LOCAL_RETAIL
      B.FOOD_AND_BEVERAGE -> Org.FOOD_AND_BEVERAGE
      B.HEALTHCARE -> Org.HEALTHCARE
