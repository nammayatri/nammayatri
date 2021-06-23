module Product.ProviderRegistry
  ( lookup,
  )
where

import Beckn.Types.Common
import qualified Beckn.Types.Storage.Organization as Org
import EulerHS.Prelude
import qualified Storage.Queries.Provider as Provider
import qualified Types.Beckn.Context as B
import qualified Types.Beckn.Domain as B

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
      B.FOOD_AND_BEVERAGE -> Org.FOOD_AND_BEVERAGE
      B.HEALTHCARE -> Org.HEALTHCARE
