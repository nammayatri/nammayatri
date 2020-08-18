{-# LANGUAGE OverloadedLabels #-}

module Product.ProviderRegistry
  ( lookup,
  )
where

import App.Types
import qualified Beckn.Types.Core.Context as B
import qualified Beckn.Types.Core.Domain as B
import qualified Beckn.Types.Storage.Organization as Org
import EulerHS.Prelude
import qualified Storage.Queries.Provider as Provider

lookup :: B.Context -> Flow [Org.Organization]
lookup _context = do
  let orgDomain = domainToOrgType (_context ^. #_domain)
  filter (isJust . Org._callbackUrl)
    <$> Provider.listProviders Org.PROVIDER orgDomain
  where
    domainToOrgType :: B.Domain -> Org.OrganizationDomain
    domainToOrgType domain = case domain of
      B.MOBILITY -> Org.MOBILITY
      B.FINAL_MILE_DELIVERY -> Org.FINAL_MILE_DELIVERY
      B.FOOD_AND_BEVERAGE -> Org.FOOD_AND_BEVERAGE
      B.HEALTHCARE -> Org.HEALTHCARE
