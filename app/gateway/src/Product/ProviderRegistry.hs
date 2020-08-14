{-# LANGUAGE OverloadedLabels #-}

module Product.ProviderRegistry
  ( lookup,
  )
where

import App.Types
import qualified Beckn.Types.Core.Context as B
import qualified Beckn.Types.Storage.Organization as Org
import Beckn.Utils.Common (decodeFromText, throwJsonError400)
import EulerHS.Prelude
import qualified Storage.Queries.Provider as Provider

lookup :: B.Context -> Flow [Org.Organization]
lookup _context = do
  orgDomain <- domainToOrgType (_context ^. #_domain)
  filter (isJust . Org._callbackUrl)
    <$> Provider.listProviders Org.PROVIDER orgDomain
  where
    domainToOrgType :: Text -> Flow Org.OrganizationDomain
    domainToOrgType domain =
      maybe
        (throwJsonError400 "Bad Request" "Domain not supported")
        return
        $ decodeFromText $ "\"" <> domain <> "\""
