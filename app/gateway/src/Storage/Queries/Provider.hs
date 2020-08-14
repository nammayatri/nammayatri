module Storage.Queries.Provider
  ( listProviders,
    lookupKey,
  )
where

import App.Types
import qualified Beckn.Types.App as App
import qualified Beckn.Types.Storage.Organization as Org
import EulerHS.Prelude
import Storage.Queries.Organization

-- FIXME: this should take a RegToken
lookupKey :: Org.OrganizationType -> App.APIKey -> Flow (Maybe Org.Organization)
lookupKey = findOrgByApiKey

listProviders :: Org.OrganizationType -> Org.OrganizationDomain -> Flow [Org.Organization]
listProviders orgType orgDomain = listOrganizations Nothing Nothing [orgType] [orgDomain]
