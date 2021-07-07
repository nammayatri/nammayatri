module Storage.Queries.Provider
  ( listProviders,
    lookupKey,
  )
where

import qualified Beckn.Types.App as App
import Beckn.Types.Common
import EulerHS.Prelude
import Storage.Queries.Organization
import qualified Types.Storage.Organization as Org

-- FIXME: this should take a RegToken
lookupKey :: DBFlow m r => Org.OrganizationType -> App.APIKey -> m (Maybe Org.Organization)
lookupKey = findOrgByApiKey

listProviders :: DBFlow m r => Org.OrganizationType -> Org.OrganizationDomain -> m [Org.Organization]
listProviders orgType orgDomain = listOrganizations Nothing Nothing [orgType] [orgDomain]
