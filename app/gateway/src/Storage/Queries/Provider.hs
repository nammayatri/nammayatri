module Storage.Queries.Provider
  ( listProviders,
  )
where

import Beckn.Types.Common
import EulerHS.Prelude
import Storage.Queries.Organization
import qualified Types.Storage.Organization as Org

listProviders :: DBFlow m r => Org.OrganizationType -> Org.OrganizationDomain -> m [Org.Organization]
listProviders orgType orgDomain = listOrganizations Nothing Nothing [orgType] [orgDomain]
