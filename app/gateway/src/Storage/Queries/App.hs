module Storage.Queries.App
  ( lookupKey,
  )
where

import qualified Beckn.Types.App as App
import Beckn.Types.Common
import qualified Beckn.Types.Storage.Organization as Org
import EulerHS.Prelude
import Storage.Queries.Organization

-- FIXME: this should take a RegToken
lookupKey :: DBFlow m r => Org.OrganizationType -> App.APIKey -> m (Maybe Org.Organization)
lookupKey = findOrgByApiKey
