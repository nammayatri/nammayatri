module Storage.Queries.Provider
  ( lookupKey,
  )
where

import App.Types
import qualified Beckn.Types.App as App
import qualified Beckn.Types.Storage.Organization as Org
import EulerHS.Prelude
import Storage.Queries.Organization

lookupKey :: App.APIKey -> Flow (Maybe Org.Organization)
lookupKey = findOrgByApiKey Org.PROVIDER
