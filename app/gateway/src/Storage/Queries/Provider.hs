module Storage.Queries.Provider
  ( listProviders,
  )
where

import App.Types
import qualified Beckn.Types.App as App
import qualified Beckn.Types.Storage.Organization as Org
import Beckn.Utils.Common (defaultLocalTime)
import EulerHS.Prelude

mkProvider :: Text -> Text -> Text -> Text -> Org.Organization
mkProvider providerId name key url =
  Org.Organization
    (App.OrganizationId providerId)
    name
    Nothing
    Nothing
    Nothing
    Org.GATEWAY
    Nothing
    Nothing
    Nothing
    Nothing
    Org.APPROVED
    True
    True
    (Just key)
    (Just url)
    defaultLocalTime
    defaultLocalTime

providers :: [Org.Organization]
providers =
  [ mkProvider "test-provider1" "Test Provider 1" "test-provider-1-key" "http://localhost:8014/v1",
    mkProvider "test-provider2" "Test Provider 2" "test-provider-2-key" "http://localhost:8014/v1"
  ]

-- FIXME: this should allow filtering by domain
listProviders :: AppFlow [Org.Organization]
listProviders = return providers
