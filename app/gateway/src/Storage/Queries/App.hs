module Storage.Queries.App
  ( lookupKey,
  )
where

import App.Types
import qualified Beckn.Types.App as App
import qualified Beckn.Types.Storage.Organization as Org
import Beckn.Utils.Common (defaultLocalTime)
import EulerHS.Prelude

mkApp :: Text -> Text -> Text -> Text -> Org.Organization
mkApp providerId name key url =
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

apps :: [Org.Organization]
apps =
  [ mkApp "test-app1" "Test App 1" "test-app-1-key" "http://localhost:8016/v1",
    mkApp "test-app2" "Test App 2" "test-app-2-key" "http://localhost:8016/v1"
  ]

-- FIXME: this should take a RegToken
lookupKey :: App.APIKey -> Flow (Maybe Org.Organization)
lookupKey apiKey =
  return $
    find (\o -> Org._apiKey o == Just apiKey) apps
