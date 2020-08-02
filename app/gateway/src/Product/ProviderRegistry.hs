module Product.ProviderRegistry
  ( lookup,
  )
where

import App.Types
import qualified Beckn.Types.Core.Context as B
import qualified Beckn.Types.Storage.Organization as Org
import EulerHS.Prelude
import qualified Storage.Queries.Provider as Provider

-- TODO: Filter by domain
lookup :: B.Context -> Flow [Org.Organization]
lookup _context =
  filter (isJust . Org._callbackUrl) <$> Provider.listProviders
