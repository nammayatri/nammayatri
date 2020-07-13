module Product.AppLookup
  ( insert,
    lookup,
  )
where

import App.Types
import qualified Data.Cache as C
import qualified EulerHS.Language as L
import EulerHS.Prelude
import System.Clock (TimeSpec (..))

insert :: Text -> Text -> AppFlow ()
insert messageId appUrl = do
  AppEnv {..} <- ask
  L.runIO $ C.insert' cache (Just $ TimeSpec 1800 0) messageId appUrl

lookup :: Text -> AppFlow (Maybe Text)
lookup messageId = do
  AppEnv {..} <- ask
  L.runIO $ C.lookup cache messageId
