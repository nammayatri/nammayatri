module Utils.Auth where

import Beckn.Types.Logging (Log)
import qualified Beckn.Utils.Servant.SignatureAuth as HttpSig
import EulerHS.Prelude

type LookupRegistryOrg = (HttpSig.LookupRegistry ())

lookup :: (MonadThrow m, Log m, MonadReader r m, HttpSig.AuthenticatingEntity r) => HttpSig.LookupAction LookupRegistryOrg m
lookup = HttpSig.lookupRegistryAction (const . pure $ Just ())
