module Beckn.Product.Auth.SignatureAuth (verify, sign) where

import Beckn.Types.Common
import Beckn.Utils.Common
import Beckn.Utils.Servant.SignatureAuth
import qualified Beckn.Utils.SignatureAuth as HttpSign
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BSL
import Data.Time
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified EulerHS.Language as L
import EulerHS.Prelude

verify :: ToJSON body => LookupMethod lookup => LookupAction lookup r -> HttpSign.SignaturePayload -> body -> FlowR r (LookupResult lookup)
verify (LookupAction runLookup) signPayload body = do
  let headers =
        [ ("(created)", maybe "" show (signPayload ^. #params . #created)),
          ("(expires)", maybe "" show (signPayload ^. #params . #expires)),
          ("digest", "")
        ]
  (result, key) <- runLookup signPayload
  unless (HttpSign.verify key (signPayload ^. #params) (BSL.toStrict . J.encode $ body) headers (signPayload ^. #signature)) $ throwBecknError401 "RESTRICTED"
  pure result

sign :: ToJSON body => body -> HttpSign.PrivateKey -> HttpSign.KeyId -> NominalDiffTime -> FlowR r HttpSign.SignaturePayload
sign body privateKey keyId validity = do
  now <- L.runIO getPOSIXTime
  let created = now
  let expires = now + validity
  let headers =
        [ ("(created)", ""),
          ("(expires)", ""),
          ("digest", "")
        ]
  let params =
        HttpSign.SignatureParams
          keyId
          HttpSign.Hs2019
          ["digest"]
          (Just created)
          (Just expires)
  signature <- HttpSign.sign privateKey params (BSL.toStrict . J.encode $ body) headers & fromMaybeM500 "CANT_SIGN_REQUEST"
  pure $ HttpSign.SignaturePayload signature params
