module SharedLogic.DummySignatureAuth (dummySignaturePayload, dummySignatureAuthResult, dummySubscriber) where

import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as BecknCtx
import Kernel.Types.Registry (Subscriber (..), SubscriberStatus (..), SubscriberType (..))
import Kernel.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import qualified Kernel.Utils.SignatureAuth as HttpSig
import Servant.Client.Core (Scheme (Http))

-- TODO: Remove Later will replace with seperate Common Module
dummySignaturePayload :: HttpSig.SignaturePayload
dummySignaturePayload =
  HttpSig.SignaturePayload
    { signature = "",
      params =
        HttpSig.SignatureParams
          { keyId = HttpSig.KeyId {subscriberId = "", uniqueKeyId = "", alg = HttpSig.Ed25519},
            algorithm = HttpSig.Ed25519,
            headers = [],
            created = Nothing,
            expires = Nothing
          }
    }

dummySubscriber :: Subscriber
dummySubscriber =
  Subscriber
    { unique_key_id = "",
      subscriber_id = "",
      subscriber_url = BaseUrl Http "" 0 "",
      _type = BAP,
      domain = BecknCtx.MOBILITY,
      city = [],
      country = Nothing,
      signing_public_key = "",
      encr_public_key = Nothing,
      valid_from = Nothing,
      valid_until = Nothing,
      status = Just SUBSCRIBED,
      created = posixSecondsToUTCTime 0,
      updated = posixSecondsToUTCTime 0
    }

dummySignatureAuthResult :: SignatureAuthResult
dummySignatureAuthResult =
  SignatureAuthResult
    { signature = dummySignaturePayload,
      subscriber = dummySubscriber
    }
