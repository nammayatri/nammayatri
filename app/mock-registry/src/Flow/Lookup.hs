module Flow.Lookup where

import App.Types (FlowHandler)
import Beckn.Prelude
import Beckn.Types.Common (getCurrentTime)
import Beckn.Types.Registry.API (LookupRequest, LookupResponse)
import Beckn.Types.Registry.Subscriber (Subscriber (..))
import Beckn.Utils.Error (withFlowHandlerAPI)
import Beckn.Utils.Registry (lookupDomain, lookupKey, lookupShortOrgId, lookupType)
import Data.Time (addUTCTime)

lookup :: LookupRequest -> FlowHandler LookupResponse
lookup req = withFlowHandlerAPI $ do
  creds <- asks (.config.credRegistry)
  let filteredCreds =
        creds
          & maybeFilter lookupDomain req.domain
          & maybeFilter lookupKey req.unique_key_id
          & maybeFilter lookupType req._type
          & maybeFilter lookupShortOrgId req.subscriber_id
  buildSubscriber `traverse` filteredCreds
  where
    oneYear = 31536000 -- in seconds
    maybeFilter filt mbArg list = maybe list (`filt` list) mbArg
    buildSubscriber cred = do
      now <- getCurrentTime
      return $
        Subscriber
          { unique_key_id = cred.uniqueKeyId,
            subscriber_id = cred.shortOrgId,
            subscriber_url = cred.url,
            _type = cred._type,
            domain = cred.domain,
            city = Nothing,
            country = Nothing,
            signing_public_key = cred.signPubKey,
            encr_public_key = Nothing,
            valid_from = Just $ (- oneYear) `addUTCTime` now,
            valid_until = Just $ oneYear `addUTCTime` now,
            status = Nothing,
            created = Nothing,
            updated = Nothing
          }
