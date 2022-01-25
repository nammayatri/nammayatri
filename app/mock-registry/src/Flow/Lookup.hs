module Flow.Lookup where

import App.Types (FlowHandler)
import Beckn.Prelude
import Beckn.Types.Common (getCurrentTime)
import Beckn.Types.Error
import Beckn.Types.Registry.API (LookupRequest, LookupResponse)
import Beckn.Types.Registry.Subscriber (Subscriber (..))
import Beckn.Utils.Error
import Data.Time (addUTCTime)

lookup :: LookupRequest -> FlowHandler LookupResponse
lookup req = withFlowHandlerAPI $ do
  creds <- asks (.config.credRegistry)
  uniqueKeyId <-
    req.unique_key_id
      & fromMaybeM (InvalidRequest "Lookup is supported only by unique_key_id")
  let mCred = find (\cred -> cred.uniqueKeyId == uniqueKeyId) creds
  case mCred of
    Just cred -> do
      now <- getCurrentTime
      let subscriber =
            Subscriber
              { unique_key_id = uniqueKeyId,
                subscriber_id = cred.shortOrgId,
                subscriber_url = cred.url,
                _type = Nothing,
                domain = Nothing,
                city = Nothing,
                country = Nothing,
                signing_public_key = cred.signPubKey,
                encr_public_key = Nothing,
                valid_from = Just $ (-oneYear) `addUTCTime` now,
                valid_until = Just $ oneYear `addUTCTime` now,
                status = Nothing,
                created = Nothing,
                updated = Nothing
              }
      pure [subscriber]
    Nothing -> pure []
  where
    oneYear = 31536000 -- in seconds
