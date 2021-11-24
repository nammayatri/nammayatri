module Flow.Lookup where

import App.Types (FlowHandler)
import Beckn.Types.Common (getCurrentTime)
import Beckn.Types.Error (GenericError (InvalidRequest))
import Beckn.Types.Registry.API (LookupRequest, LookupResponse)
import Beckn.Types.Registry.Subscriber (Subscriber (..))
import Beckn.Utils.Error (fromMaybeM, withFlowHandlerAPI)
import Beckn.Utils.Registry (lookupKey)
import Data.Time (addUTCTime)
import EulerHS.Prelude

lookup :: LookupRequest -> FlowHandler LookupResponse
lookup req = withFlowHandlerAPI $ do
  env <- ask
  let creds = env.credRegistry
  uniqueKeyId <- req.unique_key_id & fromMaybeM (InvalidRequest "Unique_key_id is not specified.")
  let mCred = lookupKey uniqueKeyId creds
  case mCred of
    Just cred -> do
      now <- getCurrentTime
      let subscriber =
            Subscriber
              { unique_key_id = uniqueKeyId,
                subscriber_id = cred.shortOrgId,
                subscriber_url = Nothing,
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
