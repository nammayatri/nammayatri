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
            emptySubscriber
              { signing_public_key = Just cred.signPubKey,
                valid_from = Just $ (- oneYear) `addUTCTime` now,
                valid_until = Just $ oneYear `addUTCTime` now
              }
      pure [subscriber]
    Nothing -> pure []
  where
    oneYear = 31536000 -- in seconds
    emptySubscriber =
      Subscriber
        { unique_key_id = Nothing,
          subscriber_id = Nothing,
          subscriber_url = Nothing,
          _type = Nothing,
          domain = Nothing,
          city = Nothing,
          country = Nothing,
          signing_public_key = Nothing,
          encr_public_key = Nothing,
          valid_from = Nothing,
          valid_until = Nothing,
          status = Nothing,
          created = Nothing,
          updated = Nothing
        }
