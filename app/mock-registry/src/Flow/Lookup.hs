module Flow.Lookup where

import App.Types (FlowHandler)
import Beckn.Types.Common (getCurrentTime)
import Beckn.Types.Error (GenericError (InvalidRequest))
import Beckn.Types.Registry.API (LookupRequest, LookupResponse)
import Beckn.Types.Registry.Subscriber (Subscriber (..))
import Beckn.Utils.Error (fromMaybeM, withFlowHandlerAPI)
import Beckn.Utils.Registry (lookupCredByShortId)
import Data.Time (addUTCTime)
import EulerHS.Prelude

lookup :: LookupRequest -> FlowHandler LookupResponse
lookup req = withFlowHandlerAPI $ do
  env <- ask
  let creds = env.credRegistry
  shortId <- req.subscriber_id & fromMaybeM (InvalidRequest "subscriber_id is not specified.")
  cred <- lookupCredByShortId shortId creds & fromMaybeM (InvalidRequest "Couldn't find any data on recieved subscriber_id.")
  now <- getCurrentTime
  let subscriber =
        emptySubscriber
          { signing_public_key = Just cred.signPubKey,
            valid_from = Just $ (- oneYear) `addUTCTime` now,
            valid_until = Just $ oneYear `addUTCTime` now
          }
  pure [subscriber]
  where
    emptySubscriber = Subscriber Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    oneYear = 31536000 -- in seconds
