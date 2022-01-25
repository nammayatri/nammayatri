module Beckn.Types.Registry
  ( module Beckn.Types.Registry,
    module E,
  )
where

import Beckn.Prelude
import Beckn.Types.Registry.Subscriber as E

class Registry m where
  registryLookup :: SimpleLookupRequest -> m (Maybe Subscriber)

data SimpleLookupRequest = SimpleLookupRequest
  { unique_key_id :: Text,
    subscriber_id :: Text
  }
  deriving (Eq, Ord)

lookupRequestToRedisKey :: SimpleLookupRequest -> Text
lookupRequestToRedisKey SimpleLookupRequest {..} = unique_key_id <> "|" <> subscriber_id
