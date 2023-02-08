{-# LANGUAGE DerivingStrategies #-}

module Dashboard.Common where

import Kernel.Prelude

data Driver

data Image

data Ride

-- | Hide secrets before storing request (or response) to DB.
--
-- By default considered that request type has no secrets.
-- So you need to provide manual type instance to hide secrets, if there are.
class ToJSON (ReqWithoutSecrets req) => HideSecrets req where
  type ReqWithoutSecrets req
  hideSecrets :: req -> ReqWithoutSecrets req
  type ReqWithoutSecrets req = req

-- FIXME next default implementation is not working
-- default hideSecrets :: req -> req
-- hideSecrets = identity

instance HideSecrets () where
  hideSecrets = identity

data Summary = Summary
  { totalCount :: Int, --TODO add db indexes
    count :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
