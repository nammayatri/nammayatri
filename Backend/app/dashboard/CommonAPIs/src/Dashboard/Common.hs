{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Dashboard.Common where

import Kernel.Prelude

data Customer

data Driver

data Image

data Ride

data Message

data File

data Receiver

data Booking

data IssueReport

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
