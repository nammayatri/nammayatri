{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.RegistrationToken where

import qualified Domain.Types.Person.Type as DP
import Kernel.Prelude
import Kernel.Types.Id

-- | One token per person: identity only. Merchant and city are no longer bound
-- to the token -- authorization keys off the person's merchant_access set
-- against the merchant/city named in each request (URL for direct-dashboard,
-- request body/param for the legacy management endpoints).
data RegistrationToken = RegistrationToken
  { id :: Id RegistrationToken,
    token :: Text,
    personId :: Id DP.Person,
    createdAt :: UTCTime,
    enabled :: Bool
  }
  deriving (Generic, Show)
