{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.EntityAccess where

import qualified Domain.Types.Entity as DEntity
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.Person as DPerson
import Kernel.Prelude
import Kernel.Types.Id

-- Person-to-entity grants, one row per (person, entity). Mirrors MerchantAccess; merchantId is
-- denormalised off the entity so a grant can be tenancy-checked without joining entity.
data EntityAccess = EntityAccess
  { id :: Id EntityAccess,
    personId :: Id DPerson.Person,
    entityId :: Id DEntity.Entity,
    merchantId :: Id DMerchant.Merchant,
    createdAt :: UTCTime
  }
  deriving (Generic, Show)
