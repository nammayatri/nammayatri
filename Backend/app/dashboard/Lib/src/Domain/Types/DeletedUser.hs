{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.DeletedUser where

import qualified Domain.Types.Person as DP
import qualified Domain.Types.Role as DRole
import Kernel.Prelude
import Kernel.Types.Id

-- Tombstone snapshot of a dashboard user captured at deletion time. Ids are held
-- as plain values (no live FK): the person row is gone, so personId/deletedBy/roleId
-- are historical references, resolvable but not enforced. email is the encrypted
-- ciphertext copied from person (decryptable with the same key).
data DeletedUser = DeletedUser
  { id :: Id DeletedUser,
    personId :: Id DP.Person,
    firstName :: Text,
    lastName :: Text,
    roleId :: Id DRole.Role,
    emailEncrypted :: Maybe Text,
    deletedBy :: Id DP.Person,
    deletedAt :: UTCTime
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)
