{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.PersonBlock
  ( isNoBlockUser,
    noBlockTagName,
  )
where

import qualified Domain.Types.Person as DPerson
import Kernel.Prelude
import qualified Lib.Yudhishthira.Tools.Utils as LYTU
import qualified Lib.Yudhishthira.Types as LYT

noBlockTagName :: LYT.TagNameValue
noBlockTagName = LYT.TagNameValue "NO_BLOCK_USER"

-- | True iff `customerNammaTags` contains a tag whose name is "NO_BLOCK_USER".
-- Value and expiry are intentionally ignored (presence-only contract).
-- Tagged users are never written to a blocked state — the storage-level guard
-- in `Storage.Queries.PersonExtra` short-circuits every block write, and the
-- fraud-detection entry points (`Search.fraudCheck`, the auth-fraud fork in
-- `Registration`, `CustomerCancellationRate.nudgeOrBlockCustomer`) skip work
-- for these users.
isNoBlockUser :: DPerson.Person -> Bool
isNoBlockUser person =
  case person.customerNammaTags of
    Nothing -> False
    Just tags -> LYTU.elemTagName noBlockTagName tags
