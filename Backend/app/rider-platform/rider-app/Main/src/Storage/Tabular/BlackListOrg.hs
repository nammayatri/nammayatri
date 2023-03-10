{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.BlackListOrg where

import qualified Domain.Types.BlackListOrg as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id

derivePersistField "Domain.BlackListOrgType"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    BlackListOrgT sql=black_list_org
      id Text
      shortId Text
      orgType Domain.BlackListOrgType sql=type
      Primary id
      Unique OrganizationShortId
      deriving Generic
    |]

instance TEntityKey BlackListOrgT where
  type DomainKey BlackListOrgT = Id Domain.BlackListOrg
  fromKey (BlackListOrgTKey _id) = Id _id
  toKey (Id id) = BlackListOrgTKey id

instance FromTType BlackListOrgT Domain.BlackListOrg where
  fromTType BlackListOrgT {..} = do
    return $
      Domain.BlackListOrg
        { id = Id id,
          shortId = ShortId shortId,
          _type = orgType,
          ..
        }

instance ToTType BlackListOrgT Domain.BlackListOrg where
  toTType Domain.BlackListOrg {..} =
    BlackListOrgT
      { id = getId id,
        shortId = getShortId shortId,
        orgType = _type,
        ..
      }
