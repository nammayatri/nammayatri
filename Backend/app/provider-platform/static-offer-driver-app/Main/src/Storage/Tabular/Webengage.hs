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

module Storage.Tabular.Webengage where

import qualified Domain.Types.Webengage as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id

mkPersist
  defaultSqlSettings
  [defaultQQ|
    WebengageT sql=webengage
        id Text
        version Text
        contentTemplateId Text
        principalEntityId Text
        infoMessageId Text
        webMessageId Text
        toNumber Text
        status Text Maybe
        Primary id
        deriving Generic
    |]

instance TEntityKey WebengageT where
  type DomainKey WebengageT = Id Domain.Webengage
  fromKey (WebengageTKey _id) = Id _id
  toKey (Id id) = WebengageTKey id

instance FromTType WebengageT Domain.Webengage where
  fromTType WebengageT {..} = do
    return $
      Domain.Webengage
        { id = Id id,
          ..
        }

instance ToTType WebengageT Domain.Webengage where
  toTType Domain.Webengage {..} =
    WebengageT
      { id = getId id,
        ..
      }
