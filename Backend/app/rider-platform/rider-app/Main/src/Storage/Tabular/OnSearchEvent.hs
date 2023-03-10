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

module Storage.Tabular.OnSearchEvent where

import qualified Domain.Types.OnSearchEvent as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id

mkPersist
  defaultSqlSettings
  [defaultQQ|
    OnSearchEventT sql=on_search_event
      id Text
      bppId Text
      messageId Text
      errorCode Text Maybe
      errorType Text Maybe
      errorMessage Text Maybe
      createdAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey OnSearchEventT where
  type DomainKey OnSearchEventT = Id Domain.OnSearchEvent
  fromKey (OnSearchEventTKey _id) = Id _id
  toKey (Id id) = OnSearchEventTKey id

instance FromTType OnSearchEventT Domain.OnSearchEvent where
  fromTType OnSearchEventT {..} = do
    return $
      Domain.OnSearchEvent
        { id = Id id,
          ..
        }

instance ToTType OnSearchEventT Domain.OnSearchEvent where
  toTType Domain.OnSearchEvent {..} =
    OnSearchEventT
      { id = getId id,
        ..
      }
