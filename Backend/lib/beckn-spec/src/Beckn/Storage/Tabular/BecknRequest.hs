{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Beckn.Storage.Tabular.BecknRequest where

import Kernel.Prelude
import Kernel.Storage.Esqueleto
import qualified Kernel.Types.BecknRequest as Domain
import Kernel.Types.Id

mkPersist
  defaultSqlSettings
  [defaultQQ|
    BecknRequestT sql=beckn_request
      id Text
      becknRequest Text
      signatureHeader Text
      timeStamp UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey BecknRequestT where
  type DomainKey BecknRequestT = Id Domain.BecknRequest
  fromKey (BecknRequestTKey _id) = Id _id
  toKey (Id id) = BecknRequestTKey id

instance FromTType BecknRequestT Domain.BecknRequest where
  fromTType BecknRequestT {..} = do
    return $
      Domain.BecknRequest
        { id = Id id,
          ..
        }

instance ToTType BecknRequestT Domain.BecknRequest where
  toTType Domain.BecknRequest {..} =
    BecknRequestT
      { id = getId id,
        ..
      }
