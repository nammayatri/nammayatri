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

module Storage.Tabular.Exophone where

import qualified Domain.Types.Exophone as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import qualified Storage.Tabular.Merchant as SM

mkPersist
  defaultSqlSettings
  [defaultQQ|
    ExophoneT sql=exophone
      id Text
      merchantId SM.MerchantTId
      primaryPhone Text
      backupPhone Text
      isPrimaryDown Bool
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey ExophoneT where
  type DomainKey ExophoneT = Id Domain.Exophone
  fromKey (ExophoneTKey _id) = Id _id
  toKey (Id id) = ExophoneTKey id

instance FromTType ExophoneT Domain.Exophone where
  fromTType ExophoneT {..} = do
    return $
      Domain.Exophone
        { id = Id id,
          merchantId = fromKey merchantId,
          ..
        }

instance ToTType ExophoneT Domain.Exophone where
  toTType Domain.Exophone {..} =
    ExophoneT
      { id = getId id,
        merchantId = toKey merchantId,
        ..
      }
