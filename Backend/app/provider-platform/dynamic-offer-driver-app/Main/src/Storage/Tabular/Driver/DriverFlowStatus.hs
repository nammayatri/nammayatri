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

module Storage.Tabular.Driver.DriverFlowStatus where

import Data.Aeson
import Data.ByteString.Lazy (fromStrict)
import qualified Domain.Types.Driver.DriverFlowStatus as Domain
import Domain.Types.Person (Person)
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Kernel.Utils.Text (encodeToText)
import Storage.Tabular.Person (PersonTId)

instance PersistField Domain.FlowStatus where
  toPersistValue = PersistText . encodeToText
  fromPersistValue (PersistByteString v) = case decode $ fromStrict v of
    Just res -> Right res
    Nothing -> Left "Unable to parse FlowStatus."
  fromPersistValue _ = Left "Invalid PersistValue type on FlowStatus parse."

instance PersistFieldSql Domain.FlowStatus where
  sqlType _ = SqlString

mkPersist
  defaultSqlSettings
  [defaultQQ|
    DriverFlowStatusT sql=driver_flow_status
      personId PersonTId
      flowStatus Domain.FlowStatus
      updatedAt UTCTime
      Primary personId
      UniqueDriverFlowStatusPersonId personId
      deriving Generic
    |]

instance TEntityKey DriverFlowStatusT where
  type DomainKey DriverFlowStatusT = Id Person
  fromKey (DriverFlowStatusTKey _id) = fromKey _id
  toKey id = DriverFlowStatusTKey $ toKey id

instance FromTType DriverFlowStatusT Domain.DriverFlowStatus where
  fromTType DriverFlowStatusT {..} =
    return $
      Domain.DriverFlowStatus
        { personId = fromKey personId,
          ..
        }

instance ToTType DriverFlowStatusT Domain.DriverFlowStatus where
  toTType Domain.DriverFlowStatus {..} =
    DriverFlowStatusT
      { personId = toKey personId,
        ..
      }
