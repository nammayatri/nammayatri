{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Driver.DriverFlowStatus where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import Beckn.Utils.Text (encodeToText)
import Data.Aeson
import Data.ByteString.Lazy (fromStrict)
import qualified Domain.Types.Driver.DriverFlowStatus as Domain
import Domain.Types.Person (Person)
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

instance TType DriverFlowStatusT Domain.DriverFlowStatus where
  fromTType DriverFlowStatusT {..} =
    return $
      Domain.DriverFlowStatus
        { personId = fromKey personId,
          ..
        }
  toTType Domain.DriverFlowStatus {..} =
    DriverFlowStatusT
      { personId = toKey personId,
        ..
      }
