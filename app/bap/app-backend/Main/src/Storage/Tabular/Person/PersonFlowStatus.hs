{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Person.PersonFlowStatus where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import Beckn.Utils.Text (encodeToText)
import Data.Aeson
import Data.ByteString.Lazy (fromStrict)
import Domain.Types.Person (Person)
import qualified Domain.Types.Person.PersonFlowStatus as Domain
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
    PersonFlowStatusT sql=person_flow_status
      personId PersonTId
      flowStatus Domain.FlowStatus
      updatedAt UTCTime
      Primary personId
      UniquePersonFlowStatusPersonId personId
      deriving Generic
    |]

instance TEntityKey PersonFlowStatusT where
  type DomainKey PersonFlowStatusT = Id Person
  fromKey (PersonFlowStatusTKey _id) = fromKey _id
  toKey id = PersonFlowStatusTKey $ toKey id

instance TType PersonFlowStatusT Domain.PersonFlowStatus where
  fromTType PersonFlowStatusT {..} =
    return $
      Domain.PersonFlowStatus
        { personId = fromKey personId,
          ..
        }
  toTType Domain.PersonFlowStatus {..} =
    PersonFlowStatusT
      { personId = toKey personId,
        ..
      }
