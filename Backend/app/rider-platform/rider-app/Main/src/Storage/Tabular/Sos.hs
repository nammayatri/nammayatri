{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Sos where

import qualified Domain.Types.Sos as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Storage.Tabular.Person (PersonTId)
import Storage.Tabular.Ride (RideTId)

derivePersistField "Domain.SosType"
derivePersistField "Domain.SosStatus"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    SosT sql=sos
      id Text
      personId PersonTId
      rideId RideTId
      flow Domain.SosType
      status Domain.SosStatus
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey SosT where
  type DomainKey SosT = Id Domain.Sos
  fromKey (SosTKey _id) = Id _id
  toKey (Id id) = SosTKey id

instance FromTType SosT Domain.Sos where
  fromTType SosT {..} = do
    return $
      Domain.Sos
        { id = Id id,
          personId = fromKey personId,
          rideId = fromKey rideId,
          ..
        }

instance ToTType SosT Domain.Sos where
  toTType Domain.Sos {..} =
    SosT
      { id = getId id,
        personId = toKey personId,
        rideId = toKey rideId,
        ..
      }
