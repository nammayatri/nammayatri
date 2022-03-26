{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Address where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Address as Domain

mkPersist
  defaultSqlSettings
  [defaultQQ|
    AddressT sql=address
      id Text
      lat Double
      lon Double
      country Text
      state Text
      city Text
      street Text
      building Text Maybe
      door Text
      name Text Maybe
      pincode Text
      instructions Text Maybe
      Primary id
      deriving Generic
    |]

instance TEntityKey AddressT where
  type DomainKey AddressT = Id Domain.Address
  fromKey (AddressTKey _id) = Id _id
  toKey id = AddressTKey id.getId

instance TEntity AddressT Domain.Address where
  fromTEntity entity = do
    let AddressT {..} = entityVal entity
    return $
      Domain.Address
        { id = Id id,
          ..
        }
  toTType Domain.Address {..} =
    AddressT
      { id = id.getId,
        ..
      }
  toTEntity a =
    Entity (toKey a.id) $ toTType a
