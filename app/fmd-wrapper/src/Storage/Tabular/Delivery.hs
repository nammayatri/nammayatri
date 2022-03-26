{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Delivery where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Delivery as Domain
import qualified ExternalAPI.Dunzo.Types as Dz
import GHC.Float
import Storage.Tabular.Address (AddressTId)
import Storage.Tabular.Organization (OrganizationTId)
import Storage.Tabular.Person (PersonTId)

derivePersistField "Dz.TaskState"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    DeliveryT sql=delivery
      id Text
      orderId Text
      deliveryServiceOrderId Text
      bapId OrganizationTId
      status Dz.TaskState
      senderId PersonTId
      receiverId PersonTId
      pickupAddressId AddressTId
      dropAddressId AddressTId
      categoryId Int
      deliveryPrice Double Maybe
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey DeliveryT where
  type DomainKey DeliveryT = Id Domain.DeliveryEntity
  fromKey (DeliveryTKey _id) = Id _id
  toKey id = DeliveryTKey id.getId

instance TEntity DeliveryT Domain.DeliveryEntity where
  fromTEntity entity = do
    let DeliveryT {..} = entityVal entity
    return $
      Domain.DeliveryEntity
        { id = Id id,
          senderId = fromKey senderId,
          receiverId = fromKey receiverId,
          pickupAddressId = fromKey pickupAddressId,
          dropAddressId = fromKey dropAddressId,
          deliveryPrice = double2Float <$> deliveryPrice,
          bapId = fromKey bapId,
          ..
        }
  toTType Domain.DeliveryEntity {..} =
    DeliveryT
      { id = id.getId,
        senderId = toKey senderId,
        receiverId = toKey receiverId,
        pickupAddressId = toKey pickupAddressId,
        dropAddressId = toKey dropAddressId,
        deliveryPrice = float2Double <$> deliveryPrice,
        bapId = toKey bapId,
        ..
      }
  toTEntity a =
    Entity (toKey a.id) $ toTType a
