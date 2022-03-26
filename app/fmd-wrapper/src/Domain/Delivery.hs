module Domain.Delivery where

import Beckn.Prelude
import Beckn.Types.Id
import Domain.Address
import Domain.Organization
import Domain.Person
import qualified ExternalAPI.Dunzo.Types as Dz

data DeliveryEntity = DeliveryEntity
  { id :: Id DeliveryEntity,
    orderId :: Text, -- Id Order
    deliveryServiceOrderId :: Text, -- TaskId in case of Dunzo
    bapId :: Id Organization,
    status :: Dz.TaskState,
    senderId :: Id Person,
    receiverId :: Id Person,
    pickupAddressId :: Id Address,
    dropAddressId :: Id Address,
    categoryId :: Int,
    deliveryPrice :: Maybe Float,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic)

data Delivery = Delivery
  { id :: Id Delivery,
    orderId :: Text, -- Id Order
    deliveryServiceOrderId :: Text, -- TaskId in case of Dunzo
    bapId :: Id Organization,
    status :: Dz.TaskState,
    sender :: Person,
    receiver :: Person,
    pickupAddress :: Address,
    dropAddress :: Address,
    categoryId :: Int,
    deliveryPrice :: Maybe Float,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }

mkDeliveryEntity :: Delivery -> DeliveryEntity
mkDeliveryEntity Delivery {..} =
  DeliveryEntity
    { id = cast id,
      senderId = sender.id,
      receiverId = receiver.id,
      pickupAddressId = pickupAddress.id,
      dropAddressId = dropAddress.id,
      ..
    }

mkDelivery ::
  (DeliveryEntity, Person, Person, Address, Address) ->
  Delivery
mkDelivery (DeliveryEntity {..}, sender, receiver, pickupAddress, dropAddress) =
  Delivery
    { id = cast id,
      ..
    }
