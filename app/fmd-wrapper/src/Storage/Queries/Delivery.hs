{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Delivery where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Delivery
import Storage.Tabular.Address
import Storage.Tabular.Delivery as T
import Storage.Tabular.Person

findById ::
  EsqDBFlow m r =>
  Id DeliveryEntity ->
  m (Maybe DeliveryEntity)
findById = Esq.findById

create :: DeliveryEntity -> SqlDB ()
create = create'

update :: Delivery -> SqlDB ()
update delivery = do
  let deliveryEntity = mkDeliveryEntity delivery
  let deliveryT = toTType deliveryEntity
  update' $ \tbl -> do
    set
      tbl
      [ DeliveryStatus =. val deliveryEntity.status,
        DeliveryDeliveryPrice =. val (T.deliveryPrice deliveryT),
        DeliveryUpdatedAt =. val deliveryEntity.updatedAt
      ]
    where_ $ tbl ^. DeliveryId ==. val (getId deliveryEntity.id)

findAggregatesById :: EsqDBFlow m r => Id Delivery -> m (Maybe Delivery)
findAggregatesById deliveryId = do
  mbTuple <- runTransaction . findOne $ do
    (delivery :& sender :& receiver :& pickupAddress :& dropAddress) <-
      from $
        table @DeliveryT
          `innerJoin` table @PersonT
            `Esq.on` ( \(delivery :& sender) ->
                         delivery ^. DeliverySenderId ==. sender ^. PersonTId
                     )
          `innerJoin` table @PersonT
            `Esq.on` ( \(delivery :& _ :& receiver) ->
                         delivery ^. DeliveryReceiverId ==. receiver ^. PersonTId
                     )
          `innerJoin` table @AddressT
            `Esq.on` ( \(delivery :& _ :& _ :& pickupAddress) ->
                         delivery ^. DeliveryPickupAddressId ==. pickupAddress ^. AddressTId
                     )
          `innerJoin` table @AddressT
            `Esq.on` ( \(delivery :& _ :& _ :& _ :& dropAddress) ->
                         delivery ^. DeliveryDropAddressId ==. dropAddress ^. AddressTId
                     )
    where_ $ delivery ^. DeliveryTId ==. val (toKey (cast deliveryId))
    pure (delivery, sender, receiver, pickupAddress, dropAddress)
  pure $ mkDelivery <$> mbTuple
