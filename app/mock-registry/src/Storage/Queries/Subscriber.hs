module Storage.Queries.Subscriber where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Domain.Subscriber
import Storage.Tabular.Subscriber

findByAll :: Transactionable m => Maybe Text -> Maybe Text -> Maybe Domain -> Maybe SubscriberType -> m [Subscriber]
findByAll mbKeyId mbSubId mbDomain mbSubType =
  Esq.findAll $ do
    parkingLocation <- from $ table @SubscriberT
    where_ $
      whenJust_ mbKeyId (\keyId -> parkingLocation ^. SubscriberUniqueKeyId ==. val keyId)
        &&. whenJust_ mbSubId (\subId -> parkingLocation ^. SubscriberSubscriberId ==. val subId)
        &&. whenJust_ mbDomain (\domain -> parkingLocation ^. SubscriberDomain ==. val domain)
        &&. whenJust_ mbSubType (\subType -> parkingLocation ^. SubscriberSubscriberType ==. val subType)
    return parkingLocation

create :: Subscriber -> SqlDB ()
create = create'

deleteByKey :: (Text, Text) -> SqlDB ()
deleteByKey = deleteByKey' @SubscriberT

findAll :: Transactionable m => m [Subscriber]
findAll =
  Esq.findAll $ do
    from $ table @SubscriberT
