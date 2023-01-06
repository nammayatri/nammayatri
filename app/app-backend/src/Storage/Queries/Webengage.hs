{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Webengage where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common
import Beckn.Types.Id
import Domain.Types.Webengage
import Storage.Tabular.Webengage

create :: Webengage -> SqlDB ()
create = Esq.create

findById :: Transactionable m => Id Webengage -> m (Maybe Webengage)
findById = Esq.findById

findByInfoMsgId :: Transactionable m => Text -> m (Maybe Webengage)
findByInfoMsgId infoMessageId =
  Esq.findOne $ do
    webengage <- from $ table @WebengageT
    where_ $ webengage ^. WebengageInfoMessageId ==. val infoMessageId
    return webengage

updateStatus :: Text -> Text -> SqlDB ()
updateStatus infoMessageId status = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ WebengageUpdatedAt =. val now,
        WebengageStatus =. val (Just status)
      ]
    where_ $ tbl ^. WebengageInfoMessageId ==. val infoMessageId
