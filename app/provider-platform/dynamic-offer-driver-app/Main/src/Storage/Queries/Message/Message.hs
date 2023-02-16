{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Message.Message where

import Domain.Types.Merchant (Merchant)
import Domain.Types.Message.Message
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Tabular.Message.Instances ()
import Storage.Tabular.Message.Message

create :: Message -> SqlDB ()
create msg = Esq.runTransaction $
  withFullEntity msg $ \(message, messageTranslations) -> do
    Esq.create' message
    traverse_ Esq.create' messageTranslations

findById :: Transactionable m => Id Message -> m (Maybe RawMessage)
findById = Esq.findById

findAllWithLimitOffset ::
  Transactionable m =>
  Maybe Int ->
  Maybe Int ->
  Id Merchant ->
  m [RawMessage]
findAllWithLimitOffset mbLimit mbOffset merchantId = do
  findAll $ do
    message <-
      from $
        table @MessageT
    where_ $
      message ^. MessageMerchantId ==. val (toKey merchantId)
    orderBy [desc $ message ^. MessageCreatedAt]
    limit limitVal
    offset offsetVal
    return message
  where
    limitVal = min (maybe 10 fromIntegral mbLimit) 10
    offsetVal = maybe 0 fromIntegral mbOffset
