 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
 
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program 
 
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of 
 
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

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
