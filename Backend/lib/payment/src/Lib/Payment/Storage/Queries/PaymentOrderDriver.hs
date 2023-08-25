{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.Payment.Storage.Queries.PaymentOrderDriver where

import Kernel.Beam.Functions (FromTType' (fromTType'), ToTType' (toTType'), createWithKV, findAllWithOptionsKV, findOneWithKV, updateOneWithKV)
import Kernel.External.Encryption (Encrypted (..), EncryptedHashed (..))
import qualified Kernel.External.Payment.Interface as Payment
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (MonadFlow, getCurrentTime)
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import qualified Lib.Payment.Storage.Beam.PaymentOrderDriver as BeamPO
import qualified Sequelize as Se

findById :: MonadFlow m => Id DOrder.PaymentOrder -> m (Maybe DOrder.PaymentOrder)
findById (Id paymentOrder) = findOneWithKV [Se.Is BeamPO.id $ Se.Eq paymentOrder]

findByShortId :: MonadFlow m => ShortId DOrder.PaymentOrder -> m (Maybe DOrder.PaymentOrder)
findByShortId (ShortId shortId) = findOneWithKV [Se.Is BeamPO.shortId $ Se.Eq shortId]

findLatestByPersonId :: MonadFlow m => Text -> m (Maybe DOrder.PaymentOrder)
findLatestByPersonId personId = findAllWithOptionsKV [Se.Is BeamPO.personId $ Se.Eq personId] (Se.Desc BeamPO.createdAt) (Just 1) Nothing <&> listToMaybe

create :: MonadFlow m => DOrder.PaymentOrder -> m ()
create = createWithKV

updateStatus :: MonadFlow m => DOrder.PaymentOrder -> m ()
updateStatus order = do
  now <- getCurrentTime
  updateOneWithKV [Se.Set BeamPO.status order.status, Se.Set BeamPO.updatedAt now] [Se.Is BeamPO.id $ Se.Eq $ getId order.id]

updateStatusToExpired :: (MonadFlow m) => Id DOrder.PaymentOrder -> m ()
updateStatusToExpired (Id orderId) = do
  now <- getCurrentTime
  updateOneWithKV [Se.Set BeamPO.updatedAt now, Se.Set BeamPO.status Payment.CLIENT_AUTH_TOKEN_EXPIRED] [Se.Is BeamPO.id $ Se.Eq orderId]

parsePaymentLinks :: MonadThrow m => BeamPO.PaymentOrder -> m Payment.PaymentLinks
parsePaymentLinks BeamPO.PaymentOrderT {..} = do
  web <- parseBaseUrl `mapM` webPaymentLink
  iframe <- parseBaseUrl `mapM` iframePaymentLink
  mobile <- parseBaseUrl `mapM` mobilePaymentLink
  pure Payment.PaymentLinks {..}

instance FromTType' BeamPO.PaymentOrder DOrder.PaymentOrder where
  fromTType' orderT@BeamPO.PaymentOrderT {..} = do
    paymentLinks <- parsePaymentLinks orderT
    pure $
      Just
        DOrder.PaymentOrder
          { id = Id id,
            shortId = ShortId shortId,
            personId = Id personId,
            merchantId = Id merchantId,
            clientAuthToken = EncryptedHashed (Encrypted clientAuthTokenEncrypted) clientAuthTokenHash,
            ..
          }

instance ToTType' BeamPO.PaymentOrder DOrder.PaymentOrder where
  toTType' DOrder.PaymentOrder {..} =
    BeamPO.PaymentOrderT
      { BeamPO.id = getId id,
        BeamPO.shortId = getShortId shortId,
        BeamPO.personId = personId.getId,
        BeamPO.merchantId = merchantId.getId,
        BeamPO.webPaymentLink = showBaseUrl <$> paymentLinks.web,
        BeamPO.iframePaymentLink = showBaseUrl <$> paymentLinks.iframe,
        BeamPO.mobilePaymentLink = showBaseUrl <$> paymentLinks.mobile,
        BeamPO.clientAuthTokenEncrypted = clientAuthToken & unEncrypted . (.encrypted),
        BeamPO.clientAuthTokenHash = clientAuthToken.hash,
        ..
      }
