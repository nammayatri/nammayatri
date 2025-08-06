{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.Payment.Storage.Queries.PaymentOrder where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Payment.Interface as Payment
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import Lib.Payment.Storage.Beam.BeamFlow
import qualified Lib.Payment.Storage.Beam.PaymentOrder as BeamPO
import qualified Sequelize as Se

findById :: BeamFlow m r => Id DOrder.PaymentOrder -> m (Maybe DOrder.PaymentOrder)
findById (Id paymentOrder) = findOneWithKV [Se.Is BeamPO.id $ Se.Eq paymentOrder]

findByShortId :: BeamFlow m r => ShortId DOrder.PaymentOrder -> m (Maybe DOrder.PaymentOrder)
findByShortId (ShortId shortId) = findOneWithKV [Se.Is BeamPO.shortId $ Se.Eq shortId]

findLatestByPersonId :: BeamFlow m r => Text -> m (Maybe DOrder.PaymentOrder)
findLatestByPersonId personId =
  findAllWithOptionsKV
    [Se.Is BeamPO.personId $ Se.Eq personId]
    (Se.Desc BeamPO.createdAt)
    (Just 1)
    Nothing
    <&> listToMaybe

create :: BeamFlow m r => DOrder.PaymentOrder -> m ()
create = createWithKV

updateStatusAndError :: BeamFlow m r => DOrder.PaymentOrder -> Maybe Text -> Maybe Text -> m ()
updateStatusAndError order bankErrorMessage bankErrorCode = do
  now <- getCurrentTime
  updateWithKV
    ( [ Se.Set BeamPO.status order.status,
        Se.Set BeamPO.bankErrorMessage bankErrorMessage,
        Se.Set BeamPO.isRetried order.isRetried,
        Se.Set BeamPO.isRetargeted order.isRetargeted,
        Se.Set BeamPO.bankErrorCode bankErrorCode,
        Se.Set BeamPO.updatedAt now
      ]
        <> [Se.Set BeamPO.retargetLink order.retargetLink | isJust order.retargetLink]
    )
    [Se.Is BeamPO.id $ Se.Eq $ getId order.id]

updateStatusToExpired :: BeamFlow m r => Id DOrder.PaymentOrder -> m ()
updateStatusToExpired orderId = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamPO.status Payment.CLIENT_AUTH_TOKEN_EXPIRED,
      Se.Set BeamPO.updatedAt now
    ]
    [Se.Is BeamPO.id $ Se.Eq $ getId orderId]

updateStatus :: BeamFlow m r => Id DOrder.PaymentOrder -> Text -> Payment.TransactionStatus -> m ()
updateStatus orderId paymentServiceOrderId status = do
  now <- getCurrentTime
  mOrder <- findById orderId
  let newStatus = maybe status (\order -> if order.status == Payment.CHARGED then order.status else status) mOrder -- don't change if status is already charged
  updateWithKV
    [ Se.Set BeamPO.status newStatus,
      Se.Set BeamPO.paymentServiceOrderId paymentServiceOrderId,
      Se.Set BeamPO.updatedAt now
    ]
    [Se.Is BeamPO.id $ Se.Eq $ getId orderId]

updateAmountAndPaymentIntentId :: BeamFlow m r => Id DOrder.PaymentOrder -> HighPrecMoney -> Text -> m ()
updateAmountAndPaymentIntentId orderId amount paymentServiceOrderId = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamPO.amount amount,
      Se.Set BeamPO.paymentServiceOrderId paymentServiceOrderId,
      Se.Set BeamPO.updatedAt now
    ]
    [Se.Is BeamPO.id $ Se.Eq $ getId orderId]

updateAmount :: BeamFlow m r => Id DOrder.PaymentOrder -> HighPrecMoney -> m ()
updateAmount orderId amount = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamPO.amount amount,
      Se.Set BeamPO.updatedAt now
    ]
    [Se.Is BeamPO.id $ Se.Eq $ getId orderId]

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
            serviceProvider = fromMaybe Payment.Juspay serviceProvider,
            clientAuthToken = case (clientAuthTokenEncrypted, clientAuthTokenHash) of
              (Just encryptedToken, Just hash) -> Just $ EncryptedHashed (Encrypted encryptedToken) hash
              (_, _) -> Nothing,
            merchantOperatingCityId = Id <$> merchantOperatingCityId,
            ..
          }
    where
      parsePaymentLinks :: MonadThrow m => BeamPO.PaymentOrder -> m Payment.PaymentLinks
      parsePaymentLinks paymentOrder = do
        web <- parseBaseUrl `mapM` paymentOrder.webPaymentLink
        iframe <- parseBaseUrl `mapM` paymentOrder.iframePaymentLink
        mobile <- parseBaseUrl `mapM` paymentOrder.mobilePaymentLink
        let deep_link = paymentOrder.deepLink
        pure Payment.PaymentLinks {..}

instance ToTType' BeamPO.PaymentOrder DOrder.PaymentOrder where
  toTType' DOrder.PaymentOrder {..} =
    BeamPO.PaymentOrderT
      { id = getId id,
        shortId = getShortId shortId,
        personId = personId.getId,
        merchantId = merchantId.getId,
        webPaymentLink = showBaseUrl <$> paymentLinks.web,
        iframePaymentLink = showBaseUrl <$> paymentLinks.iframe,
        mobilePaymentLink = showBaseUrl <$> paymentLinks.mobile,
        deepLink = paymentLinks.deep_link,
        clientAuthTokenEncrypted = clientAuthToken <&> unEncrypted . (.encrypted),
        clientAuthTokenHash = clientAuthToken <&> (.hash),
        serviceProvider = Just serviceProvider,
        merchantOperatingCityId = getId <$> merchantOperatingCityId,
        ..
      }
