{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.Payment.Storage.Tabular.PaymentOrder where

import Kernel.External.Encryption (DbHash, Encrypted (..), EncryptedHashed (..))
import qualified Kernel.External.Payment.Interface as Payment
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import qualified Lib.Payment.Domain.Types.PaymentOrder as Domain

mkPersist
  defaultSqlSettings
  [defaultQQ|
    PaymentOrderT sql=payment_order
      id Text
      shortId Text
      paymentServiceOrderId Text
      personId Text -- PersonTId
      merchantId Text -- MerchantTId
      paymentMerchantId Text Maybe
      requestId Text Maybe
      service Text Maybe
      clientId Text Maybe
      description Text Maybe
      returnUrl Text Maybe
      action Text Maybe
      amount Money
      currency Payment.Currency
      status Payment.TransactionStatus
      webPaymentLink Text Maybe
      iframePaymentLink Text Maybe
      mobilePaymentLink Text Maybe
      clientAuthTokenEncrypted Text Maybe
      clientAuthTokenHash DbHash Maybe
      clientAuthTokenExpiry UTCTime Maybe
      getUpiDeepLinksOption Bool Maybe
      environment Text Maybe
      createMandate Payment.MandateType Maybe
      mandateMaxAmount HighPrecMoney Maybe
      mandateStartDate UTCTime Maybe
      mandateEndDate UTCTime Maybe
      bankErrorCode Text Maybe
      bankErrorMessage Text Maybe
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey PaymentOrderT where
  type DomainKey PaymentOrderT = Id Domain.PaymentOrder
  fromKey (PaymentOrderTKey _id) = Id _id
  toKey (Id id) = PaymentOrderTKey id

instance FromTType PaymentOrderT Domain.PaymentOrder where
  fromTType orderT@PaymentOrderT {..} = do
    paymentLinks <- parsePaymentLinks orderT
    return $
      Domain.PaymentOrder
        { id = Id id,
          shortId = ShortId shortId,
          personId = Id personId,
          merchantId = Id merchantId,
          clientAuthToken = case (clientAuthTokenEncrypted, clientAuthTokenHash) of
            (Just encryptedToken, Just hash) -> Just $ EncryptedHashed (Encrypted encryptedToken) hash
            (_, _) -> Nothing,
          ..
        }

parsePaymentLinks :: MonadThrow m => PaymentOrderT -> m Payment.PaymentLinks
parsePaymentLinks PaymentOrderT {..} = do
  web <- parseBaseUrl `mapM` webPaymentLink
  iframe <- parseBaseUrl `mapM` iframePaymentLink
  mobile <- parseBaseUrl `mapM` mobilePaymentLink
  pure Payment.PaymentLinks {..}

instance ToTType PaymentOrderT Domain.PaymentOrder where
  toTType Domain.PaymentOrder {..} =
    PaymentOrderT
      { id = getId id,
        shortId = getShortId shortId,
        personId = personId.getId,
        merchantId = merchantId.getId,
        webPaymentLink = showBaseUrl <$> paymentLinks.web,
        iframePaymentLink = showBaseUrl <$> paymentLinks.iframe,
        mobilePaymentLink = showBaseUrl <$> paymentLinks.mobile,
        clientAuthTokenEncrypted = clientAuthToken <&> unEncrypted . (.encrypted),
        clientAuthTokenHash = clientAuthToken <&> (.hash),
        ..
      }
