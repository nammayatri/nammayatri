{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Beckn.ACL.FRFS.Init (buildInitReq) where

import qualified Beckn.ACL.FRFS.Utils as Utils
import qualified BecknV2.FRFS.Enums as Spec
import qualified BecknV2.FRFS.Types as Spec
import qualified Domain.Types.FRFSQuote as DQuote
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common

buildInitReq ::
  (MonadFlow m, HasFlowEnv m r '["nwAddress" ::: BaseUrl]) =>
  DQuote.FRFSQuote ->
  m (Spec.InitReq)
buildInitReq quote = do
  let transactionId = quote.searchId.getId

  messageId <- generateGUID

  merchantId <- quote.merchantId <&> (.getId) & fromMaybeM (InternalError "MerchantId not found")
  context <- Utils.buildContext Spec.INIT merchantId transactionId messageId Nothing

  pure $
    Spec.InitReq
      { initReqContext = context,
        initReqMessage = tfInitMessage quote
      }

tfInitMessage :: DQuote.FRFSQuote -> Spec.ConfirmReqMessage
tfInitMessage quote =
  Spec.ConfirmReqMessage
    { confirmReqMessageOrder = tfOrder quote
    }

tfOrder :: DQuote.FRFSQuote -> Spec.Order
tfOrder quote =
  Spec.Order
    { orderBilling = tfBilling quote,
      orderCancellationTerms = Nothing,
      orderCreatedAt = Nothing,
      orderFulfillments = Nothing,
      orderId = Nothing,
      orderItems = tfItems quote,
      orderPayments = tfPayments quote,
      orderProvider = tfProvider quote,
      orderQuote = Nothing,
      orderStatus = Nothing,
      orderTags = Nothing,
      orderUpdatedAt = Nothing
    }

tfBilling :: DQuote.FRFSQuote -> Maybe Spec.Billing
tfBilling _quote =
  Just $
    Spec.Billing
      { billingEmail = Nothing,
        billingName = Just "N/A", -- TODO: fix billing details
        billingPhone = Nothing
      }

tfItems :: DQuote.FRFSQuote -> Maybe [Spec.Item]
tfItems quote =
  Just $
    [ Spec.Item
        { itemCategoryIds = Nothing,
          itemDescriptor = Nothing,
          itemFulfillmentIds = Nothing,
          itemId = Just quote.bppItemId,
          itemPrice = Nothing,
          itemQuantity = tfQuantity quote,
          itemTime = Nothing
        }
    ]

tfQuantity :: DQuote.FRFSQuote -> Maybe Spec.ItemQuantity
tfQuantity quote =
  Just $
    Spec.ItemQuantity
      { itemQuantityMaximum = Nothing,
        itemQuantityMinimum = Nothing,
        itemQuantitySelected =
          Just $
            Spec.ItemQuantitySelected
              { itemQuantitySelectedCount = Just quote.quantity
              }
      }

tfPayments :: DQuote.FRFSQuote -> Maybe [Spec.Payment]
tfPayments _quote = Nothing -- TODO: add payment tags

tfProvider :: DQuote.FRFSQuote -> Maybe Spec.Provider
tfProvider quote =
  Just $
    Spec.Provider
      { providerCategories = Nothing,
        providerDescriptor = Nothing,
        providerFulfillments = Nothing,
        providerId = Just quote.providerId,
        providerItems = Nothing,
        providerPayments = Nothing,
        providerTags = Nothing,
        providerTime = Nothing
      }
