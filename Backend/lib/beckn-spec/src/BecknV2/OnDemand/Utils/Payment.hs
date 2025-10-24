{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module BecknV2.OnDemand.Utils.Payment
  ( mkPayment,
    mkPayment',
  )
where

import qualified BecknV2.OnDemand.Enums as Spec
import qualified BecknV2.OnDemand.Tags as Tag
import qualified BecknV2.OnDemand.Types as Spec
import Data.Aeson as A
import Domain.Types
import Kernel.Prelude
import Kernel.Types.Common (Price)

type TxnId = Text

type CollectedBy = Text

type BuyerFinderFee = Text

type City = Text

type SettlementType = Text

type SettlementWindow = Text

mkPayment ::
  City ->
  CollectedBy ->
  Spec.PaymentStatus ->
  Maybe Price ->
  Maybe TxnId ->
  Maybe BknPaymentParams ->
  Maybe SettlementType ->
  Maybe SettlementWindow ->
  Maybe BaseUrl ->
  Maybe BuyerFinderFee ->
  Spec.Payment
mkPayment txnCity collectedBy paymentStatus mPrice mTxnId mPaymentParams mSettlementType mSettlementWindow mSettlementTermsUrl mbff = do
  let mAmount = show . (.amount) <$> mPrice
  let mCurrency = show . (.currency) <$> mPrice
  Spec.Payment
    { paymentCollectedBy = Just collectedBy,
      paymentId = mTxnId,
      paymentParams =
        if anyTrue [isJust mTxnId, isJust mAmount, isJust mPaymentParams]
          then Just $ mkPaymentParams mPaymentParams mTxnId mAmount mCurrency
          else Nothing,
      paymentStatus = encodeToText' paymentStatus,
      paymentTags = Just $ mkPaymentTags txnCity mSettlementType mAmount mSettlementWindow mSettlementTermsUrl mbff,
      paymentType = encodeToText' Spec.ON_FULFILLMENT
    }
  where
    anyTrue = or

mkPayment' ::
  Tag.TagList ->
  CollectedBy ->
  Spec.PaymentStatus ->
  Maybe Price ->
  Maybe TxnId ->
  Maybe BknPaymentParams ->
  Spec.Payment
mkPayment' allPaymentTags collectedBy _paymentStatus _mPrice mTxnId _mPaymentParams =
  Spec.Payment
    { paymentCollectedBy = Just collectedBy, -- Mandatory per ONDC spec
      paymentId = mTxnId,
      paymentParams = Nothing, -- Not sent in search requests
      paymentStatus = Nothing, -- Not allowed in search requests per ONDC validation
      paymentTags = Tag.convertToTagGroup allPaymentTags,
      paymentType = Nothing -- Not sent in search requests
    }

mkPaymentParams :: Maybe BknPaymentParams -> Maybe TxnId -> Maybe Text -> Maybe Text -> Spec.PaymentParams
mkPaymentParams mPaymentParams _mTxnId mAmount mCurrency = do
  Spec.PaymentParams
    { paymentParamsAmount = mAmount,
      paymentParamsBankAccountNumber = mPaymentParams >>= (.bankAccNumber),
      paymentParamsBankCode = mPaymentParams >>= (.bankCode),
      paymentParamsCurrency = mCurrency,
      -- paymentParamsTransactionId = mTxnId,
      paymentParamsVirtualPaymentAddress = mPaymentParams >>= (.vpa)
    }

mkPaymentTags :: City -> Maybe SettlementType -> Maybe Text -> Maybe SettlementWindow -> Maybe BaseUrl -> Maybe BuyerFinderFee -> [Spec.TagGroup]
mkPaymentTags txnCity mSettlementType mAmount mSettlementWindow mSettlementTermsUrl mbff =
  catMaybes
    [ Just $ mkBuyerFinderFeeTagGroup mbff,
      Just $ mkSettlementTagGroup txnCity mAmount mSettlementWindow mSettlementTermsUrl,
      mkSettlementDetailsTagGroup mSettlementType
    ]

mkBuyerFinderFeeTagGroup :: Maybe BuyerFinderFee -> Spec.TagGroup
mkBuyerFinderFeeTagGroup mbff =
  Spec.TagGroup
    { tagGroupDescriptor =
        Just $
          Spec.Descriptor
            { descriptorCode = Just $ show Tag.BUYER_FINDER_FEES,
              descriptorName = Nothing,
              descriptorShortDesc = Nothing
            },
      tagGroupDisplay = Nothing,
      tagGroupList = Just [feePercentage]
    }
  where
    feePercentage =
      Spec.Tag
        { tagDescriptor =
            Just $
              Spec.Descriptor
                { descriptorCode = Just $ show Tag.BUYER_FINDER_FEES_PERCENTAGE,
                  descriptorName = Nothing,
                  descriptorShortDesc = Nothing
                },
          tagValue = Just $ fromMaybe "0" mbff,
          tagDisplay = Nothing
        }

mkSettlementTagGroup :: City -> Maybe Text -> Maybe SettlementWindow -> Maybe BaseUrl -> Spec.TagGroup
mkSettlementTagGroup txnCity mSettlementAmount mSettlementWindow mSettlementTermsUrl =
  Spec.TagGroup
    { tagGroupDescriptor =
        Just $
          Spec.Descriptor
            { descriptorCode = Just $ show Tag.SETTLEMENT_TERMS,
              descriptorName = Nothing,
              descriptorShortDesc = Nothing
            },
      tagGroupDisplay = Nothing,
      tagGroupList = Just settlementTags
    }
  where
    settlementTags =
      catMaybes
        [ mSettlementAmount <&> \samount ->
            Spec.Tag
              { tagDescriptor =
                  Just $
                    Spec.Descriptor
                      { descriptorCode = Just $ show Tag.SETTLEMENT_AMOUNT,
                        descriptorName = Nothing,
                        descriptorShortDesc = Nothing
                      },
                tagValue = Just samount,
                tagDisplay = Nothing
              },
          Just $
            Spec.Tag
              { tagDescriptor =
                  Just $
                    Spec.Descriptor
                      { descriptorCode = Just $ show Tag.SETTLEMENT_WINDOW,
                        descriptorName = Nothing,
                        descriptorShortDesc = Nothing
                      },
                tagValue = Just $ fromMaybe "PT1D" mSettlementWindow,
                tagDisplay = Nothing
              },
          Just $
            Spec.Tag
              { tagDescriptor =
                  Just $
                    Spec.Descriptor
                      { descriptorCode = Just $ show Tag.DELAY_INTEREST,
                        descriptorName = Nothing,
                        descriptorShortDesc = Nothing
                      },
                tagValue = Just "0",
                tagDisplay = Nothing
              },
          Just $
            Spec.Tag
              { tagDescriptor =
                  Just $
                    Spec.Descriptor
                      { descriptorCode = Just $ show Tag.SETTLEMENT_BASIS,
                        descriptorName = Nothing,
                        descriptorShortDesc = Nothing
                      },
                tagValue = Just "INVOICE_RECIEPT",
                tagDisplay = Nothing
              },
          Just $
            Spec.Tag
              { tagDescriptor =
                  Just $
                    Spec.Descriptor
                      { descriptorCode = Just $ show Tag.MANDATORY_ARBITRATION,
                        descriptorName = Nothing,
                        descriptorShortDesc = Nothing
                      },
                tagValue = Just "TRUE",
                tagDisplay = Nothing
              },
          Just $
            Spec.Tag
              { tagDescriptor =
                  Just $
                    Spec.Descriptor
                      { descriptorCode = Just $ show Tag.COURT_JURISDICTION,
                        descriptorName = Nothing,
                        descriptorShortDesc = Nothing
                      },
                tagValue = Just txnCity,
                tagDisplay = Nothing
              },
          Just $
            Spec.Tag
              { tagDescriptor =
                  Just $
                    Spec.Descriptor
                      { descriptorCode = Just $ show Tag.STATIC_TERMS,
                        descriptorName = Nothing,
                        descriptorShortDesc = Nothing
                      },
                tagValue = Just $ maybe "https://api.example-bap.com/booking/terms" showBaseUrl mSettlementTermsUrl,
                tagDisplay = Nothing
              }
        ]

mkSettlementDetailsTagGroup :: Maybe SettlementType -> Maybe Spec.TagGroup
mkSettlementDetailsTagGroup mSettlementType = do
  st <- mSettlementType
  return $
    Spec.TagGroup
      { tagGroupDescriptor =
          Just $
            Spec.Descriptor
              { descriptorCode = Just $ show Tag.SETTLEMENT_DETAILS,
                descriptorName = Nothing,
                descriptorShortDesc = Nothing
              },
        tagGroupDisplay = Nothing,
        tagGroupList = Just [stTag st]
      }
  where
    stTag st =
      Spec.Tag
        { tagDescriptor =
            Just $
              Spec.Descriptor
                { descriptorCode = Just $ show Tag.SETTLEMENT_TYPE,
                  descriptorName = Nothing,
                  descriptorShortDesc = Nothing
                },
          tagValue = Just st,
          tagDisplay = Nothing
        }

encodeToText' :: (ToJSON a) => a -> Maybe Text
encodeToText' = A.decode . A.encode
