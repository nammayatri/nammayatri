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
    mkPPFSettlementTags,
  )
where

import qualified BecknV2.OnDemand.Enums as Spec
import qualified BecknV2.OnDemand.Tags as Tag
import qualified BecknV2.OnDemand.Types as Spec
import Data.Aeson as A
import Domain.Types
import qualified Domain.Types.PaymentMode as DPM
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
  Bool ->
  Maybe DPM.PaymentMode ->
  Maybe Text -> -- Payment instrument
  Spec.Payment
mkPayment txnCity collectedBy paymentStatus mPrice mTxnId mPaymentParams mSettlementType mSettlementWindow mSettlementTermsUrl mbff isStripe mPaymentMode mPaymentInstrument = do
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
      paymentTags = Just $ mkPaymentTags txnCity mSettlementType mAmount mSettlementWindow mSettlementTermsUrl mbff mPaymentMode mPaymentInstrument,
      paymentTlMethod = mkTlMethod isStripe >>= encodeToText',
      paymentType = encodeToText' Spec.ON_FULFILLMENT
    }
  where
    anyTrue = or

mkTlMethod :: Bool -> Maybe Spec.TLMethod
mkTlMethod isStripe = if isStripe then Just Spec.StripeSdk else Nothing

mkPayment' ::
  Tag.TagList ->
  CollectedBy ->
  Spec.PaymentStatus ->
  Maybe Price ->
  Maybe TxnId ->
  Maybe BknPaymentParams ->
  Spec.Payment
mkPayment' allPaymentTags collectedBy paymentStatus mPrice mTxnId mPaymentParams = do
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
      paymentTags = Tag.convertToTagGroup allPaymentTags,
      paymentTlMethod = Nothing,
      paymentType = encodeToText' Spec.ON_FULFILLMENT
    }
  where
    anyTrue = or

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

mkPaymentTags :: City -> Maybe SettlementType -> Maybe Text -> Maybe SettlementWindow -> Maybe BaseUrl -> Maybe BuyerFinderFee -> Maybe DPM.PaymentMode -> Maybe Text -> [Spec.TagGroup]
mkPaymentTags txnCity mSettlementType mAmount mSettlementWindow mSettlementTermsUrl mbff mPaymentMode mPaymentInstrument =
  catMaybes
    [ Just $ mkBuyerFinderFeeTagGroup mbff,
      Just $ mkSettlementTagGroup txnCity mAmount mSettlementWindow mSettlementTermsUrl mPaymentMode mPaymentInstrument,
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
      tagGroupDisplay = Just False,
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
          tagDisplay = Just False
        }

mkSettlementTagGroup :: City -> Maybe Text -> Maybe SettlementWindow -> Maybe BaseUrl -> Maybe DPM.PaymentMode -> Maybe Text -> Spec.TagGroup
mkSettlementTagGroup txnCity mSettlementAmount mSettlementWindow mSettlementTermsUrl mPaymentMode mPaymentInstrument =
  Spec.TagGroup
    { tagGroupDescriptor =
        Just $
          Spec.Descriptor
            { descriptorCode = Just $ show Tag.SETTLEMENT_TERMS,
              descriptorName = Nothing,
              descriptorShortDesc = Nothing
            },
      tagGroupDisplay = Just False,
      tagGroupList = Just $ catMaybes [mStripeTestTag, mPaymentInstrumentTag] <> settlementTags
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
                tagDisplay = Just False
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
                tagDisplay = Just False
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
                tagDisplay = Just False
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
                tagDisplay = Just False
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
                tagDisplay = Just False
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
                tagDisplay = Just False
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
                tagDisplay = Just False
              }
        ]
    mStripeTestTag =
      if mPaymentMode == Just DPM.TEST
        then
          Just
            Spec.Tag
              { tagDescriptor =
                  Just $
                    Spec.Descriptor
                      { descriptorCode = Just $ show Tag.STRIPE_TEST,
                        descriptorName = Nothing,
                        descriptorShortDesc = Nothing
                      },
                tagValue = Just $ show True,
                tagDisplay = Just False
              }
        else Nothing
    mPaymentInstrumentTag =
      mPaymentInstrument <&> \instrument ->
        Spec.Tag
          { tagDescriptor =
              Just $
                Spec.Descriptor
                  { descriptorCode = Just $ show Tag.PAYMENT_INSTRUMENT,
                    descriptorName = Nothing,
                    descriptorShortDesc = Nothing
                  },
            tagValue = Just instrument,
            tagDisplay = Just False
          }

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
        tagGroupDisplay = Just False,
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
          tagDisplay = Just False
        }

encodeToText' :: (ToJSON a) => a -> Maybe Text
encodeToText' = A.decode . A.encode

-- | Build PPF-specific settlement tags for inclusion in BECKN payment messages.
-- These extend the standard settlement tags with PPF-required fields like
-- withholding amount, TDS, TCS, and settlement reference.
mkPPFSettlementTags ::
  Maybe Text -> -- withholding amount
  Maybe Text -> -- TDS amount
  Maybe Text -> -- TCS amount
  Maybe Text -> -- settlement reference number
  Maybe Text -> -- seller share
  Maybe Text -> -- buyer app commission
  Maybe Text -> -- PPF payment status
  [Spec.Tag]
mkPPFSettlementTags mWithholding mTds mTcs mSettlementRef mSellerShare mBuyerAppCommission mPpfPaymentStatus =
  catMaybes
    [ mWithholding <&> \val ->
        Spec.Tag
          { tagDescriptor =
              Just $
                Spec.Descriptor
                  { descriptorCode = Just $ show Tag.WITHHOLDING_AMOUNT,
                    descriptorName = Nothing,
                    descriptorShortDesc = Nothing
                  },
            tagValue = Just val,
            tagDisplay = Just False
          },
      mTds <&> \val ->
        Spec.Tag
          { tagDescriptor =
              Just $
                Spec.Descriptor
                  { descriptorCode = Just $ show Tag.TDS_AMOUNT,
                    descriptorName = Nothing,
                    descriptorShortDesc = Nothing
                  },
            tagValue = Just val,
            tagDisplay = Just False
          },
      mTcs <&> \val ->
        Spec.Tag
          { tagDescriptor =
              Just $
                Spec.Descriptor
                  { descriptorCode = Just $ show Tag.TCS_AMOUNT,
                    descriptorName = Nothing,
                    descriptorShortDesc = Nothing
                  },
            tagValue = Just val,
            tagDisplay = Just False
          },
      mSettlementRef <&> \val ->
        Spec.Tag
          { tagDescriptor =
              Just $
                Spec.Descriptor
                  { descriptorCode = Just $ show Tag.SETTLEMENT_REF_NO,
                    descriptorName = Nothing,
                    descriptorShortDesc = Nothing
                  },
            tagValue = Just val,
            tagDisplay = Just False
          },
      mSellerShare <&> \val ->
        Spec.Tag
          { tagDescriptor =
              Just $
                Spec.Descriptor
                  { descriptorCode = Just $ show Tag.SELLER_SHARE,
                    descriptorName = Nothing,
                    descriptorShortDesc = Nothing
                  },
            tagValue = Just val,
            tagDisplay = Just False
          },
      mBuyerAppCommission <&> \val ->
        Spec.Tag
          { tagDescriptor =
              Just $
                Spec.Descriptor
                  { descriptorCode = Just $ show Tag.BUYER_APP_COMMISSION,
                    descriptorName = Nothing,
                    descriptorShortDesc = Nothing
                  },
            tagValue = Just val,
            tagDisplay = Just False
          },
      mPpfPaymentStatus <&> \val ->
        Spec.Tag
          { tagDescriptor =
              Just $
                Spec.Descriptor
                  { descriptorCode = Just $ show Tag.PPF_PAYMENT_STATUS,
                    descriptorName = Nothing,
                    descriptorShortDesc = Nothing
                  },
            tagValue = Just val,
            tagDisplay = Just False
          }
    ]
