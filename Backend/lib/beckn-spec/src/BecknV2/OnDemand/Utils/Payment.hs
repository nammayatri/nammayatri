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
  Tag.getFullTagGroup Tag.BUYER_FINDER_FEES
    [ Tag.mkTag Tag.BUYER_FINDER_FEES_PERCENTAGE (Just $ fromMaybe "0" mbff)
    ]

mkSettlementTagGroup :: City -> Maybe Text -> Maybe SettlementWindow -> Maybe BaseUrl -> Maybe DPM.PaymentMode -> Maybe Text -> Spec.TagGroup
mkSettlementTagGroup txnCity mSettlementAmount mSettlementWindow mSettlementTermsUrl mPaymentMode mPaymentInstrument =
  Tag.getFullTagGroup Tag.SETTLEMENT_TERMS $
    catMaybes
      [ Tag.mkOptionalTag Tag.STRIPE_TEST (if mPaymentMode == Just DPM.TEST then Just (show True) else Nothing),
        Tag.mkOptionalTag Tag.PAYMENT_INSTRUMENT mPaymentInstrument
      ]
      <> catMaybes
        [ Tag.mkOptionalTag Tag.SETTLEMENT_AMOUNT mSettlementAmount,
          Just $ Tag.mkTag Tag.SETTLEMENT_WINDOW (Just $ fromMaybe "PT1D" mSettlementWindow),
          Just $ Tag.mkTag Tag.DELAY_INTEREST (Just "0"),
          Just $ Tag.mkTag Tag.SETTLEMENT_BASIS (Just "INVOICE_RECIEPT"),
          Just $ Tag.mkTag Tag.MANDATORY_ARBITRATION (Just "TRUE"),
          Just $ Tag.mkTag Tag.COURT_JURISDICTION (Just txnCity),
          Just $ Tag.mkTag Tag.STATIC_TERMS (Just $ maybe "https://api.example-bap.com/booking/terms" showBaseUrl mSettlementTermsUrl)
        ]

mkSettlementDetailsTagGroup :: Maybe SettlementType -> Maybe Spec.TagGroup
mkSettlementDetailsTagGroup mSettlementType = do
  st <- mSettlementType
  return $ Tag.getFullTagGroup Tag.SETTLEMENT_DETAILS
    [ Tag.mkTag Tag.SETTLEMENT_TYPE (Just st)
    ]

encodeToText' :: (ToJSON a) => a -> Maybe Text
encodeToText' = A.decode . A.encode
