{-
  Finance.Invoice.RenderTemplate

  Types and helpers for the config-driven invoice PDF pipeline.
  Orchestrator lives in each app's SharedLogic (finance-kernel cannot
  depend on yudhishthira/utils).
-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Lib.Finance.Invoice.RenderTemplate
  ( InvoiceContext (..),
    RenderLineItem (..),
    BuildInvoiceContextInput (..),
    buildInvoiceContext,
    buildTemplateChecked,
    cleanJson,
    renderLineItemsHtml,
    flatten,
  )
where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Time as DT
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Domain.Types.Invoice (InvoiceType)
import Kernel.External.Types (Language)
import Kernel.Prelude
import Kernel.Types.Common (Currency (..), HighPrecMoney)
import qualified Lib.Finance.Domain.Types.IndirectTaxTransaction as FITxn
import qualified Lib.Finance.Domain.Types.Invoice as FInvoice
import Lib.Finance.Invoice.Interface (InvoiceLineItem, ItemType (..), LineItemDescription)
import Text.Printf (printf)

data InvoiceContext = InvoiceContext
  { invoiceNumber :: Text,
    referenceInvoiceNumber :: Maybe Text,
    issuedAt :: UTCTime,
    dueAt :: Maybe UTCTime,
    invoiceType :: InvoiceType,
    language :: Language,
    currency :: Currency,
    currencyCode :: Text,
    merchantId :: Text,
    merchantShortId :: Text,
    paymentMode :: Maybe Text,
    hasAdjustments :: Bool,
    issuedToName :: Maybe Text,
    issuedToAddress :: Maybe Text,
    supplierName :: Maybe Text,
    supplierAddress :: Maybe Text,
    supplierGSTIN :: Maybe Text,
    supplierTaxNo :: Maybe Text,
    issuedByName :: Maybe Text,
    issuedByAddress :: Maybe Text,
    merchantGstin :: Maybe Text,
    sellerTradeName :: Maybe Text,
    mbRecipientBusinessId :: Maybe Text,
    mbSellerBusinessId :: Maybe Text,
    mbSellerVatNumber :: Maybe Text,
    logoUrl :: Maybe Text,
    appName :: Maybe Text,
    periodStart :: Maybe UTCTime,
    periodEnd :: Maybe UTCTime,
    taxTxnRate :: Maybe Double,
    taxTxnGstRate :: Maybe Double,
    cardBrand :: Maybe Text,
    cardLastFour :: Maybe Text,
    lineItems :: [RenderLineItem]
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data RenderLineItem = RenderLineItem
  { description :: Text,
    descriptionType :: Maybe LineItemDescription,
    amount :: HighPrecMoney,
    taxAmount :: HighPrecMoney,
    groupId :: Maybe Text,
    isExternalCharge :: Bool,
    itemType :: Maybe ItemType,
    language :: Language
  }
  deriving (Generic, Show, ToJSON, FromJSON)

-- | Attach paired-tax info to each Fare row (by 'groupId') so the renderer
-- can show inline VAT. Returns ALL items unchanged in count — only Fare items
-- get a non-zero taxAmount; Tax/Adjustment/external items pass through with 0.
-- JL filters per section ('itemType == Fare' for main table, 'isExternalCharge'
-- for externals, etc.) handle which items render where.
attachTaxToFares :: [InvoiceLineItem] -> [(InvoiceLineItem, HighPrecMoney)]
attachTaxToFares items =
  let taxByGroup =
        Map.fromListWith
          (+)
          [ (gid, li.lineTotal)
            | li <- items,
              li.itemType == Just Tax,
              Just gid <- [li.groupId]
          ]
      taxFor li
        | li.itemType == Just Fare = fromMaybe 0 (li.groupId >>= (`Map.lookup` taxByGroup))
        | otherwise = 0
   in [(li, taxFor li) | li <- items]

stampLanguage :: Language -> [(InvoiceLineItem, HighPrecMoney)] -> [RenderLineItem]
stampLanguage lang = map go
  where
    go (li, taxAmt) =
      RenderLineItem
        { description = li.description,
          descriptionType = li.descriptionType,
          amount = li.lineTotal,
          taxAmount = taxAmt,
          groupId = li.groupId,
          isExternalCharge = li.isExternalCharge,
          itemType = li.itemType,
          language = lang
        }

containsAdjustments :: [InvoiceLineItem] -> Bool
containsAdjustments = any (\li -> li.itemType == Just Adjustment)

paymentModeUpper :: Maybe Text -> Maybe Text
paymentModeUpper = fmap T.toUpper

templateText :: Text -> Text
templateText key = "{#" <> key <> "#}"

buildTemplate :: [(Text, Text)] -> Text -> Text
buildTemplate paramVars tmpl =
  foldl' (\msg (k, v) -> T.replace (templateText k) v msg) tmpl paramVars

-- | 'buildTemplate' + strip leftover @{#...#}@ placeholders, returning their
-- names so callers can log JL/template contract drift.
buildTemplateChecked :: [(Text, Text)] -> Text -> (Text, [Text])
buildTemplateChecked paramVars tmpl = stripResiduals (buildTemplate paramVars tmpl)
  where
    stripResiduals :: Text -> (Text, [Text])
    stripResiduals input =
      case T.breakOn "{#" input of
        (before, rest)
          | T.null rest -> (before, [])
          | otherwise ->
            let afterOpen = T.drop 2 rest
             in case T.breakOn "#}" afterOpen of
                  (key, closing)
                    | T.null closing -> (before <> rest, [])
                    | otherwise ->
                      let (tailCleaned, tailLeftovers) = stripResiduals (T.drop 2 closing)
                       in (before <> tailCleaned, key : tailLeftovers)

-- | Format every leaf for display: Number → @"%.2f"@; ISO8601 String →
-- @"%d.%m.%Y"@; other String → HTML-escaped; Null → @""@; Bool → @"true"@\/@"false"@.
cleanJson :: DT.TimeZone -> A.Value -> A.Value
cleanJson tz = go
  where
    go (A.Number n) = A.String (T.pack (printf "%.2f" (realToFrac n :: Double)))
    go (A.String s) = case iso8601ParseM (T.unpack s) :: Maybe UTCTime of
      Just utc -> A.String (T.pack (DT.formatTime DT.defaultTimeLocale "%d.%m.%Y" (DT.utcToLocalTime tz utc)))
      Nothing -> A.String (escHtml s)
    go A.Null = A.String ""
    go (A.Bool b) = A.String (if b then "true" else "false")
    go (A.Array xs) = A.Array (fmap go xs)
    go (A.Object o) = A.Object (KM.map go o)

escHtml :: Text -> Text
escHtml = T.concatMap $ \case
  '<' -> "&lt;"
  '>' -> "&gt;"
  '&' -> "&amp;"
  '"' -> "&quot;"
  '\'' -> "&#x27;"
  c -> T.singleton c

renderLineItemsHtml :: Text -> [A.Value] -> Text
renderLineItemsHtml rowTpl = T.concat . mapMaybe renderOne
  where
    renderOne (A.Object o) = Just (buildTemplate (flatten (A.Object o)) rowTpl)
    renderOne _ = Nothing

-- | Inputs callers gather before calling the orchestrator. Many fields are
-- 'Maybe' because not every invoice type uses every field (e.g. only
-- AggregatedCommission needs seller/recipient business IDs).
data BuildInvoiceContextInput = BuildInvoiceContextInput
  { language :: Language,
    logoUrl :: Maybe Text,
    sellerTradeName :: Maybe Text,
    appName :: Maybe Text,
    invoice :: FInvoice.Invoice,
    lineItems :: [InvoiceLineItem],
    mbTaxTxn :: Maybe FITxn.IndirectTaxTransaction,
    mbPaymentMode :: Maybe Text,
    mbCardBrand :: Maybe Text,
    mbCardLastFour :: Maybe Text,
    mbRecipientBusinessId :: Maybe Text,
    mbSellerBusinessId :: Maybe Text,
    mbSellerVatNumber :: Maybe Text
  }

buildInvoiceContext :: BuildInvoiceContextInput -> InvoiceContext
buildInvoiceContext BuildInvoiceContextInput {..} =
  InvoiceContext
    { invoiceNumber = invoice.invoiceNumber,
      referenceInvoiceNumber = invoice.referenceInvoiceNumber,
      issuedAt = invoice.issuedAt,
      dueAt = invoice.dueAt,
      invoiceType = invoice.invoiceType,
      language = language,
      currency = invoice.currency,
      currencyCode = currencyText invoice.currency,
      merchantId = invoice.merchantId,
      merchantShortId = invoice.merchantId,
      paymentMode = paymentModeUpper mbPaymentMode,
      hasAdjustments = containsAdjustments lineItems,
      issuedToName = invoice.issuedToName,
      issuedToAddress = invoice.issuedToAddress,
      supplierName = invoice.supplierName,
      supplierAddress = invoice.supplierAddress,
      supplierGSTIN = invoice.supplierGSTIN,
      supplierTaxNo = invoice.supplierTaxNo,
      issuedByName = invoice.issuedByName,
      issuedByAddress = invoice.issuedByAddress,
      merchantGstin = invoice.merchantGstin,
      sellerTradeName = sellerTradeName,
      mbRecipientBusinessId = mbRecipientBusinessId,
      mbSellerBusinessId = mbSellerBusinessId,
      mbSellerVatNumber = mbSellerVatNumber,
      logoUrl = logoUrl,
      appName = appName,
      periodStart = invoice.periodStart,
      periodEnd = invoice.periodEnd,
      taxTxnRate = mbTaxTxn >>= (.taxRate),
      taxTxnGstRate = (.gstRate) <$> mbTaxTxn,
      cardBrand = mbCardBrand,
      cardLastFour = mbCardLastFour,
      lineItems = stampLanguage language (attachTaxToFares lineItems)
    }
  where
    currencyText :: Currency -> Text
    currencyText EUR = "EUR"
    currencyText INR = "INR"
    currencyText USD = "USD"

-- | Flatten an Object to @[(dot.path, value)]@ pairs; Arrays skipped
-- (handled by 'renderLineItemsHtml').
flatten :: A.Value -> [(Text, Text)]
flatten = go ""
  where
    go prefix (A.Object o) =
      concatMap (\(k, v) -> go (joined prefix (AK.toText k)) v) (KM.toList o)
    go prefix (A.String s) = [(prefix, s)]
    go _ (A.Array _) = []
    go prefix _ = [(prefix, "")]
    joined "" k = k
    joined p k = p <> "." <> k
