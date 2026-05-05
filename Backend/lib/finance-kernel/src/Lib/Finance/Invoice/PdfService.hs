module Lib.Finance.Invoice.PdfService
  ( -- * Config
    InvoicePdfConfig (..),
    InvoiceLocale (..),
    DateOrTime (..),
    toUTCTimeFrom,
    toUTCTimeTo,

    -- * PDF data
    InvoicePdfData (..),
    buildInvoicePdfData,

    -- * HTML rendering
    renderInvoiceHtml,
    renderBatchInvoiceHtml,
    parseLineItems,
  )
where

import qualified Data.Aeson as Aeson
import Data.List (groupBy, sortOn)
import Data.OpenApi (ToParamSchema (..))
import qualified Data.Text as T
import Data.Time (UTCTime (..))
import qualified Data.Time as DT
import Kernel.Prelude
import Kernel.Types.Common (Currency (..), HighPrecMoney (..))
import Lib.Finance.Domain.Types.IndirectTaxTransaction (IndirectTaxTransaction (..))
import Lib.Finance.Domain.Types.Invoice (Invoice (..), InvoiceType (..))
import Lib.Finance.Invoice.Interface (InvoiceLineItem (..))
import Servant (FromHttpApiData (..), ToHttpApiData (..))
import Text.Printf (printf)

-- ---------------------------------------------------------------------------
-- Config
-- ---------------------------------------------------------------------------

data InvoiceLocale = EN | FI | NL
  deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

data InvoicePdfConfig = InvoicePdfConfig
  { locale :: InvoiceLocale,
    timezone :: DT.TimeZone,
    logoUrl :: Maybe Text
  }
  deriving (Eq, Show, Generic)

-- ---------------------------------------------------------------------------
-- DateOrTime — accept both date-only and full UTC timestamp as query params
-- ---------------------------------------------------------------------------

data DateOrTime
  = DateOnly DT.Day
  | DateTime UTCTime
  deriving (Eq, Show, Generic)

instance FromHttpApiData DateOrTime where
  parseQueryParam t =
    case parseQueryParam t of
      Right utc -> Right (DateTime utc)
      Left _ -> DateOnly <$> parseQueryParam t

instance ToHttpApiData DateOrTime where
  toQueryParam (DateTime t) = toQueryParam t
  toQueryParam (DateOnly d) = toQueryParam d

instance ToParamSchema DateOrTime where
  toParamSchema _ = toParamSchema (Proxy :: Proxy Text)

toUTCTimeFrom :: DateOrTime -> UTCTime
toUTCTimeFrom (DateOnly day) = UTCTime day 0
toUTCTimeFrom (DateTime t) = t

-- | Exclusive upper bound: start of next day for date-only
toUTCTimeTo :: DateOrTime -> UTCTime
toUTCTimeTo (DateOnly day) = UTCTime (DT.addDays 1 day) 0
toUTCTimeTo (DateTime t) = t

-- ---------------------------------------------------------------------------
-- InvoicePdfData — all data needed to render the invoice PDF
-- ---------------------------------------------------------------------------

data InvoicePdfData = InvoicePdfData
  { financeInvoice :: Invoice,
    parsedLineItems :: [InvoiceLineItem],
    -- | from indirect_tax_transaction
    mbTaxTxn :: Maybe IndirectTaxTransaction,
    -- | paymentMethodType from payment_transaction table
    mbPaymentMethodType :: Maybe Text,
    -- | cardBrand + cardLastFourDigits from payment_transaction
    mbCardInfo :: Maybe Text
  }

-- | Smart constructor — derive all the computed fields callers need
buildInvoicePdfData ::
  Invoice ->
  [InvoiceLineItem] ->
  Maybe IndirectTaxTransaction ->
  -- | paymentMethodType
  Maybe Text ->
  -- | cardBrand
  Maybe Text ->
  -- | cardLastFourDigits
  Maybe Text ->
  InvoicePdfData
buildInvoicePdfData inv items mbTax mbPayType mbBrand mbLast4 =
  InvoicePdfData
    { financeInvoice = inv,
      parsedLineItems = items,
      mbTaxTxn = mbTax,
      mbPaymentMethodType = mbPayType,
      mbCardInfo = buildCardInfo mbPayType mbBrand mbLast4
    }
  where
    buildCardInfo Nothing _ _ = Nothing
    buildCardInfo (Just pt) brand last4 =
      Just $ pt <> maybe "" (\b -> " " <> b) brand <> maybe "" (\l -> " \x2022\x2022\x2022\x2022 " <> l) last4

-- ---------------------------------------------------------------------------
-- Line item parsing
-- ---------------------------------------------------------------------------

parseLineItems :: Aeson.Value -> [InvoiceLineItem]
parseLineItems val =
  case Aeson.fromJSON val of
    Aeson.Success items -> items
    Aeson.Error _ -> []

-- ---------------------------------------------------------------------------
-- HTML rendering — pure, no IO
-- ---------------------------------------------------------------------------

-- | Render a finance Invoice as an HTML string ready for wkhtmltopdf.
--
--   Layout (from spec):
--   top-right  : logo + invoice number
--   left       : Recipient (issuedToName) + Start (issuedToAddress)
--   right      : Supplier name/address/gstin/tax_no
--   table      : Title | Amount (EUR) | ALV% | ALV (EUR) | Total (EUR)
--     Ride      → "Price of the Trip"
--     if externalCharges > 0 → second row "Toll charges"
--     Subscription → "App Subscriptions" / "Commission"
--   totals     : Taxable Trip Price | VAT rows | Total incl. VAT | Tips | Invoiced Value + payment
renderInvoiceHtml :: InvoicePdfConfig -> InvoicePdfData -> Text
renderInvoiceHtml cfg pdfData =
  let lbls = localeLabels cfg.locale
      inv = pdfData.financeInvoice
   in T.concat
        [ "<!DOCTYPE html><html><head>",
          "<meta charset='UTF-8'>",
          "<title>",
          escHtml (lbls.invoiceTitle <> " " <> inv.invoiceNumber),
          "</title>",
          "<style>",
          invoiceCss,
          "</style>",
          "</head><body><div class='wrap'>",
          renderHeader cfg pdfData lbls,
          renderParties inv lbls,
          renderFromLocation inv,
          renderLineItemsTable cfg pdfData lbls,
          renderTotals cfg pdfData lbls,
          "</div></body></html>"
        ]

-- ---------------------------------------------------------------------------
-- Header
-- ---------------------------------------------------------------------------

renderHeader :: InvoicePdfConfig -> InvoicePdfData -> Labels -> Text
renderHeader cfg pdfData lbls =
  let inv = pdfData.financeInvoice
   in T.concat
        [ "<table class='header'><tr>",
          "<td>",
          maybe "" (\u -> "<img src='" <> escHtml u <> "' alt='' class='logo'>") cfg.logoUrl,
          "</td>",
          "<td class='header-right-cell'>",
          "<div class='inv-number'><b>",
          lbls.invoiceNumberLabel,
          " ",
          escHtml inv.invoiceNumber,
          "</b></div>",
          "<div class='inv-date'>",
          lbls.dateLabel,
          ": ",
          formatDate cfg.timezone inv.issuedAt,
          "</div>",
          maybe "" (\d -> "<div class='inv-date'>" <> lbls.dueDateLabel <> ": " <> formatDate cfg.timezone d <> "</div>") inv.dueAt,
          "</td>",
          "</tr></table>"
        ]

-- ---------------------------------------------------------------------------
-- Parties
-- ---------------------------------------------------------------------------

renderParties :: Invoice -> Labels -> Text
renderParties inv lbls =
  T.concat
    [ "<table class='parties'><tr>",
      "<td class='party-left'>",
      "<div class='party-lbl-plain'>",
      lbls.recipientLabel,
      ":</div>",
      "<div class='party-name'>",
      escHtml (fromMaybe "" inv.issuedToName),
      "</div>",
      "</td>",
      "<td class='party-right'>",
      "<div class='party-name'>",
      escHtml (fromMaybe "" inv.supplierName),
      "</div>",
      maybe "" (\a -> "<div class='party-addr'>" <> escHtml a <> "</div>") inv.supplierAddress,
      maybe "" (\g -> "<div class='party-tax'>" <> lbls.gstinLabel <> " " <> escHtml g <> "</div>") inv.supplierGSTIN,
      maybe "" (\t -> "<div class='party-tax'>" <> lbls.vatNumberLabel <> " " <> escHtml t <> "</div>") inv.supplierTaxNo,
      "</td>",
      "</tr></table>"
    ]

renderFromLocation :: Invoice -> Text
renderFromLocation inv =
  case inv.issuedToAddress of
    Nothing -> ""
    Just addr ->
      "<div class='from-location'>" <> escHtml addr <> "</div>"

-- ---------------------------------------------------------------------------
-- Line items table
-- Title | Amount (EUR) | ALV% | ALV (EUR) | Total (EUR)
-- ---------------------------------------------------------------------------

renderLineItemsTable :: InvoicePdfConfig -> InvoicePdfData -> Labels -> Text
renderLineItemsTable _cfg pdfData lbls =
  let inv = pdfData.financeInvoice
      cur = inv.currency
      rows = buildRows pdfData lbls
   in T.concat
        [ "<table>",
          "<thead><tr>",
          th "" lbls.titleLabel,
          th "n" (lbls.amountLabel <> " (" <> currencyCode cur <> ")"),
          th "n" (lbls.vatPctLabel <> " (%)"),
          th "n" (lbls.vatAmtLabel <> " (" <> currencyCode cur <> ")"),
          th "n" (lbls.totalLabel <> " (" <> currencyCode cur <> ")"),
          "</tr></thead><tbody>",
          T.concat rows,
          "</tbody></table>"
        ]
  where
    th cls lbl = "<th" <> (if T.null cls then "" else " class='" <> cls <> "'") <> ">" <> lbl <> "</th>"

-- | Build the table rows from line items + tax transaction, following the spec mapping.
buildRows :: InvoicePdfData -> Labels -> [Text]
buildRows pdfData lbls =
  let inv = pdfData.financeInvoice
      cur = inv.currency
      mbTax = pdfData.mbTaxTxn
      -- Filter out internal breakdown items (Ride Fare / Ride Tax) and external charges
      mainItems =
        filter
          ( \i ->
              not i.isExternalCharge
                && not (isRideFareItem i)
                && not (isRideTaxItem i)
                && not (isTipItem i)
          )
          pdfData.parsedLineItems
      tollItems = filter (.isExternalCharge) pdfData.parsedLineItems
      -- Fallback VAT from line items when no indirect_tax_transaction row exists
      mbRideFare = find isRideFareItem pdfData.parsedLineItems
      mbRideTax = find isRideTaxItem pdfData.parsedLineItems
      mainTitle = case inv.invoiceType of
        Ride -> lbls.rideLabel
        RideCancellation -> lbls.cancellationLabel
        SubscriptionPurchase -> lbls.subscriptionLabel
      -- ALV% for main item: from indirect_tax_transaction, or derived from Ride Fare/Ride Tax items
      mainVatPct = case mbTax of
        Just t -> Just $ fmtPct (fromMaybe t.gstRate t.taxRate)
        Nothing -> do
          fare <- mbRideFare
          tax <- mbRideTax
          guard (fare.lineTotal > 0)
          pure $ fmtPct (realToFrac (tax.lineTotal / fare.lineTotal) * 100.0)
      -- ALV (EUR) for main item: from indirect_tax_transaction, or from Ride Tax item
      mainVatAmt = case mbTax of
        Just t -> Just $ fmtMoneyNum cur t.totalGstAmount
        Nothing -> fmtMoneyNum cur . (.lineTotal) <$> mbRideTax
      -- Amount (subtotal): from indirect_tax_transaction subtotal, or Ride Fare item, or inv.subtotal
      mainAmountVal = case mbTax of
        Just _ -> inv.subtotal
        Nothing -> maybe inv.subtotal (.lineTotal) mbRideFare
      mainAmount = fmtMoneyNum cur mainAmountVal
      mainTotal = fmtMoneyNum cur inv.totalAmount
      mainRow =
        tr
          [ td mainTitle,
            td' "n" mainAmount,
            td' "n" (fromMaybe "" mainVatPct),
            td' "n" (fromMaybe "" mainVatAmt),
            td' "n" mainTotal
          ]
      -- Toll charges rows — only if externalCharges > 0
      tollRows = map (buildTollRow cur mbTax lbls) tollItems
      -- Tips row — from line items with description "Tips"
      tipRows = map (buildTipRow cur) (filter isTipItem pdfData.parsedLineItems)
   in [mainRow] <> tollRows <> tipRows <> map (buildGenericRow cur) mainItems

buildTollRow :: Currency -> Maybe IndirectTaxTransaction -> Labels -> InvoiceLineItem -> Text
buildTollRow cur mbTax lbls item =
  let tollAmt = fmtMoneyNum cur item.unitPrice
      -- external_charges_tax_rate from indirect_tax_transaction (not yet implemented per spec, show blank)
      tollVatPct = mbTax >>= \_ -> Nothing
      tollVatAmt = mbTax >>= \t -> t.externalCharges >>= \_ -> Just $ fmtMoneyNum cur (t.totalGstAmount - t.totalGstAmount) -- placeholder: external_charges_tax_amount
      tollTotal = fmtMoneyNum cur item.lineTotal
   in tr
        [ td lbls.tollChargesLabel,
          td' "n" tollAmt,
          td' "n" (fromMaybe "" tollVatPct),
          td' "n" (fromMaybe "" tollVatAmt),
          td' "n" tollTotal
        ]

buildTipRow :: Currency -> InvoiceLineItem -> Text
buildTipRow cur item =
  tr
    [ td item.description,
      td' "n" (fmtMoneyNum cur item.unitPrice),
      td' "n" "",
      td' "n" "",
      td' "n" (fmtMoneyNum cur item.lineTotal)
    ]

buildGenericRow :: Currency -> InvoiceLineItem -> Text
buildGenericRow cur item =
  tr
    [ td item.description,
      td' "n" (fmtMoneyNum cur item.unitPrice),
      td' "n" "",
      td' "n" "",
      td' "n" (fmtMoneyNum cur item.lineTotal)
    ]

isTipItem :: InvoiceLineItem -> Bool
isTipItem i = T.toLower i.description == "tips" || T.toLower i.description == "tip"

isRideFareItem :: InvoiceLineItem -> Bool
isRideFareItem i = T.toLower i.description == "ride fare"

isRideTaxItem :: InvoiceLineItem -> Bool
isRideTaxItem i = T.toLower i.description == "ride tax"

tr :: [Text] -> Text
tr cells = "<tr>" <> T.concat cells <> "</tr>"

td :: Text -> Text
td t = "<td>" <> escHtml t <> "</td>"

td' :: Text -> Text -> Text
td' cls t = "<td class='" <> cls <> "'>" <> escHtml t <> "</td>"

-- ---------------------------------------------------------------------------
-- Totals block (right-aligned)
-- Taxable Trip Price | VAT (rate%) | Toll charges | VAT (toll) | Total incl. VAT | Tips | Invoiced Value
-- ---------------------------------------------------------------------------

renderTotals :: InvoicePdfConfig -> InvoicePdfData -> Labels -> Text
renderTotals cfg pdfData lbls =
  let inv = pdfData.financeInvoice
      cur = inv.currency
      mbTax = pdfData.mbTaxTxn
      -- Taxable Trip Price = subtotal, or Ride Fare item when no tax transaction
      taxableAmtVal = case mbTax of
        Just _ -> inv.subtotal
        Nothing -> maybe inv.subtotal (.lineTotal) (find isRideFareItem pdfData.parsedLineItems)
      taxableAmt = fmtMoneyNum cur taxableAmtVal
      -- Fallback VAT from line items when no indirect_tax_transaction row exists
      mbRideFare = find isRideFareItem pdfData.parsedLineItems
      mbRideTax = find isRideTaxItem pdfData.parsedLineItems
      -- VAT label: "VAT (14%)" — rate from tax_rate / gst_rate, or derived from line items
      vatPct = case mbTax of
        Just t -> fmtPct (fromMaybe t.gstRate t.taxRate)
        Nothing -> fromMaybe "" $ do
          fare <- mbRideFare
          tax <- mbRideTax
          guard (fare.lineTotal > 0)
          pure $ fmtPct (realToFrac (tax.lineTotal / fare.lineTotal) * 100.0)
      vatLabel = lbls.vatLabel <> if T.null vatPct then "" else " (" <> vatPct <> "%)"
      -- VAT EUR = total_gst_amount, or from Ride Tax line item
      vatAmt = case mbTax of
        Just t -> fmtMoneyNum cur t.totalGstAmount
        Nothing -> maybe "" (fmtMoneyNum cur . (.lineTotal)) mbRideTax
      -- Toll charges (only if externalCharges > 0)
      tollItems = filter (.isExternalCharge) pdfData.parsedLineItems
      -- Tips
      tipItems = filter isTipItem pdfData.parsedLineItems
      tipTotal = sum $ map (.lineTotal) tipItems
      -- Total incl. VAT = total_amount from finance_invoice
      grandTotal = fmtMoneyNum cur inv.totalAmount
      -- Invoiced Value = Total incl. VAT + Tips
      invoicedValue = fmtMoneyNum cur (inv.totalAmount + tipTotal)
   in T.concat
        [ "<table class='totals'>",
          totRow lbls.taxablePriceLabel taxableAmt,
          if T.null vatAmt then "" else totRow vatLabel vatAmt,
          T.concat (map (renderTollTotRow cfg cur mbTax lbls) tollItems),
          "<tr class='tot-row grand'><td>",
          lbls.totalInclVatLabel,
          "</td><td class='tot-val'>",
          grandTotal,
          "</td></tr>",
          if tipTotal > 0 then totRow lbls.tipsLabel (fmtMoneyNum cur tipTotal) else "",
          totRow lbls.invoicedValueLabel invoicedValue,
          maybe "" (\pm -> "<tr class='tot-row payment'><td></td><td class='tot-val payment-detail'>" <> escHtml pm <> "</td></tr>") pdfData.mbCardInfo,
          "</table>"
        ]
  where
    totRow lbl val =
      "<tr class='tot-row'><td>" <> lbl <> "</td><td class='tot-val'>" <> val <> "</td></tr>"

renderTollTotRow :: InvoicePdfConfig -> Currency -> Maybe IndirectTaxTransaction -> Labels -> InvoiceLineItem -> Text
renderTollTotRow _cfg cur _mbTax lbls item =
  let tollAmt = fmtMoneyNum cur item.unitPrice
   in "<tr class='tot-row'><td>" <> lbls.tollChargesLabel <> "</td><td class='tot-val'>" <> tollAmt <> "</td></tr>"

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

fmtMoneyNum :: Currency -> HighPrecMoney -> Text
fmtMoneyNum _cur (HighPrecMoney amt) =
  T.pack (printf "%.2f" (fromRational amt :: Double))

fmtPct :: Double -> Text
fmtPct d = T.pack (printf "%.1f" d)

currencyCode :: Currency -> Text
currencyCode EUR = "EUR"
currencyCode INR = "INR"
currencyCode USD = "USD"

formatDate :: DT.TimeZone -> UTCTime -> Text
formatDate tz t =
  T.pack $ DT.formatTime DT.defaultTimeLocale "%d.%m.%Y" (DT.utcToLocalTime tz t)

escHtml :: Text -> Text
escHtml = T.concatMap $ \case
  '<' -> "&lt;"
  '>' -> "&gt;"
  '&' -> "&amp;"
  '"' -> "&quot;"
  '\'' -> "&#x27;"
  c -> T.singleton c

-- ---------------------------------------------------------------------------
-- Labels
-- ---------------------------------------------------------------------------

data Labels = Labels
  { invoiceTitle :: Text,
    invoiceNumberLabel :: Text,
    dateLabel :: Text,
    dueDateLabel :: Text,
    recipientLabel :: Text,
    supplierLabel :: Text,
    vatNumberLabel :: Text,
    gstinLabel :: Text,
    titleLabel :: Text,
    amountLabel :: Text,
    vatPctLabel :: Text,
    vatAmtLabel :: Text,
    totalLabel :: Text,
    taxablePriceLabel :: Text,
    vatLabel :: Text,
    tollChargesLabel :: Text,
    totalInclVatLabel :: Text,
    tipsLabel :: Text,
    invoicedValueLabel :: Text,
    rideLabel :: Text,
    cancellationLabel :: Text,
    subscriptionLabel :: Text
  }

localeLabels :: InvoiceLocale -> Labels
localeLabels EN =
  Labels
    { invoiceTitle = "Invoice",
      invoiceNumberLabel = "Invoice No.",
      dateLabel = "Date",
      dueDateLabel = "Due Date",
      recipientLabel = "Recipient",
      supplierLabel = "Supplier",
      vatNumberLabel = "VAT No.",
      gstinLabel = "GSTIN",
      titleLabel = "Title",
      amountLabel = "Amount",
      vatPctLabel = "VAT",
      vatAmtLabel = "VAT",
      totalLabel = "Total",
      taxablePriceLabel = "Taxable Trip Price",
      vatLabel = "VAT",
      tollChargesLabel = "Toll charges",
      totalInclVatLabel = "Total incl. VAT",
      tipsLabel = "Tips",
      invoicedValueLabel = "Invoiced Value",
      rideLabel = "Price of the Trip",
      cancellationLabel = "Cancellation Fee",
      subscriptionLabel = "App Subscriptions"
    }
localeLabels FI =
  Labels
    { invoiceTitle = "Lasku",
      invoiceNumberLabel = "Laskun nro.",
      dateLabel = "P\228iv\228m\228\228r\228",
      dueDateLabel = "Er\228p\228iv\228",
      recipientLabel = "Vastaanottaja",
      supplierLabel = "Toimittaja",
      vatNumberLabel = "ALV-nro.",
      gstinLabel = "GSTIN",
      titleLabel = "Nimike",
      amountLabel = "M\228\228r\228",
      vatPctLabel = "ALV%",
      vatAmtLabel = "ALV",
      totalLabel = "Kokonaissumma",
      taxablePriceLabel = "Verollinen matkan hinta (EUR)",
      vatLabel = "ALV",
      tollChargesLabel = "Tiemaksut (EUR)",
      totalInclVatLabel = "Yhteens\228 sis. ALV (EUR)",
      tipsLabel = "Tippi",
      invoicedValueLabel = "Laskutettu arvo",
      rideLabel = "Matkan hinta",
      cancellationLabel = "Peruutusmaksu",
      subscriptionLabel = "Sovellustilaus"
    }
localeLabels NL =
  Labels
    { invoiceTitle = "Factuur",
      invoiceNumberLabel = "Factuurnr.",
      dateLabel = "Datum",
      dueDateLabel = "Vervaldatum",
      recipientLabel = "Ontvanger",
      supplierLabel = "Leverancier",
      vatNumberLabel = "BTW-nr.",
      gstinLabel = "GSTIN",
      titleLabel = "Omschrijving",
      amountLabel = "Bedrag",
      vatPctLabel = "BTW%",
      vatAmtLabel = "BTW",
      totalLabel = "Totaal",
      taxablePriceLabel = "Belastbare ritprijs (EUR)",
      vatLabel = "BTW",
      tollChargesLabel = "Tolgelden (EUR)",
      totalInclVatLabel = "Totaal incl. BTW (EUR)",
      tipsLabel = "Fooi",
      invoicedValueLabel = "Gefactureerde waarde",
      rideLabel = "Ritprijs",
      cancellationLabel = "Annuleringskosten",
      subscriptionLabel = "App-abonnement"
    }

-- ---------------------------------------------------------------------------
-- Batch rendering — aggregate N invoices into one page
-- ---------------------------------------------------------------------------

-- | For N > 1 invoices: aggregate all amounts into a single InvoicePdfData
--   (summing subtotals, totalAmounts, VAT amounts, merging line items by description),
--   use the last invoice's number/supplier/recipient, and show the date range.
renderBatchInvoiceHtml :: InvoicePdfConfig -> [InvoicePdfData] -> Text
renderBatchInvoiceHtml cfg pdfDatas =
  let aggregated = aggregatePdfDatas pdfDatas
      firstDate = minimum $ map (\d -> d.financeInvoice.issuedAt) pdfDatas
      lastDate = maximum $ map (\d -> d.financeInvoice.issuedAt) pdfDatas
      lbls = localeLabels cfg.locale
      inv = aggregated.financeInvoice
   in T.concat
        [ "<!DOCTYPE html><html><head>",
          "<meta charset='UTF-8'>",
          "<title>",
          escHtml (lbls.invoiceTitle <> " " <> inv.invoiceNumber),
          "</title>",
          "<style>",
          invoiceCss,
          "</style>",
          "</head><body><div class='wrap'>",
          renderBatchHeader cfg aggregated lbls firstDate lastDate,
          renderParties inv lbls,
          renderFromLocation inv,
          renderLineItemsTable cfg aggregated lbls,
          renderTotals cfg aggregated lbls,
          "</div></body></html>"
        ]

renderBatchHeader :: InvoicePdfConfig -> InvoicePdfData -> Labels -> UTCTime -> UTCTime -> Text
renderBatchHeader cfg pdfData lbls firstDate lastDate =
  let inv = pdfData.financeInvoice
   in T.concat
        [ "<table class='header'><tr>",
          "<td>",
          maybe "" (\u -> "<img src='" <> escHtml u <> "' alt='' class='logo'>") cfg.logoUrl,
          "</td>",
          "<td class='header-right-cell'>",
          "<div class='inv-number'><b>",
          lbls.invoiceNumberLabel,
          " ",
          escHtml inv.invoiceNumber,
          "</b></div>",
          "<div class='inv-date'>",
          lbls.dateLabel,
          ": ",
          formatDate cfg.timezone firstDate,
          " \x2013 ",
          formatDate cfg.timezone lastDate,
          "</div>",
          "</td>",
          "</tr></table>"
        ]

aggregatePdfDatas :: [InvoicePdfData] -> InvoicePdfData
aggregatePdfDatas [] = error "aggregatePdfDatas: empty list"
aggregatePdfDatas pdfDatas =
  let lastData = last pdfDatas
      lastInv = lastData.financeInvoice
      totalSubtotal = sum $ map (.financeInvoice.subtotal) pdfDatas
      totalAmount = sum $ map (.financeInvoice.totalAmount) pdfDatas
      mergedInv =
        lastInv
          { subtotal = totalSubtotal,
            totalAmount = totalAmount
          }
      mergedTaxTxn = case mapMaybe (.mbTaxTxn) pdfDatas of
        [] -> Nothing
        txns ->
          let headTxn = head txns
              summedGst = sum $ map (.totalGstAmount) txns
              summedCgst = sum $ map (.cgstAmount) txns
              summedSgst = sum $ map (.sgstAmount) txns
              summedTaxable = sum $ map (.taxableValue) txns
           in Just $ headTxn {totalGstAmount = summedGst, cgstAmount = summedCgst, sgstAmount = summedSgst, taxableValue = summedTaxable}
      mergedItems = mergeLineItems $ concatMap (.parsedLineItems) pdfDatas
   in InvoicePdfData
        { financeInvoice = mergedInv,
          parsedLineItems = mergedItems,
          mbTaxTxn = mergedTaxTxn,
          mbPaymentMethodType = lastData.mbPaymentMethodType,
          mbCardInfo = Nothing
        }

mergeLineItems :: [InvoiceLineItem] -> [InvoiceLineItem]
mergeLineItems items =
  map mergeGroup grouped
  where
    grouped = groupBy (\a b -> a.description == b.description) $ sortOn (.description) items
    mergeGroup [] = error "mergeLineItems: empty group"
    mergeGroup grp@(headItem : _) =
      headItem
        { unitPrice = sum $ map (.unitPrice) grp,
          lineTotal = sum $ map (.lineTotal) grp
        }

-- ---------------------------------------------------------------------------
-- CSS
-- ---------------------------------------------------------------------------

invoiceCss :: Text
invoiceCss =
  T.concat
    [ "* { margin:0; padding:0; box-sizing:border-box; }",
      "body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Arial, sans-serif;",
      "       color:#2d2d2d; background:#fff; padding:32px;",
      "       -webkit-print-color-adjust:exact !important; print-color-adjust:exact !important; }",
      ".wrap { max-width:820px; margin:0 auto; }",
      -- Header
      ".header { width:100%; border-collapse:collapse; border:none; margin-bottom:28px; }",
      ".header td { vertical-align:top; border:none; }",
      ".header td.header-right-cell { text-align:right; border:none; }",
      ".inv-type { font-size:13px; font-weight:600; color:#555; margin-top:4px; }",
      ".header-right { text-align:right; }",
      ".logo { height:44px; width:auto; margin-bottom:10px; display:block; }",
      ".inv-number { font-size:15px; font-weight:700; color:#2d2d2d; margin-bottom:10px; }",
      ".inv-date { font-size:13px; color:#2d2d2d; margin-top:2px; }",
      -- Parties
      ".parties { width:100%; border-collapse:collapse; border:none; margin-bottom:24px; padding:16px 0; }",
      ".party-right { text-align:right; vertical-align:top; border:none; }",
      ".party-left { vertical-align:top; width:50%; border:none; }",
      ".party-lbl { font-size:11px; text-transform:uppercase; letter-spacing:0.8px; color:#2d2d2d; margin-bottom:5px; }",
      ".party-lbl-plain { font-size:13px; color:#2d2d2d; margin-bottom:4px; }",
      ".party-name { font-size:15px; font-weight:600; color:#2d2d2d; margin-bottom:3px; }",
      ".party-addr { font-size:13px; color:#2d2d2d; line-height:1.6; }",
      ".party-tax { font-size:12px; color:#2d2d2d; margin-top:3px; }",
      -- From location
      ".from-location { font-size:13px; color:#2d2d2d; margin-bottom:20px; }",
      -- Table
      "table { width:100%; border-collapse:collapse; margin-bottom:24px; border:2px solid #2d2d2d; }",
      "th { font-size:13px; font-weight:700; color:#2d2d2d; text-transform:none; letter-spacing:0;",
      "     border:1px solid #2d2d2d; padding:8px 10px; text-align:left; background:#fff; }",
      "td { font-size:13px; padding:9px 10px; border:1px solid #2d2d2d; color:#2d2d2d; }",
      "th.n, td.n { text-align:right; }",
      -- Totals
      ".totals { width:400px; margin-left:auto; padding-top:24px; border-collapse:collapse; border:none; }",
      ".tot-row { width:100%; }",
      ".tot-row td { font-size:13px; color:#2d2d2d; padding:3px 0; border:none; }",
      ".tot-row td.tot-val { text-align:right; padding-left:24px; white-space:nowrap; border:none; }",
      ".tot-row.grand td { font-size:15px; font-weight:700; padding-top:12px; border:none; }",
      ".tot-row.payment td { font-size:12px; padding-top:4px; border:none; }",
      ".payment-detail { font-style:italic; }",
      "@media print { body { padding:16px; } }"
    ]
