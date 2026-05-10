module Lib.Finance.Invoice.PdfService
  ( -- * Config
    InvoicePdfConfig (..),
    InvoiceLocale (..),
    countryToLocale,
    languageToLocale,

    -- * Re-exported from beckn-spec
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

import Control.Applicative ((<|>))
import qualified Data.Aeson as Aeson
import Data.List (groupBy, sortOn)
import qualified Data.Text as T
import qualified Data.Time as DT
import Domain.Types.Invoice (DateOrTime (..), toUTCTimeFrom, toUTCTimeTo)
import qualified Kernel.External.Types as ExtTypes
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common (Currency (..), HighPrecMoney (..))
import Lib.Finance.Domain.Types.IndirectTaxTransaction (IndirectTaxTransaction (..))
import Lib.Finance.Domain.Types.Invoice (Invoice (..))
import Lib.Finance.Invoice.Interface (InvoiceLineItem (..), ItemType (..), LineItemDescription (..))
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

countryToLocale :: Context.Country -> InvoiceLocale
countryToLocale Context.Finland = FI
countryToLocale Context.Netherlands = NL
countryToLocale _ = EN

-- | Resolve invoice locale from a Person's preferred language; unsupported
-- languages and Nothing both fall back to English.
languageToLocale :: Maybe ExtTypes.Language -> InvoiceLocale
languageToLocale (Just ExtTypes.FINNISH) = FI
languageToLocale (Just ExtTypes.DUTCH) = NL
languageToLocale _ = EN

-- | Resolve a typed line-item description to its locale-specific display string.
renderLineItemDescription :: InvoiceLocale -> LineItemDescription -> Text
renderLineItemDescription locale = \case
  RideFarePostDiscount cur amt ->
    renderLineItemDescription locale RideFare
      <> " ("
      <> postDiscountSuffix locale
      <> " "
      <> currencyCode cur
      <> " "
      <> fmtMoneyNum cur amt
      <> ")"
  d -> case locale of
    EN -> renderEN d
    FI -> renderFI d
    NL -> renderNL d
  where
    postDiscountSuffix EN = "Post Discount"
    postDiscountSuffix FI = "Alennuksen jälkeen"
    postDiscountSuffix NL = "Na korting"

    renderEN = \case
      BaseFare -> "Base Fare"
      RideFare -> "Ride Fare"
      RideFareInclVat -> "Ride Fare (Incl. VAT)"
      RideFarePostDiscount _ _ -> "" -- handled above
      RideTax -> "Tax"
      TollFare -> "Toll Fare"
      TollCharges -> "Toll Charges"
      TollFareInclVat -> "Toll Fare (Incl. VAT)"
      TollTax -> "Toll Tax"
      TollChargesTax -> "Toll Charges Tax"
      ParkingCharges -> "Parking Charges"
      ParkingChargesInclVat -> "Parking Charges (Incl. VAT)"
      ParkingChargesTax -> "Parking Charges Tax"
      Tip -> "Tip"
      PlatformCommission -> "Platform Commission"
      CancellationFee -> "Cancellation Fee"
      CancellationFeeVat -> "Cancellation Fee VAT"
      CancellationFeeInclVat -> "Cancellation Fee (Incl. VAT)"
      CustomerCancellationFee -> "Customer Cancellation Fee"
      GstOnCancellationFee -> "GST on Cancellation Fee"
      DriverCancellationPenalty -> "Driver Cancellation Penalty"
      SubscriptionPlanFee -> "Subscription Plan Fee"
      Gst -> "GST"
      WalletTopup -> "Wallet Top-up"
      AirportCashRecharge -> "Airport Cash Recharge"
      CashbackOffer -> "Cashback Offer"
      VatInput -> "VAT Input"

    renderFI = \case
      BaseFare -> "Perusmaksu"
      RideFare -> "Matkan hinta"
      RideFareInclVat -> "Matkan hinta (sis. ALV)"
      RideFarePostDiscount _ _ -> ""
      RideTax -> "ALV"
      TollFare -> "Tiemaksu"
      TollCharges -> "Tiemaksut"
      TollFareInclVat -> "Tiemaksu (sis. ALV)"
      TollTax -> "Tiemaksun ALV"
      TollChargesTax -> "Tiemaksujen ALV"
      ParkingCharges -> "Pysäköintimaksut"
      ParkingChargesInclVat -> "Pysäköintimaksut (sis. ALV)"
      ParkingChargesTax -> "Pysäköintimaksujen ALV"
      Tip -> "Tippi"
      PlatformCommission -> "Sovelluksen palkkio"
      CancellationFee -> "Peruutusmaksu"
      CancellationFeeVat -> "Peruutusmaksun ALV"
      CancellationFeeInclVat -> "Peruutusmaksu (sis. ALV)"
      CustomerCancellationFee -> "Asiakkaan peruutusmaksu"
      GstOnCancellationFee -> "Peruutusmaksun ALV"
      DriverCancellationPenalty -> "Kuljettajan peruutussakko"
      SubscriptionPlanFee -> "Sovellustilauksen maksu"
      Gst -> "ALV"
      WalletTopup -> "Lompakon lataus"
      AirportCashRecharge -> "Lentokentän käteislataus"
      CashbackOffer -> "Käteishyvitys"
      VatInput -> "ALV-vähennys"

    renderNL = \case
      BaseFare -> "Basisritprijs"
      RideFare -> "Ritprijs"
      RideFareInclVat -> "Ritprijs (incl. BTW)"
      RideFarePostDiscount _ _ -> ""
      RideTax -> "BTW"
      TollFare -> "Tolgeld"
      TollCharges -> "Tolgelden"
      TollFareInclVat -> "Tolgeld (incl. BTW)"
      TollTax -> "BTW op tol"
      TollChargesTax -> "BTW op tolgelden"
      ParkingCharges -> "Parkeerkosten"
      ParkingChargesInclVat -> "Parkeerkosten (incl. BTW)"
      ParkingChargesTax -> "BTW op parkeerkosten"
      Tip -> "Fooi"
      PlatformCommission -> "Platformcommissie"
      CancellationFee -> "Annuleringskosten"
      CancellationFeeVat -> "BTW op annuleringskosten"
      CancellationFeeInclVat -> "Annuleringskosten (incl. BTW)"
      CustomerCancellationFee -> "Annuleringskosten klant"
      GstOnCancellationFee -> "BTW op annuleringskosten"
      DriverCancellationPenalty -> "Annuleringssanctie chauffeur"
      SubscriptionPlanFee -> "Abonnementskosten"
      Gst -> "BTW"
      WalletTopup -> "Wallet bijladen"
      AirportCashRecharge -> "Luchthaven contant bijladen"
      CashbackOffer -> "Cashback aanbieding"
      VatInput -> "BTW Voorbelasting"

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

-- | Throws on decode failure or when any item lacks the typed 'itemType'.
-- List endpoints wrap this in 'withTryCatch' to skip bad rows; single-invoice
-- endpoints propagate the 500.
parseLineItems :: Aeson.Value -> [InvoiceLineItem]
parseLineItems val =
  case Aeson.fromJSON val of
    Aeson.Success items
      | all (isJust . (.itemType)) items -> items
      | otherwise -> error "parseLineItems: encountered line item without typed itemType"
    Aeson.Error err -> error $ "parseLineItems: invoice lineItems JSON failed to decode: " <> T.pack err

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
          "<div class='inv-number' style='font-weight:700;color:#000000'>",
          lbls.invoiceNumberLabel,
          " ",
          escHtml inv.invoiceNumber,
          "</div>",
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
      "<div class='party-name' style='font-weight:700;color:#000000'>",
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
renderLineItemsTable cfg pdfData lbls =
  let inv = pdfData.financeInvoice
      cur = inv.currency
      rows = buildRows cfg.locale pdfData lbls
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
    th cls lbl = "<th style='font-weight:700;color:#000000'" <> (if T.null cls then "" else " class='" <> cls <> "'") <> ">" <> lbl <> "</th>"

-- | Display the typed 'descriptionType' if set; otherwise fall back to the
-- raw 'description' Text (for old DB rows that pre-date the typed field).
displayItemDescription :: InvoiceLocale -> InvoiceLineItem -> Text
displayItemDescription locale item =
  maybe item.description (renderLineItemDescription locale) item.descriptionType

-- | Pair each Fare with its (single) Tax by shared groupId. Standalone fares get Nothing.
pairByGroupId :: [InvoiceLineItem] -> [(InvoiceLineItem, Maybe InvoiceLineItem)]
pairByGroupId items =
  let fares = [item | item <- items, item.itemType == Just Fare]
      taxes = [item | item <- items, item.itemType == Just Tax]
      taxFor Nothing = Nothing
      taxFor target = find (\taxItem -> taxItem.groupId == target) taxes
   in [(fareItem, taxFor fareItem.groupId) | fareItem <- fares]

-- | Main table rows. Adjustments and externals render in 'renderTotals' instead.
buildRows :: InvoiceLocale -> InvoicePdfData -> Labels -> [Text]
buildRows locale pdfData _lbls =
  let inv = pdfData.financeInvoice
      cur = inv.currency
      mainTableItems =
        [item | item <- pdfData.parsedLineItems, not item.isExternalCharge, item.itemType == Just Fare || item.itemType == Just Tax]
      pairs = pairByGroupId mainTableItems
   in map (renderFareTaxRow locale cur) pairs

renderFareTaxRow :: InvoiceLocale -> Currency -> (InvoiceLineItem, Maybe InvoiceLineItem) -> Text
renderFareTaxRow locale cur (fare, mbTax) =
  let taxAmount = maybe 0 (.lineTotal) mbTax
      hasTax = isJust mbTax && fare.lineTotal > 0
      vatPctText = if hasTax then fmtPct (realToFrac (taxAmount / fare.lineTotal) * 100.0) else ""
      vatAmtText = maybe "" (const (fmtMoneyNum cur taxAmount)) mbTax
   in tr
        [ td (displayItemDescription locale fare),
          td' "n" (fmtMoneyNum cur fare.lineTotal),
          td' "n" vatPctText,
          td' "n" vatAmtText,
          td' "n" (fmtMoneyNum cur (fare.lineTotal + taxAmount))
        ]

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

-- | Ladder: Taxable → VAT → externals → Total → adjustments → Net Total → payment mode.
-- Net Total only renders when adjustments exist.
renderTotals :: InvoicePdfConfig -> InvoicePdfData -> Labels -> Text
renderTotals cfg pdfData lbls =
  let inv = pdfData.financeInvoice
      cur = inv.currency
      locale = cfg.locale
      mbTax = pdfData.mbTaxTxn
      items = pdfData.parsedLineItems
      tableFares = [item | item <- items, item.itemType == Just Fare, not item.isExternalCharge]
      tableTaxes = [item | item <- items, item.itemType == Just Tax, not item.isExternalCharge]
      externalItems = filter (.isExternalCharge) items
      adjustmentItems = [item | item <- items, item.itemType == Just Adjustment]
      taxableSum = sum (map (.lineTotal) tableFares)
      taxSum = sum (map (.lineTotal) tableTaxes)
      externalsSum = sum (map (.lineTotal) externalItems)
      adjustmentsSum = sum (map (.lineTotal) adjustmentItems)
      totalLine = taxableSum + taxSum + externalsSum
      netTotalLine = totalLine + adjustmentsSum
      -- Prefer the explicit tax-transaction rate; fall back to computing from sums.
      vatPctText = case mbTax of
        Just taxTxn | taxableSum > 0 -> fmtPct (fromMaybe taxTxn.gstRate taxTxn.taxRate)
        _
          | taxableSum > 0 && taxSum > 0 ->
            fmtPct (realToFrac (taxSum / taxableSum) * 100.0)
        _ -> ""
      vatLabel =
        lbls.vatLabel
          <> if T.null vatPctText then "" else " (" <> vatPctText <> "%)"
      paymentMethodStr = inv.paymentMode <|> pdfData.mbCardInfo
   in T.concat
        [ "<table class='totals'>",
          totRow lbls.taxablePriceLabel (fmtMoneyNum cur taxableSum),
          if taxSum > 0 then totRow vatLabel (fmtMoneyNum cur taxSum) else "",
          T.concat (map (renderExternalRow locale cur) externalItems),
          "<tr class='tot-row grand'><td style='font-weight:700;color:#000000'>",
          lbls.totalInclVatLabel,
          "</td><td class='tot-val' style='font-weight:700;color:#000000'>",
          fmtMoneyNum cur totalLine,
          "</td></tr>",
          T.concat (map (renderAdjustmentRow locale cur) adjustmentItems),
          if not (null adjustmentItems)
            then
              let invoicedLabel = lbls.invoicedValueLabel <> maybe "" (\pm -> " " <> pm) paymentMethodStr <> ":"
               in "<tr class='tot-row grand'><td style='font-weight:700;color:#000000'>"
                    <> escHtml invoicedLabel
                    <> "</td><td class='tot-val' style='font-weight:700;color:#000000'>"
                    <> fmtMoneyNum cur netTotalLine
                    <> "</td></tr>"
            else maybe "" (\pm -> "<tr class='tot-row grand'><td style='font-weight:700;color:#000000'>" <> escHtml (lbls.invoicedValueLabel <> " " <> pm <> ":") <> "</td><td class='tot-val' style='font-weight:700;color:#000000'>" <> fmtMoneyNum cur totalLine <> "</td></tr>") paymentMethodStr,
          "</table>"
        ]
  where
    totRow lbl val =
      "<tr class='tot-row'><td>" <> lbl <> "</td><td class='tot-val'>" <> val <> "</td></tr>"

renderExternalRow :: InvoiceLocale -> Currency -> InvoiceLineItem -> Text
renderExternalRow locale cur item =
  "<tr class='tot-row'><td>"
    <> escHtml (displayItemDescription locale item)
    <> "</td><td class='tot-val'>"
    <> fmtMoneyNum cur item.lineTotal
    <> "</td></tr>"

renderAdjustmentRow :: InvoiceLocale -> Currency -> InvoiceLineItem -> Text
renderAdjustmentRow locale cur item =
  "<tr class='tot-row'><td>"
    <> escHtml (displayItemDescription locale item)
    <> "</td><td class='tot-val'>"
    <> fmtMoneyNum cur item.lineTotal
    <> "</td></tr>"

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
    totalInclVatLabel :: Text,
    invoicedValueLabel :: Text
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
      totalInclVatLabel = "Total incl. VAT",
      invoicedValueLabel = "Invoiced Value"
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
      totalInclVatLabel = "Yhteens\228 sis. ALV (EUR)",
      invoicedValueLabel = "Laskutettu arvo"
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
      totalInclVatLabel = "Totaal incl. BTW (EUR)",
      invoicedValueLabel = "Gefactureerde waarde"
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
          "<div class='inv-number' style='font-weight:700;color:#000000'>",
          lbls.invoiceNumberLabel,
          " ",
          escHtml inv.invoiceNumber,
          "</div>",
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
