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
    parseLineItems,
  )
where

import Control.Applicative ((<|>))
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Data.Time as DT
import Domain.Types.Invoice (DateOrTime (..), InvoiceType (..), toUTCTimeFrom, toUTCTimeTo)
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
      BaseFare -> "Price Of The Trip"
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
    -- | cardBrand and last 4 digits from payment_transaction; rendered into a
    -- locale-aware "Received via VISA ****8821" string in renderTotals.
    mbCardBrand :: Maybe Text,
    mbCardLastFour :: Maybe Text,
    -- | AggregatedCommission party metadata: recipient Y-tunnus, merchant
    -- Y-tunnus + VAT. Live-fetched (not persisted on the Invoice row);
    -- recipient VAT comes from inv.supplierTaxNo (handler maps it there).
    mbRecipientBusinessId :: Maybe Text,
    mbSellerBusinessId :: Maybe Text,
    mbSellerVatNumber :: Maybe Text
  }

-- | Smart constructor — derive all the computed fields callers need.
-- Positional (rather than record-update) for the trailing args so external
-- packages aren't blocked by 'DuplicateRecordFields'.
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
  -- | mbRecipientBusinessId
  Maybe Text ->
  -- | mbSellerBusinessId
  Maybe Text ->
  -- | mbSellerVatNumber
  Maybe Text ->
  InvoicePdfData
buildInvoicePdfData inv items mbTax mbPayType mbBrand mbLast4 mbRecipientBid mbSellerBid mbSellerVat =
  InvoicePdfData
    { financeInvoice = inv,
      parsedLineItems = items,
      mbTaxTxn = mbTax,
      mbPaymentMethodType = mbPayType,
      mbCardBrand = mbBrand,
      mbCardLastFour = mbLast4,
      mbRecipientBusinessId = mbRecipientBid,
      mbSellerBusinessId = mbSellerBid,
      mbSellerVatNumber = mbSellerVat
    }

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
          escHtml (docTitle lbls inv <> " " <> inv.invoiceNumber),
          "</title>",
          "<style>",
          invoiceCss,
          "</style>",
          "</head><body><div class='wrap'>",
          renderHeader cfg pdfData lbls,
          renderParties pdfData lbls,
          renderFromLocation inv,
          renderInvoicingPeriod cfg pdfData lbls,
          renderLineItemsTable cfg pdfData lbls,
          renderTotals cfg pdfData lbls,
          renderAggregationFooter cfg inv lbls,
          "</div></body></html>"
        ]

-- | AggregatedCommission gets "App bookings" / "Sovellustilaukset" / "App-boekingen";
-- other types keep the generic "Invoice" / "Lasku" / "Factuur".
docTitle :: Labels -> Invoice -> Text
docTitle lbls inv = case inv.invoiceType of
  AggregatedCommission -> lbls.aggregatedCommissionTitle
  _ -> lbls.invoiceTitle

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

-- | AggregatedCommission shows recipient + seller with full info; other types
-- show recipient name + supplier full info. Each side's <td> is suppressed
-- entirely when none of its data fields are populated, so a missing party
-- doesn't leave a labeled-but-empty block in the rendered PDF.
renderParties :: InvoicePdfData -> Labels -> Text
renderParties pdfData lbls =
  let inv = pdfData.financeInvoice
   in case inv.invoiceType of
        AggregatedCommission ->
          let leftHas =
                isJust inv.issuedToName || isJust inv.issuedToAddress
                  || isJust pdfData.mbRecipientBusinessId
                  || isJust inv.supplierTaxNo
              rightHas =
                isJust inv.issuedByName || isJust inv.issuedByAddress
                  || isJust pdfData.mbSellerBusinessId
                  || isJust pdfData.mbSellerVatNumber
              leftBlock =
                T.concat
                  [ "<td class='party-left'>",
                    "<div class='party-lbl-plain'>",
                    lbls.recipientLabel,
                    ":</div>",
                    maybe "" (\n -> "<div class='party-name'>" <> escHtml n <> "</div>") inv.issuedToName,
                    maybe "" (\a -> "<div class='party-addr'>" <> escHtml a <> "</div>") inv.issuedToAddress,
                    maybe "" (\b -> "<div class='party-tax'>" <> lbls.businessIdLabel <> ": " <> escHtml b <> "</div>") pdfData.mbRecipientBusinessId,
                    -- Recipient VAT: handler maps recipient VAT into supplierTaxNo.
                    maybe "" (\t -> "<div class='party-tax'>" <> lbls.vatNumberLabel <> ": " <> escHtml t <> "</div>") inv.supplierTaxNo,
                    "</td>"
                  ]
              rightBlock =
                T.concat
                  [ "<td class='party-right'>",
                    "<div class='party-lbl-plain'>",
                    lbls.sellerLabel,
                    ":</div>",
                    maybe "" (\n -> "<div class='party-name'>" <> escHtml n <> "</div>") inv.issuedByName,
                    maybe "" (\a -> "<div class='party-addr'>" <> escHtml a <> "</div>") inv.issuedByAddress,
                    maybe "" (\b -> "<div class='party-tax'>" <> lbls.businessIdLabel <> ": " <> escHtml b <> "</div>") pdfData.mbSellerBusinessId,
                    maybe "" (\t -> "<div class='party-tax'>" <> lbls.vatNumberLabel <> ": " <> escHtml t <> "</div>") pdfData.mbSellerVatNumber,
                    "</td>"
                  ]
           in if not leftHas && not rightHas
                then ""
                else
                  T.concat
                    [ "<table class='parties'><tr>",
                      if leftHas then leftBlock else "<td class='party-left'></td>",
                      if rightHas then rightBlock else "<td class='party-right'></td>",
                      "</tr></table>"
                    ]
        _ ->
          -- Right side merges supplier* with issuedBy* / merchantGstin so
          -- callers that only populate one side still get a sensible render.
          let rightName = inv.supplierName <|> inv.issuedByName
              rightAddress = inv.supplierAddress <|> inv.issuedByAddress
              rightGstin = inv.supplierGSTIN <|> inv.merchantGstin
              leftHas = isJust inv.issuedToName
              rightHas =
                isJust rightName || isJust rightAddress
                  || isJust rightGstin
                  || isJust inv.supplierTaxNo
              leftBlock =
                T.concat
                  [ "<td class='party-left'>",
                    "<div class='party-lbl-plain'>",
                    lbls.recipientLabel,
                    ":</div>",
                    maybe "" (\n -> "<div class='party-name' style='font-weight:700;color:#000000'>" <> escHtml n <> "</div>") inv.issuedToName,
                    "</td>"
                  ]
              rightBlock =
                T.concat
                  [ "<td class='party-right'>",
                    maybe "" (\n -> "<div class='party-name'>" <> escHtml n <> "</div>") rightName,
                    maybe "" (\a -> "<div class='party-addr'>" <> escHtml a <> "</div>") rightAddress,
                    maybe "" (\g -> "<div class='party-tax'>" <> lbls.gstinLabel <> " " <> escHtml g <> "</div>") rightGstin,
                    maybe "" (\t -> "<div class='party-tax'>" <> lbls.vatNumberLabel <> " " <> escHtml t <> "</div>") inv.supplierTaxNo,
                    "</td>"
                  ]
           in if not leftHas && not rightHas
                then ""
                else
                  T.concat
                    [ "<table class='parties'><tr>",
                      if leftHas then leftBlock else "<td class='party-left'></td>",
                      if rightHas then rightBlock else "<td class='party-right'></td>",
                      "</tr></table>"
                    ]

-- | Suppressed for AggregatedCommission (recipient address already in parties block);
-- other invoice types keep the existing behavior.
renderFromLocation :: Invoice -> Text
renderFromLocation inv = case inv.invoiceType of
  AggregatedCommission -> ""
  _ -> case inv.issuedToAddress of
    Nothing -> ""
    Just addr ->
      "<div class='from-location'>" <> escHtml addr <> "</div>"

-- | "Laskutusjakso: <start> - <end>" band between parties and line items.
-- AggregatedCommission only.
renderInvoicingPeriod :: InvoicePdfConfig -> InvoicePdfData -> Labels -> Text
renderInvoicingPeriod cfg pdfData lbls =
  let inv = pdfData.financeInvoice
   in case inv.invoiceType of
        AggregatedCommission -> case (inv.periodStart, inv.periodEnd) of
          (Just s, Just e) ->
            "<div class='invoicing-period'>"
              <> escHtml lbls.invoicingPeriodLabel
              <> ": <strong>"
              <> formatDate cfg.timezone s
              <> " - "
              <> formatDate cfg.timezone e
              <> "</strong></div>"
          _ -> ""
        _ -> ""

-- ---------------------------------------------------------------------------
-- Line items table
-- Title | Amount (EUR) | ALV% | ALV (EUR) | Total (EUR)
-- ---------------------------------------------------------------------------

-- | AggregatedCommission: one consolidated row, 6 cols. Other types: per-item
-- rows, 5 cols.
renderLineItemsTable :: InvoicePdfConfig -> InvoicePdfData -> Labels -> Text
renderLineItemsTable cfg pdfData lbls =
  let inv = pdfData.financeInvoice
      cur = inv.currency
   in case inv.invoiceType of
        AggregatedCommission ->
          let total = sum (map (.lineTotal) pdfData.parsedLineItems)
              periodSuffix = case (inv.periodStart, inv.periodEnd) of
                (Just s, Just e) -> " (" <> formatDate cfg.timezone s <> " - " <> formatDate cfg.timezone e <> ")"
                _ -> ""
              description = lbls.aggregatedCommissionTitle <> periodSuffix
              vatZeroHeader = lbls.vatLabel <> " 0%"
           in T.concat
                [ "<table>",
                  "<thead><tr>",
                  th "" lbls.titleLabel,
                  th "n" lbls.quantityLabel,
                  th "n" lbls.unitPriceLabel,
                  th "n" lbls.aggLineSumLabel,
                  th "n" vatZeroHeader,
                  th "n" lbls.totalLabel,
                  "</tr></thead><tbody>",
                  "<tr>",
                  td description,
                  td' "n" "1",
                  td' "n" (fmtMoneyNum cur total),
                  td' "n" (fmtMoneyNum cur total),
                  td' "n" (fmtMoneyNum cur 0),
                  td' "n" (fmtMoneyNum cur total),
                  "</tr>",
                  "</tbody></table>"
                ]
        _ ->
          let rows = buildRows cfg.locale pdfData lbls
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

-- | Main table rows. Includes external items (toll/parking) so VAT-bearing
-- pass-through charges show alongside ride fare; their breakdown still appears
-- below in 'renderTotals'. Adjustments render in 'renderTotals' only.
buildRows :: InvoiceLocale -> InvoicePdfData -> Labels -> [Text]
buildRows locale pdfData _lbls =
  let inv = pdfData.financeInvoice
      cur = inv.currency
      mainTableItems =
        [item | item <- pdfData.parsedLineItems, item.itemType == Just Fare || item.itemType == Just Tax]
      pairs = pairByGroupId mainTableItems
   in map (renderFareTaxRow locale cur) pairs

-- | Fare/tax row. VAT% displayed as tax/(net+tax) (share of gross), per Lynx
-- invoice convention. Empty VAT cells look broken in the table; always render
-- an explicit "0.0"/"0.00" when no tax is paired so tax-exempt items still
-- look intentional rather than missing.
renderFareTaxRow :: InvoiceLocale -> Currency -> (InvoiceLineItem, Maybe InvoiceLineItem) -> Text
renderFareTaxRow locale cur (fare, mbTax) =
  let taxAmount = maybe 0 (.lineTotal) mbTax
      gross = fare.lineTotal + taxAmount
      vatPctText =
        if isJust mbTax && gross > 0
          then fmtPct (realToFrac (taxAmount / fare.lineTotal) * 100.0)
          else "0.0"
      vatAmtText = fmtMoneyNum cur taxAmount
   in tr
        [ td (displayItemDescription locale fare),
          td' "n" (fmtMoneyNum cur fare.lineTotal),
          td' "n" vatPctText,
          td' "n" vatAmtText,
          td' "n" (fmtMoneyNum cur gross)
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
-- Net Total only renders when adjustments exist. AggregatedCommission takes a
-- simpler 3-row branch (Yhteensä / ALV 0% / Yhteensä sis. ALV).
renderTotals :: InvoicePdfConfig -> InvoicePdfData -> Labels -> Text
renderTotals cfg pdfData lbls =
  let inv = pdfData.financeInvoice
      cur = inv.currency
   in case inv.invoiceType of
        AggregatedCommission ->
          let total = sum (map (.lineTotal) pdfData.parsedLineItems)
              vatZeroRowLabel = lbls.vatLabel <> " 0%"
           in T.concat
                [ "<table class='totals'>",
                  totRow lbls.aggSubtotalLabel (fmtMoneyNum cur total),
                  totRow vatZeroRowLabel (fmtMoneyNum cur 0),
                  "<tr class='tot-row grand'><td style='font-weight:700;color:#000000'>",
                  lbls.totalInclVatLabel,
                  "</td><td class='tot-val' style='font-weight:700;color:#000000'>",
                  fmtMoneyNum cur total,
                  "</td></tr>",
                  "</table>"
                ]
        _ ->
          let locale = cfg.locale
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
              -- Prefer the explicit tax-transaction rate when present; otherwise compute
              -- as share of gross (tax / (net+tax)) so it agrees with per-row VAT% in
              -- the line items table.
              vatPctText = case mbTax of
                Just taxTxn | taxableSum > 0 -> fmtPct (fromMaybe taxTxn.gstRate taxTxn.taxRate)
                _
                  | taxableSum + taxSum > 0 && taxSum > 0 ->
                    fmtPct (realToFrac (taxSum / (taxableSum + taxSum)) * 100.0)
                _ -> ""
              vatLabelText =
                lbls.vatLabel
                  <> if T.null vatPctText then "" else " (" <> vatPctText <> "%)"
              paymentMethodStr = formatPaymentMethod lbls inv.paymentMode pdfData.mbCardBrand pdfData.mbCardLastFour
              taxableLabel = case inv.invoiceType of
                RideCancellation -> lbls.taxableCancellationLabel
                SubscriptionPurchase -> lbls.taxableSubscriptionLabel
                Commission -> lbls.taxableCommissionLabel
                _ -> lbls.taxablePriceLabel
           in T.concat
                [ "<table class='totals'>",
                  totRow taxableLabel (fmtMoneyNum cur taxableSum),
                  if taxSum > 0 then totRow vatLabelText (fmtMoneyNum cur taxSum) else "",
                  T.concat (map (renderExternalRow locale cur) externalItems),
                  "<tr class='tot-row grand'><td style='font-weight:700;color:#000000'>",
                  lbls.totalInclVatLabel,
                  "</td><td class='tot-val' style='font-weight:700;color:#000000'>",
                  fmtMoneyNum cur totalLine,
                  "</td></tr>",
                  T.concat (map (renderAdjustmentRow locale cur) adjustmentItems),
                  if not (null adjustmentItems)
                    then
                      "<tr class='tot-row grand'><td style='font-weight:700;color:#000000'>"
                        <> lbls.invoicedValueLabel
                        <> "</td><td class='tot-val' style='font-weight:700;color:#000000'>"
                        <> fmtMoneyNum cur netTotalLine
                        <> "</td></tr>"
                    else "",
                  maybe "" (\pm -> "<tr class='tot-row payment'><td></td><td class='tot-val payment-detail'>" <> escHtml pm <> "</td></tr>") paymentMethodStr,
                  "</table>"
                ]
  where
    totRow lbl val =
      "<tr class='tot-row'><td>" <> lbl <> "</td><td class='tot-val'>" <> val <> "</td></tr>"

-- | Build the payment-method string shown beneath the totals. Card details
-- (when present) take precedence and produce "Received via VISA ****8821".
-- Otherwise, "ONLINE"/"CASH" map to localized words; anything else passes
-- through verbatim so unknown modes still display.
formatPaymentMethod :: Labels -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text
formatPaymentMethod lbls mbMode mbBrand mbLast4 =
  case (mbBrand, mbLast4) of
    (Just brand, Just last4) ->
      Just (lbls.receivedViaLabel <> " " <> brand <> " ****" <> last4)
    _ -> case mbMode of
      Just mode -> case T.toUpper mode of
        "ONLINE" -> Just lbls.onlineLabel
        "CASH" -> Just lbls.cashLabel
        _ -> Just mode
      Nothing -> Nothing

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
    aggregatedCommissionTitle :: Text,
    invoiceNumberLabel :: Text,
    dateLabel :: Text,
    dueDateLabel :: Text,
    invoicingPeriodLabel :: Text,
    recipientLabel :: Text,
    supplierLabel :: Text,
    -- | Distinct from supplierLabel: the entity selling the platform service
    -- (the merchant), not the entity supplying the rides (fleet owner / driver).
    sellerLabel :: Text,
    businessIdLabel :: Text,
    vatNumberLabel :: Text,
    gstinLabel :: Text,
    titleLabel :: Text,
    amountLabel :: Text,
    quantityLabel :: Text,
    unitPriceLabel :: Text,
    -- | Distinct from amountLabel: in FI, amountLabel = "Määrä" (quantity);
    -- this label is the explicit "Summa" word for the line sum.
    aggLineSumLabel :: Text,
    vatPctLabel :: Text,
    vatAmtLabel :: Text,
    totalLabel :: Text,
    taxablePriceLabel :: Text,
    taxableCancellationLabel :: Text,
    taxableSubscriptionLabel :: Text,
    taxableCommissionLabel :: Text,
    vatLabel :: Text,
    totalInclVatLabel :: Text,
    invoicedValueLabel :: Text,
    -- | Distinct from totalLabel: totalLabel is a table column header,
    -- aggSubtotalLabel is the "Yhteensä" row label in the totals block.
    aggSubtotalLabel :: Text,
    -- | Payment method line: "Received via VISA ****8821" / "Online" / "Cash".
    receivedViaLabel :: Text,
    onlineLabel :: Text,
    cashLabel :: Text
  }

localeLabels :: InvoiceLocale -> Labels
localeLabels EN =
  Labels
    { invoiceTitle = "Invoice",
      aggregatedCommissionTitle = "App bookings",
      invoiceNumberLabel = "Invoice No.",
      dateLabel = "Date",
      dueDateLabel = "Due Date",
      invoicingPeriodLabel = "Invoicing Period",
      recipientLabel = "Recipient",
      supplierLabel = "Supplier",
      sellerLabel = "Seller",
      businessIdLabel = "Business ID",
      vatNumberLabel = "VAT No.",
      gstinLabel = "GSTIN",
      titleLabel = "Title",
      amountLabel = "Amount",
      quantityLabel = "Quantity",
      unitPriceLabel = "Unit Price",
      aggLineSumLabel = "Amount",
      vatPctLabel = "VAT",
      vatAmtLabel = "VAT",
      totalLabel = "Total",
      taxablePriceLabel = "Taxable Trip Price",
      taxableCancellationLabel = "Taxable Cancellation Fee",
      taxableSubscriptionLabel = "Taxable Subscription Amount",
      taxableCommissionLabel = "Taxable Commission",
      vatLabel = "VAT",
      totalInclVatLabel = "Total incl. VAT",
      invoicedValueLabel = "Invoiced Value",
      aggSubtotalLabel = "Subtotal",
      receivedViaLabel = "Received via",
      onlineLabel = "Received online",
      cashLabel = "Received in cash"
    }
localeLabels FI =
  Labels
    { invoiceTitle = "Lasku",
      aggregatedCommissionTitle = "Sovellustilaukset",
      invoiceNumberLabel = "Laskun nro.",
      dateLabel = "P\228iv\228m\228\228r\228",
      dueDateLabel = "Er\228p\228iv\228",
      invoicingPeriodLabel = "Laskutusjakso",
      recipientLabel = "Vastaanottaja",
      supplierLabel = "Toimittaja",
      sellerLabel = "Myyj\228",
      businessIdLabel = "Y-tunnus",
      vatNumberLabel = "ALV-nro.",
      gstinLabel = "GSTIN",
      titleLabel = "Nimike",
      amountLabel = "M\228\228r\228",
      quantityLabel = "M\228\228r\228",
      unitPriceLabel = "Yksikk\246hinta",
      aggLineSumLabel = "Summa",
      vatPctLabel = "ALV%",
      vatAmtLabel = "ALV",
      totalLabel = "Kokonaissumma",
      taxablePriceLabel = "Verollinen matkan hinta (EUR)",
      taxableCancellationLabel = "Verollinen peruutusmaksu (EUR)",
      taxableSubscriptionLabel = "Verollinen tilausmaksu (EUR)",
      taxableCommissionLabel = "Verollinen provisio (EUR)",
      vatLabel = "ALV",
      totalInclVatLabel = "Yhteens\228 sis. ALV (EUR)",
      invoicedValueLabel = "Laskutettu arvo",
      aggSubtotalLabel = "Yhteens\228",
      receivedViaLabel = "Vastaanotettu",
      onlineLabel = "Vastaanotettu verkossa",
      cashLabel = "Vastaanotettu k\228teisell\228"
    }
localeLabels NL =
  Labels
    { invoiceTitle = "Factuur",
      aggregatedCommissionTitle = "App-boekingen",
      invoiceNumberLabel = "Factuurnr.",
      dateLabel = "Datum",
      dueDateLabel = "Vervaldatum",
      invoicingPeriodLabel = "Factureringsperiode",
      recipientLabel = "Ontvanger",
      supplierLabel = "Leverancier",
      sellerLabel = "Verkoper",
      businessIdLabel = "Bedrijfs-ID",
      vatNumberLabel = "BTW-nr.",
      gstinLabel = "GSTIN",
      titleLabel = "Omschrijving",
      amountLabel = "Bedrag",
      quantityLabel = "Aantal",
      unitPriceLabel = "Eenheidsprijs",
      aggLineSumLabel = "Bedrag",
      vatPctLabel = "BTW%",
      vatAmtLabel = "BTW",
      totalLabel = "Totaal",
      taxablePriceLabel = "Belastbare ritprijs (EUR)",
      taxableCancellationLabel = "Belastbare annuleringskosten (EUR)",
      taxableSubscriptionLabel = "Belastbaar abonnementsbedrag (EUR)",
      taxableCommissionLabel = "Belastbare commissie (EUR)",
      vatLabel = "BTW",
      totalInclVatLabel = "Totaal incl. BTW (EUR)",
      invoicedValueLabel = "Gefactureerde waarde",
      aggSubtotalLabel = "Subtotaal",
      receivedViaLabel = "Ontvangen via",
      onlineLabel = "Ontvangen online",
      cashLabel = "Ontvangen contant"
    }

-- | Closing paragraph for AggregatedCommission, with merchant name interpolated.
aggregationFooterText :: InvoiceLocale -> Text -> Text
aggregationFooterText EN merchantName =
  "This invoice represents services provided during the period. Amounts payable to "
    <> merchantName
    <> " have already been deducted from your balance. Check the "
    <> merchantName
    <> " app for up-to-date balance information."
aggregationFooterText FI merchantName =
  "T\228m\228 lasku kuvaa ajanjakson aikana tarjottuja palveluja, ja "
    <> merchantName
    <> "ille maksettavat summat on jo v\228hennetty saldostasi. Tarkista "
    <> merchantName
    <> "-sovelluksesta ajantasaiset tiedot saldostasi."
aggregationFooterText NL merchantName =
  "Deze factuur vertegenwoordigt de tijdens de periode geleverde diensten. Bedragen verschuldigd aan "
    <> merchantName
    <> " zijn reeds afgetrokken van uw saldo. Raadpleeg de "
    <> merchantName
    <> "-app voor actuele saldo-informatie."

-- | Footer paragraph below the totals block, only on AggregatedCommission.
renderAggregationFooter :: InvoicePdfConfig -> Invoice -> Labels -> Text
renderAggregationFooter cfg inv _lbls = case inv.invoiceType of
  AggregatedCommission ->
    let merchantName = fromMaybe "" inv.issuedByName
     in "<div class='agg-footer'>"
          <> escHtml (aggregationFooterText cfg.locale merchantName)
          <> "</div>"
  _ -> ""

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
      ".invoicing-period { font-size:13px; color:#2d2d2d; margin-bottom:12px; }",
      ".agg-footer { font-size:13px; color:#2d2d2d; line-height:1.6; margin-top:24px;",
      "              padding-top:12px; border-top:1px solid #e0e0e0; }",
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
