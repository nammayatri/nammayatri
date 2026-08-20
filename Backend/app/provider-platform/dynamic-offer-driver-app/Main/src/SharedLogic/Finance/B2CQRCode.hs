{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Self-generated B2C dynamic QR payload (GST compliance, no IRN).
--
--   For a B2C invoice the driver/supplier is not GST-registered, so there is no
--   IRP-signed QR ('signedQRCode'). Instead the ECO/platform self-generates an
--   /unsigned/ QR carrying the seller GSTIN, payee UPI/bank details and the
--   invoice + tax summary, persisted on 'unsignedQRCode'. The QR-image encoding
--   and template embed happen later in the render pipeline.
--
--   NB: UPI ID / bank account / IFSC are payee fields that will be sourced from
--   merchant config once wired; for now they render empty so the payload shape
--   matches the required GST layout.
module SharedLogic.Finance.B2CQRCode
  ( generateB2CQRForInvoice,
    buildB2CQRPayload,
  )
where

import Control.Applicative ((<|>))
import qualified Data.Text as T
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock (utctDay)
import qualified Domain.Types.TransporterConfig as DTC
import Kernel.Prelude
import Kernel.Types.Id (Id)
import Kernel.Utils.Common
import qualified Lib.Finance.Domain.Types.IndirectTaxTransaction as IndirectTax
import qualified Lib.Finance.Domain.Types.Invoice as FInvoice
import qualified Lib.Finance.Storage.Beam.BeamFlow as BeamFlow
import qualified Lib.Finance.Storage.Queries.IndirectTaxTransaction as QIndirectTax
import qualified Lib.Finance.Storage.Queries.Invoice as QFInvoice
import Text.Printf (printf)

-- | Build + persist the unsigned B2C QR payload for an invoice. Skips B2B
--   invoices (they already carry an IRP-signed QR) and never overwrites an
--   existing unsigned QR. Safe to call for any customer (B2C) invoice.
generateB2CQRForInvoice ::
  (BeamFlow.BeamFlow m r) =>
  DTC.TransporterConfig ->
  Id FInvoice.Invoice ->
  m ()
generateB2CQRForInvoice transporterConfig invoiceId = do
  mbInvoice <- QFInvoice.findById invoiceId
  whenJust mbInvoice $ \invoice ->
    -- B2B invoices carry an IRP-signed QR; only self-generate for B2C.
    when (isNothing invoice.signedQRCode && isNothing invoice.unsignedQRCode) $ do
      mbTaxTxn <- listToMaybe <$> QIndirectTax.findByInvoiceNumber (Just invoice.invoiceNumber)
      let payload = buildB2CQRPayload transporterConfig invoice mbTaxTxn
      QFInvoice.updateUnsignedQRCode (Just payload) Nothing Nothing invoice.id
      logInfo $ "B2CQRCode: stored unsigned QR for invoice " <> invoice.id.getId

-- | Assemble the human-readable QR payload string from invoice + tax summary.
--   Missing fields render empty. Newline-separated @Key: Value@ pairs decode to
--   a readable GST B2C QR.
buildB2CQRPayload ::
  DTC.TransporterConfig ->
  FInvoice.Invoice ->
  Maybe IndirectTax.IndirectTaxTransaction ->
  Text
buildB2CQRPayload transporterConfig invoice mbTaxTxn =
  let mbCfg = transporterConfig.invoiceConfig
      gstin = fromMaybe "" (invoice.merchantGstin <|> (mbCfg >>= (.ecoGstin)) <|> invoice.supplierGSTIN)
      -- Payee details to be sourced from merchant config when wired; empty for now.
      upiId = "" :: Text
      bankAcc = "" :: Text
      ifsc = "" :: Text
      igst = maybe 0 (.igstAmount) mbTaxTxn
      cgst = maybe 0 (.cgstAmount) mbTaxTxn
      sgst = maybe 0 (.sgstAmount) mbTaxTxn
   in T.intercalate
        "\n"
        [ "GSTIN: " <> gstin,
          "UPI ID: " <> upiId,
          "Bank Account No.: " <> bankAcc,
          "IFSC: " <> ifsc,
          "Invoice Number: " <> invoice.invoiceNumber,
          "Invoice Date: " <> formatQRDate invoice.issuedAt,
          "Total Invoice Value: ₹" <> fmtMoney invoice.totalAmount,
          "Taxes: IGST: ₹" <> fmtMoney igst <> " CGST: ₹" <> fmtMoney cgst <> " SGST: ₹" <> fmtMoney sgst
        ]

-- | Round HighPrecMoney to 2 decimals for display.
fmtMoney :: HighPrecMoney -> Text
fmtMoney x = T.pack (printf "%.2f" (realToFrac x :: Double))

-- | DD/MM/YYYY.
formatQRDate :: UTCTime -> Text
formatQRDate t =
  let (y, m, d) = toGregorian (utctDay t)
      pad2 n = if n < 10 then "0" <> show n else show n
   in pad2 d <> "/" <> pad2 m <> "/" <> show y
