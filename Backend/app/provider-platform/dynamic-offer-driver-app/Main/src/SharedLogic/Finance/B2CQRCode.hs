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
--   merchant config once wired; until then they are omitted (not rendered as
--   empty lines), and the QR is only generated when a seller GSTIN is present.
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
      -- Nothing when the seller GSTIN is missing — we never persist a QR without
      -- it (a non-compliant QR reads as "done" later, which is worse than none).
      whenJust (buildB2CQRPayload transporterConfig invoice mbTaxTxn) $ \payload -> do
        QFInvoice.updateUnsignedQRCode (Just payload) Nothing Nothing invoice.id
        logInfo $ "B2CQRCode: stored unsigned QR for invoice " <> invoice.id.getId

-- | Assemble the human-readable QR payload from invoice + tax summary. Returns
--   'Nothing' (QR gated off) unless a seller GSTIN is available. Absent payee
--   fields are omitted rather than rendered as empty @Key:@ lines.
--
--   NB: UPI ID / bank account / IFSC are payee fields still to be sourced from
--   merchant config (see review comment I-4); until then the QR carries the GST
--   + invoice summary but not the payment rails.
buildB2CQRPayload ::
  DTC.TransporterConfig ->
  FInvoice.Invoice ->
  Maybe IndirectTax.IndirectTaxTransaction ->
  Maybe Text
buildB2CQRPayload transporterConfig invoice mbTaxTxn =
  let mbCfg = transporterConfig.invoiceConfig
      mbGstin = nonBlank =<< (invoice.merchantGstin <|> (mbCfg >>= (.ecoGstin)) <|> invoice.supplierGSTIN)
      -- Payee UPI/bank/IFSC to be sourced from merchant config once wired.
      mbUpiId = Nothing :: Maybe Text
      mbBankAcc = Nothing :: Maybe Text
      mbIfsc = Nothing :: Maybe Text
      igst = maybe 0 (.igstAmount) mbTaxTxn
      cgst = maybe 0 (.cgstAmount) mbTaxTxn
      sgst = maybe 0 (.sgstAmount) mbTaxTxn
      optLine label = maybe [] (\v -> [label <> v])
   in mbGstin <&> \gstin ->
        T.intercalate "\n" $
          concat
            [ ["GSTIN: " <> gstin],
              optLine "UPI ID: " mbUpiId,
              optLine "Bank Account No.: " mbBankAcc,
              optLine "IFSC: " mbIfsc,
              ["Invoice Number: " <> invoice.invoiceNumber],
              ["Invoice Date: " <> formatQRDate invoice.issuedAt],
              ["Total Invoice Value: ₹" <> fmtMoney invoice.totalAmount],
              ["Taxes: IGST: ₹" <> fmtMoney igst <> " CGST: ₹" <> fmtMoney cgst <> " SGST: ₹" <> fmtMoney sgst]
            ]
  where
    nonBlank t = if T.null (T.strip t) then Nothing else Just t

-- | Format HighPrecMoney to 2 decimals by rounding the exact 'Rational' (never
-- via 'Double') so the QR total can't disagree with the invoice total by a paisa.
fmtMoney :: HighPrecMoney -> Text
fmtMoney x =
  let paise = round (toRational x * 100) :: Integer
      (rupees, p) = paise `quotRem` 100
      absP = abs p
   in T.pack (show rupees <> "." <> (if absP < 10 then "0" else "") <> show absP)

-- | DD/MM/YYYY.
formatQRDate :: UTCTime -> Text
formatQRDate t =
  let (y, m, d) = toGregorian (utctDay t)
      pad2 n = if n < 10 then "0" <> show n else show n
   in pad2 d <> "/" <> pad2 m <> "/" <> show y
