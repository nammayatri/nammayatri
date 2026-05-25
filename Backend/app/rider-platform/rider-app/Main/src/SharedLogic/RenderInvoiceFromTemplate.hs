{-# OPTIONS_GHC -Wno-deprecations #-}

module SharedLogic.RenderInvoiceFromTemplate
  ( renderHtml,
  )
where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Time as DT
import qualified Data.Vector as V
import qualified "beckn-spec" Domain.Types.Invoice
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Kernel.External.Types
import Kernel.Prelude
import Kernel.Storage.Clickhouse.Config (ClickhouseFlow)
import Kernel.Types.Error
import Kernel.Types.Id (Id, cast)
import Kernel.Utils.Common
import qualified Lib.Finance.Invoice.RenderTemplate as RT
import qualified Lib.Finance.Storage.Beam.BeamFlow
import qualified Lib.Yudhishthira.Storage.Beam.BeamFlow
import qualified Lib.Yudhishthira.Tools.DebugLog as LYDL
import qualified Lib.Yudhishthira.Types as LYT
import Storage.Beam.Yudhishthira ()
import qualified Storage.CachedQueries.InvoiceTemplate as CQIT
import Tools.DynamicLogic (getAppDynamicLogic)

-- | Look up the per-(mocId,scope,language) HTML template + INVOICE_TEMPLATE JL
-- rule, run the rule on the 'InvoiceContext', then substitute the cleaned
-- result into the template. Returns the final HTML; callers pipe through
-- wkhtmltopdf to produce the PDF. Throws 'InvalidRequest' if either the
-- template row or the JL rule is missing.
renderHtml ::
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    ClickhouseFlow m r,
    Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r,
    Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  Maybe Domain.Types.Invoice.InvoiceType ->
  Kernel.External.Types.Language ->
  DT.TimeZone ->
  RT.InvoiceContext ->
  m Text
renderHtml mocId mbInvoiceType language tz ctx = do
  tmpl <-
    CQIT.findByMerchantOpCityIdInvoiceTypeAndLanguage mocId mbInvoiceType language Nothing
      >>= fromMaybeM (InvalidRequest $ "InvoiceTemplate not found: mocId=" <> mocId.getId <> " invoiceType=" <> show mbInvoiceType <> " language=" <> show language)

  -- Derive scope from the LOADED template (not caller input), so JL rule
  -- lookup matches whichever template scope was actually returned by the
  -- Specific→Generic fallback in CachedQueries. Keeps the (template, JL) pair coherent.
  let scope = maybe LYT.InvoiceTypeGeneric LYT.InvoiceTypeSpecific tmpl.invoiceType
  now <- getCurrentTime
  (allLogics, _) <- getAppDynamicLogic (cast mocId) (LYT.INVOICE_TEMPLATE scope) now Nothing Nothing
  when (null allLogics) $
    throwError $ InvalidRequest $ "No INVOICE_TEMPLATE JL rule for scope=" <> show scope

  jlResp <- LYDL.runLogicsWithDebugLog LYDL.Rider (cast mocId) (LYT.INVOICE_TEMPLATE scope) Nothing allLogics ctx
  unless (null jlResp.errors) $
    logWarning $ "INVOICE_TEMPLATE JL non-fatal errors: " <> show jlResp.errors
  let cleaned = RT.cleanJson tz jlResp.result

  let mainItemsHtml = RT.renderLineItemsHtml tmpl.lineItemRowTemplate (extractArray "mainTableItems" cleaned)
      externalsHtml = RT.renderLineItemsHtml tmpl.totalsLineRowTemplate (extractArray "externalItems" cleaned)
      adjustmentsHtml = RT.renderLineItemsHtml tmpl.totalsLineRowTemplate (extractArray "adjustmentItems" cleaned)
      vars =
        RT.flatten cleaned
          <> [ ("mainTableItemsHtml", mainItemsHtml),
               ("externalItemsHtml", externalsHtml),
               ("adjustmentItemsHtml", adjustmentsHtml)
             ]
      (rendered, missing) = RT.buildTemplateChecked vars tmpl.template

  unless (null missing) $
    logWarning $ "INVOICE_TEMPLATE: unresolved placeholders: " <> show missing

  pure rendered
  where
    extractArray :: Text -> A.Value -> [A.Value]
    extractArray field (A.Object o) = case KM.lookup (AK.fromText field) o of
      Just (A.Array xs) -> V.toList xs
      _ -> []
    extractArray _ _ = []
