module Tools.InvoicePdf
  ( generateFinanceInvoicePdf,
  )
where

import Control.Exception (catch, try)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Kernel.Prelude hiding (catch, try)
import Kernel.Utils.Common (MonadFlow, logError, logInfo, logWarning, throwError)
import Kernel.Types.Error (GenericError (..))
import System.Directory (createDirectoryIfMissing, doesFileExist, getTemporaryDirectory, removeFile)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import System.Timeout (timeout)

-- | Render HTML to PDF via wkhtmltopdf and return base64-encoded PDF bytes.
--   Falls back to base64-encoded HTML if wkhtmltopdf is unavailable.
generateFinanceInvoicePdf ::
  (MonadFlow m) =>
  -- | unique key for temp file naming (e.g. invoiceNumber)
  Text ->
  -- | rendered HTML from Lib.Finance.Invoice.PdfService.renderInvoiceHtml
  Text ->
  m Text
generateFinanceInvoicePdf key html = do
  tempDir <- liftIO getTemporaryDirectory
  let invoiceDir = tempDir </> "finance_invoices"
  liftIO $ createDirectoryIfMissing True invoiceDir
  let safeKey = T.unpack $ T.replace "/" "_" key
      htmlPath = invoiceDir </> ("invoice_" <> safeKey <> ".html")
      pdfPath = invoiceDir </> ("invoice_" <> safeKey <> ".pdf")
  liftIO $ BS.writeFile htmlPath (encodeUtf8 html)
  let cleanup = liftIO $ do
        removeFile htmlPath `catch` (\(_ :: IOException) -> pure ())
        removeFile pdfPath `catch` (\(_ :: IOException) -> pure ())
  ok <- generatePdfFromHtml htmlPath pdfPath
  result <-
    if ok
      then do
        exists <- liftIO $ doesFileExist pdfPath
        if exists
          then do
            logInfo $ "Finance invoice PDF generated: " <> T.pack pdfPath
            bytes <- liftIO $ BS.readFile pdfPath
            pure . TE.decodeUtf8 $ B64.encode bytes
          else do
            logError "PDF file not found after successful wkhtmltopdf run"
            throwError $ InternalError "PDF generation failed: output file missing"
      else do
        logError "wkhtmltopdf unavailable or failed"
        throwError $ InternalError "PDF generation failed: wkhtmltopdf error"
  cleanup
  pure result

generatePdfFromHtml :: (MonadFlow m) => FilePath -> FilePath -> m Bool
generatePdfFromHtml htmlPath pdfPath = do
  result <-
    liftIO $
      timeout 60000000 $
        ( try
            ( readProcessWithExitCode
                "wkhtmltopdf"
                [ "--enable-local-file-access",
                  "--enable-external-links",
                  "--load-error-handling",
                  "ignore",
                  "--encoding",
                  "utf-8",
                  "--print-media-type",
                  htmlPath,
                  pdfPath
                ]
                ""
            ) ::
            IO (Either SomeException (ExitCode, String, String))
        )
  case result of
    Just (Right (ExitSuccess, _, stderr)) -> do
      unless (null stderr) $ logWarning $ "wkhtmltopdf stderr: " <> T.pack stderr
      return True
    Just (Right (code, stdout, stderr)) -> do
      logError $ "wkhtmltopdf failed: " <> T.pack (show code)
      logError $ "stdout: " <> T.pack stdout
      logError $ "stderr: " <> T.pack stderr
      return False
    Just (Left ex) -> do
      logError $ "wkhtmltopdf exception: " <> T.pack (show ex)
      return False
    Nothing -> do
      logError "wkhtmltopdf timed out after 60s"
      return False
