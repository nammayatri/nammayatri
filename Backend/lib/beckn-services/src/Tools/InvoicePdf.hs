module Tools.InvoicePdf
  ( generateFinanceInvoicePdf,
  )
where

import Control.Exception (catch, finally, try)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Kernel.Prelude hiding (catch, finally, try)
import Kernel.Types.Error (GenericError (..))
import Kernel.Utils.Common (MonadFlow, logError, logInfo, logWarning, throwError)
import System.Directory (doesFileExist, getTemporaryDirectory, removeFile)
import System.Exit (ExitCode (..))
import System.IO (hClose, openTempFile)
import System.Process (readProcessWithExitCode)
import System.Timeout (timeout)

-- | Render HTML to PDF via wkhtmltopdf and return base64-encoded PDF bytes.
generateFinanceInvoicePdf ::
  (MonadFlow m) =>
  -- | unique key for temp file naming (e.g. invoiceNumber)
  Text ->
  -- | rendered HTML from SharedLogic.RenderInvoiceFromTemplate.renderHtml
  Text ->
  m Text
generateFinanceInvoicePdf key html = do
  tempDir <- liftIO getTemporaryDirectory
  (htmlPath, htmlHandle) <- liftIO $ openTempFile tempDir "finance_invoice_.html"
  liftIO $ hClose htmlHandle
  (pdfPath, pdfHandle) <- liftIO $ openTempFile tempDir "finance_invoice_.pdf"
  liftIO $ hClose pdfHandle
  let cleanup = do
        removeFile htmlPath `catch` (\(_ :: IOException) -> pure ())
        removeFile pdfPath `catch` (\(_ :: IOException) -> pure ())
  liftIO $ BS.writeFile htmlPath (encodeUtf8 html)
  result <- liftIO (generateBody htmlPath pdfPath `finally` cleanup)
  case result of
    Left err -> do
      logError $ "Finance invoice PDF generation failed for " <> key <> ": " <> err
      throwError $ InternalError err
    Right (bytes, warnings) -> do
      unless (T.null warnings) $ logWarning $ "wkhtmltopdf warnings for " <> key <> ": " <> warnings
      logInfo "Finance invoice PDF generated"
      pure . TE.decodeUtf8 $ B64.encode bytes
  where
    generateBody htmlPath pdfPath = do
      outcome <- runGeneratePdfFromHtml htmlPath pdfPath
      case outcome of
        Left err -> pure $ Left err
        Right (exitCode, stderr') -> do
          bytes <- readPdf pdfPath
          let stderrText = T.strip (T.pack stderr')
          pure $ case (exitCode, bytes) of
            (ExitSuccess, Just pdf) -> Right (pdf, stderrText)
            (ExitFailure 1, Just pdf) -> Right (pdf, stderrText)
            (ExitSuccess, Nothing) -> Left "PDF generation failed: output file missing"
            (ExitFailure code, _) -> Left $ "PDF generation failed: wkhtmltopdf exited with " <> show code <> ": " <> stderrText

    readPdf pdfPath = do
      exists <- doesFileExist pdfPath
      if exists
        then do
          bytes <- BS.readFile pdfPath
          pure $ if BS.null bytes then Nothing else Just bytes
        else pure Nothing

wkhtmltopdfTimeoutSeconds :: Int
wkhtmltopdfTimeoutSeconds = 60

runGeneratePdfFromHtml :: FilePath -> FilePath -> IO (Either Text (ExitCode, String))
runGeneratePdfFromHtml htmlPath pdfPath = do
  result <-
    timeout (wkhtmltopdfTimeoutSeconds * 1000000) $
      ( try
          ( readProcessWithExitCode
              "wkhtmltopdf"
              [ "--enable-external-links",
                "--load-error-handling",
                "ignore",
                "--load-media-error-handling",
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
  pure $ case result of
    Nothing -> Left $ "PDF generation failed: wkhtmltopdf timed out after " <> show wkhtmltopdfTimeoutSeconds <> "s"
    Just (Left err) -> Left $ "PDF generation failed: could not run wkhtmltopdf: " <> show err
    Just (Right (exitCode, _, stderr')) -> Right (exitCode, stderr')
