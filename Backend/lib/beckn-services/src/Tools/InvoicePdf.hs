module Tools.InvoicePdf
  ( generateFinanceInvoicePdf,
  )
where

import Control.Exception (catch, finally, try)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text.Encoding as TE
import Kernel.Prelude hiding (catch, finally, try)
import Kernel.Types.Error (GenericError (..))
import Kernel.Utils.Common (MonadFlow, logInfo, throwError)
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
  -- | rendered HTML from Lib.Finance.Invoice.PdfService.renderInvoiceHtml
  Text ->
  m Text
generateFinanceInvoicePdf _key html = do
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
    Left err -> throwError $ InternalError err
    Right bytes -> do
      logInfo $ "Finance invoice PDF generated"
      pure . TE.decodeUtf8 $ B64.encode bytes
  where
    generateBody htmlPath pdfPath = do
      ok <- runGeneratePdfFromHtml htmlPath pdfPath
      if ok
        then do
          exists <- doesFileExist pdfPath
          if exists
            then Right <$> BS.readFile pdfPath
            else pure $ Left "PDF generation failed: output file missing"
        else pure $ Left "PDF generation failed: wkhtmltopdf error"

runGeneratePdfFromHtml :: FilePath -> FilePath -> IO Bool
runGeneratePdfFromHtml htmlPath pdfPath = do
  result <-
    timeout 60000000 $
      ( try
          ( readProcessWithExitCode
              "wkhtmltopdf"
              [ "--enable-external-links",
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
    Just (Right (ExitSuccess, _, _)) -> return True
    _ -> return False
