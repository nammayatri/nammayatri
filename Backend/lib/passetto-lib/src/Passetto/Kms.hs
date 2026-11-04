module Passetto.Kms (decryptKmsTexts) where

import qualified Amazonka
import qualified Amazonka.KMS as KMS
import qualified Amazonka.KMS.Lens as KMS
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, throwIO, try)
import Control.Lens ((^.))
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.IO.Error (userError)

kmsPrefix :: Text
kmsPrefix = "kms:"

decryptAttempts :: Int
decryptAttempts = 3

-- | Decrypts the values written as @"kms:<base64 ciphertext>"@, other values are returned as they are.
-- The AWS client is only created if there is at least one such value. Credentials come from the
-- default AWS credential chain (the pod's AWS_ROLE_ARN / AWS_WEB_IDENTITY_TOKEN_FILE).
decryptKmsTexts :: [Text] -> IO [Text]
decryptKmsTexts texts
  | not (any (T.isPrefixOf kmsPrefix) texts) = pure texts
  | otherwise = do
    env <- Amazonka.newEnv Amazonka.discover
    mapM (decryptText env) texts

decryptText :: Amazonka.Env -> Text -> IO Text
decryptText env txt = case T.stripPrefix kmsPrefix txt of
  Nothing -> pure txt
  Just payload -> do
    ciphertext <- either (const $ failWith "value after 'kms:' is not valid base64") pure $ Base64.decode (TE.encodeUtf8 (T.strip payload))
    plaintext <- withRetries $ do
      response <- runResourceT $ Amazonka.send env (KMS.newDecrypt ciphertext)
      maybe (failWith "AWS KMS Decrypt returned no plaintext") pure (response ^. KMS.decryptResponse_plaintext)
    either (const $ failWith "decrypted value is not valid UTF-8") pure (TE.decodeUtf8' plaintext)
  where
    failWith :: String -> IO a
    failWith reason = ioError . userError $ "[passetto-lib] Can't decrypt kms: value: " <> reason

withRetries :: IO BS.ByteString -> IO BS.ByteString
withRetries action = go 1
  where
    go n = do
      result <- try action
      case result of
        Right a -> pure a
        Left (e :: SomeException)
          | n >= decryptAttempts -> throwIO . userError $ "[passetto-lib] AWS KMS Decrypt failed after " <> show decryptAttempts <> " attempts: " <> show e
          | otherwise -> threadDelay (n * 1000000) >> go (n + 1)
