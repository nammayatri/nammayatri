module Passetto.Lib
  ( mkPassettoContextFromKeys,
    mkPassettoContextAuto,
  )
where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Crypto.Saltine (sodiumInit)
import qualified Crypto.Saltine.Class as Saltine
import qualified Crypto.Saltine.Core.Box as Box
import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as LBS
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word16)
import Passetto.Client
  ( PassettoContext (..),
    mkDefPassettoContext,
  )
import Passetto.Client.Bulk
  ( BulkDecryptCall,
    BulkEncryptCall,
    EncryptedRaw (..),
  )
import Passetto.Client.Error (PassettoClientError (..))
import System.Environment (getEnv, lookupEnv)
import System.IO.Error (userError)
import System.Random (randomRIO)
import Text.Read (readMaybe)

type KeyPair = (Box.SecretKey, Box.PublicKey)

data KeyEntry = KeyEntry
  { keyId :: Int,
    keySecretKey :: Text,
    keyPublicKey :: Text
  }

instance Aeson.FromJSON KeyEntry where
  parseJSON = Aeson.withObject "KeyEntry" $ \o ->
    KeyEntry
      <$> o .: "id"
      <*> o .: "secretKey"
      <*> o .: "publicKey"

newtype KeysConfig = KeysConfig {kcKeys :: [KeyEntry]}

instance Aeson.FromJSON KeysConfig where
  parseJSON = Aeson.withObject "KeysConfig" $ \o ->
    KeysConfig <$> o .: "keys"

parseKeypair :: KeyEntry -> Either String KeyPair
parseKeypair KeyEntry {..} = do
  skBs <- Base64.decode (TE.encodeUtf8 keySecretKey)
  pkBs <- Base64.decode (TE.encodeUtf8 keyPublicKey)
  sk <- note "secretKey must be 32 bytes" (Saltine.decode skBs :: Maybe Box.SecretKey)
  pk <- note "publicKey must be 32 bytes" (Saltine.decode pkBs :: Maybe Box.PublicKey)
  return (sk, pk)
  where
    note msg = maybe (Left msg) Right

loadKeypairs :: IO (Vector KeyPair)
loadKeypairs = do
  putStrLn "[passetto-lib] Loading keys from PASSETTO_KEYS env var..."
  raw <- getEnv "PASSETTO_KEYS"
  cfg <- case Aeson.eitherDecodeStrict (TE.encodeUtf8 $ T.pack raw) of
    Left err -> ioError . userError $ "[passetto-lib] PASSETTO_KEYS parse error: " <> err
    Right c -> return (c :: KeysConfig)
  let sorted = sortOn keyId (kcKeys cfg)
  when (null sorted) $ ioError (userError "[passetto-lib] PASSETTO_KEYS contains no keys")
  case mapM parseKeypair sorted of
    Left err -> ioError . userError $ "[passetto-lib] Invalid keypair in PASSETTO_KEYS: " <> err
    Right kps -> do
      putStrLn $ "[passetto-lib] Loaded " <> show (length kps) <> " keypair(s) successfully."
      return $ V.fromList kps

passettoVersion :: Text
passettoVersion = "0.1.0"

taggedPlaintext :: Aeson.Value -> Text
taggedPlaintext = \case
  Aeson.Bool True -> "T"
  Aeson.Bool False -> "F"
  Aeson.Number n -> "N" <> T.pack (show n)
  Aeson.Null -> "0"
  other -> "S" <> TE.decodeUtf8 (LBS.toStrict (Aeson.encode other))

bulkEncryptWithKeys :: Vector KeyPair -> BulkEncryptCall
bulkEncryptWithKeys _ [] = return []
bulkEncryptWithKeys keys vals = liftIO $ mapM encryptOne vals
  where
    encryptOne val = do
      idx <- randomRIO (0, V.length keys - 1)
      let (_, pk) = keys V.! idx
      cipherBs <- Box.boxSeal pk (TE.encodeUtf8 $ taggedPlaintext val)
      let cipher64 = TE.decodeUtf8 $ Base64.encode cipherBs
      return . EncryptedRaw $
        passettoVersion <> "|" <> T.pack (show idx) <> "|" <> cipher64

bulkDecryptWithKeys :: Vector KeyPair -> BulkDecryptCall
bulkDecryptWithKeys _ [] = return []
bulkDecryptWithKeys keys encs = mapM decryptOne encs
  where
    decryptOne (EncryptedRaw encText) = do
      (idxTxt, cipher64) <- case T.splitOn "|" encText of
        [_ver, i, c] -> return (i, c)
        _ ->
          throwError $
            UnexpectedResponse
              "Invalid ciphertext: expected 'version|idx|base64'"
      idx <- case readMaybe (T.unpack idxTxt) :: Maybe Int of
        Just i -> return i
        Nothing ->
          throwError $
            UnexpectedResponse $
              "Invalid key index: " <> idxTxt
      (sk, pk) <- case keys V.!? idx of
        Just kp -> return kp
        Nothing ->
          throwError $
            UnexpectedResponse $
              "Key index out of range: " <> T.pack (show idx)
      cipherBs <- case Base64.decode (TE.encodeUtf8 cipher64) of
        Right bs -> return bs
        Left _ -> throwError $ UnexpectedResponse "Invalid base64 in ciphertext"
      plainBs <- case Box.boxSealOpen pk sk cipherBs of
        Just bs -> return bs
        Nothing ->
          throwError $
            UnexpectedResponse
              "Decryption failed wrong key or corrupted ciphertext"
      jsonText <- case TE.decodeUtf8' plainBs of
        Right t -> return t
        Left _ ->
          throwError $
            UnexpectedResponse
              "Decrypted bytes are not valid UTF-8"
      parseJsonText jsonText

    parseJsonText txt =
      case T.uncons txt of
        Just ('S', rest) ->
          case Aeson.eitherDecodeStrict (TE.encodeUtf8 rest) of
            Right v@(Aeson.String _) -> return v
            Right _ -> return (Aeson.String rest)
            Left _ -> return (Aeson.String rest)
        Just ('N', rest) ->
          case Aeson.eitherDecodeStrict (TE.encodeUtf8 rest) of
            Right v -> return v
            Left err ->
              throwError $
                UnexpectedResponse $
                  "Invalid number after N prefix: " <> T.pack err
        Just ('T', rest) | T.null rest -> return (Aeson.Bool True)
        Just ('F', rest) | T.null rest -> return (Aeson.Bool False)
        Just ('0', rest) | T.null rest -> return Aeson.Null
        _ -> parseWithoutPrefix txt

    parseWithoutPrefix txt =
      case Aeson.eitherDecodeStrict (TE.encodeUtf8 txt) of
        Right v -> return v
        Left err ->
          throwError $
            UnexpectedResponse $
              "Failed to decode decrypted JSON: " <> T.pack err

mkPassettoContextFromKeys :: MonadIO m => m PassettoContext
mkPassettoContextFromKeys = liftIO $ do
  sodiumInit
  keys <- loadKeypairs
  putStrLn "[passetto-lib] PassettoContext built all encrypt/decrypt calls will use local crypto."
  return
    PassettoContext
      { bulkEncryptAction = bulkEncryptWithKeys keys,
        bulkDecryptAction = bulkDecryptWithKeys keys
      }

mkPassettoContextAuto :: MonadIO m => String -> Word16 -> m PassettoContext
mkPassettoContextAuto host port = do
  useLib <- liftIO $ lookupEnv "USE_PASSETTO_LIB"
  case useLib of
    Just "true" -> do
      liftIO $ putStrLn "[passetto-lib] USE_PASSETTO_LIB=true using in-process crypto lib."
      mkPassettoContextFromKeys
    Just val -> do
      liftIO $ putStrLn $ "[passetto-lib] USE_PASSETTO_LIB=" <> val <> " (not 'true') using passetto HTTP service at " <> host <> ":" <> show port
      mkDefPassettoContext host port
    Nothing -> do
      liftIO $ putStrLn $ "[passetto-lib] USE_PASSETTO_LIB not set using passetto HTTP service at " <> host <> ":" <> show port
      mkDefPassettoContext host port
