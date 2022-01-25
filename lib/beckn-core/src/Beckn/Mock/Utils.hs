module Beckn.Mock.Utils where

import Beckn.Types.Core.Error
import Control.Concurrent
import Data.Aeson hiding (Error)
import qualified Data.Aeson as Ae
import qualified Data.Aeson.Types as Ae
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Either.Extra
import Data.List
import Data.String.Conversions
import qualified Data.Text as T
import Data.Time
import Relude
import System.Random

-- | Read formatted time.
-- Here %F means the same as %Y-%m-%d, and %R acts like %H:%M.
-- Example: readUTCTime "2021-12-01 18:00"
readUTCTime :: Text -> Maybe UTCTime
readUTCTime = parseTimeM True defaultTimeLocale "%F %R" . T.unpack

rightToMaybe :: Either e a -> Maybe a
rightToMaybe = either (const Nothing) Just

textToError :: Text -> Error
textToError desc =
  Error
    { _type = CORE_ERROR,
      code = "400",
      path = Nothing,
      message = Just desc
    }

generateOrderId :: (MonadIO m) => m Text
generateOrderId = fmap show $ liftIO $ randomRIO (1000000, 9999999 :: Int)

whenRight :: Applicative m => Either e a -> (a -> m ()) -> m ()
whenRight eith f = either (\_ -> pure ()) f eith

threadDelaySec :: (MonadIO m) => Int -> m ()
threadDelaySec sec = liftIO $ threadDelay $ sec * 1000000

encodeJSON :: (ToJSON a) => a -> BSL.ByteString
encodeJSON = Ae.encode . toJSON

decodeJSON :: (FromJSON a) => BS.ByteString -> Maybe a
decodeJSON bs = Ae.decode (BSL.fromStrict bs) >>= Ae.parseMaybe parseJSON

decodingErrorMessage :: BS.ByteString -> Text
decodingErrorMessage bs = "failed to decode JSON: " <> cs bs

decodeEitherJSON :: (FromJSON a) => BS.ByteString -> Either Text a
decodeEitherJSON bs = do
  val <- maybeToEither (decodingErrorMessage bs) (Ae.decode (BSL.fromStrict bs))
  first T.pack $ Ae.parseEither parseJSON val

findAndDecode :: (FromJSON a) => BS.ByteString -> [(BS.ByteString, BS.ByteString)] -> Either Text a
findAndDecode key list = maybeToEither errMsg (lookup key list) >>= decodeEitherJSON
  where
    errMsg = "failed to find key: " <> cs key
