module Utils where

import Beckn.Prelude
import Beckn.Types.Core.Error
import Control.Concurrent
import qualified Data.Aeson as Ae
import qualified Data.Aeson.Types as Ae
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import Data.Time
import System.Random

-- | Read formatted time.
-- Here %F means the same as %Y-%m-%d, and %R acts like %H:%M.
-- Example: readUTCTime "2021-12-01 18:00"
readUTCTime :: Text -> Maybe UTCTime
readUTCTime = parseTimeM True defaultTimeLocale "%F %R" . T.unpack

textToError :: Text -> Error
textToError desc =
  Error
    { _type = CORE_ERROR,
      code = "400",
      path = Nothing,
      message = Just desc
    }

generateOrderId :: IO Text
generateOrderId = show <$> randomRIO (1000000, 9999999 :: Int)

whenRight :: Applicative m => Either e a -> (a -> m ()) -> m ()
whenRight eith f = either (\_ -> pure ()) f eith

threadDelaySec :: Int -> IO ()
threadDelaySec sec = threadDelay $ sec * 1000000

encodeJSON :: (ToJSON a) => a -> BSL.ByteString
encodeJSON = Ae.encode . toJSON

decodeJSON :: (FromJSON a) => BSL.ByteString -> Maybe a
decodeJSON bs = Ae.decode bs >>= Ae.parseMaybe parseJSON
