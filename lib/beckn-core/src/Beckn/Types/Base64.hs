module Beckn.Types.Base64 where

import Beckn.Prelude
import Beckn.Utils.Dhall ()
import qualified Data.Aeson as A
import Data.ByteString
import qualified Data.ByteString.Base64 as Base64
import Dhall hiding (map)

newtype Base64 = Base64 ByteString
  deriving (Show, Eq)

instance FromDhall Base64 where
  autoWith = parseBase64 . autoWith
    where
      parseBase64 :: Decoder Text -> Decoder Base64
      parseBase64 Decoder {..} =
        Decoder
          { extract = \x -> fromMonadic do
              txt <- toMonadic (extract x)
              Base64.decode (encodeUtf8 txt)
                & either (toMonadic . extractError . show) (pure . Base64),
            ..
          }

instance ToJSON Base64 where
  toJSON (Base64 bs) = A.String $ decodeUtf8 $ Base64.encode bs

instance FromJSON Base64 where
  parseJSON = A.withText "Base64" $ \txt ->
    Base64.decode (encodeUtf8 txt)
      & either (fail . show) (pure . Base64)

instance IsString Base64 where
  fromString = Base64 . Base64.decodeLenient . encodeUtf8
