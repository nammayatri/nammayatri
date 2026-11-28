module Engineering.Error.Types.ApiResponseError where

import Prelude
import Foreign.Object (Object)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Combinators ((.:))
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))

type ApiResponseError = 
  { code :: Int
  , responseHeaders :: Object (Array String)
  , response :: 
      { error :: Boolean
      , errorMessage :: ErrorRepresentation
      , userMessage :: String
      }
  , status :: String
  }

data ErrorRepresentation
  = RawError String
  | ParsedError ParsedErrorMessage

newtype ParsedErrorMessage = ParsedErrorMessage
  { errorCode :: String
  , errorMessage :: String
  , errorPayload :: String
  }

instance decodeJsonParsedErrorMessage :: DecodeJson ParsedErrorMessage where
  decodeJson json = do
    obj <- decodeJson json
    errorCode <- obj .: "errorCode"
    errorMessage <- obj .: "errorMessage"
    errorPayload <- obj .: "errorPayload"
    pure $ ParsedErrorMessage { errorCode, errorMessage, errorPayload }

parseErrorMessage :: String -> ErrorRepresentation
parseErrorMessage jsonStr =
  case jsonParser jsonStr of
    Right json -> case decodeJson json of
                    Right parsed -> ParsedError parsed
                    Left _ -> RawError jsonStr
    Left _ -> RawError jsonStr
