module Services.CacAPIType where


import PaymentPage
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Debug (spy)
import Foreign.Class (class Decode, class Encode, decode, encode)
import Foreign.Generic (decodeJSON)
import Foreign.Generic.EnumEncoding (genericDecodeEnum, genericEncodeEnum, defaultGenericEnumOptions)
import Prelude (class Eq, class Show, bind, show, ($), (<$>))
import Control.Alt ((<|>))
import Data.Maybe (Maybe(..))
import Presto.Core.Types.API (class RestEndpoint, class StandardEncode, ErrorResponse, Method(..), defaultMakeRequest, standardEncode, defaultDecodeResponse, defaultMakeRequestString)
import Presto.Core.Utils.Encoding (defaultDecode, defaultEncode)
import Services.EndPoints as EP
import Foreign (ForeignError(..), fail)


data GetUiConfigReq = GetUiConfigReq Int

data GetUiConfigResp = EmptyGetUiConfigResp {} --CACTODO: Change to actual resp type
  | GetUiConfigResp {
      currency :: String
  }


instance makeGetUiConfigReq :: RestEndpoint GetUiConfigReq GetUiConfigResp where
  makeRequest reqBody@(GetUiConfigReq toss) headers = defaultMakeRequest GET (EP.getUiConfig toss) headers reqBody Nothing
  decodeResponse = decodeJSON
  encodeRequest req = standardEncode req

derive instance genericGetUiConfigReq :: Generic GetUiConfigReq _
instance showGetUiConfigReq :: Show GetUiConfigReq where show = genericShow
instance standardEncodeGetUiConfigReq :: StandardEncode GetUiConfigReq where standardEncode (GetUiConfigReq req) = standardEncode req
instance decodeGetUiConfigReq :: Decode GetUiConfigReq where decode = defaultDecode
instance encodeGetUiConfigReq :: Encode GetUiConfigReq where encode = defaultEncode

derive instance genericGetUiConfigResp :: Generic GetUiConfigResp _
instance standardGetUiConfigResp :: StandardEncode GetUiConfigResp where 
  standardEncode (GetUiConfigResp body) = standardEncode body
  standardEncode (EmptyGetUiConfigResp _) = standardEncode {} 
instance showGetUiConfigResp :: Show GetUiConfigResp where show = genericShow
instance decodeGetUiConfigResp :: Decode GetUiConfigResp where decode body = (GetUiConfigResp <$> decode body) <|> (EmptyGetUiConfigResp <$> decode body) <|> (fail $ ForeignError "Unknown response")
instance encodeGetUiConfigResp  :: Encode GetUiConfigResp where encode = defaultEncode
instance eqGetUiConfigResp :: Eq GetUiConfigResp where eq = genericEq