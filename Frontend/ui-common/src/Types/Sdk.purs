module Common.Types.Sdk where
  
import Data.Maybe
import Presto.Core.Utils.Encoding (defaultDecode, defaultEncode)
import Foreign.Class (class Decode, class Encode)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)

newtype ErrorResponse = ErrorResponse
  { success :: Boolean
  , message :: String
  }

derive instance genericErrorResponse :: Generic ErrorResponse _
derive instance newtypeErrorResponse :: Newtype ErrorResponse _
instance encodeErrorResponse :: Encode ErrorResponse where encode = defaultEncode
instance decodeErrorResponse :: Decode ErrorResponse where decode = defaultDecode

newtype SDKResponse b = SDKResponse 
  { 
    success :: Boolean
  , event :: String
  , payload :: Maybe b
  }

derive instance genericSDKResponse :: Generic (SDKResponse b)  _
derive instance newtypeSDKResponse :: Newtype (SDKResponse b) _
instance decodeSDKResponse :: Decode b => Decode (SDKResponse b) where decode = defaultDecode
instance encodeSDKResponse :: Encode b => Encode (SDKResponse b) where encode = defaultEncode

newtype SDKRequest a = SDKRequest 
  { event :: String
    ,payload :: a
  }

derive instance genericSDKRequest :: Generic (SDKRequest a)  _
derive instance newtypeSDKRequest :: Newtype (SDKRequest a) _
instance decodeSDKRequest :: Decode a => Decode (SDKRequest a) where decode = defaultDecode
instance encodeSDKRequest :: Encode a => Encode (SDKRequest a) where encode = defaultEncode  

newtype FetchIdentifierResponse = FetchIdentifierResponse
  { aaVpa :: String   
  }

derive instance genericFetchIdentifierResponse :: Generic (FetchIdentifierResponse )  _
derive instance newtypeFetchIdentifierResponse :: Newtype (FetchIdentifierResponse ) _
instance decodeFetchIdentifierResponse :: Decode (FetchIdentifierResponse ) where decode = defaultDecode
instance encodeFetchIdentifierResponse :: Encode (FetchIdentifierResponse ) where encode = defaultEncode


newtype GetConsentResponse = GetConsentResponse 
  { success :: Boolean
  }

derive instance genericGetConsentResponse :: Generic (GetConsentResponse )  _
derive instance newtypeGetConsentResponse :: Newtype (GetConsentResponse ) _
instance decodeGetConsentResponse :: Decode (GetConsentResponse ) where decode = defaultDecode
instance encodeGetConsentResponse :: Encode (GetConsentResponse ) where encode = defaultEncode  

newtype FetchIdentifierRequest  =  FetchIdentifierRequest
  { mobileNumber :: String
   ,pan          :: String
   ,timeStamp    :: String
   , url :: String
   , name :: String
  }

derive instance genericFetchIdentifierRequest :: Generic (FetchIdentifierRequest )  _
derive instance newtypeFetchIdentifierRequest :: Newtype (FetchIdentifierRequest ) _
instance decodeFetchIdentifierRequest :: Decode (FetchIdentifierRequest ) where decode = defaultDecode
instance encodeFetchIdentifierRequest :: Encode (FetchIdentifierRequest ) where encode = defaultEncode

newtype GetConsentRequest  =  GetConsentRequest 
  { getConsentHandle :: Array String
   ,mobileNumber :: String
   ,pan          :: String
   ,timeStamp    :: String
   , url :: String
   , name :: String
  }

derive instance genericGetConsentRequest :: Generic (GetConsentRequest )  _
derive instance newtypeGetConsentRequest :: Newtype (GetConsentRequest ) _
instance decodeGetConsentRequest :: Decode (GetConsentRequest ) where decode = defaultDecode
instance encodeGetConsentRequest :: Encode (GetConsentRequest ) where encode = defaultEncode
