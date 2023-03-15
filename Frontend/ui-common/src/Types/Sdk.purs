{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

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
