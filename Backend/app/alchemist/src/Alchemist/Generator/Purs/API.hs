module Alchemist.Generator.Purs.API where

import Alchemist.DSL.Syntax.API
import Alchemist.Utils (apiTypeToText, capitalize)
import Data.Text (Text)
import qualified Data.Text as T
import Prelude

mkRestEndpointInstance :: ApiTT -> Maybe (Text, Text) -> Text
mkRestEndpointInstance apiTT wrapperReq =
  "instance make" <> req <> " :: RestEndpoint " <> req <> " " <> resp <> " where\n"
    <> "  makeRequest "
    <> reqBodyText
    <> " headers = do\n"
    <> "    let url = "
    <> baseUrlText
    <> " <> "
    <> url
    <> "\n"
    <> "    defaultMakeRequest "
    <> httpMethod
    <> " url headers reqBody Nothing\n"
    <> "  decodeResponse = decodeJSON\n"
    <> "  encodeRequest req = standardEncode req\n"
  where
    req = requestType apiTT
    resp = responseType apiTT
    httpMethod = T.toUpper $ apiTypeToText $ _apiType apiTT
    url = getUrlText apiTT
    reqBodyText = do
      case wrapperReq of
        Just (wrReq, urlParams) -> "reqBody@(" <> wrReq <> " " <> urlParams <> " body)"
        Nothing -> "reqBody"

mkOtherInstances :: Text -> Maybe Text -> Text
mkOtherInstances reqRespType extraParams =
  "derive instance generic" <> reqRespType <> " :: Generic " <> reqRespType <> " _\n"
    <> "derive instance newtype"
    <> reqRespType
    <> " :: Newtype "
    <> reqRespType
    <> " _\n"
    <> "instance standardEncode"
    <> reqRespType
    <> " :: StandardEncode "
    <> reqRespType
    <> " where standardEncode ("
    <> reqRespType
    <> maybe T.empty (" " <>) extraParams
    <> " body) = standardEncode body\n"
    <> "instance show"
    <> reqRespType
    <> " :: Show "
    <> reqRespType
    <> " where show = genericShow\n"
    <> "instance decode"
    <> reqRespType
    <> " :: Decode "
    <> reqRespType
    <> " where decode = defaultDecode\n"
    <> "instance encode"
    <> reqRespType
    <> " :: Encode "
    <> reqRespType
    <> " where encode = defaultEncode\n"

mkApiTypeInstances :: ApiTT -> Text
mkApiTypeInstances apiTT =
  mkRestEndpointInstance apiTT wrapperReq <> "\n"
    <> mkOtherInstances req Nothing
    <> "\n"
    <> mkOtherInstances resp Nothing
    <> "\n"
    <> mkWrapperReqInstances
  where
    req = requestType apiTT
    wrapperReq = do
      let urlParams = getUrlParamsName apiTT
      if T.null urlParams
        then Nothing
        else Just (req <> "Wrapper", urlParams)
    resp = responseType apiTT
    mkWrapperReqInstances = do
      case wrapperReq of
        Just (wrReq, urlParams) -> mkOtherInstances wrReq (Just urlParams) <> "\n"
        Nothing -> T.empty

generateAPIIntegrationCode :: Apis -> String
generateAPIIntegrationCode input = do
  T.unpack $ T.intercalate "\n------------------------\n" (map mkApiTypeInstances (_apis input))

requestType :: ApiTT -> Text
requestType apiTT =
  case _apiReqType apiTT of
    Just (ApiReq reqType _) -> getLastElement reqType
    Nothing -> do
      let apiName = handlerFunctionName apiTT
      T.pack $ (capitalize (T.unpack apiName)) <> "Request"

responseType :: ApiTT -> Text
responseType apiTT =
  case _apiResType apiTT of
    ApiRes resType _ -> getLastElement resType

getLastElement :: Text -> Text
getLastElement str = last $ T.splitOn (T.pack ".") str

getUrlText :: ApiTT -> Text
getUrlText api = do
  foldl urlPartToText T.empty (_urlParts api)
  where
    urlPartToText :: Text -> UrlParts -> Text
    urlPartToText acc (UnitPath name) = acc <> (T.pack " <> \"/") <> name <> (T.pack "\"")
    urlPartToText acc (Capture name _) = acc <> (T.pack " <> \"/\" <> ") <> name
    urlPartToText acc (QueryParam name _ _) =
      if "?" `T.isInfixOf` acc
        then acc <> " <> \"&" <> name <> "=\" <> " <> name
        else acc <> " <> \"?" <> name <> "=\" <> " <> name

baseUrlText :: Text
baseUrlText = "(getBaseUrl \"\")"

getUrlParamsType :: ApiTT -> Text
getUrlParamsType input =
  T.intercalate " " $ filter (/= T.empty) $ map urlParamType (_urlParts input)
  where
    urlParamType :: UrlParts -> Text
    urlParamType (Capture _ _) = "String"
    urlParamType (QueryParam _ _ _) = "String"
    urlParamType _ = T.empty

getUrlParamsName :: ApiTT -> Text
getUrlParamsName apiTT = do
  let urlParams = filter (/= T.empty) (map urlParamName (_urlParts apiTT))
  T.intercalate " " urlParams
  where
    urlParamName :: UrlParts -> Text
    urlParamName (UnitPath _) = T.empty
    urlParamName (Capture name _) = name
    urlParamName (QueryParam name _ _) = name

handlerFunctionName :: ApiTT -> Text
handlerFunctionName apiTT =
  let apiTypeText = T.toLower $ apiTypeToText (_apiType apiTT)
      urlPartsText = map urlPartToName (_urlParts apiTT)
   in apiTypeText <> T.intercalate "" (filter (/= T.empty) urlPartsText)
  where
    urlPartToName :: UrlParts -> Text
    urlPartToName (UnitPath name) = (T.toUpper . T.singleton . T.head) name <> T.tail name
    urlPartToName _ = ""

--
-- generateAPIEndpoint' :: ApiTT -> Text
-- generateAPIEndpoint' apiTT =
--   let functionName = handlerFunctionName apiTT
--       urlSignature' = getUrlSignature apiTT
--       urlSignature = if T.null urlSignature' then "String" else urlSignature'
--       urlParamsText' = getUrlParamsText apiTT
--       urlParamsText = if T.null urlParamsText' then " _" else " " <> urlParamsText'
--       functionSignature = functionName <> (T.pack " :: ") <> urlSignature <> " -> String"
--       urlText = getUrlText apiTT
--    in functionSignature <> (T.pack "\n") <> functionName <> urlParamsText <> baseUrlText <> urlText
