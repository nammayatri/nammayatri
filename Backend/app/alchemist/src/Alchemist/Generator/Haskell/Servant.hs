module Alchemist.Generator.Haskell.Servant where

import Alchemist.DSL.Syntax.API
import Alchemist.Utils
import Data.List (intercalate)
import Data.List.Extra (snoc)
import Data.Text (Text)
import qualified Data.Text as T
import Prelude

apisToText :: Apis -> String
apisToText input =
  "module Data.Types." <> T.unpack (head (map _moduleName input)) <> " where \n\n"
    <> intercalate "\n" (makeImport <$> figureOutImports (T.unpack <$> concatMap handlerImports input))
    <> T.unpack
      ( ("\n\ntype API = \n " <> T.intercalate "\n <|> " (map apiTTToText input))
          <> "\n \nhandler  :: Flowserver API\nhandler = \n\t"
          <> T.intercalate "\n\t:<|> " (map handlerFunctionText input)
      )
  where
    makeImport :: String -> String
    makeImport x = "import qualified " <> x <> " as " <> x

apiTTToText :: ApiTT -> Text
apiTTToText apiTT =
  let urlPartsText = map urlPartToText (_urlParts apiTT)
      authTypeText = case _authType apiTT of
        Just AdminTokenAuth -> "AdminTokenAuth"
        Just TokenAuth -> "TokenAuth"
        Nothing -> ""
      apiTypeText = apiTypeToText (_apiType apiTT)
      apiReqText = apiReqToText (_apiReqType apiTT)
      apiResText = apiResToText (_apiResType apiTT)
      headerText = map headerToText (_header apiTT)
   in authTypeText <> " " <> T.concat urlPartsText <> T.concat headerText <> apiReqText <> " :> " <> apiTypeText <> apiResText
  where
    urlPartToText :: UrlParts -> Text
    urlPartToText (UnitPath path) = " :> \"" <> path <> "\""
    urlPartToText (Capture path ty) = " :> Capture \"" <> path <> "\" (" <> ty <> ")"
    urlPartToText (QueryParam path ty isMandatory) =
      " :> " <> (if isMandatory then "Mandatory" else "") <> "QueryParam \"" <> path <> "\" " <> ty

    apiReqToText :: Maybe ApiReq -> Text
    apiReqToText Nothing = ""
    apiReqToText (Just (ApiReq ty name)) = " :> ReqBody '[" <> ty <> "] " <> name

    apiResToText :: ApiRes -> Text
    apiResToText (ApiRes ty name) = " '[" <> ty <> "] " <> name

    headerToText :: HeaderType -> Text
    headerToText (Header name ty) = " :> Header \"" <> name <> "\" " <> ty

handlerFunctionText :: ApiTT -> Text
handlerFunctionText apiTT =
  let apiTypeText = T.toLower $ apiTypeToText (_apiType apiTT)
      urlPartsText = map urlPartToName (_urlParts apiTT)
   in apiTypeText <> T.intercalate "" (filter (/= T.empty) urlPartsText)
  where
    urlPartToName :: UrlParts -> Text
    urlPartToName (UnitPath name) = (T.toUpper . T.singleton . T.head) name <> T.tail name
    urlPartToName _ = ""

handlerImports :: ApiTT -> [Text]
handlerImports input =
  let urlTypeText = map urlToText (_urlParts input)
      headerTypeText = map (\(Header _ ty) -> ty) (_header input)
      reqTypeText = reqTypeToText $ _apiReqType input
      resTypeText = (\(ApiRes _ ty) -> ty) $ _apiResType input
   in snoc (snoc (urlTypeText ++ headerTypeText) reqTypeText) resTypeText -- `snoc` resTypeText
  where
    urlToText :: UrlParts -> Text
    urlToText (Capture _ ty) = ty
    urlToText (QueryParam _ ty _) = ty
    urlToText _ = ""

    reqTypeToText :: Maybe ApiReq -> Text
    reqTypeToText Nothing = ""
    reqTypeToText (Just (ApiReq _ ty)) = ty

apiTypeToText :: ApiType -> Text
apiTypeToText apitype = case apitype of
  GET -> "Get"
  POST -> "Post"
  PUT -> "Put"
  DELETE -> "Delete"
