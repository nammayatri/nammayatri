module Alchemist.Generator.Purs.Utils where

import Alchemist.DSL.Syntax.API
import Data.Text (Text)
import qualified Data.Text as T
import Prelude

apiTypeToText :: ApiType -> Text
apiTypeToText apitype = case apitype of
  GET -> "Get"
  POST -> "Post"
  PUT -> "Put"
  DELETE -> "Delete"

getLastElement :: Text -> Text
getLastElement str = last $ T.splitOn (T.pack ".") str

handlerFunctionName :: ApiTT -> Text
handlerFunctionName apiTT =
  let apiTypeText = T.toLower $ apiTypeToText (_apiType apiTT)
      urlPartsText = map urlPartToName (_urlParts apiTT)
   in apiTypeText <> T.intercalate "" (filter (/= T.empty) urlPartsText)
  where
    urlPartToName :: UrlParts -> Text
    urlPartToName (UnitPath name) = (T.toUpper . T.singleton . T.head) name <> T.tail name
    urlPartToName _ = ""

convertUrlPartsText :: UrlParts -> Text
convertUrlPartsText (UnitPath _) = ""
convertUrlPartsText (Capture name _) = name
convertUrlPartsText (QueryParam name _ _) = name

getUrlParamsText :: ApiTT -> Text
getUrlParamsText apiTT = do
  let urlParams = filter (/= T.empty) (map convertUrlPartsText (_urlParts apiTT))
  T.intercalate " " urlParams
