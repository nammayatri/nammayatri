module Alchemist.Generator.Purs.Endpoint where

import Alchemist.DSL.Syntax.API
import Alchemist.Generator.Purs.Utils
import Data.Text (Text)
import qualified Data.Text as T
import Prelude

-- ticketPlaceBook :: String -> String
-- ticketPlaceBook placeId = (getBaseUrl "43") <> "/ticket/places/" <> placeId <> "/book"

generateAPIEndpoint :: Apis -> String
generateAPIEndpoint input = do
  T.unpack $ T.intercalate "\n\n" (map generateAPIEndpoint' (_apis input))

generateAPIEndpoint' :: ApiTT -> Text
generateAPIEndpoint' apiTT =
  let functionName = handlerFunctionName apiTT
      urlSignature' = getUrlSignature apiTT
      urlSignature = if T.null urlSignature' then "String" else urlSignature'
      urlParamsText' = getUrlParamsText apiTT
      urlParamsText = if T.null urlParamsText' then " _" else " " <> urlParamsText'
      functionSignature = functionName <> (T.pack " :: ") <> urlSignature <> " -> String"
      urlText = getUrlText apiTT
   in functionSignature <> (T.pack "\n") <> functionName <> urlParamsText <> baseUrlText <> urlText

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
baseUrlText = " = (getBaseUrl \"\")"

getUrlSignature :: ApiTT -> Text
getUrlSignature input =
  T.intercalate " -> " $ filter (/= T.empty) $ map urlToText (_urlParts input)
  where
    urlToText :: UrlParts -> Text
    urlToText (Capture _ _) = "String"
    urlToText (QueryParam _ _ _) = "String"
    urlToText _ = ""
