module Alchemist.Generator.Purs.Backend where

import Alchemist.DSL.Syntax.API
import Alchemist.Generator.Purs.Utils
import Data.List.Extra (snoc)
import Data.Text (Text)
import qualified Data.Text as T
import Prelude

-- bookTicketsBT :: TicketBookingReq -> String -> FlowBT String CreateOrderRes
-- bookTicketsBT payload placeId = do
--     headers <- getHeaders' "" false
--     withAPIResultBT (EP.ticketPlaceBook placeId) (\x -> x) errorHandler (lift $ lift $ callAPI headers (TicketBookingRequest placeId payload))
--     where
--     errorHandler errorPayload = do
--             BackT $ pure GoBack

generateBackendCallAPI :: Apis -> String
generateBackendCallAPI input = do
  T.unpack $ T.intercalate "\n" (map generateBackendCallAPI' (_apis input))

generateBackendCallAPI' :: ApiTT -> Text
generateBackendCallAPI' api =
  (handlerFunctionSignature api) <> getHeadersText <> (withAPIResultText api) <> (T.pack "    where\n") <> errorHandlerText

errorHandlerText :: Text
errorHandlerText =
  "    errorHandler errorPayload = do\n"
    <> "      BackT $ pure GoBack\n"

getHeadersText :: Text
getHeadersText =
  "    headers <- getHeaders' \"\" false\n"

withAPIResultText :: ApiTT -> Text
withAPIResultText apiT = do
  let urlParamsText = getUrlParamsText apiT
  let addSpaceForUrlParams = if T.null urlParamsText then "" else " "
  let addEmptyStringForUrlParams = if T.null urlParamsText then " \"\" " else " "
  "    withAPIResultBT (EP."
    <> handlerFunctionName apiT
    <> addEmptyStringForUrlParams
    <> urlParamsText
    <> addSpaceForUrlParams
    <> ") (\\x -> x) errorHandler (lift $ lift $ callAPI headers ("
    <> (reqTypeToText $ _apiReqType apiT)
    <> " payload"
    <> addSpaceForUrlParams
    <> urlParamsText
    <> addSpaceForUrlParams
    <> "))\n"

handlerFunctionSignature :: ApiTT -> Text
handlerFunctionSignature apiT =
  let functionName = handlerFunctionName apiT
      handlerSignature = getHandlerTypeSignature apiT
      handlerInputSignature = case filter (/= T.empty) (init handlerSignature) of
        [] -> T.empty
        ty -> " -> " <> T.intercalate " -> " ty
      handlerTypes = handlerInputSignature <> " -> FlowBT String " <> last handlerSignature
      urlParamsText = getUrlParamsText apiT
      addSpaceForUrlParams = if T.null urlParamsText then "" else " "
      functionParams = (T.pack " payload") <> addSpaceForUrlParams <> urlParamsText
   in functionName <> handlerTypes <> (T.pack "\n") <> functionName <> functionParams <> (T.pack " = do\n")

getHandlerTypeSignature :: ApiTT -> [Text]
getHandlerTypeSignature input =
  let urlTypeText = map urlToText (_urlParts input)
      reqTypeText = reqTypeToText $ _apiReqType input
      resTypeText = (\(ApiRes ty _) -> getLastElement ty) $ _apiResType input
   in filter (/= T.empty) (snoc (snoc urlTypeText reqTypeText) resTypeText)
  where
    urlToText :: UrlParts -> Text
    urlToText (Capture _ _) = "String"
    urlToText (QueryParam _ _ _) = "String"
    urlToText _ = ""

reqTypeToText :: Maybe ApiReq -> Text
reqTypeToText Nothing = ""
reqTypeToText (Just (ApiReq ty _)) = getLastElement ty
