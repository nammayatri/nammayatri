module Alchemist.Generator.Haskell.Servant where

import Alchemist.DSL.Syntax.API
import Alchemist.Utils
import Data.List (intercalate, intersect, nub)
import Data.List.Extra (snoc)
import Data.Text (Text)
import qualified Data.Text as T
import Prelude

generateServantAPI :: Apis -> String
generateServantAPI input =
  "{-# OPTIONS_GHC -Wno-orphans #-}\n"
    <> "{-# OPTIONS_GHC -Wno-unused-imports #-}\n\n"
    <> "module API.UI."
    <> T.unpack (_moduleName input)
    <> " where \n\n"
    <> intercalate "\n" (map ("import " <>) defaultImports)
    <> "\n\n"
    <> intercalate "\n" (nub $ (map makeQualifiedImport defaultQualifiedImport) <> (makeQualifiedImport <$> figureOutImports (T.unpack <$> concatMap handlerSignature (_apis input))))
    <> "\n\n"
    -- <> intercalate "\n"  (T.unpack <$> concatMap handlerImports (_apis input))
    <> "\nimport Domain.Action.UI."
    <> T.unpack (_moduleName input)
    <> " ("
    <> intercalate ", " (map (T.unpack . fst) (_types input))
    <> ")"
    <> "\nimport qualified Domain.Action.UI."
    <> T.unpack (_moduleName input)
    <> " as "
    <> "Domain.Action.UI."
    <> T.unpack (_moduleName input)
    <> T.unpack
      ( ("\n\ntype API = \n " <> T.intercalate "\n :<|> " (map apiTTToText (_apis input)))
          <> "\n \nhandler  :: Environment.FlowServer API\nhandler = "
          <> T.intercalate "\n  :<|> " (map handlerFunctionText (_apis input))
          <> "\n\n"
          <> T.intercalate "\n" (map handlerFunctionDef (_apis input))
      )
  where
    defaultImports :: [String]
    defaultImports = ["EulerHS.Prelude", "Servant", "Tools.Auth", "Kernel.Utils.Common"]

    defaultQualifiedImport :: [String]
    defaultQualifiedImport = ["Domain.Types.Person", "Kernel.Prelude", "Domain.Types.Merchant", "Environment", "Kernel.Types.Id"]

    makeQualifiedImport :: String -> String
    makeQualifiedImport impts = "import qualified " <> impts

    generateParams :: Bool -> Int -> Int -> Text
    generateParams _ _ 0 = ""
    generateParams isbackParam mx n =
      ( if mx == n && isbackParam
          then "(Kernel.Prelude.first Kernel.Prelude.Just a" <> T.pack (show n) <> ")"
          else " a" <> T.pack (show n)
      )
        <> generateParams isbackParam mx (n - 1)

    handlerFunctionDef :: ApiTT -> Text
    handlerFunctionDef apiT =
      let functionName = handlerFunctionText apiT
          allTypes = handlerSignature apiT
          showType = case filter (/= T.empty) (init allTypes) of
            [] -> T.empty
            ty -> " -> " <> T.intercalate " -> " ty
          handlerTypes = showType <> " -> Environment.FlowHandler " <> last allTypes
       in functionName <> " :: (Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant)" <> handlerTypes
            <> "\n"
            <> functionName
            <> generateParams False (length allTypes) (length allTypes)
            <> " = withFlowHandlerAPI $ "
            <> "Domain.Action.UI."
            <> _moduleName input
            <> "."
            <> functionName
            <> generateParams True (length allTypes) (length allTypes)
            <> "\n"

apiTTToText :: ApiTT -> Text
apiTTToText apiTT =
  let urlPartsText = map urlPartToText (_urlParts apiTT)
      authTypeText = case _authType apiTT of
        Just AdminTokenAuth -> "AdminTokenAuth"
        Just TokenAuth -> "TokenAuth"
        Nothing -> "TokenAuth"
      apiTypeText = apiTypeToText (_apiType apiTT)
      apiReqText = apiReqToText (_apiReqType apiTT)
      apiResText = apiResToText (_apiResType apiTT)
      headerText = map headerToText (_header apiTT)
   in authTypeText <> T.concat urlPartsText <> T.concat headerText <> apiReqText <> " :> " <> apiTypeText <> apiResText
  where
    urlPartToText :: UrlParts -> Text
    urlPartToText (UnitPath path) = " :> \"" <> path <> "\""
    urlPartToText (Capture path ty) = " :> Capture \"" <> path <> "\" (" <> ty <> ")"
    urlPartToText (QueryParam path ty isMandatory) =
      " :> " <> (if isMandatory then "Mandatory" else "") <> "QueryParam \"" <> path <> "\" (" <> ty <> ")"

    apiReqToText :: Maybe ApiReq -> Text
    apiReqToText Nothing = ""
    apiReqToText (Just (ApiReq ty frmt)) = " :> ReqBody '[" <> frmt <> "] " <> ty

    apiResToText :: ApiRes -> Text
    apiResToText (ApiRes name ty) = " '[" <> ty <> "] " <> name

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

allImports :: Apis -> [Text]
allImports input =
  let definedTypes = map fst (_types input)
      typesDef = map snd $ concatMap (snd) (_types input)
      allTypes = concatMap handlerSignature (_apis input)
   in filter (/= T.empty) ((typesDef `intersect` definedTypes) ++ (allTypes `intersect` definedTypes))

handlerSignature :: ApiTT -> [Text]
handlerSignature input =
  let urlTypeText = map urlToText (_urlParts input)
      headerTypeText = map (\(Header _ ty) -> ty) (_header input)
      reqTypeText = reqTypeToText $ _apiReqType input
      resTypeText = (\(ApiRes ty _) -> ty) $ _apiResType input
   in filter (/= T.empty) (snoc (snoc (urlTypeText ++ headerTypeText) reqTypeText) resTypeText)
  where
    urlToText :: UrlParts -> Text
    urlToText (Capture _ ty) = ty
    urlToText (QueryParam _ ty isMandatory) = do
      if isMandatory
        then ty
        else "Kernel.Prelude.Maybe (" <> ty <> ")"
    urlToText _ = ""

    reqTypeToText :: Maybe ApiReq -> Text
    reqTypeToText Nothing = ""
    reqTypeToText (Just (ApiReq ty _)) = ty

apiTypeToText :: ApiType -> Text
apiTypeToText apitype = case apitype of
  GET -> "Get"
  POST -> "Post"
  PUT -> "Put"
  DELETE -> "Delete"
