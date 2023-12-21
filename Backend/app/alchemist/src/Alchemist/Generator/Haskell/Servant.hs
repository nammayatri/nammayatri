module Alchemist.Generator.Haskell.Servant (generateServantAPI, handlerSignature, handlerFunctionText) where

import Alchemist.DSL.Syntax.API
import Alchemist.GeneratorCore
import Alchemist.Utils
import Control.Lens ((^.))
import Data.List (intercalate, nub)
import Data.List.Extra (snoc)
import qualified Data.Text as T
import Kernel.Prelude hiding (replicateM)

generateServantAPI :: Apis -> Code
generateServantAPI input =
  generateCode generatorInput
  where
    generatorInput :: GeneratorInput
    generatorInput =
      GeneratorInput
        { _ghcOptions = ["-Wno-orphans", "-Wno-unused-imports"],
          _extensions = [],
          _moduleNm = "API.Action.UI." <> T.unpack (_moduleName input),
          _simpleImports = allSimpleImports,
          _qualifiedImports = allQualifiedImports,
          _codeBody = generateCodeBody mkCodeBody input
        }
    defaultQualifiedImport :: [String]
    defaultQualifiedImport =
      [ "Domain.Types.Person",
        "Kernel.Prelude",
        "Domain.Types.Merchant",
        "Environment",
        "Kernel.Types.Id"
      ]

    allQualifiedImports :: [String]
    allQualifiedImports =
      [ "Domain.Action.UI."
          <> T.unpack (_moduleName input)
          <> " as "
          <> "Domain.Action.UI."
          <> T.unpack (_moduleName input)
      ]
        <> ( nub $
               defaultQualifiedImport
                 <> ( figureOutImports
                        (T.unpack <$> concatMap handlerSignature (_apis input))
                    )
           )

    allSimpleImports :: [String]
    allSimpleImports =
      [ "EulerHS.Prelude",
        "Servant",
        "Tools.Auth",
        "Kernel.Utils.Common",
        "Storage.Beam.SystemConfigs ()",
        "API.Types.UI."
          <> T.unpack (_moduleName input)
          <> " ("
          <> intercalate ", " (map (T.unpack . fst) (input ^. apiTypes . types))
          <> ")"
      ]

mkCodeBody :: ApisM ()
mkCodeBody = do
  input <- ask
  let allApis = _apis input
      moduleName' = _moduleName input
      seperator = onNewLine (tellM " :<|> ")
      handlerFunctionText' = tellM . T.unpack . handlerFunctionText
  onNewLine $ do
    tellM "type API = "
    onNewLine $ withSpace $ intercalateA seperator (map apiTTToText allApis)
  onNewLine $ do
    tellM "handler  :: Environment.FlowServer API"
    onNewLine $ tellM "handler = "
    withSpace $ intercalateA seperator (map handlerFunctionText' allApis)
  onNewLine $ intercalateA newLine (map (handlerFunctionDef moduleName') allApis)
  where
    isAuthPresent :: ApiTT -> Bool
    isAuthPresent apiT = case _authType apiT of
      Just NoAuth -> False
      _ -> True

    generateParams :: Bool -> Bool -> Int -> Int -> Text
    generateParams _ _ _ 0 = ""
    generateParams isAuth isbackParam mx n =
      ( if mx == n && isbackParam && isAuth
          then " (Kernel.Prelude.first Kernel.Prelude.Just a" <> T.pack (show n) <> ")"
          else " a" <> T.pack (show n)
      )
        <> generateParams isAuth isbackParam mx (n - 1)

    handlerFunctionDef :: Text -> ApiTT -> ApisM ()
    handlerFunctionDef moduleName' apiT =
      let functionName = handlerFunctionText apiT
          allTypes = handlerSignature apiT
          showType = case filter (/= T.empty) (init allTypes) of
            [] -> T.empty
            ty -> T.intercalate " -> " ty
          handlerTypes = showType <> (if length allTypes > 1 then " -> " else " ") <> "Environment.FlowHandler " <> last allTypes
       in tellM $
            T.unpack $
              functionName <> (if isAuthPresent apiT then " :: (Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> " else " :: ") <> handlerTypes
                <> "\n"
                <> functionName
                <> generateParams (isAuthPresent apiT) False (length allTypes) (if isAuthPresent apiT then length allTypes else length allTypes - 1)
                <> " = withFlowHandlerAPI $ "
                <> "Domain.Action.UI."
                <> moduleName'
                <> "."
                <> functionName
                <> generateParams (isAuthPresent apiT) True (length allTypes) (if isAuthPresent apiT then length allTypes else length allTypes - 1)
                <> "\n"

apiTTToText :: ApiTT -> ApisM ()
apiTTToText apiTT =
  let urlPartsText = map urlPartToText (_urlParts apiTT)
      apiTypeText = apiTypeToText (_apiType apiTT)
      apiReqText = apiReqToText (_apiReqType apiTT)
      apiResText = apiResToText (_apiResType apiTT)
      headerText = map headerToText (_header apiTT)
   in tellM $
        T.unpack $
          addAuthToApi (_authType apiTT) (T.concat urlPartsText <> T.concat headerText <> apiReqText <> " :> " <> apiTypeText <> apiResText)
  where
    addAuthToApi :: Maybe AuthType -> Text -> Text
    addAuthToApi authtype apiDef = case authtype of
      Just AdminTokenAuth -> "AdminTokenAuth" <> apiDef
      Just TokenAuth -> "TokenAuth" <> apiDef
      Just NoAuth -> fromMaybe apiDef (T.stripPrefix " :>" apiDef)
      Nothing -> "TokenAuth" <> apiDef

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
