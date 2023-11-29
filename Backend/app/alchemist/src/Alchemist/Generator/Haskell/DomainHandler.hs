module Alchemist.Generator.Haskell.DomainHandler where

import Alchemist.DSL.Syntax.API
import Alchemist.Generator.Haskell.Servant (handlerFunctionText, handlerImports)
import Alchemist.Utils
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T
import Prelude

generateDomainHandler :: Apis -> String
generateDomainHandler input =
  "module API.UI." <> T.unpack (_moduleName input) <> " where \n\n"
    <> intercalate "\n" (map ("import " <>) defaultImports)
    <> "\n\n"
    <> intercalate "\n" (map makeImport defaultQualifiedImport)
    <> "\n"
    <> intercalate "\n" (makeImport <$> figureOutImports (T.unpack <$> concatMap handlerImports (_apis input)))
    <> T.unpack ("\n\n" <> T.intercalate "\n\n" (map handlerFunctionDef (_apis input)))
  where
    defaultImports :: [String]
    defaultImports = ["EulerHS.Prelude", "Servant", "Tools.Auth"]

    defaultQualifiedImport :: [String]
    defaultQualifiedImport = ["Domain.Types.Person", "Domain.Types.Merchant", "Environment"]

    makeImport :: String -> String
    makeImport x = "import qualified " <> x <> " as " <> x

    handlerFunctionDef :: ApiTT -> Text
    handlerFunctionDef apiT =
      let functionName = handlerFunctionText apiT
          allTypes = handlerImports apiT
          showType = case filter (/= T.empty) (init allTypes) of
            [] -> T.empty
            ty -> " -> " <> T.intercalate " -> " ty
          handlerTypes = showType <> " -> " <> last allTypes
       in functionName <> " :: (Id Person.Person, Id Merchant.Merchant)" <> handlerTypes
            <> "\n"
            <> functionName
            <> " = error \"Logic yet to be decided\""
