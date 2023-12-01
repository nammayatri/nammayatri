module Alchemist.Generator.Haskell.DomainHandler where

-- import Alchemist.DSL.Parser.API hiding (figureOutImports)
import Alchemist.DSL.Syntax.API
import Alchemist.Generator.Haskell.Servant (handlerFunctionText, handlerSignature)
import Alchemist.Utils
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T
import Prelude

generateDomainHandler :: Apis -> String
generateDomainHandler input =
  "{-# OPTIONS_GHC -Wno-orphans #-}\n"
    <> "{-# OPTIONS_GHC -Wno-unused-imports #-}"
    <> "\n\n"
    <> "module Domain.Action.UI."
    <> T.unpack (_moduleName input)
    <> " where \n\n"
    <> intercalate "\n" (map ("import " <>) defaultImports)
    <> "\n\n"
    <> intercalate "\n" (map makeImport defaultQualifiedImport)
    <> "\n"
    <> intercalate "\n" (makeImport <$> figureOutImports (T.unpack <$> concatMap handlerSignature (_apis input)))
    <> "\n\n"
    <> T.unpack (generateHaskellTypes (_types input))
    <> T.unpack (T.intercalate "\n\n" (map handlerFunctionDef (_apis input)))
  where
    defaultImports :: [String]
    defaultImports = ["EulerHS.Prelude hiding (id)", "Servant", "Tools.Auth", "Data.OpenApi (ToSchema)"]

    defaultQualifiedImport :: [String]
    defaultQualifiedImport = ["Domain.Types.Person", "Domain.Types.Merchant", "Environment", "Kernel.Types.Id"]

    makeImport :: String -> String
    makeImport x = "import qualified " <> x <> " as " <> x

    handlerFunctionDef :: ApiTT -> Text
    handlerFunctionDef apiT =
      let functionName = handlerFunctionText apiT
          allTypes = handlerSignature apiT
          showType = case filter (/= T.empty) (init allTypes) of
            [] -> T.empty
            ty -> " -> " <> T.intercalate " -> " ty
          handlerTypes = showType <> " -> " <> "Environment.Flow " <> last allTypes
       in functionName <> " :: (Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant)" <> handlerTypes
            <> "\n"
            <> functionName
            <> " = error \"Logic yet to be decided\""

    generateHaskellTypes :: [TypeObject] -> Text
    generateHaskellTypes typeObj = T.unlines $ concatMap processType typeObj
      where
        processType :: TypeObject -> [Text]
        processType (typeName, fields)
          | isEnum fields = generateEnum typeName fields
          | otherwise = generateDataStructure typeName fields

        isEnum :: [(Text, Text)] -> Bool
        isEnum [("enum", _)] = True
        isEnum _ = False

        generateEnum :: Text -> [(Text, Text)] -> [Text]
        generateEnum typeName [("enum", values)] =
          let enumValues = T.splitOn "," values
           in ["data " <> typeName <> " = " <> T.intercalate " | " enumValues]
                ++ ["  deriving (Eq, Show, Generic, ToJSON, FromJSON)\n"]
        generateEnum _ _ = error "Invalid enum definition"

        generateDataStructure :: Text -> [(Text, Text)] -> [Text]
        generateDataStructure typeName fields =
          ["data " <> typeName <> " = " <> typeName]
            ++ ["  { " <> T.intercalate ",\n    " (map formatField fields) <> "\n  }"]
            ++ ["  deriving (Generic, ToJSON, FromJSON, ToSchema)\n"]

        formatField :: (Text, Text) -> Text
        formatField (fieldName, fieldType) = fieldName <> " :: " <> fieldType
