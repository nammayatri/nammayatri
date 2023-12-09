module Alchemist.Generator.Haskell.ApiTypes where

import Alchemist.DSL.Syntax.API
import Control.Lens ((^.))
import Data.List (intercalate, isInfixOf, nub)
import Data.Text (Text)
import qualified Data.Text as T
import Prelude

generateApiTypes :: Apis -> String
generateApiTypes input =
  "{-# OPTIONS_GHC -Wno-orphans #-}\n"
    <> "{-# OPTIONS_GHC -Wno-unused-imports #-}"
    <> "\n\n"
    <> "module API.Types.UI."
    <> T.unpack (_moduleName input)
    <> " where \n\n"
    <> intercalate "\n" (map ("import " <>) defaultImports)
    <> "\n\n"
    <> intercalate "\n" (nub $ preventSameModuleImports $ makeImport <$> (T.unpack <$> input ^. apiTypes . typeImports) <> defaultQualifiedImport)
    <> "\n\n"
    <> T.unpack (generateHaskellTypes (input ^. apiTypes . types))
  where
    qualifiedModuleName = T.unpack ("API.Types.UI." <> _moduleName input)

    defaultImports :: [String]
    defaultImports = ["EulerHS.Prelude hiding (id)", "Servant", "Tools.Auth", "Data.OpenApi (ToSchema)"]

    preventSameModuleImports :: [String] -> [String]
    preventSameModuleImports = filter (\x -> not (qualifiedModuleName `isInfixOf` x))

    defaultQualifiedImport :: [String]
    defaultQualifiedImport = ["Kernel.Prelude", "Domain.Types.Person", "Domain.Types.Merchant", "Environment", "Kernel.Types.Id"]

    makeImport :: String -> String
    makeImport x = "import qualified " <> x

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
           in ("data " <> typeName <> " = " <> T.intercalate " | " enumValues) :
              ["  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)\n"]
        generateEnum _ _ = error "Invalid enum definition"

        generateDataStructure :: Text -> [(Text, Text)] -> [Text]
        generateDataStructure typeName fields =
          ["data " <> typeName <> " = " <> typeName]
            ++ ["  { " <> T.intercalate ",\n    " (map formatField fields) <> "\n  }"]
            ++ ["  deriving (Generic, ToJSON, FromJSON, ToSchema)\n"]

        formatField :: (Text, Text) -> Text
        formatField (fieldName, fieldType) = fieldName <> " :: " <> fieldType
