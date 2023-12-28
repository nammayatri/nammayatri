module Alchemist.Generator.Haskell.ApiTypes where

import Alchemist.DSL.Syntax.API
import Alchemist.GeneratorCore
import Control.Lens ((^.))
import Data.List (isInfixOf, nub)
import qualified Data.Text as T
import Kernel.Prelude hiding (replicateM)

generateApiTypes :: Apis -> Code
generateApiTypes input = generateCode generatorInput
  where
    generatorInput :: GeneratorInput
    generatorInput =
      GeneratorInput
        { _ghcOptions = ["-Wno-orphans", "-Wno-unused-imports"],
          _extensions = [],
          _moduleNm = "API.Types.UI." <> T.unpack (_moduleName input),
          _simpleImports = allSimpleImports,
          _qualifiedImports = allQualifiedImports,
          _codeBody = generateCodeBody mkCodeBody input
        }
    qualifiedModuleName = T.unpack ("API.Types.UI." <> _moduleName input)

    allSimpleImports :: [String]
    allSimpleImports =
      [ "EulerHS.Prelude hiding (id)",
        "Servant",
        "Tools.Auth",
        "Data.OpenApi (ToSchema)"
      ]

    allQualifiedImports :: [String]
    allQualifiedImports =
      nub $
        preventSameModuleImports $
          (T.unpack <$> input ^. apiTypes . typeImports)
            <> defaultQualifiedImport

    preventSameModuleImports :: [String] -> [String]
    preventSameModuleImports = filter (\x -> not (qualifiedModuleName `isInfixOf` x))

    defaultQualifiedImport :: [String]
    defaultQualifiedImport = ["Kernel.Prelude", "Domain.Types.Person", "Domain.Types.Merchant", "Environment", "Kernel.Types.Id"]

mkCodeBody :: ApisM ()
mkCodeBody = do
  input <- ask
  generateHaskellTypes (input ^. apiTypes . types)

generateHaskellTypes :: [TypeObject] -> ApisM ()
generateHaskellTypes typeObj =
  onNewLine $
    tellM $
      T.unpack $
        T.unlines (concatMap processType typeObj)
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
