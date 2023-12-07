module Alchemist.Generator.Haskell.DomainType where

import Alchemist.DSL.Syntax.Storage
import qualified Data.List as L
import qualified Data.List.Split as L
import Data.Tuple.Extra (both)
import Kernel.Prelude

generateDomainType :: TableDef -> String
generateDomainType tableDef = do
  let defaultImports = createDefaultImports tableDef

  "{-# LANGUAGE TemplateHaskell #-}\n"
    ++ "module "
    ++ moduleName
    ++ " where\n\n"
    ++ L.intercalate "\n" (map ("import qualified " ++) (removeDefaultImports defaultImports moduleName $ imports tableDef))
    ++ "\n"
    ++ L.intercalate "\n" (map ("import " ++) defaultImports)
    ++ "\n\ndata "
    ++ tableNameHaskell tableDef
    ++ " = "
    ++ tableNameHaskell tableDef
    ++ "\n  { "
    ++ L.intercalate "\n  , " (map fieldDefToHaskell (fields tableDef))
    ++ "\n  }\n  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)\n\n\n"
    ++ maybe "" (uncurry (++) . generateHaskellTypes) (types tableDef)
  where
    moduleName = "Domain.Types." ++ tableNameHaskell tableDef

removeDefaultImports :: [String] -> String -> [String] -> [String]
removeDefaultImports defaultImports moduleName = filter ((/=) moduleName) . filter (`notElem` defaultImports)

-- Convert FieldDef to Haskell field
fieldDefToHaskell :: FieldDef -> String
fieldDefToHaskell fieldDef =
  fieldName fieldDef ++ " :: " ++ haskellType fieldDef

createDefaultImports :: TableDef -> [String]
createDefaultImports tableDef =
  ["Kernel.Prelude"] <> ["Tools.Beam.UtilsTH" | shouldImportUtilsTH (fromMaybe [] $ types tableDef)]
    <> ["Kernel.Utils.TH" | isHttpInstanceDerived (fromMaybe [] $ types tableDef)]
    <> ["Data.Aeson" | isHttpInstanceDerived (fromMaybe [] $ types tableDef)]

shouldImportUtilsTH :: [TypeObject] -> Bool
shouldImportUtilsTH typeObj =
  any
    ( \case
        TypeObject (_, (fields, _)) -> isEnum fields
    )
    typeObj

isHttpInstanceDerived :: [TypeObject] -> Bool
isHttpInstanceDerived typeObj =
  any
    ( \case
        TypeObject (_, (_, derive)) ->
          case derive of
            Just "HttpInstance" -> True
            _ -> False
    )
    typeObj

isEnum :: [(String, String)] -> Bool
isEnum [("enum", _)] = True
isEnum _ = False

generateHaskellTypes :: [TypeObject] -> (String, String)
generateHaskellTypes typeObj = (both concat . unzip . map (both L.unlines . processType)) typeObj
  where
    processType :: TypeObject -> ([String], [String])
    processType (TypeObject (typeName, (fields, _)))
      | isEnum fields = generateEnum typeName fields
      | otherwise = generateDataStructure typeName fields

    generateEnum :: String -> [(String, String)] -> ([String], [String])
    generateEnum typeName [("enum", values)] =
      let enumValues = L.splitOn "," values
       in ( ("data " <> typeName <> " = " <> L.intercalate " | " enumValues) :
            ["  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema" <> (if isHttpInstanceDerived typeObj then ", ToParamSchema)\n\n" else ")\n\n")],
            ("$(mkBeamInstancesForEnum ''" <> typeName <> ")\n\n") :
              ["$(mkHttpInstancesForEnum ''" <> typeName <> ")\\n" | isHttpInstanceDerived typeObj]
          )
    generateEnum _ _ = error "Invalid enum definition"

    generateDataStructure :: String -> [(String, String)] -> ([String], [String])
    generateDataStructure typeName fields =
      ( ["data " <> typeName <> " = " <> typeName]
          ++ ["  { " <> L.intercalate ",\n    " (map formatField fields) <> "\n  }"]
          ++ ["  deriving (Generic, ToJSON, FromJSON, ToSchema)\n"],
        []
      )

    formatField :: (String, String) -> String
    formatField (fieldName, fieldType) = fieldName ++ " :: " ++ fieldType
