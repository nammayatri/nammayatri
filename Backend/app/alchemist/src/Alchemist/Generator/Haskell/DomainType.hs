module Alchemist.Generator.Haskell.DomainType where

import Alchemist.DSL.Syntax.Storage
import Alchemist.Utils (isMaybeType)
import qualified Data.List as L
import qualified Data.List.Split as L
import Data.Tuple.Extra (both)
import Kernel.Prelude

tableNameHaskellType :: TableDef -> String
tableNameHaskellType tableDef = do
  if tableDef.containsEncryptedField
    then tableNameHaskell tableDef ++ "E e"
    else tableNameHaskell tableDef

generateDomainType :: TableDef -> String
generateDomainType tableDef = do
  let defaultImports = createDefaultImports tableDef

  "{-# LANGUAGE TemplateHaskell #-}\n"
    ++ "{-# LANGUAGE ApplicativeDo #-}\n"
    ++ "module "
    ++ moduleName
    ++ " where\n\n"
    ++ L.intercalate "\n" (map ("import qualified " ++) (removeDefaultImports defaultImports moduleName $ imports tableDef))
    ++ "\n"
    ++ L.intercalate "\n" (map ("import " ++) defaultImports)
    ++ "\n\ndata "
    ++ tableNameHaskellType tableDef
    ++ " = "
    ++ tableNameHaskell tableDef
    ++ "\n  { "
    ++ L.intercalate "\n  , " (map fieldDefToHaskell (fields tableDef))
    ++ "\n  }\n  deriving ("
    ++ L.intercalate "," (derivingInstances $ containsEncryptedField tableDef)
    ++ ")\n\n"
    ++ (if tableDef.containsEncryptedField then generateEncryptionInstance tableDef else "")
    ++ maybe "" (uncurry (++) . generateHaskellTypes) (types tableDef)
  where
    moduleName = "Domain.Types." ++ tableNameHaskell tableDef

derivingInstances :: Bool -> [String]
derivingInstances containsEncryptedField =
  if containsEncryptedField
    then ["Generic"]
    else ["Generic", "Show", "ToJSON", "FromJSON", "ToSchema"]

generateEncryptionInstance :: TableDef -> String
generateEncryptionInstance tableDef =
  unlines $
    [ "type " ++ baseType ++ " = " ++ encryptedType ++ "\n",
      "type " ++ decryptBaseType ++ " = " ++ decryptedType ++ "\n",
      "instance EncryptedItem " ++ baseType ++ " where",
      "  type Unencrypted " ++ baseType ++ " = (" ++ decryptBaseType ++ ", HashSalt)",
      "  encryptItem (" ++ baseType ++ " {..}, salt) = do",
      unlines ((catMaybes $ map encryptField (fields tableDef)) ++ ["    return " ++ baseType ++ " {" ++ L.intercalate "," (catMaybes $ map mapFields (fields tableDef)) ++ ", ..}"]),
      "  decryptItem " ++ baseType ++ " {..} = do",
      unlines ((catMaybes $ map decryptField (fields tableDef)) ++ ["    return (" ++ baseType ++ " {" ++ L.intercalate "," (catMaybes $ map mapFields (fields tableDef)) ++ ", ..}, \"\")\n"]),
      "instance EncryptedItem' " ++ baseType ++ " where",
      "  type UnencryptedItem " ++ baseType ++ " = " ++ decryptBaseType,
      "  toUnencrypted a salt = (a, salt)",
      "  fromUnencrypted = fst\n\n"
    ]
  where
    baseType = tableNameHaskell tableDef
    decryptBaseType = "Decrypted" ++ tableNameHaskell tableDef
    encryptedType = baseType ++ "E 'AsEncrypted"
    decryptedType = baseType ++ "E 'AsUnencrypted"
    encryptField field = do
      if field.isEncrypted
        then
          if isMaybeType field.haskellType
            then Just $ "    " ++ field.fieldName ++ "_ <- encryptItem $ (,salt) <$> " ++ field.fieldName
            else Just $ "    " ++ field.fieldName ++ "_ <- encryptItem $ (" ++ field.fieldName ++ ",salt)"
        else Nothing
    decryptField field = do
      if field.isEncrypted
        then
          if isMaybeType field.haskellType
            then Just $ "    " ++ field.fieldName ++ "_ <- fmap fst <$> decryptItem " ++ field.fieldName
            else Just $ "    " ++ field.fieldName ++ "_ <- fst <$> decryptItem " ++ field.fieldName
        else Nothing

    mapFields field = do
      if field.isEncrypted
        then Just $ field.fieldName ++ " = " ++ field.fieldName ++ "_"
        else Nothing

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
    <> ["Kernel.External.Encryption" | tableDef.containsEncryptedField]

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
              ["$(mkHttpInstancesForEnum ''" <> typeName <> ")\n" | isHttpInstanceDerived typeObj]
          )
    generateEnum _ _ = error "Invalid enum definition"

    generateDataStructure :: String -> [(String, String)] -> ([String], [String])
    generateDataStructure typeName fields =
      ( ["data " <> typeName <> " = " <> typeName]
          ++ ["  { " <> L.intercalate ",\n    " (map formatField fields) <> "\n  }"]
          ++ ["  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)\n"],
        []
      )

    formatField :: (String, String) -> String
    formatField (fieldName, fieldType) = fieldName ++ " :: " ++ fieldType
