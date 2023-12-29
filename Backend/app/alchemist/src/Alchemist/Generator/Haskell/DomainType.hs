module Alchemist.Generator.Haskell.DomainType where

import Alchemist.DSL.Syntax.Storage
import Alchemist.GeneratorCore
import Alchemist.Utils (isMaybeType)
import qualified Data.List as L
import qualified Data.List.Split as L
import Data.Tuple.Extra (both)
import Kernel.Prelude

generateDomainType :: TableDef -> Code
generateDomainType tableDef =
  generateCode generatorInput
  where
    moduleName' = "Domain.Types." ++ tableNameHaskell tableDef

    allSimpleImports :: [String]
    allSimpleImports = createDefaultImports tableDef

    allQualifiedImports :: [String]
    allQualifiedImports = removeDefaultImports allSimpleImports moduleName' (imports tableDef)

    generatorInput :: GeneratorInput
    generatorInput =
      GeneratorInput
        { _ghcOptions = [],
          _extensions = ["ApplicativeDo", "TemplateHaskell"],
          _moduleNm = moduleName',
          _simpleImports = allSimpleImports,
          _qualifiedImports = allQualifiedImports,
          _codeBody = generateCodeBody mkCodeBody tableDef
        }

mkCodeBody :: StorageM ()
mkCodeBody = do
  def <- ask
  let seperator = onNewLine $ tellM $ "  , "
  onNewLine $
    tellM $
      "data " <> tableNameHaskellType def <> " = " <> tableNameHaskell def
  onNewLine $
    withSomeSpaces 2 $
      withinCurls $
        lineSpace $
          withinSpaces $ intercalateA seperator (map fieldDefToHaskell (fields def))
  onNewLine $ tellM $ "  deriving (" ++ L.intercalate "," (derivingInstances $ containsEncryptedField def) ++ ")\n\n"
  if def.containsEncryptedField then generateEncryptionInstance def else pure ()
  onNewLine $ tellM $ maybe "" (uncurry (++) . generateHaskellTypes) (types def)

tableNameHaskellType :: TableDef -> String
tableNameHaskellType tableDef = do
  if tableDef.containsEncryptedField
    then tableNameHaskell tableDef ++ "E e"
    else tableNameHaskell tableDef

derivingInstances :: Bool -> [String]
derivingInstances containsEncryptedField =
  if containsEncryptedField
    then ["Generic"]
    else ["Generic", "Show", "ToJSON", "FromJSON", "ToSchema"]

generateEncryptionInstance :: TableDef -> StorageM ()
generateEncryptionInstance tableDef =
  onNewLine $
    tellM $
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
fieldDefToHaskell :: FieldDef -> StorageM ()
fieldDefToHaskell fieldDef =
  tellM $
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
  any (\case TypeObject (_, (_, derive)) -> "HttpInstance" `elem` derive) typeObj

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
            ["  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema" <> addRestDerivations (concatMap (\case TypeObject (_, (_, d)) -> d) typeObj)],
            ("$(mkBeamInstancesForEnum ''" <> typeName <> ")\n\n") :
              ["$(mkHttpInstancesForEnum ''" <> typeName <> ")\n" | isHttpInstanceDerived typeObj]
          )
    generateEnum _ _ = error "Invalid enum definition"

    addRestDerivations :: [String] -> String
    addRestDerivations [] = ")\n\n"
    addRestDerivations derivations = ", " <> L.intercalate ", " (map toInstanceName derivations) <> ")\n\n"

    toInstanceName = \case
      "HttpInstance" -> "ToParamSchema"
      val -> error "Invalid instance derivation specified: " <> val

    generateDataStructure :: String -> [(String, String)] -> ([String], [String])
    generateDataStructure typeName fields =
      ( ["data " <> typeName <> " = " <> typeName]
          ++ ["  { " <> L.intercalate ",\n    " (map formatField fields) <> "\n  }"]
          ++ ["  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)\n"],
        []
      )

    formatField :: (String, String) -> String
    formatField (fieldName, fieldType) = fieldName ++ " :: " ++ fieldType
