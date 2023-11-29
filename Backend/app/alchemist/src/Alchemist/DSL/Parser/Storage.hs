{-# LANGUAGE OverloadedStrings #-}

module Alchemist.DSL.Parser.Storage (storageParser) where

import Alchemist.DSL.Syntax.Storage
import Alchemist.Utils (figureOutImports, makeTypeQualified, _String)
import Control.Lens.Combinators
import Control.Lens.Operators
import Data.Aeson
import Data.Aeson.Key (fromString, toString)
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Lens (key, _Object, _Value)
import qualified Data.ByteString as BS
import Data.List.Split (splitOn)
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Kernel.Prelude hiding (fromString, toString, try)
import Text.Regex.TDFA ((=~))

storageParser :: FilePath -> IO TableDef
storageParser filepath = do
  contents <- BS.readFile filepath
  case Yaml.decodeEither' contents of
    Left _ -> error "Not a Valid Yaml"
    Right yml -> pure $ parseTableDef yml

parseTableDef :: Object -> TableDef
parseTableDef obj =
  let parsedFields = parseFields obj
      parsedImports = parseImports parsedFields
      (primaryKey, secondaryKey) = extractKeys parsedFields
   in TableDef (parseDomainName obj) (parseTableName obj) parsedFields parsedImports primaryKey secondaryKey

parseDomainName :: Object -> String
parseDomainName obj = view (ix "model" . key "name" . _String) obj

parseTableName :: Object -> String
parseTableName = view (ix "model" .key "tableName" . _String)

parseImports :: [FieldDef] -> [String]
parseImports fields =
  let extraImports = concatMap (\f -> (maybe [] pure $ toTType f) <> (maybe [] pure $ fromTType f)) fields
   in figureOutImports $ (map haskellType fields <> extraImports)

parseFields :: Object -> [FieldDef]
parseFields obj =
  let fields = preview (ix "model" . key "fields" . _Value . to mkList) obj
      constraintsObj = obj ^? (ix "model" . key "constraints" . _Object)
      sqlTypeObj = obj ^? (ix "model" . key "sqlType" . _Object)
      defaultsObj = obj ^? (ix "model" . key "defaults" . _Object)
      getFieldDef field =
        let fieldName = fst field
            haskellType = snd field
            fieldKey = fromString fieldName
            sqlType = fromMaybe (findMatchingSqlType haskellType) (sqlTypeObj >>= preview (ix fieldKey . _String))
            constraints = fromMaybe [] (constraintsObj >>= preview (ix fieldKey . _String . to (splitOn "|") . to (map getProperConstraint)))
            defaultValue = defaultsObj >>= preview (ix fieldKey . _String)
            parseToTType = obj ^? (ix "model" . key "toTType" . _Object) >>= preview (ix fieldKey . _String)
            parseFromTType = obj ^? (ix "model" . key "fromTType" . _Object) >>= preview (ix fieldKey . _String)
         in FieldDef
              { fieldName = fieldName,
                haskellType = makeTypeQualified obj haskellType,
                sqlType = sqlType,
                constraints = constraints,
                defaultVal = defaultValue,
                toTType = parseToTType,
                fromTType = parseFromTType
              }
   in case (map getFieldDef) <$> fields of
        Just f -> f
        Nothing -> error "Error Parsing Fields"

getProperConstraint :: String -> FieldConstraint
getProperConstraint txt = case txt of
  "PrimaryKey" -> PrimaryKey
  "SecondaryKey" -> SecondaryKey
  "NotNull" -> NotNull
  "AUTOINCREMENT" -> AUTOINCREMENT
  _ -> error "No a proper contraint type"

mkList :: Value -> [(String, String)]
mkList (Object obj) =
  KM.toList obj >>= \(k, v) -> case v of
    String t -> [(toString k, T.unpack t)]
    _ -> []
mkList _ = []

-- SQL Types --
findMatchingSqlType :: String -> String
findMatchingSqlType haskellType =
  case filter ((haskellType =~) . fst) defaultSQLTypes of
    [] -> error $ T.pack ("\"" ++ haskellType ++ "\": No Sql type found")
    ((_, sqlType) : _) -> sqlType

defaultSQLTypes :: [(String, String)]
defaultSQLTypes =
  [ ("Text", "text"),
    ("Id ", "character varying(36)"),
    ("Int", "integer"),
    ("Double", "double precision"),
    ("Bool", "boolean"),
    ("UTCTime", "timestamp with time zone"),
    ("Day", "date"),
    ("TimeOfDay", "time without time zone")
  ]

extractKeys :: [FieldDef] -> ([String], [String])
extractKeys fieldDefs =
  let primaryKeyFields = [fieldName fd | fd <- fieldDefs, PrimaryKey `elem` constraints fd]
      secondaryKeyFields = [fieldName fd | fd <- fieldDefs, SecondaryKey `elem` constraints fd]
   in (primaryKeyFields, secondaryKeyFields)
