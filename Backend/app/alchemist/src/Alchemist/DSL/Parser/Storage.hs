{-# LANGUAGE OverloadedStrings #-}

module Alchemist.DSL.Parser.Storage (storageParser) where

import Alchemist.DSL.Syntax.Storage
import Alchemist.Utils (figureOutImports, makeTypeQualified, _String)
import Control.Lens.Combinators
import Control.Lens.Operators
import Data.Aeson
import Data.Aeson.Key (fromString, toString)
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Lens (_Object, _Value)
import qualified Data.ByteString as BS
import qualified Data.List as L
import Data.List.Split (splitOn)
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Kernel.Prelude hiding (fromString, toString, try)
import Text.Regex.TDFA ((=~))

storageParser :: FilePath -> IO [TableDef]
storageParser filepath = do
  contents <- BS.readFile filepath
  case Yaml.decodeEither' contents of
    Left _ -> error "Not a Valid Yaml"
    Right yml -> pure $ map (parseTableDef yml) $ filter ((/= "imports") . fst) $ (toModelList yml)

parseTableDef :: Object -> (String, Object) -> TableDef
parseTableDef importObj (parseDomainName, obj) =
  let parsedFields = parseFields importObj obj
      parsedImports = parseImports parsedFields
      (primaryKey, secondaryKey) = extractKeys parsedFields
   in TableDef parseDomainName (parseTableName obj) parsedFields parsedImports primaryKey secondaryKey

parseTableName :: Object -> String
parseTableName = view (ix "tableName" . _String)

parseImports :: [FieldDef] -> [String]
parseImports fields =
  let extraImports = concatMap (\f -> (maybe [] pure $ toTType f) <> (maybe [] pure $ fromTType f)) fields
   in figureOutImports $ (map haskellType fields <> extraImports)

parseFields :: Object -> Object -> [FieldDef]
parseFields impObj obj =
  let fields = preview (ix "fields" . _Value . to mkList) obj
      constraintsObj = obj ^? (ix "constraints" . _Object)
      sqlTypeObj = obj ^? (ix "sqlType" . _Object)
      beamTypeObj = obj ^? (ix "beamType" ._Object)
      defaultsObj = obj ^? (ix "defaults" . _Object)
      getFieldDef field =
        let fieldName = fst field
            haskellType = snd field
            fieldKey = fromString fieldName
            sqlType = fromMaybe (findMatchingSqlType haskellType) (sqlTypeObj >>= preview (ix fieldKey . _String))
            beamType = fromMaybe (if L.isPrefixOf "Id " haskellType then "Text" else haskellType) (beamTypeObj >>= preview (ix fieldKey . _String))
            constraints = fromMaybe [] (constraintsObj >>= preview (ix fieldKey . _String . to (splitOn "|") . to (map getProperConstraint)))
            defaultValue = defaultsObj >>= preview (ix fieldKey . _String)
            parseToTType = obj ^? (ix "toTType" . _Object) >>= preview (ix fieldKey . _String)
            parseFromTType = obj ^? (ix "fromTType" . _Object) >>= preview (ix fieldKey . _String)
         in FieldDef
              { fieldName = fieldName,
                haskellType = makeTypeQualified impObj haskellType,
                beamType = makeTypeQualified impObj beamType,
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
getProperConstraint txt = case (T.unpack . T.strip . T.pack) txt of
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

toModelList :: Object -> [(String, Object)]
toModelList obj =
  KM.toList obj >>= \(k, v) -> case v of
    Object o -> [(toString k, o)]
    _ -> []

-- SQL Types --
findMatchingSqlType :: String -> String
findMatchingSqlType haskellType =
  case filter ((haskellType =~) . fst) defaultSQLTypes of
    [] -> error $ T.pack ("\"" ++ haskellType ++ "\": No Sql type found")
    ((_, sqlType) : _) -> sqlType

defaultSQLTypes :: [(String, String)]
defaultSQLTypes =
  [ ("\\[Text\\]", "text[]"),
    ("Text", "text"),
    ("Id ", "character varying(36)"),
    ("Int", "integer"),
    ("Double", "double precision"),
    ("Bool", "boolean"),
    ("UTCTime", "timestamp with time zone"),
    ("TimeOfDay", "time without time zone"),
    ("Day", "date")
  ]

extractKeys :: [FieldDef] -> ([String], [String])
extractKeys fieldDefs =
  let primaryKeyFields = [fieldName fd | fd <- fieldDefs, PrimaryKey `elem` constraints fd]
      secondaryKeyFields = [fieldName fd | fd <- fieldDefs, SecondaryKey `elem` constraints fd]
   in (primaryKeyFields, secondaryKeyFields)
