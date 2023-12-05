{-# LANGUAGE OverloadedStrings #-}

module Alchemist.DSL.Parser.Storage (storageParser) where

import Alchemist.DSL.Syntax.Storage
import Alchemist.Utils (figureOutImports, makeTypeQualified, _String)
import Control.Lens.Combinators
import Control.Lens.Operators
import Data.Aeson
import Data.Aeson.Key (fromString, toString, toText)
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Lens (key, _Array, _Object, _Value)
import qualified Data.ByteString as BS
import qualified Data.List as L
import qualified Data.List.Extra as L
import Data.List.Split (split, splitOn, whenElt)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Yaml as Yaml
import Kernel.Prelude hiding (fromString, toString, toText, traceShowId, try)
import Text.Casing (quietSnake)
import Text.Regex.TDFA ((=~))

storageParser :: FilePath -> IO [TableDef]
storageParser filepath = do
  contents <- BS.readFile filepath
  case Yaml.decodeEither' contents of
    Left _ -> error "Not a Valid Yaml"
    Right yml -> do
      let modelList = toModelList yml
          dList = fst <$> modelList
      pure $ map (parseTableDef dList yml) $ filter ((/= "imports") . fst) modelList

parseTableDef :: [String] -> Object -> (String, Object) -> TableDef
parseTableDef dList importObj (parseDomainName, obj) =
  let parsedTypesAndExcluded = parseExtraTypes (Just parseDomainName) dList importObj obj
      parsedTypes = parsedTypesAndExcluded >>= pure . fst
      excludedList = parsedTypesAndExcluded >>= pure . snd
      parsedFields = parseFields (Just parseDomainName) excludedList dList importObj obj
      parsedImports = parseImports parsedFields
      parsedQueries = parseQueries (Just parseDomainName) excludedList dList importObj obj
      (primaryKey, secondaryKey) = extractKeys parsedFields
   in TableDef parseDomainName (quietSnake parseDomainName) parsedFields parsedImports parsedQueries primaryKey secondaryKey parsedTypes

parseImports :: [FieldDef] -> [String]
parseImports fields =
  let extraImports = concatMap (\f -> (maybe [] pure $ toTType f) <> (maybe [] pure $ fromTType f)) fields
   in figureOutImports $ (map haskellType fields <> extraImports)

searchForKey :: Object -> String -> (String, String)
searchForKey obj inputKey = (inputKey, fromMaybe (error $ T.pack $ "Query param " ++ inputKey ++ " not found in fields") $ obj ^? (ix "fields" . key (fromString inputKey) . _String))

parseQueries :: Maybe String -> Maybe [String] -> [String] -> Object -> Object -> [QueryDef]
parseQueries moduleName excludedList dList impObj obj = do
  let mbQueries = preview (ix "queries" . _Value . to mkListObject) obj
      defaultImportModule = "Domain.Types."
      makeTypeQualified' = makeTypeQualified moduleName excludedList (Just dList) defaultImportModule impObj
      parseQuery query =
        let queryName = fst query
            queryDataObj = snd query
            params = map (\(k, v) -> (k, makeTypeQualified' v)) $ fromMaybe [] (queryDataObj ^? ix "params" . _Array . to V.toList . to (map (searchForKey obj . valueToString)))
            kvFunction = fromMaybe (error $ "kvFunction is neccessary") (queryDataObj ^? ix "kvFunction" . _String)
            whereClause = fromMaybe EmptyWhere (queryDataObj ^? ix "where" . to (parseWhereClause makeTypeQualified' obj))
         in QueryDef queryName kvFunction params whereClause

  case mbQueries of
    Just queries -> map parseQuery queries
    Nothing -> []
  where
    valueToString :: Value -> String
    valueToString (String s) = T.unpack s
    valueToString _ = error "Param not a string"

parseWhereClause :: (String -> String) -> Object -> Value -> WhereClause
parseWhereClause mkQTypeFunc obj (String st) = do
  let (key_, value) = searchForKey obj (T.unpack st)
  Leaf (key_, mkQTypeFunc value)
parseWhereClause mkQTypeFunc obj (Object clauseObj) = do
  let clauseObj' = KM.toList clauseObj
  case clauseObj' of
    [(operatorStr, value)] -> do
      case value of
        Array arr_ -> do
          let clauses = map (parseWhereClause mkQTypeFunc obj) (V.toList arr_)
          Query (parseOperator (toString operatorStr), clauses)
        _ -> error "Invalid where clause, operator must be followed by an array of clauses"
    _ -> error "Invalid where clause, element of where clause array must be an single key object"
  where
    parseOperator :: String -> Operator
    parseOperator "and" = And
    parseOperator "or" = Or
    parseOperator _ = error "Invalid operator"
parseWhereClause _ _ val = error $ T.pack $ "Invalid where clause, must be a string or an object: " <> show val

parseTypes :: Object -> Maybe [TypeObject]
parseTypes obj = case preview (ix "types" ._Object) obj of
  Just typesObj -> Just $ parseTypeObjects typesObj
  _ -> Nothing

parseTypeObjects :: Object -> [TypeObject]
parseTypeObjects obj =
  map (processType1) $ KM.toList obj
  where
    extractFields :: KM.KeyMap Value -> [(Text, Text)]
    extractFields = map (first toText) . KM.toList . fmap extractString

    extractString :: Value -> Text
    extractString (String t) = t
    extractString _ = error "Non-string type found in field definition"

    processType1 :: (Key, Value) -> TypeObject
    processType1 (typeName, Object typeDef) =
      TypeObject (toText typeName, extractFields typeDef)
    processType1 _ = error "Expected an object in fields"

parseExtraTypes :: Maybe String -> [String] -> Object -> Object -> Maybe ([TypeObject], [String])
parseExtraTypes moduleName dList importObj obj = do
  _types <- parseTypes obj
  let allExcludeQualified = map (\(TypeObject (name, _)) -> T.unpack name) _types
  return $ (map (mkQualifiedTypeObject allExcludeQualified) _types, allExcludeQualified)
  where
    mkQualifiedTypeObject :: [String] -> TypeObject -> TypeObject
    mkQualifiedTypeObject excluded (TypeObject (_nm, arrOfFields)) =
      let defaultImportModule = "Domain.Types."
       in TypeObject (_nm, map (\(_n, _t) -> (_n, T.pack $ makeTypeQualified moduleName (Just excluded) (Just dList) defaultImportModule importObj $ T.unpack _t)) arrOfFields)

parseFields :: Maybe String -> Maybe [String] -> [String] -> Object -> Object -> [FieldDef]
parseFields moduleName excludedList dataList impObj obj =
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
            beamType = fromMaybe (findBeamType haskellType) (beamTypeObj >>= preview (ix fieldKey . _String))
            constraints = L.nub $ getDefaultFieldConstraints fieldName haskellType ++ fromMaybe [] (constraintsObj >>= preview (ix fieldKey . _String . to (splitOn "|") . to (map getProperConstraint)))
            defaultValue = maybe (sqlDefaultsWrtName fieldName) pure (defaultsObj >>= preview (ix fieldKey . _String))
            parseToTType = obj ^? (ix "toTType" . _Object) >>= preview (ix fieldKey . _String)
            parseFromTType = obj ^? (ix "fromTType" . _Object) >>= preview (ix fieldKey . _String)
            defaultImportModule = "Domain.Types."
         in FieldDef
              { fieldName = fieldName,
                haskellType = makeTypeQualified moduleName excludedList (Just dataList) defaultImportModule impObj haskellType,
                beamType = makeTypeQualified moduleName excludedList (Just dataList) defaultImportModule impObj beamType,
                sqlType = sqlType,
                constraints = constraints,
                defaultVal = defaultValue,
                toTType = parseToTType,
                fromTType = parseFromTType
              }
   in case (map getFieldDef) <$> fields of
        Just f -> f
        Nothing -> error "Error Parsing Fields"

findBeamType :: String -> String
findBeamType str = concatMap (typeMapper . L.trim) (split (whenElt (`elem` typeDelimiter)) str)
  where
    typeDelimiter :: String
    typeDelimiter = "()[]"
    typeMapper :: String -> String
    typeMapper hkType
      | L.isPrefixOf "Id " hkType || L.isPrefixOf "Kernel.Types.Id.Id " hkType = "Text"
      | L.isPrefixOf "ShortId " hkType || L.isPrefixOf "Kernel.Types.Id.ShortId " hkType = "Text"
      | otherwise = hkType

getDefaultFieldConstraints :: FieldName -> HaskellType -> [FieldConstraint]
getDefaultFieldConstraints nm tp = primaryKeyConstraint ++ notNullConstraint
  where
    trimmedTp = L.trim tp
    primaryKeyConstraint = [PrimaryKey | nm == "id"]
    notNullConstraint =
      [ NotNull
        | not
            ( L.isPrefixOf "Maybe " trimmedTp
                || L.isPrefixOf "Data.Maybe.Maybe " trimmedTp
            )
      ]

getProperConstraint :: String -> FieldConstraint
getProperConstraint txt = case L.trim txt of
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

mkListObject :: Value -> [(String, Object)]
mkListObject (Object obj) =
  KM.toList obj >>= \(k, v) -> case v of
    Object t -> [(toString k, t)]
    _ -> []
mkListObject _ = []

-- SQL Types --
findMatchingSqlType :: String -> String
findMatchingSqlType haskellType =
  case filter ((haskellType =~) . fst) sqlTypeWrtType of
    [] -> error $ T.pack ("SQL TYPE NOT DETERMINED for " ++ haskellType)
    ((_, sqlType) : _) -> sqlType

sqlDefaultsWrtName :: String -> Maybe String
sqlDefaultsWrtName = \case
  "createdAt" -> Just "CURRENT_TIMESTAMP"
  "updatedAt" -> Just "CURRENT_TIMESTAMP"
  _ -> Nothing

sqlTypeWrtType :: [(String, String)]
sqlTypeWrtType =
  [ ("\\[Text\\]", "text[]"),
    ("Text", "text"),
    ("Id ", "character varying(36)"),
    ("ShortId ", "character varying(36)"),
    ("Int", "integer"),
    ("Double", "double precision"),
    ("Bool", "boolean"),
    ("UTCTime", "timestamp with time zone"),
    ("TimeOfDay", "time without time zone"),
    ("Day", "date"),
    ("Meters", "interger"),
    ("HighPrecMeters", "integer"),
    ("Kilometers", "integer")
  ]

extractKeys :: [FieldDef] -> ([String], [String])
extractKeys fieldDefs =
  let primaryKeyFields = [fieldName fd | fd <- fieldDefs, PrimaryKey `elem` constraints fd]
      secondaryKeyFields = [fieldName fd | fd <- fieldDefs, SecondaryKey `elem` constraints fd]
   in (primaryKeyFields, secondaryKeyFields)
