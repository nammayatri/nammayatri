{-# LANGUAGE OverloadedStrings #-}

module Alchemist.DSL.Parser.Storage (storageParser) where

import Alchemist.DSL.Syntax.Storage
import Alchemist.Utils (figureOutImports, makeTypeQualified, _String)
import Control.Lens.Combinators
import Control.Lens.Operators
import Data.Aeson
import Data.Aeson.Key (fromString, toString)
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Lens (_Array, _Object, _Value)
import qualified Data.ByteString as BS
import qualified Data.List as L
import qualified Data.List.Extra as L
import Data.List.Split (split, splitOn, splitWhen, whenElt)
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
  let parsedTypesAndExcluded = parseExtraTypes parseDomainName dList importObj obj
      parsedTypes = (view _1) <$> parsedTypesAndExcluded
      excludedList = (view _2) <$> parsedTypesAndExcluded
      enumList = fromMaybe [] ((view _3) <$> parsedTypesAndExcluded)
      parsedFields = parseFields (Just parseDomainName) excludedList dList enumList importObj obj
      parsedImports = parseImports parsedFields (fromMaybe [] parsedTypes)
      parsedQueries = parseQueries (Just parseDomainName) excludedList dList parsedFields importObj obj
      (primaryKey, secondaryKey) = extractKeys parsedFields
   in TableDef parseDomainName (quietSnake parseDomainName) parsedFields parsedImports parsedQueries primaryKey secondaryKey parsedTypes

parseImports :: [FieldDef] -> [TypeObject] -> [String]
parseImports fields typObj =
  figureOutImports (map haskellType fields <> extraImports <> concatMap figureOutInsideTypeImports typObj)
  where
    figureOutInsideTypeImports :: TypeObject -> [String]
    figureOutInsideTypeImports (TypeObject (_, (tps, _))) =
      concatMap
        ( ( \potentialImport ->
              if "," `L.isInfixOf` potentialImport
                then filter ('.' `elem`) $ splitWhen (`elem` ("() []," :: String)) potentialImport
                else [potentialImport]
          )
            . snd
        )
        tps
    extraImports = concatMap (\f -> maybe [] pure (toTType f) <> maybe [] pure (fromTType f)) fields

searchForKey :: [FieldDef] -> String -> (String, String)
searchForKey fields inputKey = (inputKey, haskellType $ fromMaybe (error $ T.pack $ "Query param " ++ inputKey ++ " not found in fields") $ find ((== inputKey) . fieldName) fields)

parseQueries :: Maybe String -> Maybe [String] -> [String] -> [FieldDef] -> Object -> Object -> [QueryDef]
parseQueries moduleName excludedList dList fields impObj obj = do
  let mbQueries = preview (ix "queries" . _Value . to mkListObject) obj
      defaultImportModule = "Domain.Types."
      makeTypeQualified' = makeTypeQualified moduleName excludedList (Just dList) defaultImportModule impObj
      parseQuery query =
        let queryName = fst query
            queryDataObj = snd query
            params = addDefaultUpdatedAtToQueryParams queryName $ map (\(k, v) -> (k, makeTypeQualified' v)) $ fromMaybe [] (queryDataObj ^? ix "params" . _Array . to V.toList . to (map (searchForKey fields . valueToString)))
            kvFunction = fromMaybe (error $ "kvFunction is neccessary") (queryDataObj ^? ix "kvFunction" . _String)
            whereClause = fromMaybe EmptyWhere (queryDataObj ^? ix "where" . to (parseWhereClause makeTypeQualified' fields))
         in QueryDef queryName kvFunction params whereClause False

  case mbQueries of
    Just queries -> map parseQuery queries
    Nothing -> []
  where
    addDefaultUpdatedAtToQueryParams :: String -> [(String, String)] -> [(String, String)]
    addDefaultUpdatedAtToQueryParams queryName params =
      if "update" `L.isPrefixOf` queryName
        then if any (\(k, _) -> k == "updatedAt") params then params else params <> [("updatedAt", "Kernel.Prelude.UTCTime")]
        else params

    valueToString :: Value -> String
    valueToString (String s) = T.unpack s
    valueToString _ = error "Param not a string"

parseWhereClause :: (String -> String) -> [FieldDef] -> Value -> WhereClause
parseWhereClause mkQTypeFunc fields (String st) = do
  let (key_, value) = searchForKey fields (T.unpack st)
  Leaf (key_, mkQTypeFunc value)
parseWhereClause mkQTypeFunc fields (Object clauseObj) = do
  let clauseObj' = KM.toList clauseObj
  case clauseObj' of
    [(operatorStr, value)] -> do
      case value of
        Array arr_ -> do
          let clauses = map (parseWhereClause mkQTypeFunc fields) (V.toList arr_)
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
    extractFields :: KM.KeyMap Value -> [(String, String)]
    extractFields = map (first toString) . KM.toList . fmap extractString

    extractString :: Value -> String
    extractString (String t) = T.unpack t
    extractString _ = error "Non-string type found in field definition"

    splitTypeAndDerivation :: [(String, String)] -> ([(String, String)], Maybe String)
    splitTypeAndDerivation fields = (filter (\(k, _) -> k /= "derive") fields, extractDerive fields)
      where
        extractDerive :: [(String, String)] -> Maybe String
        extractDerive [] = Nothing
        extractDerive ((k, value) : xs)
          | k == "derive" = Just value
          | otherwise = extractDerive xs

    processType1 :: (Key, Value) -> TypeObject
    processType1 (typeName, Object typeDef) =
      TypeObject (toString typeName, splitTypeAndDerivation $ extractFields typeDef)
    processType1 _ = error "Expected an object in fields"

parseExtraTypes :: String -> [String] -> Object -> Object -> Maybe ([TypeObject], [String], [String])
parseExtraTypes moduleName dList importObj obj = do
  _types <- parseTypes obj
  let allExcludeQualified = map (\(TypeObject (name, _)) -> name) _types
  let allEnums = map (\(TypeObject (name, _)) -> name) $ filter isEnumType _types
  return $ (map (mkQualifiedTypeObject allExcludeQualified) _types, allExcludeQualified, map (\nm -> defaultImportModule ++ moduleName ++ "." ++ nm) allEnums ++ allEnums)
  where
    defaultImportModule = "Domain.Types."

    isEnumType :: TypeObject -> Bool
    isEnumType (TypeObject (_, (arrOfFields, _))) = any (\(k, _) -> k == "enum") arrOfFields

    mkEnumTypeQualified :: [String] -> String -> String
    mkEnumTypeQualified excluded enumTp =
      let individualEnums = L.trim <$> L.splitOn "," enumTp
       in L.intercalate "," $ map (uncurry (<>) . second (makeTypeQualified (Just moduleName) (Just excluded) (Just dList) defaultImportModule importObj) . L.breakOn " ") individualEnums

    mkQualifiedTypeObject :: [String] -> TypeObject -> TypeObject
    mkQualifiedTypeObject excluded (TypeObject (_nm, (arrOfFields, derive))) =
      TypeObject
        ( _nm,
          ( map
              ( \(_n, _t) ->
                  ( _n,
                    if _n == "enum"
                      then mkEnumTypeQualified excluded _t
                      else makeTypeQualified (Just moduleName) (Just excluded) (Just dList) defaultImportModule importObj _t
                  )
              )
              arrOfFields,
            derive
          )
        )

parseFields :: Maybe String -> Maybe [String] -> [String] -> [String] -> Object -> Object -> [FieldDef]
parseFields moduleName excludedList dataList enumList impObj obj =
  let fields = (++) <$> preview (ix "fields" . _Value . to mkList . to filterOutAlreadyDefaultTypes) obj <*> pure defaultFields
      constraintsObj = obj ^? (ix "constraints" . _Object)
      sqlTypeObj = obj ^? (ix "sqlType" . _Object)
      beamTypeObj = obj ^? (ix "beamType" ._Object)
      defaultsObj = obj ^? (ix "default" . _Object)
      getFieldDef field =
        let fieldName = fst field
            haskellType = snd field
            fieldKey = fromString fieldName
            sqlType = fromMaybe (findMatchingSqlType enumList haskellType) (sqlTypeObj >>= preview (ix fieldKey . _String))
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
findBeamType str = concatMap (typeMapper . L.trimStart) (split (whenElt (`elem` typeDelimiter)) str)
  where
    typeDelimiter :: String
    typeDelimiter = "()[]"
    typeMapper :: String -> String
    typeMapper hkType
      | L.isPrefixOf "Id " hkType || L.isPrefixOf "Kernel.Types.Id.Id " hkType = "Text"
      | L.isPrefixOf "ShortId " hkType || L.isPrefixOf "Kernel.Types.Id.ShortId " hkType = "Text"
      | otherwise = hkType

getDefaultFieldConstraints :: String -> String -> [FieldConstraint]
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

-- Default Fields --
defaultFields :: [(String, String)]
defaultFields =
  [ ("merchantId", "Maybe (Id Domain.Types.Merchant.Merchant)"),
    ("merchantOperatingCityId", "Maybe (Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity)"),
    ("createdAt", "UTCTime"),
    ("updatedAt", "UTCTime")
  ]

filterOutAlreadyDefaultTypes :: [(String, String)] -> [(String, String)]
filterOutAlreadyDefaultTypes = filter (\(k, _) -> k `notElem` map fst defaultFields)

-- SQL Types --
findMatchingSqlType :: [String] -> String -> String
findMatchingSqlType allEnums haskellType =
  if any (haskellType =~) allEnums
    then "text"
    else case filter ((haskellType =~) . fst) sqlTypeWrtType of
      [] -> "text"
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
    ("\\[Id ", "text[]"),
    ("Id ", "character varying(36)"),
    ("\\[ShortId ", "text[]"),
    ("ShortId ", "character varying(36)"),
    ("Int", "integer"),
    ("Double", "double precision"),
    ("HighPrecMoney", "double precision"),
    ("Money", "integer"),
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
