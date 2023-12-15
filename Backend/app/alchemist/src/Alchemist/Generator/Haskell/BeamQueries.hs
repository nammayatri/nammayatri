module Alchemist.Generator.Haskell.BeamQueries where

import Alchemist.DSL.Syntax.Storage
import Alchemist.Utils
import Data.List (intercalate, isInfixOf, isPrefixOf, nub)
import qualified Data.Text as Text
import Kernel.Prelude

generateImports :: TableDef -> String
generateImports tableDef =
  "{-# OPTIONS_GHC -Wno-orphans #-}\n"
    ++ "{-# OPTIONS_GHC -Wno-unused-imports #-}\n\n"
    ++ "module Storage.Queries."
    ++ (capitalize $ tableNameHaskell tableDef)
    ++ " where\n\n"
    ++ "import Kernel.Beam.Functions\n"
    ++ "import Kernel.Prelude\n"
    ++ "import Kernel.External.Encryption\n"
    ++ "import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime)\n"
    ++ "import qualified Storage.Beam."
    ++ (capitalize $ tableNameHaskell tableDef)
    ++ " as Beam\n"
    ++ "import qualified Sequelize as Se\n"
    ++ intercalate "\n" (map ("import qualified " ++) $ imports tableDef)
    ++ "\n\n"

toTTypeConversionFunction :: Maybe String -> String -> String -> String
toTTypeConversionFunction transformer haskellType fieldName
  | (isJust transformer) = fromJust transformer ++ " ( " ++ fieldName ++ " )"
  | "Kernel.Types.Id.Id " `Text.isPrefixOf` Text.pack haskellType = "Kernel.Types.Id.getId " ++ fieldName
  | "Kernel.Types.Id.Id " `Text.isInfixOf` Text.pack haskellType = "Kernel.Types.Id.getId <$> " ++ fieldName
  | "Kernel.Types.Id.ShortId " `Text.isPrefixOf` Text.pack haskellType = "Kernel.Types.Id.getShortId " ++ fieldName
  | "Kernel.Types.Id.ShortId " `Text.isInfixOf` Text.pack haskellType = "Kernel.Types.Id.getShortId <$> " ++ fieldName
  | otherwise = fieldName

fromTTypeConversionFunction :: Maybe String -> String -> String -> String
fromTTypeConversionFunction fromTTypeFunc haskellType fieldName
  | (isJust fromTTypeFunc) = fromJust fromTTypeFunc ++ " " ++ fieldName
  | "Kernel.Types.Id.Id " `Text.isPrefixOf` Text.pack haskellType = "Kernel.Types.Id.Id " ++ fieldName
  | "Kernel.Types.Id.Id " `Text.isInfixOf` Text.pack haskellType = "Kernel.Types.Id.Id <$> " ++ fieldName
  | "Kernel.Types.Id.ShortId " `Text.isPrefixOf` Text.pack haskellType = "Kernel.Types.Id.ShortId " ++ fieldName
  | "Kernel.Types.Id.ShortId " `Text.isInfixOf` Text.pack haskellType = "Kernel.Types.Id.ShortId <$> " ++ fieldName
  | otherwise = fieldName

-- Generates the FromTType' instance
fromTTypeInstance :: TableDef -> String
fromTTypeInstance tableDef =
  "instance FromTType' Beam." ++ tableNameHaskell tableDef ++ " Domain.Types." ++ tableNameHaskell tableDef ++ "." ++ tableNameHaskell tableDef ++ " where\n"
    ++ "  fromTType' Beam."
    ++ tableNameHaskell tableDef
    ++ "T {..} = do\n"
    ++ "    pure $\n"
    ++ "      Just\n"
    ++ "        "
    ++ "Domain.Types."
    ++ (tableNameHaskell tableDef)
    ++ "."
    ++ (tableNameHaskell tableDef)
    ++ "\n"
    ++ "          { "
    ++ intercalate ",\n            " (map fromField (fields tableDef))
    ++ "\n          }\n\n"
  where
    getFromTTypeParams :: FieldDef -> String
    getFromTTypeParams hfield = intercalate " " $ map bFieldName (beamFields hfield)

    fromField field =
      if isEncrypted field
        then do
          let mapOperator = if isMaybeType (haskellType field) then " <$> " else " "
          let applicativeOperator = if isMaybeType (haskellType field) then " <*> " else " "
          fieldName field ++ " = EncryptedHashed" ++ mapOperator ++ "(Encrypted" ++ mapOperator ++ fieldName field ++ "Encrypted)" ++ applicativeOperator ++ fieldName field ++ "Hash"
        else fieldName field ++ " = " ++ fromTTypeConversionFunction (fromTType field) (haskellType field) (getFromTTypeParams field)

-- Generates the ToTType' instance
toTTypeInstance :: TableDef -> String
toTTypeInstance tableDef =
  "instance ToTType' Beam." ++ tableNameHaskell tableDef ++ " Domain.Types." ++ tableNameHaskell tableDef ++ "." ++ tableNameHaskell tableDef ++ " where\n"
    ++ "  toTType' "
    ++ "Domain.Types."
    ++ (tableNameHaskell tableDef)
    ++ "."
    ++ (tableNameHaskell tableDef)
    ++ " {..} = do\n"
    ++ "    Beam."
    ++ tableNameHaskell tableDef
    ++ "T\n"
    ++ "      { "
    ++ intercalate ",\n        " (concatMap toField (fields tableDef))
    ++ "\n      }\n\n"
  where
    toField hfield =
      map
        ( \field ->
            if bIsEncrypted field
              then do
                let mapOperator = if isMaybeType (bFieldType field) then " <&> " else " & "
                let encryptedField = "Beam." ++ bFieldName field ++ "Encrypted = " ++ bFieldName field ++ mapOperator ++ "unEncrypted . (.encrypted)"
                let hashField = "Beam." ++ bFieldName field ++ "Hash = " ++ bFieldName field ++ mapOperator ++ "(.hash)"
                encryptedField ++ ",\n        " ++ hashField
              else "Beam." ++ bFieldName field ++ " = " ++ toTTypeConversionFunction (bToTType field) (haskellType hfield) (toTTypeExtractor (makeExtractorFunction $ bfieldExtractor field) (fieldName hfield))
        )
        (beamFields hfield)

toTTypeExtractor :: Maybe String -> String -> String
toTTypeExtractor extractor field
  | (isJust extractor) = fromJust extractor ++ " (" ++ field ++ " )"
  | otherwise = field

generateDefaultCreateQuery :: String -> String
generateDefaultCreateQuery tableNameHaskell =
  let qname = "Domain.Types." ++ tableNameHaskell ++ "." ++ tableNameHaskell
   in "create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => " ++ qname ++ "-> m ()\n"
        ++ "create = createWithKV\n\n"

generateDefaultCreateManyQuery :: String -> String
generateDefaultCreateManyQuery tableNameHaskell =
  let qname = "Domain.Types." ++ tableNameHaskell ++ "." ++ tableNameHaskell
   in "createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => " ++ "[" ++ qname ++ "]" ++ "-> m ()\n"
        ++ "createMany = traverse_ createWithKV\n\n"

generateBeamQuery :: [FieldDef] -> String -> QueryDef -> String
generateBeamQuery allHaskellFields tableNameHaskell query =
  generateFunctionSignature
    query
    tableNameHaskell
    ++ generateBeamFunctionCall query.kvFunction
    ++ generateQueryParams allHaskellFields (query.params)
    ++ "    ["
    ++ genWhereClause
    ++ (if genWhereClause == "" then "" else "\n    ")
    ++ "]\n"
    ++ orderAndLimit query
  where
    genWhereClause = generateClause allHaskellFields query.takeFullObjectAsInput 6 0 query.whereClause

orderAndLimit :: QueryDef -> String
orderAndLimit query = do
  if query.kvFunction `elem` ["findAllWithOptionsKV", "findAllWithOptionsKV'", "findAllWithOptionsKVScheduler", "findAllWithOptionsDb"]
    then
      "    (Se.Desc Beam.createdAt)\n"
        ++ "    limit\n"
        ++ "    offset\n\n"
    else "\n"

ignoreEncryptionFlag :: ((String, String), Bool) -> (String, String)
ignoreEncryptionFlag ((field, tp), _) = (field, tp)

generateFunctionSignature :: QueryDef -> String -> String
generateFunctionSignature query tableNameHaskell =
  let qparams = filter ((/= "updatedAt") . fst) $ map getIdsOut $ nub (map ignoreEncryptionFlag (params query) ++ addLimitParams query ++ getWhereClauseFieldNamesAndTypes (whereClause query))
   in query.queryName
        ++ " :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => "
        ++ bool (foldMap ((++ " -> ") . snd) qparams) ("Domain.Types." ++ tableNameHaskell ++ "." ++ tableNameHaskell ++ " -> ") query.takeFullObjectAsInput
        ++ "m ("
        ++ generateQueryReturnType query.kvFunction tableNameHaskell
        ++ ")\n"
        ++ query.queryName
        ++ " "
        ++ bool (foldMap ((++ " ") . fst) qparams) ("Domain.Types." ++ tableNameHaskell ++ "." ++ tableNameHaskell ++ " {..} ") query.takeFullObjectAsInput
        ++ "= do\n"

addLimitParams :: QueryDef -> [(String, String)]
addLimitParams query =
  if query.kvFunction `elem` ["findAllWithOptionsKV", "findAllWithOptionsKV'", "findAllWithOptionsKVScheduler", "findAllWithOptionsDb"]
    then [("limit", "Maybe Int"), ("offset", "Maybe Int")]
    else []

getIdsOut :: (String, String) -> (String, String)
getIdsOut (k, t)
  | "Kernel.Types.Id.Id " `isPrefixOf` t = ("(Kernel.Types.Id.Id " ++ k ++ ")", t)
  | "Kernel.Types.Id.ShortId " `isPrefixOf` t = ("(Kernel.Types.Id.ShortId " ++ k ++ ")", t)
  | otherwise = (k, t)

generateQueryReturnType :: String -> String -> String
generateQueryReturnType kvFunction tableNameHaskell = do
  if kvFunction `elem` ["findOneWithKV", "findOneWithKVScheduler", "findOneWithDb"]
    then "Maybe (Domain.Types." ++ tableNameHaskell ++ "." ++ tableNameHaskell ++ ")"
    else
      if kvFunction `elem` ["findAllWithKV", "findAllWithKVScheduler", "findAllWithOptionsKV", "findAllWithOptionsKV'", "findAllWithOptionsKVScheduler", "findAllWithDb", "findAllWithOptionsDb", "findAllWithKVAndConditionalDB"]
        then ("[" ++ "Domain.Types." ++ tableNameHaskell ++ "." ++ tableNameHaskell ++ "]")
        else ""

getWhereClauseFieldNamesAndTypes :: WhereClause -> [(String, String)]
getWhereClauseFieldNamesAndTypes EmptyWhere = []
getWhereClauseFieldNamesAndTypes (Leaf (field, _type)) = [(field, _type)]
getWhereClauseFieldNamesAndTypes (Query (_, clauses)) = concatMap getWhereClauseFieldNamesAndTypes clauses

generateBeamFunctionCall :: String -> String
generateBeamFunctionCall kvFunction =
  (if "update" `isPrefixOf` kvFunction then "   " ++ "now <- getCurrentTime\n" else "") ++ "   " ++ kvFunction ++ "\n"

generateQueryParams :: [FieldDef] -> [((String, String), Bool)] -> String
generateQueryParams _ [] = ""
generateQueryParams allFields params = "    [ " ++ intercalate ",\n      " (map (generateQueryParam allFields) params) ++ "\n    ]\n"

generateQueryParam :: [FieldDef] -> ((String, String), Bool) -> String
generateQueryParam allFields ((field, tp), encrypted) =
  let bFields = maybe (error "Param not found in data type") beamFields $ find (\f -> fieldName f == field) allFields
      isComplex = length bFields > 1
   in intercalate ",\n      " $
        map
          ( \bField ->
              if encrypted
                then do
                  let mapOperator = if isMaybeType tp then " <&> " else " & "
                  let encryptedField = "Se.Set Beam." ++ field ++ "Encrypted $ " ++ field ++ mapOperator ++ "unEncrypted . (.encrypted)"
                  let hashField = "Se.Set Beam." ++ field ++ "Hash $ " ++ field ++ mapOperator ++ "(.hash)"
                  encryptedField ++ ",\n      " ++ hashField
                else "Se.Set Beam." ++ bFieldName bField ++ " $ " ++ correctSetField field tp bField isComplex
          )
          bFields

correctSetField :: String -> String -> BeamField -> Bool -> String
correctSetField field tp beamField isComplex
  | isComplex = (maybe "" (++ " $ ") (bToTType beamField)) ++ toTTypeExtractor (makeExtractorFunction $ bfieldExtractor beamField) field
  | "Kernel.Types.Id.Id " `isInfixOf` tp && not ("Kernel.Types.Id.Id " `isPrefixOf` tp) = "(Kernel.Types.Id.getId <$> " ++ field ++ ")"
  | "Kernel.Types.Id.ShortId " `isInfixOf` tp && not ("Kernel.Types.Id.ShortId " `isPrefixOf` tp) = "(Kernel.Types.Id.getShortId <$> " ++ field ++ ")"
  | "Kernel.Types.Id.Id " `isPrefixOf` tp = "(Kernel.Types.Id.getId " ++ field ++ ")"
  | "Kernel.Types.Id.ShortId " `isPrefixOf` tp = "(Kernel.Types.Id.getShortId " ++ field ++ ")"
  | field == "updatedAt" = "now"
  | otherwise = field

correctEqField :: String -> String -> String
correctEqField field tp
  | "Kernel.Types.Id.Id " `isInfixOf` tp && not ("Kernel.Types.Id.Id " `isPrefixOf` tp) = "(Kernel.Types.Id.getId <$> " ++ field ++ ")"
  | "Kernel.Types.Id.ShortId " `isInfixOf` tp && not ("Kernel.Types.Id.ShortId " `isPrefixOf` tp) = "(Kernel.Types.Id.getShortId <$> " ++ field ++ ")"
  | otherwise = field

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex f xs = zipWith f [0 ..] xs

-- Function to process each clause
generateClause :: [FieldDef] -> Bool -> Int -> Int -> WhereClause -> String
generateClause _ _ _ _ EmptyWhere = ""
generateClause allFields isFullObjInp n i (Leaf (field, tp)) =
  let bFields = maybe (error "Param not found in data type") beamFields $ find (\f -> fieldName f == field) allFields
      isComplex = length bFields > 1
   in intercalate "\n," $ map (\bfield -> (if i == 0 then " " else spaces n) ++ "Se.Is Beam." ++ field ++ " $ Se.Eq " ++ (if isFullObjInp then correctSetField field tp bfield isComplex else correctEqField field tp)) bFields
generateClause allFields isFullObjInp n i (Query (op, clauses)) =
  (if i == 0 then " " else spaces n) ++ operator op
    ++ "\n"
    ++ spaces (n + 2)
    ++ "["
    ++ intercalate ",\n" (mapWithIndex (generateClause allFields isFullObjInp (n + 4)) clauses)
    ++ "\n"
    ++ spaces (n + 2)
    ++ "]"

generateToTTypeFuncs :: [FieldDef] -> String
generateToTTypeFuncs fields = intercalate "\n" $ map generateToTTypeFunc fields
  where
    generateToTTypeFunc :: FieldDef -> String
    generateToTTypeFunc field =
      intercalate "\n" $
        filter (not . null) $
          map
            ( \bfield ->
                case bToTType bfield of
                  Just func -> func ++ " :: " ++ hFieldType bfield ++ " -> " ++ bFieldType bfield ++ "\n" ++ func ++ " = error \"TODO\""
                  Nothing -> ""
            )
            (beamFields field)

generateFromTypeFuncs :: [FieldDef] -> String
generateFromTypeFuncs fields = intercalate "\n" $ filter (not . null) $ map generateFromTTypeFunc fields
  where
    generateFromTTypeFunc :: FieldDef -> String
    generateFromTTypeFunc field =
      let (params, types) = first (map ("_" ++)) $ unzip $ map (\bfield -> (bFieldName bfield, bFieldType bfield)) (beamFields field)
          funcType = " :: " ++ intercalate " -> " types ++ " -> " ++ haskellType field
       in case fromTType field of
            Just func -> func ++ funcType ++ "\n" ++ func ++ " " ++ intercalate " " params ++ " = error \"TODO\""
            Nothing -> ""

-- Helper to determine the operator
operator :: Operator -> String
operator And = "Se.And"
operator Or = "Se.Or"

spaces :: Int -> String
spaces n = replicate n ' '

-- Generates both FromTType' and ToTType' instances
generateBeamQueries :: TableDef -> String
generateBeamQueries tableDef =
  generateImports tableDef
    ++ generateDefaultCreateQuery tableDef.tableNameHaskell
    ++ generateDefaultCreateManyQuery tableDef.tableNameHaskell
    ++ intercalate "\n" (map (generateBeamQuery tableDef.fields tableDef.tableNameHaskell) (queries tableDef ++ defaultQueryDefs tableDef))
    ++ fromTTypeInstance tableDef
    ++ toTTypeInstance tableDef
    ++ generateToTTypeFuncs (fields tableDef)
    ++ "\n"
    ++ generateFromTypeFuncs (fields tableDef)

defaultQueryDefs :: TableDef -> [QueryDef]
defaultQueryDefs tableDef =
  [ QueryDef "findByPrimaryKey" "findOneWithKV" [] findByPrimayKeyWhereClause False,
    QueryDef "updateByPrimaryKey" "updateWithKV" (getAllFieldNamesWithTypesExcludingPks (fields tableDef)) findByPrimayKeyWhereClause True
  ]
  where
    getAllFieldNamesWithTypesExcludingPks :: [FieldDef] -> [((String, String), Bool)]
    getAllFieldNamesWithTypesExcludingPks fieldDefs = map (\fieldDef -> ((fieldName fieldDef, haskellType fieldDef), isEncrypted fieldDef)) $ filter (\fieldDef -> PrimaryKey `notElem` constraints fieldDef) fieldDefs

    getAllPrimaryKeyWithTypes :: [FieldDef] -> [(String, String)]
    getAllPrimaryKeyWithTypes fieldDefs = map (\fieldDef -> (fieldName fieldDef, haskellType fieldDef)) $ filter (\fieldDef -> PrimaryKey `elem` constraints fieldDef) fieldDefs

    primaryKeysAndTypes :: [(String, String)]
    primaryKeysAndTypes = getAllPrimaryKeyWithTypes (fields tableDef)

    findByPrimayKeyWhereClause :: WhereClause
    findByPrimayKeyWhereClause = Query (And, Leaf <$> primaryKeysAndTypes)

makeExtractorFunction :: [String] -> Maybe String
makeExtractorFunction funcs = if null funcs then Nothing else Just $ "( " ++ intercalate " . " funcs ++ " )"
