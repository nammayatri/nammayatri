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
    ++ "import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow)\n"
    ++ "import qualified Storage.Beam."
    ++ (capitalize $ tableNameHaskell tableDef)
    ++ " as Beam\n"
    ++ "import qualified Sequelize as Se\n"
    ++ intercalate "\n" (map ("import qualified " ++) $ imports tableDef)
    ++ "\n\n"

toTTypeConversionFunction :: Maybe String -> String -> String -> String
toTTypeConversionFunction transformer haskellType fieldName
  | (isJust transformer) = fromJust transformer ++ " " ++ fieldName
  | "Int" <- haskellType = "roundToIntegral " ++ fieldName
  | "[Kernel.Types.Id.Id " `Text.isPrefixOf` (Text.pack haskellType) = "Kernel.Types.Id.getId <$> " ++ fieldName
  | "Kernel.Types.Id.Id " `Text.isPrefixOf` (Text.pack haskellType) = "Kernel.Types.Id.getId " ++ fieldName
  | "[Kernel.Types.Id.ShortId " `Text.isPrefixOf` (Text.pack haskellType) = "Kernel.Types.Id.getShortId <$> " ++ fieldName
  | "Kernel.Types.Id.ShortId " `Text.isPrefixOf` (Text.pack haskellType) = "Kernel.Types.Id.getShortId " ++ fieldName
  | otherwise = fieldName

fromTTypeConversionFunction :: Maybe String -> String -> String -> String
fromTTypeConversionFunction transformer haskellType fieldName
  | (isJust transformer) = fromJust transformer ++ " " ++ fieldName
  | "Int" <- haskellType = "realToFrac " ++ fieldName
  | "[Kernel.Types.Id.Id " `Text.isPrefixOf` (Text.pack haskellType) = "Kernel.Types.Id.Id <$> " ++ fieldName
  | "Kernel.Types.Id.Id " `Text.isPrefixOf` (Text.pack haskellType) = "Kernel.Types.Id.Id " ++ fieldName
  | "[Kernel.Types.Id.ShortId " `Text.isPrefixOf` (Text.pack haskellType) = "Kernel.Types.Id.ShortId <$> " ++ fieldName
  | "Kernel.Types.Id.ShortId " `Text.isPrefixOf` (Text.pack haskellType) = "Kernel.Types.Id.ShortId " ++ fieldName
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
    fromField field = fieldName field ++ " = " ++ fromTTypeConversionFunction (fromTType field) (haskellType field) (fieldName field)

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
    ++ intercalate ",\n        " (map toField (fields tableDef))
    ++ "\n      }\n\n"
  where
    toField field = "Beam." ++ fieldName field ++ " = " ++ toTTypeConversionFunction (toTType field) (haskellType field) (fieldName field)

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

generateBeamQuery :: String -> QueryDef -> String
generateBeamQuery tableNameHaskell query =
  generateFunctionSignature
    query
    tableNameHaskell
    ++ generateBeamFunctionCall query.kvFunction
    ++ generateQueryParams query.params
    ++ "    ["
    ++ genWhereClause
    ++ (if genWhereClause == "" then "" else "\n    ")
    ++ "]\n"
    ++ orderAndLimit query
  where
    genWhereClause = generateClause query.takeFullObjectAsInput 6 0 query.whereClause

orderAndLimit :: QueryDef -> String
orderAndLimit query = do
  if query.kvFunction `elem` ["findAllWithOptionsKV", "findAllWithOptionsKV'", "findAllWithOptionsKVScheduler", "findAllWithOptionsDb"]
    then
      "    (Se.Desc Beam.createdAt)\n"
        ++ "    limit\n"
        ++ "    offset\n\n"
    else "\n"

generateFunctionSignature :: QueryDef -> String -> String
generateFunctionSignature query tableNameHaskell =
  let qparams = map getIdsOut $ nub (params query ++ addLimitParams query ++ (getWhereClauseFieldNamesAndTypes (whereClause query)))
   in query.queryName
        ++ " :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => "
        ++ bool (foldMap (\s -> s ++ " -> ") (map snd qparams)) ("Domain.Types." ++ tableNameHaskell ++ "." ++ tableNameHaskell ++ " -> ") query.takeFullObjectAsInput
        ++ "m ("
        ++ generateQueryReturnType query.kvFunction tableNameHaskell
        ++ ")\n"
        ++ query.queryName
        ++ " "
        ++ bool (foldMap (\s -> s ++ " ") (map fst qparams)) ("Domain.Types." ++ tableNameHaskell ++ "." ++ tableNameHaskell ++ " {..} ") query.takeFullObjectAsInput
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
generateBeamFunctionCall kvFunction = "   " ++ kvFunction ++ "\n"

generateQueryParams :: [(String, String)] -> String
generateQueryParams [] = ""
generateQueryParams params = "    [ " ++ intercalate ",\n      " (map (\(field, tp) -> "Se.Set Beam." ++ field ++ " " ++ correctSetField field tp) params) ++ "\n    ]\n"

correctSetField :: String -> String -> String
correctSetField field tp
  | "Kernel.Types.Id.Id " `isInfixOf` tp && not ("Kernel.Types.Id.Id " `isPrefixOf` tp) = "(Kernel.Types.Id.getId <$> " ++ field ++ ")"
  | "Kernel.Types.Id.ShortId " `isInfixOf` tp && not ("Kernel.Types.Id.ShortId " `isPrefixOf` tp) = "(Kernel.Types.Id.getShortId <$> " ++ field ++ ")"
  | "Kernel.Types.Id.Id " `isPrefixOf` tp = "(Kernel.Types.Id.getId " ++ field ++ ")"
  | "Kernel.Types.Id.ShortId " `isPrefixOf` tp = "(Kernel.Types.Id.getShortId " ++ field ++ ")"
  | otherwise = field

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex f xs = zipWith f [0 ..] xs

-- Function to process each clause
generateClause :: Bool -> Int -> Int -> WhereClause -> String
generateClause _ _ _ EmptyWhere = ""
generateClause isFullObjInp n i (Leaf (field, tp)) =
  (if i == 0 then " " else spaces n) ++ "Se.Is Beam." ++ field ++ " $ Se.Eq " ++ (if isFullObjInp then correctSetField field tp else field)
generateClause isFullObjInp n i (Query (op, clauses)) =
  (if i == 0 then " " else spaces n) ++ operator op
    ++ "\n"
    ++ spaces (n + 2)
    ++ "["
    ++ intercalate ",\n" (mapWithIndex (generateClause isFullObjInp (n + 4)) clauses)
    ++ "\n"
    ++ spaces (n + 2)
    ++ "]"

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
    ++ intercalate "\n" (map (generateBeamQuery tableDef.tableNameHaskell) (queries tableDef ++ defaultQueryDefs tableDef))
    ++ fromTTypeInstance tableDef
    ++ toTTypeInstance tableDef

defaultQueryDefs :: TableDef -> [QueryDef]
defaultQueryDefs tableDef =
  [ QueryDef "findByPrimaryKey" "findOneWithKV" [] findByPrimayKeyWhereClause False,
    QueryDef "updateByPrimaryKey" "updateWithKV" (getAllFieldNamesWithTypesExcludingPks (fields tableDef)) findByPrimayKeyWhereClause True
  ]
  where
    getAllFieldNamesWithTypesExcludingPks :: [FieldDef] -> [(String, String)]
    getAllFieldNamesWithTypesExcludingPks fieldDefs = map (\fieldDef -> (fieldName fieldDef, haskellType fieldDef)) $ filter (\fieldDef -> PrimaryKey `notElem` constraints fieldDef) fieldDefs

    getAllPrimaryKeyWithTypes :: [FieldDef] -> [(String, String)]
    getAllPrimaryKeyWithTypes fieldDefs = map (\fieldDef -> (fieldName fieldDef, haskellType fieldDef)) $ filter (\fieldDef -> PrimaryKey `elem` constraints fieldDef) fieldDefs

    primaryKeysAndTypes :: [(String, String)]
    primaryKeysAndTypes = getAllPrimaryKeyWithTypes (fields tableDef)

    findByPrimayKeyWhereClause :: WhereClause
    findByPrimayKeyWhereClause = Query (And, Leaf <$> primaryKeysAndTypes)
