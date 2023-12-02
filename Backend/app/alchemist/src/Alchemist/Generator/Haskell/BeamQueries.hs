module Alchemist.Generator.Haskell.BeamQueries where

import Alchemist.DSL.Syntax.Storage
import Alchemist.Utils
import Data.List (intercalate, isPrefixOf, nub)
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
    ++ (intercalate "\n" $ map (\i -> "import qualified " ++ i ++ " as " ++ i) $ imports tableDef)
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
    ++ "]\n\n"
  where
    genWhereClause = generateClause 6 0 query.whereClause

generateFunctionSignature :: QueryDef -> String -> String
generateFunctionSignature query tableNameHaskell =
  let qparams = map getIdsOut $ nub (params query ++ (getWhereClauseFieldNamesAndTypes (whereClause query)))
   in query.queryName
        ++ " :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => "
        ++ foldMap (\s -> s ++ " -> ") (map snd qparams)
        ++ "m ("
        ++ generateQueryReturnType query.kvFunction tableNameHaskell
        ++ ")\n"
        ++ query.queryName
        ++ " "
        ++ foldMap (\s -> s ++ " ") (map fst qparams)
        ++ "= do\n"
  where
    getIdsOut :: (String, String) -> (String, String)
    getIdsOut (k, t) = if (isPrefixOf "Kernel.Types.Id.Id " t) then ("(Kernel.Types.Id.Id " ++ k ++ ")", t) else (k, t)

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
generateQueryParams params = "    [ " ++ intercalate ",\n      " (map (\(field, _) -> "Se.Set Beam." ++ field ++ " " ++ field) params) ++ "\n    ]\n"

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex f xs = zipWith f [0 ..] xs

-- Function to process each clause
generateClause :: Int -> Int -> WhereClause -> String
generateClause _ _ EmptyWhere = ""
generateClause n i (Leaf (field, _)) =
  (if i == 0 then " " else spaces n) ++ "Se.Is Beam." ++ field ++ " $ Se.Eq " ++ field
generateClause n i (Query (op, clauses)) =
  (if i == 0 then " " else spaces n) ++ operator op
    ++ "\n"
    ++ spaces (n + 2)
    ++ "["
    ++ intercalate ",\n" (mapWithIndex (generateClause (n + 4)) clauses)
    ++ "\n"
    ++ spaces (n + 2)
    ++ "]"

-- Helper to determine the operator
operator :: Operator -> String
operator And = "Se.And "
operator Or = "Se.Or "

spaces :: Int -> String
spaces n = replicate n ' '

-- Generates both FromTType' and ToTType' instances
generateBeamQueries :: TableDef -> String
generateBeamQueries tableDef =
  generateImports tableDef
    ++ generateDefaultCreateQuery tableDef.tableNameHaskell
    ++ intercalate "\n" (map (generateBeamQuery tableDef.tableNameHaskell) (queries tableDef))
    ++ fromTTypeInstance tableDef
    ++ toTTypeInstance tableDef
