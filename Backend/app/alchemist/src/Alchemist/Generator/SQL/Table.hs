module Alchemist.Generator.SQL.Table (generateSQL) where

import Alchemist.DSL.Syntax.Storage
import Data.List (intercalate)
import Kernel.Prelude
import Text.Casing (quietSnake)

-- Generates SQL for creating a table and altering it to add columns
generateSQL :: TableDef -> String
generateSQL tableDef =
  createTableSQL tableDef ++ "\n" ++ alterTableSQL tableDef

-- SQL for creating an empty table
createTableSQL :: TableDef -> String
createTableSQL tableDef =
  "CREATE TABLE " ++ tableNameSql tableDef ++ " ();\n"

-- SQL for altering the table to add each column
alterTableSQL :: TableDef -> String
alterTableSQL tableDef =
  intercalate "\n" $ map (addColumnSQL (tableNameSql tableDef)) (fields tableDef)

-- SQL for adding a single column with constraints
addColumnSQL :: String -> FieldDef -> String
addColumnSQL tableName fieldDef =
  "ALTER TABLE " ++ tableName ++ " ADD COLUMN " ++ quietSnake (fieldName fieldDef) ++ " "
    ++ sqlType fieldDef
    ++ " "
    ++ intercalate " " (catMaybes $ map constraintToSQL (constraints fieldDef))
    ++ maybe "" ((++) " Default ") (defaultVal fieldDef)
    ++ ";"

-- Converts a FieldConstraint to SQL
constraintToSQL :: FieldConstraint -> Maybe String
constraintToSQL NotNull = Just "NOT NULL"
constraintToSQL _ = Nothing
