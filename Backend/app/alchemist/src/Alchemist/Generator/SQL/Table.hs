module Alchemist.Generator.SQL.Table where

import Alchemist.DSL.Syntax.Storage
import Data.List (intercalate)
import Kernel.Prelude

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
  "ALTER TABLE " ++ tableName ++ " ADD COLUMN " ++ fieldName fieldDef ++ " "
    ++ sqlType fieldDef
    ++ " "
    ++ intercalate " " (map constraintToSQL (constraints fieldDef))
    ++ ";"

-- Converts a FieldConstraint to SQL
constraintToSQL :: FieldConstraint -> String
constraintToSQL PrimaryKey = "PRIMARY KEY"
constraintToSQL SecondaryKey = "UNIQUE"
constraintToSQL NotNull = "NOT NULL"
constraintToSQL (Default value) = "DEFAULT " ++ value
constraintToSQL (CustomConstraint value) = value
