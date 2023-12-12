module Alchemist.Generator.SQL.Table (generateSQL) where

import Alchemist.DSL.Syntax.Storage
import Data.List (intercalate)
import Kernel.Prelude
import Text.Casing (quietSnake)

-- Generates SQL for creating a table and altering it to add columns
generateSQL :: TableDef -> String
generateSQL tableDef =
  createTableSQL tableDef ++ "\n" ++ alterTableSQL tableDef ++ "\n" ++ addKeySQL tableDef

-- SQL for creating an empty table
createTableSQL :: TableDef -> String
createTableSQL tableDef =
  "CREATE TABLE atlas_app." ++ tableNameSql tableDef ++ " ();\n"

-- SQL for altering the table to add each column
alterTableSQL :: TableDef -> String
alterTableSQL tableDef =
  intercalate "\n" $ map (addColumnSQL (tableNameSql tableDef)) (fields tableDef)

-- SQL for adding a single column with constraints
addColumnSQL :: String -> FieldDef -> String
addColumnSQL tableName fieldDef =
  if isEncrypted fieldDef
    then
      generateAlterColumnSQL (quietSnake (fieldName fieldDef) ++ "_hash") "bytea"
        ++ "\n"
        ++ generateAlterColumnSQL (quietSnake (fieldName fieldDef) ++ "_encrypted") "character varying(255)"
    else generateAlterColumnSQL (quietSnake (fieldName fieldDef)) (sqlType fieldDef)
  where
    generateAlterColumnSQL :: String -> String -> String
    generateAlterColumnSQL fieldName_ sqlType_ =
      "ALTER TABLE atlas_app." ++ tableName ++ " ADD COLUMN " ++ fieldName_ ++ " " ++ sqlType_ ++ " "
        ++ intercalate " " (catMaybes $ map constraintToSQL (constraints fieldDef))
        ++ maybe "" ((++) " default ") (defaultVal fieldDef)
        ++ ";"

addKeySQL :: TableDef -> String
addKeySQL tableDef =
  let keys = map quietSnake $ primaryKey tableDef <> secondaryKey tableDef
   in "ALTER TABLE atlas_app." ++ (tableNameSql tableDef) ++ " ADD PRIMARY KEY ( "
        ++ intercalate ", " keys
        ++ ");"

-- ALTER TABLE tablename ADD PRIMARY KEY (column1, column2, ...);

-- Converts a FieldConstraint to SQL
constraintToSQL :: FieldConstraint -> Maybe String
constraintToSQL NotNull = Just "NOT NULL"
constraintToSQL _ = Nothing
