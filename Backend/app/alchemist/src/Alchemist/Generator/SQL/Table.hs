{-# LANGUAGE BangPatterns #-}

module Alchemist.Generator.SQL.Table (generateSQL) where

import Alchemist.DSL.Syntax.Storage
import Data.List (intercalate)
import qualified Data.Map as M
import qualified Data.Set as DS
import Kernel.Prelude
import Text.Casing (quietSnake)

mkSnake :: BeamField -> String
mkSnake = quietSnake . bFieldName

-- Generates SQL for creating a table and altering it to add columns
generateSQL :: Maybe MigrationFile -> TableDef -> String
generateSQL (Just oldSqlFile) tableDef = do
  let (updatedFields, newFields, deletedFields) = getUpdatesAndRest oldSqlFile tableDef
      tableName = tableNameSql tableDef
      anyChanges = length (updatedFields <> newFields <> deletedFields) > 0
      updateQueries = generateUpdateSQL tableDef tableName updatedFields
      newColumnQueries = addColumnSQL tableName newFields
      deleteQueries = generateDeleteSQL tableName deletedFields
  if anyChanges
    then oldSqlFile.rawLastSqlFile ++ updateStamp ++ intercalate "\n" (filter (not . null) [updateQueries, newColumnQueries, deleteQueries])
    else oldSqlFile.rawLastSqlFile
generateSQL Nothing tableDef =
  createTableSQL tableDef ++ "\n" ++ alterTableSQL tableDef ++ "\n" ++ addKeySQL tableDef

updateStamp :: String
updateStamp = "\n\n------ SQL updates ------\n\n"

generateDeleteSQL :: String -> [BeamField] -> String
generateDeleteSQL tableName beamFields = intercalate "\n" . (flip map) beamFields $ \beamField -> do
  "ALTER TABLE atlas_app." ++ tableName ++ " DROP COLUMN " ++ (mkSnake beamField) ++ ";"

generateUpdateSQL :: TableDef -> String -> [BeamField] -> String
generateUpdateSQL tableDef tableName beamFields = intercalate "\n" . (flip map) beamFields $ \beamField -> intercalate "\n" . filter (not . null) . (flip map) (bFieldUpdates beamField) $ \fieldUpdaes -> case fieldUpdaes of
  DropDefault -> "ALTER TABLE atlas_app." <> tableName <> " ALTER COLUMN " <> (mkSnake beamField) <> " DROP DEFAULT;"
  AddDefault _ -> maybe "" (\dv -> "ALTER TABLE atlas_app." <> tableName <> " ALTER COLUMN " <> (mkSnake beamField) <> " SET DEFAULT " <> dv <> ";") (bDefaultVal beamField)
  TypeChange -> "ALTER TABLE atlas_app." <> tableName <> " ALTER COLUMN " <> (mkSnake beamField) <> " TYPE " <> (bSqlType beamField) <> ";"
  DropNotNull -> "ALTER TABLE atlas_app." <> tableName <> " ALTER COLUMN " <> (mkSnake beamField) <> " DROP NOT NULL;"
  AddNotNull -> "ALTER TABLE atlas_app." <> tableName <> " ALTER COLUMN " <> (mkSnake beamField) <> " SET NOT NULL;"
  DropPrimaryKey -> "ALTER TABLE atlas_app." <> tableName <> " DROP CONSTRAINT " <> tableName <> "_pkey;\n" <> addKeySQL tableDef
  AddPrimaryKey -> "ALTER TABLE atlas_app." <> tableName <> " DROP CONSTRAINT " <> tableName <> "_pkey;\n" <> addKeySQL tableDef

whichChanges :: BeamField -> BeamField -> [SqlFieldUpdates]
whichChanges oldField newField = do
  let nCs = DS.fromList $ bConstraints newField
  let oCs = DS.fromList $ bConstraints oldField
  let addedConstraints = DS.difference nCs oCs
  let removedConstraints = DS.difference oCs nCs
  let isChangeApplicable change =
        case change of
          DropDefault -> isNothing (bDefaultVal newField) && isJust (bDefaultVal oldField)
          AddDefault _ -> isJust (bDefaultVal newField) && isNothing (bDefaultVal oldField) || bDefaultVal oldField /= bDefaultVal newField
          TypeChange -> bSqlType newField /= bSqlType oldField
          DropNotNull -> DS.member NotNull removedConstraints
          AddNotNull -> DS.member NotNull addedConstraints
          DropPrimaryKey -> DS.member PrimaryKey removedConstraints || (DS.member SecondaryKey removedConstraints)
          AddPrimaryKey -> DS.member PrimaryKey addedConstraints || (DS.member SecondaryKey addedConstraints)
  filter isChangeApplicable [DropNotNull, DropDefault, AddNotNull, AddDefault "Not_Required_Here", TypeChange, DropPrimaryKey, AddPrimaryKey]

getUpdatesAndRest :: MigrationFile -> TableDef -> ([BeamField], [BeamField], [BeamField])
getUpdatesAndRest oldSqlFile tableDef = do
  let newSqlFields = M.fromList . map (\beamField -> (beamField.bFieldName, beamField)) $ concatMap (\field -> field.beamFields) (fields tableDef)
  let oldSqlFields = M.fromList . map (\beamField -> (beamField.bFieldName, beamField)) $ concatMap (\field -> field.beamFields) (fields_ oldSqlFile)
  let updatedFields =
        fst $
          M.mapAccumWithKey
            ( \acc columnName newField ->
                case M.lookup columnName oldSqlFields of
                  Just oldField -> do
                    let changes = whichChanges oldField newField
                    if null changes
                      then (acc, Nothing)
                      else ((newField {bFieldUpdates = changes}) : acc, Just newField)
                  Nothing -> (acc, Nothing)
            )
            []
            newSqlFields
  let newFields =
        fst $
          M.mapAccumWithKey
            ( \acc columnName newField ->
                case M.lookup columnName oldSqlFields of
                  Just _ -> (acc, Nothing)
                  Nothing -> (newField : acc, Just newField)
            )
            []
            newSqlFields
  let deletedFields =
        fst $
          M.mapAccumWithKey
            ( \acc columnName oldField ->
                case M.lookup columnName newSqlFields of
                  Just _ -> (acc, Nothing)
                  Nothing -> (oldField : acc, Just oldField)
            )
            []
            oldSqlFields
  (updatedFields, newFields, deletedFields)

-- SQL for creating an empty table
createTableSQL :: TableDef -> String
createTableSQL tableDef =
  "CREATE TABLE atlas_app." ++ tableNameSql tableDef ++ " ();\n"

-- SQL for altering the table to add each column
alterTableSQL :: TableDef -> String
alterTableSQL tableDef =
  intercalate "\n" $ map (addColumnSQL (tableNameSql tableDef) . beamFields) (fields tableDef)

-- SQL for adding a single column with constraints
addColumnSQL :: String -> [BeamField] -> String
addColumnSQL tableName beamFields =
  intercalate "\n" $
    map
      ( \fieldDef ->
          if bIsEncrypted fieldDef
            then
              generateAlterColumnSQL (mkSnake fieldDef ++ "_hash") "bytea" fieldDef
                ++ "\n"
                ++ generateAlterColumnSQL (mkSnake fieldDef ++ "_encrypted") "character varying(255)" fieldDef
            else generateAlterColumnSQL (mkSnake fieldDef) (bSqlType fieldDef) fieldDef
      )
      beamFields
  where
    generateAlterColumnSQL :: String -> String -> BeamField -> String
    generateAlterColumnSQL fieldName_ sqlType_ beamField =
      "ALTER TABLE atlas_app." ++ tableName ++ " ADD COLUMN " ++ fieldName_ ++ " " ++ sqlType_ ++ " "
        ++ unwords (mapMaybe constraintToSQL (bConstraints beamField))
        ++ maybe "" (" default " ++) (bDefaultVal beamField)
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
