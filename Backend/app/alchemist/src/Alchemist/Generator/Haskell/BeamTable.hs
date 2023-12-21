{-# LANGUAGE OverloadedStrings #-}

module Alchemist.Generator.Haskell.BeamTable (generateBeamTable) where

import Alchemist.DSL.Syntax.Storage
import Alchemist.GeneratorCore
import Alchemist.Utils
import Data.List (intercalate, isInfixOf)
import qualified Data.Text as T
import Kernel.Prelude hiding (replicateM)

generateBeamTable :: TableDef -> Code
generateBeamTable tableDef =
  generateCode generatorInput
  where
    generatorInput :: GeneratorInput
    generatorInput =
      GeneratorInput
        { _ghcOptions = ["-Wno-unused-imports"],
          _extensions = ["DerivingStrategies", "TemplateHaskell"],
          _moduleNm = "Storage.Beam." <> (capitalize $ tableNameHaskell tableDef),
          _simpleImports = ["Kernel.Prelude", "Tools.Beam.UtilsTH", "Kernel.External.Encryption"],
          _qualifiedImports = ["Database.Beam as B"] <> imports tableDef,
          _codeBody = generateCodeBody mkCodeBody tableDef
        }

mkCodeBody :: StorageM ()
mkCodeBody = do
  beamDataType
  primaryKeyToBeam
  tableInstancesToBeam

beamDataType :: StorageM ()
beamDataType = do
  dataSignature
  dataFields
  derivingInstances

primaryKeyToBeam :: StorageM ()
primaryKeyToBeam = do
  def <- ask
  let tableName = tellM (tableNameHaskell def)
  onNewLine $ do
    tellM "instance B.Table "
    withSpace tableName
    tellM "T where"
    onNewLine $
      withSpace $ withinSpaces $ tellM "data PrimaryKey"
    tableName
    tellM "T f ="
    withSpace tableName
    fetchPrimaryKey def
    derivingInstances
    onNewLine $ withSpace $ withinSpaces $ tellM "primaryKey ="
    tableName
    "Id "
      `followedBy` (tellM $ fromMaybe (error $ T.pack ("Primary Key not found for " ++ tableNameHaskell def)) (getPrimaryKeys (primaryKey def)))
  where
    fetchPrimaryKey tableDef = tellM $ fromMaybe (error $ T.pack ("Primary Key not found for " ++ tableNameHaskell tableDef)) (generateKeyTypes (filter (\f -> fieldName f `elem` primaryKey tableDef) $ fields tableDef))
    getPrimaryKeys [] = Nothing
    getPrimaryKeys [xs] = Just $ ". " <> xs
    getPrimaryKeys xs = Just $ foldl' handleAccPrimary "<$> " xs
    handleAccPrimary acc x = if acc == "<$> " then (acc ++ x) else acc ++ " <*> " ++ x

    getBeamTypeOfPrimaryKey :: FieldDef -> String
    getBeamTypeOfPrimaryKey field = case beamFields field of
      [beamField] -> bFieldType beamField
      _ -> error "Primary key should have only one beam field"

    formatType :: String -> String
    formatType t = if " " `isInfixOf` t then "(" ++ t ++ ")" else t

    generateKeyTypes :: [FieldDef] -> Maybe String
    generateKeyTypes [] = Nothing
    generateKeyTypes xs = Just $ foldl' handleAcc "Id" xs
      where
        handleAcc acc x = acc ++ " (B.C f " ++ formatType (getBeamTypeOfPrimaryKey x) ++ ")"

dataSignature :: StorageM ()
dataSignature = do
  def <- ask
  let tableName = tellM (tableNameHaskell def)
  onNewLine $ do
    tellM "data"
    space
    tableName
    tellM "T f ="
    tableName
    tellM "T"

dataFields :: StorageM ()
dataFields = do
  def <- ask
  withSomeSpaces 2 $
    withinCurls $
      intercalateA
        ( do
            comma
            newLine
            replicateM 4 space
        )
        $ map fieldDefToBeam (fields def)

derivingInstances :: StorageM ()
derivingInstances = onNewLine $
  withSomeSpaces 3 $ do
    tellM "deriving "
    withinParens $
      intercalateA comma (map tellM derives)
  where
    derives = ["Generic", "B.Beamable"]

tableInstancesToBeam :: StorageM ()
tableInstancesToBeam = do
  def <- ask
  let tableName = tellM (tableNameHaskell def)
  onNewLine $ do
    tellM "type"
    withSpace tableName
    withSpace $ tellM "="
    withSpace tableName
    tellM "T Identity"
  onNewLine $ do
    tellM "$(enableKVPG ''"
    tableName
    tellM "T ['"
    tellM $ intercalate ", '" (primaryKey def)
    tellM "] "
    if (null (secondaryKey def))
      then tellM "[]"
      else
        tellM $
          "[['"
            <> intercalate ", '" (secondaryKey def)
            <> "]]"
    tellM ")"
  onNewLine $ do
    newLine
    tellM "$(mkTableInstances ''"
    tableName
    tellM "T \""
    tellM (tableNameSql def)
    tellM "\")"

fieldDefToBeam :: FieldDef -> StorageM ()
fieldDefToBeam hfield = do
  intercalateA newLine $
    map
      ( \field ->
          let bfName = tellM (bFieldName field)
           in if bIsEncrypted field
                then do
                  bfName
                  tellM "Encrypted :: B.C f"
                  space
                  wrapMaybe "Text" field
                  comma
                  replicateM 4 space
                  bfName
                  tellM "Hash :: B.C f"
                  space
                  wrapMaybe "DbHash" field
                else do
                  bfName
                  withinSpaces $ tellM ":: B.C f"
                  formatType $ bFieldType field
      )
      (beamFields hfield)
  where
    formatType :: String -> StorageM ()
    formatType t = if " " `isInfixOf` t then withinParens $ tellM t else tellM t

    wrapMaybe :: String -> BeamField -> StorageM ()
    wrapMaybe beamType field =
      if isMaybeType (bFieldType field)
        then do
          withinParens $ do
            tellM "Maybe"
            space
            tellM beamType
        else tellM beamType
