module Alchemist.DSL.Parser.Storage (parseStorageDSL) where

import Alchemist.DSL.Syntax.Storage
import Alchemist.Utils (figureOutImports, findMatchingSqlType, makeTypeQualified)
import Kernel.Prelude hiding (try)
import Text.Parsec hiding (spaces)
import Text.Parsec.String (Parser)

parseConstraint :: Parser FieldConstraint
parseConstraint =
  try (string "PrimaryKey" *> return PrimaryKey)
    <|> try (string "SecondaryKey" *> return SecondaryKey)
    <|> try (string "NotNull" *> return NotNull)
    <|> try (Default <$> (string "Default" *> spaces *> between (char '"') (char '"') (many (noneOf "\""))))
    <|> ( do
            _ <- string "CustomConstraint"
            spaces
            value <- between (char '"') (char '"') (many (noneOf "\""))
            return $ CustomConstraint value
        )

parseFieldDef :: Parser FieldDef
parseFieldDef = do
  _ <- string "Field"
  spaces
  fieldName <- between (char '"') (char '"') (many (noneOf "\""))
  spaces
  haskellType <- between (char '"') (char '"') (many (noneOf "\""))
  spaces
  sqlType <-
    option
      ( case findMatchingSqlType haskellType of
          Just tp -> tp
          Nothing -> error "SQL type not found in map"
      )
      (between (char '"') (char '"') (many (noneOf "\"")))
  spaces
  constraints <- many (try (spaces *> parseConstraint))
  return $ FieldDef fieldName haskellType sqlType constraints

parseStorageDSL :: Parser TableDef
parseStorageDSL = do
  _ <- string "Table"
  spaces
  tableNameHaskell <- between (char '"') (char '"') (many (noneOf "\""))
  spaces
  tableNameSql <- between (char '"') (char '"') (many (noneOf "\""))
  spaces
  fields <- many (try (spaces *> parseFieldDef))
  let (primaryKey, secondaryKey) = extractKeys fields
  let reqImports = figureOutImports (map haskellType fields)
  let qualified = map makeVerboseType fields
  return $ TableDef tableNameHaskell tableNameSql reqImports qualified primaryKey secondaryKey
  where
    makeVerboseType :: FieldDef -> FieldDef
    makeVerboseType fieldDef = fieldDef {haskellType = makeTypeQualified (fieldDef.haskellType)}

    extractKeys :: [FieldDef] -> ([String], [String])
    extractKeys fieldDefs =
      let primaryKeyFields = [fieldName fd | fd <- fieldDefs, PrimaryKey `elem` constraints fd]
          secondaryKeyFields = [fieldName fd | fd <- fieldDefs, SecondaryKey `elem` constraints fd]
       in (primaryKeyFields, secondaryKeyFields)

spaces :: Parser ()
spaces = skipMany (char ' ' <|> char '\n' <|> char '\t')
