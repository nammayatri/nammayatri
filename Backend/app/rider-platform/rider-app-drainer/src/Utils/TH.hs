{-# LANGUAGE TemplateHaskell #-}

module Utils.TH
  ( mkDBCreateObjects,
    mkDBModelObjects,
  )
where

import Kernel.Prelude
import Language.Haskell.TH as TH

-- CREATE --

mkDBCreateObjects :: [Name] -> Q [Dec]
mkDBCreateObjects tables = do
  dbCreateObjectType <- mkDBCreateObjectType tables
  pure [dbCreateObjectType]

mkDBCreateObjectType :: [Name] -> Q Dec
mkDBCreateObjectType tables = do
  let typeName = mkName "DBCreateObject"
  constructors <-
    forM tables $ \table -> do
      constructorName <- mkName . (<> "Object") <$> getModelName table -- TODO reuse getModelName
      pure $ NormalC constructorName [(defaultBang, ConT table `AppT` ConT ''Identity)]
  let deriveClauses =
        [ DerivClause (Just StockStrategy) [ConT ''Generic, ConT ''Show],
          DerivClause (Just AnyclassStrategy) [ConT ''FromJSON, ConT ''ToJSON]
        ]
  pure $ DataD [] typeName [] Nothing constructors deriveClauses

-- DELETE --

-- mkDBDeleteObjects :: [Name] -> Q [Dec]
-- mkDBDeleteObjects tables = do
--   dbCreateObjectType <- mkDeleteModelType tables
--   -- (getTagDeleteSign, getTagDeleteBody) <- getTagDeleteFunc tables
--   pure [dbCreateObjectType]

-- deleteModelTypeName :: Name
-- deleteModelTypeName = mkName "DeleteModel"

-- -- WARNING constructors should be the same everywhere because "incomplete-patterns" warning does not work for TH generated code
-- -- https://gitlab.haskell.org/ghc/ghc/-/issues/14838
-- -- remove this
-- mkDeleteModelConstructorName :: Name -> Q Name
-- mkDeleteModelConstructorName table = mkName . (<> "Delete") <$> getModelName table

-- mkDeleteModelType :: [Name] -> Q Dec
-- mkDeleteModelType tables = do
--   constructors <- forM tables $ \table -> do
--     constructor <- mkDeleteModelConstructorName table
--     pure $ NormalC constructor []
--   let deriveClauses =
--         [ DerivClause (Just StockStrategy) [ConT ''Generic, ConT ''Show, ConT ''Read]
--         ]
--   pure $ DataD [] deleteModelTypeName [] Nothing constructors deriveClauses

-- do not used
-- getTagDeleteFunc :: [Name] -> Q (Dec, Dec)
-- getTagDeleteFunc tables = do
--   let funcName = mkName "getTagDelete"
--   let funcSign = SigD funcName (InfixT (ConT deleteModelTypeName) (mkName "->") (ConT ''Text))
--   let funcBody =
--         FunD funcName $
--           tables <&> \table -> do
--             -- TODO incomplete-patterns error does not appear, so check constructors manually!
--             let constructorName = ConP (mkDeleteModelConstructorName table) []
--             let tagName = LitE . StringL $ getModelName table <> "Options"
--             Clause [constructorName] (NormalB tagName) []
--   return (funcSign, funcBody)

-- MODEL --

-- WARNING constructors should be the same everywhere because "incomplete-patterns" warning does not work for TH generated code
mkDBModelObjects :: [Name] -> Q [Dec]
mkDBModelObjects tablesT = do
  dbCreateObjectType <- mkDBModelType tablesT
  isDbTableInstances <- mkIsDbTableInstances tablesT
  -- (getTagDeleteSign, getTagDeleteBody) <- getTagDeleteFunc tables
  pure $ dbCreateObjectType : isDbTableInstances

dbModelTypeName :: Name
dbModelTypeName = mkName "DBModel"

mkDBModelConstructorName :: Name -> Q Name
mkDBModelConstructorName table = mkName <$> getModelName table

mkDBModelType :: [Name] -> Q Dec
mkDBModelType tables = do
  constructors <- forM tables $ \table -> do
    constructor <- mkDBModelConstructorName table
    pure $ NormalC constructor []
  let deriveClauses =
        [ DerivClause (Just StockStrategy) [ConT ''Show, ConT ''Read]
        ]
  pure $ DataD [] dbModelTypeName [] Nothing constructors deriveClauses

mkIsDbTableInstances :: [Name] -> Q [Dec]
mkIsDbTableInstances tablesT = forM tablesT $ \tableT -> do
  let className = mkName "IsDbTable"
  pure $ InstanceD Nothing [] (AppT (ConT className) (ConT tableT)) []

-- COMMON --

getModelName :: Name -> Q String
getModelName name = do
  let name' = nameBase name
  case last name' of
    'T' -> pure $ take (length name' - 1) name'
    _ -> fail $ "Table type should have suffix T: " <> name'

defaultBang :: Bang
defaultBang = Bang NoSourceUnpackedness NoSourceStrictness
