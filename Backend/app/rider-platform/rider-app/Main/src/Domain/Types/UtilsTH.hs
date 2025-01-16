{-# LANGUAGE TypeApplications #-}

module Domain.Types.UtilsTH where

import Control.Monad
import qualified Data.Aeson as A
import Data.Proxy
-- import Data.Functor
import Kernel.Prelude hiding (Type)
import Language.Haskell.TH

checkField :: forall a. A.FromJSON a => Proxy a -> A.Value -> Bool
checkField _ value = case A.fromJSON @a value of
  A.Success _ -> True
  A.Error _ -> False

class CheckParse table where
  checkParse :: Proxy table -> Text -> A.Value -> Bool

reifyFields :: Name -> Q [(Name, Type)]
reifyFields name = do
  let nameStr = nameBase name
  mbTypeName <- lookupTypeName nameStr
  typeName <- case mbTypeName of
    Nothing -> fail $ nameStr <> " should be type name"
    Just n -> pure n
  tableTypeInfo <- reify typeName
  case tableTypeInfo of
    TyConI dec -> do
      case dec of
        DataD _ _ _ _ [constructor] _ -> do
          case constructor of
            RecC _ records -> forM records $ \(fieldName, _, fieldType) -> do
              case fieldType of
                AppT (AppT _c _f) a -> pure (fieldName, a)
                _ -> fail "field should have type C f a"
            _ -> fail $ nameStr <> " should contain records"
        _ -> fail $ nameStr <> " should be data type with one constructor"
    _ -> fail $ nameStr <> " should be type name"

mkCacParseInstance :: Name -> Q [Dec]
mkCacParseInstance name = do
  let tableColumn = mkName "columnName"
      value = mkName "value"
      hN = mkName "_"
      pN = mkName "Proxy"
  fieldNames <- reifyFields name
  let fieldExpressions =
        [ Match
            (LitP (StringL (nameBase fieldName)))
            --    (NormalB ((VarE 'checkField) `AppE` (SigE (ConE 'Proxy) (AppT (ConT 'Proxy) fieldType)) `AppE` VarE value))
            (NormalB (VarE 'checkField `AppE` (ConE pN `AppTypeE` fieldType) `AppE` VarE value))
            []
          | (fieldName, fieldType) <- fieldNames
        ]
  let bodyExpr = CaseE (VarE tableColumn) fieldExpressions
  let mapTableFunc = FunD 'checkParse [Clause [VarP hN, VarP tableColumn, VarP value] (NormalB bodyExpr) []]
  let instanceDec = InstanceD Nothing [] (AppT (ConT ''CheckParse) (ConT name)) [mapTableFunc]
  return [instanceDec]
