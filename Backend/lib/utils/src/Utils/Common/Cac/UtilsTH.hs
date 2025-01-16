{-# LANGUAGE TypeApplications #-}

module Utils.Common.Cac.UtilsTH where

import Control.Monad
import Data.Char (isUpper, toLower)
import Data.Text (pack)
import Kernel.Prelude hiding (Type)
import Language.Haskell.TH

toSnakeCase :: String -> String
toSnakeCase "Empty" = ""
toSnakeCase [] = []
toSnakeCase (x : xs) = toLower x : concatMap convert xs
  where
    convert c
      | isUpper c = ['_', toLower c]
      | otherwise = [c]

mkCacFunction :: Name -> String -> String -> Q [Dec]
mkCacFunction typ fnName source = do
  TyConI (DataD _ _ _ _ constructors _) <- reify typ
  clauses <- mapM genClause constructors
  let funName = mkName fnName
      funBody = [FunD funName clauses]
      funSig = SigD funName (AppT (AppT ArrowT (ConT typ)) (ConT ''Text))
  return $ funSig : funBody
  where
    genClause :: Con -> Q Clause
    genClause = \case
      NormalC conName _ -> mkClause conName
      _ -> mkClause (mkName "UnknowType")
    mkClause conName = do
      let conStr = (toSnakeCase . nameBase) conName
      let errorStr = conStr ++ source
      let pat = ConP conName [] []
      let body = NormalB (AppE (VarE 'pack) (LitE (StringL errorStr)))
      return $ Clause [pat] body []
