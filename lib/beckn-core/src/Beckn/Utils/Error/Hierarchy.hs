{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Beckn.Utils.Error.Hierarchy where

import Data.Typeable (cast)
import EulerHS.Prelude
import Language.Haskell.TH

instanceExceptionWithParent :: Name -> Name -> DecsQ
instanceExceptionWithParent parent child =
  [d|
    instance Exception $(conT child) where
      toException = toException . $(conE parent)
      fromException = $(pure unPat) <=< fromException
    |]
  where
    unPat =
      let x = mkName "x"
       in LamE [ConP parent [VarP x]] $
            AppE (VarE 'cast) (VarE x)
