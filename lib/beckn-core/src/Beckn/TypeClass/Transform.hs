{-# LANGUAGE MultiParamTypeClasses #-}

module Beckn.TypeClass.Transform where

import Beckn.Types.App
import Beckn.Types.Common
import qualified EulerHS.Language as L

class ModifyTransform a b m where
  modifyTransform :: a -> b -> m b

class CreateTransform a b m where
  createTransform :: a -> m b
