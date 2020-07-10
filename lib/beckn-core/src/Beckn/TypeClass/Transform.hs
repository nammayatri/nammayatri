{-# LANGUAGE MultiParamTypeClasses #-}

module Beckn.TypeClass.Transform where

import Beckn.Types.App
import Beckn.Types.Common
import qualified EulerHS.Language as L

class Transform f g | f -> g, g -> f where
  transform :: f -> g -> g

  -- transformFlow :: f -> Flow g
  transformFlow2 :: f -> g -> Flow g

class Transform2 f g where
  transformFlow :: f -> Flow g
