{-# LANGUAGE MultiParamTypeClasses #-}

module Beckn.TypeClass.Transform where

import qualified EulerHS.Language                        as L

class Transform f g  | f -> g, g ->f where
  transform :: f -> g -> g
  -- transformFlow :: f -> L.Flow g
  transformFlow2 :: f -> g -> L.Flow g

class Transform2 f g where
  transformFlow :: f -> L.Flow g