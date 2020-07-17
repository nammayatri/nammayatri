{-# LANGUAGE MultiParamTypeClasses #-}

module Beckn.TypeClass.Transform where

class ModifyTransform a b m where
  modifyTransform :: a -> b -> m b

class CreateTransform a b m where
  createTransform :: a -> m b
