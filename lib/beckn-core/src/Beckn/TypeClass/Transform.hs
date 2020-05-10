{-# LANGUAGE MultiParamTypeClasses #-}

module Beckn.TypeClass.Transform where

class Transform f g where
  transform :: f -> g -> g
