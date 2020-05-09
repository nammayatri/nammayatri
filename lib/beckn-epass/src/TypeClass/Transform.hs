{-# LANGUAGE MultiParamTypeClasses #-}

module TypeClass.Transform where

class Transform f g where
  transform :: f -> g
