module DFlow where

import Prelude

import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Debug (spy)

flow :: String -> Aff String
flow s = do
  liftEffect $ log "Flow 1"
  _ <- pure $ spy "inside DFlow" s
  pure $ "Flow 1"