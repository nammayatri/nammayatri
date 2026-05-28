{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Conductor where

import qualified API.Types.ProviderPlatform.Conductor.Registration
import qualified Data.List
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude
import qualified Text.Read
import qualified Text.Show

newtype ConductorUserActionType
  = REGISTRATION API.Types.ProviderPlatform.Conductor.Registration.RegistrationUserActionType
  deriving stock (Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Text.Show.Show ConductorUserActionType where
  show = \case
    REGISTRATION e -> "REGISTRATION/" <> show e

instance Text.Read.Read ConductorUserActionType where
  readsPrec d' = Text.Read.readParen (d' > app_prec) (\r -> [(REGISTRATION v1, r2) | r1 <- stripPrefix "REGISTRATION/" r, (v1, r2) <- Text.Read.readsPrec (app_prec + 1) r1])
    where
      app_prec = 10
      stripPrefix pref r = bool [] [Data.List.drop (length pref) r] $ Data.List.isPrefixOf pref r

$(Data.Singletons.TH.genSingletons [(''ConductorUserActionType)])
