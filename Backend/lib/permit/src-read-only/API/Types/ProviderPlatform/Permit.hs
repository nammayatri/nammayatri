{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Permit where

import qualified API.Types.ProviderPlatform.Permit.Permit
import qualified Data.List
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude
import qualified Text.Read
import qualified Text.Show

newtype PermitUserActionType
  = PERMIT API.Types.ProviderPlatform.Permit.Permit.PermitUserActionType
  deriving stock (Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Text.Show.Show PermitUserActionType where
  show = \case
    PERMIT e -> "PERMIT/" <> show e

instance Text.Read.Read PermitUserActionType where
  readsPrec d' = Text.Read.readParen (d' > app_prec) (\r -> [(PERMIT v1, r2) | r1 <- stripPrefix "PERMIT/" r, (v1, r2) <- Text.Read.readsPrec (app_prec + 1) r1])
    where
      app_prec = 10
      stripPrefix pref r = bool [] [Data.List.drop (length pref) r] $ Data.List.isPrefixOf pref r

$(Data.Singletons.TH.genSingletons [(''PermitUserActionType)])
