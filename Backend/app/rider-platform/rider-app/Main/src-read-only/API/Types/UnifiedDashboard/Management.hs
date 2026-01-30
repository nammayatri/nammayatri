{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UnifiedDashboard.Management where

import qualified API.Types.UnifiedDashboard.Management.HealthCheck
import qualified Data.List
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude
import qualified Text.Read
import qualified Text.Show

newtype ManagementUserActionType
  = HEALTH_CHECK API.Types.UnifiedDashboard.Management.HealthCheck.HealthCheckUserActionType
  deriving stock (Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Text.Show.Show ManagementUserActionType where
  show = \case
    HEALTH_CHECK e -> "HEALTH_CHECK/" <> show e

instance Text.Read.Read ManagementUserActionType where
  readsPrec d' = Text.Read.readParen (d' > app_prec) (\r -> [(HEALTH_CHECK v1, r2) | r1 <- stripPrefix "HEALTH_CHECK/" r, (v1, r2) <- Text.Read.readsPrec (app_prec + 1) r1])
    where
      app_prec = 10
      stripPrefix pref r = bool [] [Data.List.drop (length pref) r] $ Data.List.isPrefixOf pref r

$(Data.Singletons.TH.genSingletons [(''ManagementUserActionType)])
