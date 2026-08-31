{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.IncentiveJourney where

import qualified API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney
import qualified Data.List
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude
import qualified Text.Read
import qualified Text.Show

newtype IncentiveJourneyUserActionType
  = INCENTIVE_JOURNEY API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.IncentiveJourneyUserActionType
  deriving stock (Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Text.Show.Show IncentiveJourneyUserActionType where
  show = \case
    INCENTIVE_JOURNEY e -> "INCENTIVE_JOURNEY/" <> show e

instance Text.Read.Read IncentiveJourneyUserActionType where
  readsPrec d' = Text.Read.readParen (d' > app_prec) (\r -> [(INCENTIVE_JOURNEY v1, r2) | r1 <- stripPrefix "INCENTIVE_JOURNEY/" r, (v1, r2) <- Text.Read.readsPrec (app_prec + 1) r1])
    where
      app_prec = 10
      stripPrefix pref r = bool [] [Data.List.drop (length pref) r] $ Data.List.isPrefixOf pref r

$(Data.Singletons.TH.genSingletons [''IncentiveJourneyUserActionType])
