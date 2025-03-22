{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Fleet where

import qualified API.Types.ProviderPlatform.Fleet.Driver
import qualified API.Types.ProviderPlatform.Fleet.Onboarding
import qualified Data.List
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude
import qualified Text.Read
import qualified Text.Show

data FleetUserActionType
  = DRIVER API.Types.ProviderPlatform.Fleet.Driver.DriverUserActionType
  | ONBOARDING API.Types.ProviderPlatform.Fleet.Onboarding.OnboardingUserActionType
  deriving stock (Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Text.Show.Show FleetUserActionType where
  show = \case
    DRIVER e -> "DRIVER/" <> show e
    ONBOARDING e -> "ONBOARDING/" <> show e

instance Text.Read.Read FleetUserActionType where
  readsPrec d' =
    Text.Read.readParen
      (d' > app_prec)
      ( \r ->
          [(DRIVER v1, r2) | r1 <- stripPrefix "DRIVER/" r, (v1, r2) <- Text.Read.readsPrec (app_prec + 1) r1]
            ++ [ ( ONBOARDING v1,
                   r2
                 )
                 | r1 <- stripPrefix "ONBOARDING/" r,
                   (v1, r2) <- Text.Read.readsPrec (app_prec + 1) r1
               ]
      )
    where
      app_prec = 10
      stripPrefix pref r = bool [] [Data.List.drop (length pref) r] $ Data.List.isPrefixOf pref r

$(Data.Singletons.TH.genSingletons [(''FleetUserActionType)])
