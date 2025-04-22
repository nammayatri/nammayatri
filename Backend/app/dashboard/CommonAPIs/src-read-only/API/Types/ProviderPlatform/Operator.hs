{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Operator where

import qualified API.Types.ProviderPlatform.Operator.Driver
import qualified API.Types.ProviderPlatform.Operator.FleetManagement
import qualified API.Types.ProviderPlatform.Operator.Registration
import qualified Data.List
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude
import qualified Text.Read
import qualified Text.Show

data OperatorUserActionType
  = DRIVER API.Types.ProviderPlatform.Operator.Driver.DriverUserActionType
  | FLEET_MANAGEMENT API.Types.ProviderPlatform.Operator.FleetManagement.FleetManagementUserActionType
  | REGISTRATION API.Types.ProviderPlatform.Operator.Registration.RegistrationUserActionType
  deriving stock (Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Text.Show.Show OperatorUserActionType where
  show = \case
    DRIVER e -> "DRIVER/" <> show e
    FLEET_MANAGEMENT e -> "FLEET_MANAGEMENT/" <> show e
    REGISTRATION e -> "REGISTRATION/" <> show e

instance Text.Read.Read OperatorUserActionType where
  readsPrec d' =
    Text.Read.readParen
      (d' > app_prec)
      ( \r ->
          [(DRIVER v1, r2) | r1 <- stripPrefix "DRIVER/" r, (v1, r2) <- Text.Read.readsPrec (app_prec + 1) r1]
            ++ [ ( FLEET_MANAGEMENT v1,
                   r2
                 )
                 | r1 <- stripPrefix "FLEET_MANAGEMENT/" r,
                   (v1, r2) <- Text.Read.readsPrec (app_prec + 1) r1
               ]
            ++ [ ( REGISTRATION v1,
                   r2
                 )
                 | r1 <- stripPrefix "REGISTRATION/" r,
                   ( v1,
                     r2
                     ) <-
                     Text.Read.readsPrec (app_prec + 1) r1
               ]
      )
    where
      app_prec = 10
      stripPrefix pref r = bool [] [Data.List.drop (length pref) r] $ Data.List.isPrefixOf pref r

$(Data.Singletons.TH.genSingletons [''OperatorUserActionType])
