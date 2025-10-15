{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.AppManagement where

import qualified API.Types.Dashboard.AppManagement.Driver
import qualified API.Types.Dashboard.AppManagement.DriverSubscription
import qualified API.Types.Dashboard.AppManagement.Overlay
import qualified API.Types.Dashboard.AppManagement.Penalty
import qualified API.Types.Dashboard.AppManagement.Subscription
import qualified API.Types.Dashboard.AppManagement.SubscriptionTransaction
import qualified Data.List
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude
import qualified Text.Read
import qualified Text.Show

data AppManagementUserActionType
  = DRIVER API.Types.Dashboard.AppManagement.Driver.DriverUserActionType
  | DRIVER_SUBSCRIPTION API.Types.Dashboard.AppManagement.DriverSubscription.DriverSubscriptionUserActionType
  | OVERLAY API.Types.Dashboard.AppManagement.Overlay.OverlayUserActionType
  | PENALTY API.Types.Dashboard.AppManagement.Penalty.PenaltyUserActionType
  | SUBSCRIPTION API.Types.Dashboard.AppManagement.Subscription.SubscriptionUserActionType
  | SUBSCRIPTION_TRANSACTION API.Types.Dashboard.AppManagement.SubscriptionTransaction.SubscriptionTransactionUserActionType
  deriving stock (Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Text.Show.Show AppManagementUserActionType where
  show = \case
    DRIVER e -> "DRIVER/" <> show e
    DRIVER_SUBSCRIPTION e -> "DRIVER_SUBSCRIPTION/" <> show e
    OVERLAY e -> "OVERLAY/" <> show e
    PENALTY e -> "PENALTY/" <> show e
    SUBSCRIPTION e -> "SUBSCRIPTION/" <> show e
    SUBSCRIPTION_TRANSACTION e -> "SUBSCRIPTION_TRANSACTION/" <> show e

instance Text.Read.Read AppManagementUserActionType where
  readsPrec d' =
    Text.Read.readParen
      (d' > app_prec)
      ( \r ->
          [(DRIVER v1, r2) | r1 <- stripPrefix "DRIVER/" r, (v1, r2) <- Text.Read.readsPrec (app_prec + 1) r1]
            ++ [ ( DRIVER_SUBSCRIPTION v1,
                   r2
                 )
                 | r1 <- stripPrefix "DRIVER_SUBSCRIPTION/" r,
                   (v1, r2) <- Text.Read.readsPrec (app_prec + 1) r1
               ]
            ++ [ ( OVERLAY v1,
                   r2
                 )
                 | r1 <- stripPrefix "OVERLAY/" r,
                   ( v1,
                     r2
                     ) <-
                     Text.Read.readsPrec (app_prec + 1) r1
               ]
            ++ [ ( PENALTY v1,
                   r2
                 )
                 | r1 <- stripPrefix "PENALTY/" r,
                   ( v1,
                     r2
                     ) <-
                     Text.Read.readsPrec (app_prec + 1) r1
               ]
            ++ [ ( SUBSCRIPTION v1,
                   r2
                 )
                 | r1 <- stripPrefix "SUBSCRIPTION/" r,
                   ( v1,
                     r2
                     ) <-
                     Text.Read.readsPrec (app_prec + 1) r1
               ]
            ++ [ ( SUBSCRIPTION_TRANSACTION v1,
                   r2
                 )
                 | r1 <- stripPrefix "SUBSCRIPTION_TRANSACTION/" r,
                   ( v1,
                     r2
                     ) <-
                     Text.Read.readsPrec (app_prec + 1) r1
               ]
      )
    where
      app_prec = 10
      stripPrefix pref r = bool [] [Data.List.drop (length pref) r] $ Data.List.isPrefixOf pref r

$(Data.Singletons.TH.genSingletons [''AppManagementUserActionType])
