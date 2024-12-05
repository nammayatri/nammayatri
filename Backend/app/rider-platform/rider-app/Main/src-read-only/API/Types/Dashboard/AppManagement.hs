{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.AppManagement where

import qualified API.Types.Dashboard.AppManagement.HotSpot
import qualified API.Types.Dashboard.AppManagement.Tickets
import qualified Data.List
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude
import qualified Text.Read
import qualified Text.Show

data AppManagementUserActionType
  = HOT_SPOT API.Types.Dashboard.AppManagement.HotSpot.HotSpotUserActionType
  | TICKETS API.Types.Dashboard.AppManagement.Tickets.TicketsUserActionType
  deriving stock (Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Text.Show.Show AppManagementUserActionType where
  show = \case
    HOT_SPOT e -> "HOT_SPOT/" <> show e
    TICKETS e -> "TICKETS/" <> show e

instance Text.Read.Read AppManagementUserActionType where
  readsPrec d' =
    Text.Read.readParen
      (d' > app_prec)
      ( \r ->
          [(HOT_SPOT v1, r2) | r1 <- stripPrefix "HOT_SPOT/" r, (v1, r2) <- Text.Read.readsPrec (app_prec + 1) r1]
            ++ [ ( TICKETS v1,
                   r2
                 )
                 | r1 <- stripPrefix "TICKETS/" r,
                   (v1, r2) <- Text.Read.readsPrec (app_prec + 1) r1
               ]
      )
    where
      app_prec = 10
      stripPrefix pref r = bool [] [Data.List.drop (length pref) r] $ Data.List.isPrefixOf pref r

$(Data.Singletons.TH.genSingletons [''AppManagementUserActionType])
