{-# LANGUAGE TypeApplications #-}

module Tools.Roles.Instances (module Tools.Roles.Instances, module Reexport) where

import Beckn.Prelude
import Data.Singletons.TH
import Tools.Roles as Reexport (ApiAccessType (..), ApiEntity (..), DashboardAccessType (..))
import qualified Tools.Roles as Roles
import Tools.Servant.HeaderAuth

-- These types are similar to Roles.ApiAccessLevel and Roles.DashboardAccessType, but used only on type level

data ApiAccessLevel at ae

data DashboardAccessLevel at

instance
  forall (at :: Roles.ApiAccessType) (ae :: Roles.ApiEntity).
  (SingI at, SingI ae) =>
  (VerificationPayload Roles.RequiredAccessLevel) (ApiAccessLevel at ae)
  where
  toPayloadType _ =
    Roles.RequiredApiAccessLevel $
      Roles.ApiAccessLevel
        { apiAccessType = fromSing (sing @at),
          apiEntity = fromSing (sing @ae)
        }

instance
  forall (at :: Roles.DashboardAccessType).
  SingI at =>
  (VerificationPayload Roles.RequiredAccessLevel) (DashboardAccessLevel at)
  where
  toPayloadType _ =
    Roles.RequiredDashboardAccessLevel (fromSing (sing @at))
