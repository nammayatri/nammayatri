{-# LANGUAGE TypeApplications #-}

module Tools.Roles.Instances (module Tools.Roles.Instances, module Reexport) where

import Beckn.Prelude
import Data.Singletons.TH
import Domain.Types.AccessMatrix as Reexport (ApiAccessType (..), ApiEntity (..))
import qualified Domain.Types.AccessMatrix as DMatrix
import Domain.Types.Role as Reexport (DashboardAccessType (..))
import qualified Domain.Types.Role as DRole
import Tools.Servant.HeaderAuth

-- These types are similar to DMatrix.ApiAccessLevel and DRole.DashboardAccessType, but used only on type level

data ApiAccessLevel at ae

data DashboardAccessLevel at

instance
  forall (at :: DMatrix.ApiAccessType) (ae :: DMatrix.ApiEntity).
  (SingI at, SingI ae) =>
  (VerificationPayload DMatrix.RequiredAccessLevel) (ApiAccessLevel at ae)
  where
  toPayloadType _ =
    DMatrix.RequiredApiAccessLevel $
      DMatrix.ApiAccessLevel
        { apiAccessType = fromSing (sing @at),
          apiEntity = fromSing (sing @ae)
        }

instance
  forall (at :: DRole.DashboardAccessType).
  SingI at =>
  (VerificationPayload DMatrix.RequiredAccessLevel) (DashboardAccessLevel at)
  where
  toPayloadType _ =
    DMatrix.RequiredDashboardAccessLevel (fromSing (sing @at))
