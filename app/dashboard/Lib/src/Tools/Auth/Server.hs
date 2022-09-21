{-# LANGUAGE TypeApplications #-}

module Tools.Auth.Server (module Tools.Auth.Server, module Reexport) where

import Data.Singletons.TH
import Domain.Types.RegistrationToken as Reexport (ServerName (..))
import qualified Domain.Types.RegistrationToken as DReg
import Domain.Types.Role as Reexport (DashboardAccessType (..))
import Tools.Servant.HeaderAuth

-- This type is similar to DReg.ServerName, but used only on type level

data ServerAccess sn

instance
  forall (sn :: DReg.ServerName).
  SingI sn =>
  (VerificationPayload DReg.ServerName) (ServerAccess sn)
  where
  toPayloadType _ = fromSing (sing @sn)
