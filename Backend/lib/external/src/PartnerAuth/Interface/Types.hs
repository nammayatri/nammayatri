module PartnerAuth.Interface.Types where

import Kernel.Prelude
import qualified PartnerAuth.BHIM.Config as BHIM

-- | Sum of all provider configs. One constructor per PartnerAuthService.
data PartnerAuthServiceConfig = BHIMConfig BHIM.BhimCfg
  deriving (Show, Eq, Generic, FromJSON, ToJSON)
