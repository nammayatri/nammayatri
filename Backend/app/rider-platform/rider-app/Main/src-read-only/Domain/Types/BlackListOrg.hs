{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.BlackListOrg where

import Data.Aeson
import Domain.Types.Common (UsageSafety (..))
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Domain
import qualified Kernel.Types.Id
import qualified Kernel.Types.Registry
import qualified Tools.Beam.UtilsTH

data BlackListOrgD (s :: UsageSafety) = BlackListOrg
  { id :: Kernel.Types.Id.Id Domain.Types.BlackListOrg.BlackListOrg,
    subscriberId :: Kernel.Types.Id.ShortId Kernel.Types.Registry.Subscriber,
    domain :: Kernel.Types.Beckn.Domain.Domain,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)

type BlackListOrg = BlackListOrgD 'Safe

instance FromJSON (BlackListOrgD 'Unsafe)

instance ToJSON (BlackListOrgD 'Unsafe)

instance FromJSON (BlackListOrgD 'Safe)

instance ToJSON (BlackListOrgD 'Safe)
