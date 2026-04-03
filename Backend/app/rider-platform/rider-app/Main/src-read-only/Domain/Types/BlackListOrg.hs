{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.BlackListOrg where
import Kernel.Prelude
import Domain.Types.Common (UsageSafety (..))
import Data.Aeson
import qualified Kernel.Types.Beckn.Domain
import qualified Kernel.Types.Id
import qualified Kernel.Types.Registry
import qualified Tools.Beam.UtilsTH



data BlackListOrgD (s :: UsageSafety)
    = BlackListOrg {createdAt :: Kernel.Prelude.UTCTime,
                    domain :: Kernel.Types.Beckn.Domain.Domain,
                    id :: Kernel.Types.Id.Id Domain.Types.BlackListOrg.BlackListOrg,
                    subscriberId :: Kernel.Types.Id.ShortId Kernel.Types.Registry.Subscriber,
                    updatedAt :: Kernel.Prelude.UTCTime}
    deriving Generic
type BlackListOrg = BlackListOrgD ('Safe)
instance FromJSON (BlackListOrgD 'Unsafe)
instance ToJSON (BlackListOrgD 'Unsafe)
instance FromJSON (BlackListOrgD 'Safe)
instance ToJSON (BlackListOrgD 'Safe)



