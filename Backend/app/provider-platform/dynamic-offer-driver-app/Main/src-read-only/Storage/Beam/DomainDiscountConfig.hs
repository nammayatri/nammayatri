{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.DomainDiscountConfig where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified SharedLogic.Type
import qualified Kernel.Prelude
import qualified Domain.Types.Common
import qualified Database.Beam as B



data DomainDiscountConfigT f
    = DomainDiscountConfigT {billingCategory :: (B.C f SharedLogic.Type.BillingCategory),
                             createdAt :: (B.C f Kernel.Prelude.UTCTime),
                             discountPercentage :: (B.C f Kernel.Prelude.Double),
                             domain :: (B.C f Kernel.Prelude.Text),
                             enabled :: (B.C f Kernel.Prelude.Bool),
                             merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
                             updatedAt :: (B.C f Kernel.Prelude.UTCTime),
                             vehicleServiceTier :: (B.C f Domain.Types.Common.ServiceTierType),
                             merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)))}
    deriving (Generic, B.Beamable)
instance B.Table DomainDiscountConfigT
    where data PrimaryKey DomainDiscountConfigT f
              = DomainDiscountConfigId (B.C f SharedLogic.Type.BillingCategory) (B.C f Kernel.Prelude.Text) (B.C f Kernel.Prelude.Text) (B.C f Domain.Types.Common.ServiceTierType)
              deriving (Generic, B.Beamable)
          primaryKey = DomainDiscountConfigId <$> billingCategory <*> domain <*> merchantOperatingCityId <*> vehicleServiceTier
type DomainDiscountConfig = DomainDiscountConfigT Identity

$(enableKVPG (''DomainDiscountConfigT) [('billingCategory), ('domain), ('merchantOperatingCityId), ('vehicleServiceTier)] [])

$(mkTableInstances (''DomainDiscountConfigT) "domain_discount_config")

