{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.PassCategory where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Database.Beam as B



data PassCategoryT f
    = PassCategoryT {description :: (B.C f Kernel.Prelude.Text),
                     id :: (B.C f Kernel.Prelude.Text),
                     merchantId :: (B.C f Kernel.Prelude.Text),
                     merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
                     name :: (B.C f Kernel.Prelude.Text),
                     createdAt :: (B.C f Kernel.Prelude.UTCTime),
                     updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table PassCategoryT
    where data PrimaryKey PassCategoryT f = PassCategoryId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = PassCategoryId . id
type PassCategory = PassCategoryT Identity

$(enableKVPG (''PassCategoryT) [('id)] [])

$(mkTableInstances (''PassCategoryT) "pass_category")

