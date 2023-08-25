{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Storage.Beam.Mandate where

import Data.Serialize
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import qualified Domain.Types.Mandate as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import EulerHS.Prelude (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common (HighPrecMoney)
import Lib.Utils ()
import Sequelize

data MandateT f = MandateT
  { id :: B.C f Text,
    status :: B.C f Domain.MandateStatus,
    payerVpa :: B.C f (Maybe Text),
    startDate :: B.C f UTCTime,
    endDate :: B.C f UTCTime,
    maxAmount :: B.C f HighPrecMoney,
    payerApp :: B.C f (Maybe Text),
    payerAppName :: B.C f (Maybe Text),
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table MandateT where
  data PrimaryKey MandateT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type Mandate = MandateT Identity

mandateTMod :: MandateT (B.FieldModification (B.TableField MandateT))
mandateTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      status = B.fieldNamed "status",
      startDate = B.fieldNamed "start_date",
      payerVpa = B.fieldNamed "payer_vpa",
      payerApp = B.fieldNamed "payer_app",
      payerAppName = B.fieldNamed "payer_app_name",
      endDate = B.fieldNamed "end_date",
      maxAmount = B.fieldNamed "max_amount",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at"
    }

$(enableKVPG ''MandateT ['id] [])
$(mkTableInstances ''MandateT "mandate" "atlas_driver_offer_bpp")
