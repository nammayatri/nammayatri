{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.AccessMatrix where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import qualified Kernel.Prelude
import qualified Domain.Types.AccessMatrix
import qualified Database.Beam as B



data AccessMatrixT f
    = AccessMatrixT {createdAt :: (B.C f Kernel.Prelude.UTCTime),
                     id :: (B.C f Kernel.Prelude.Text),
                     roleId :: (B.C f Kernel.Prelude.Text),
                     serverName :: (B.C f (Kernel.Prelude.Maybe Domain.Types.AccessMatrix.ServerName)),
                     updatedAt :: (B.C f Kernel.Prelude.UTCTime),
                     userActionType :: (B.C f Domain.Types.AccessMatrix.UserActionType)}
    deriving (Generic, B.Beamable)
instance B.Table AccessMatrixT
    where data PrimaryKey AccessMatrixT f = AccessMatrixId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = AccessMatrixId . id
type AccessMatrix = AccessMatrixT Identity

$(enableKVPG (''AccessMatrixT) [('id)] [[('roleId)], [('serverName)], [('userActionType)]])

$(mkTableInstances (''AccessMatrixT) "access_matrix")

