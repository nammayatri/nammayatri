{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.QuestionModuleMapping where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Lib.DriverCoins.Types
import qualified Database.Beam as B



data QuestionModuleMappingT f
    = QuestionModuleMappingT {moduleId :: (B.C f Kernel.Prelude.Text),
                              questionId :: (B.C f Kernel.Prelude.Text),
                              quizCoinFunction :: (B.C f (Kernel.Prelude.Maybe Lib.DriverCoins.Types.DriverCoinsFunctionType)),
                              createdAt :: (B.C f Kernel.Prelude.UTCTime),
                              updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table QuestionModuleMappingT
    where data PrimaryKey QuestionModuleMappingT f = QuestionModuleMappingId (B.C f Kernel.Prelude.Text) (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = QuestionModuleMappingId <$> moduleId <*> questionId
type QuestionModuleMapping = QuestionModuleMappingT Identity

$(enableKVPG (''QuestionModuleMappingT) [('moduleId), ('questionId)] [])

$(mkTableInstances (''QuestionModuleMappingT) "question_module_mapping")

