{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Lib.Yudhishthira.Storage.Beam.NammaTagV2 where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import qualified Kernel.Prelude
import qualified Data.Aeson
import qualified Lib.Yudhishthira.Types.NammaTagV2
import qualified Lib.Yudhishthira.Types
import qualified Kernel.Types.Common
import qualified Database.Beam as B



data NammaTagV2T f
    = NammaTagV2T {actionEngine :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
                   category :: (B.C f Kernel.Prelude.Text),
                   description :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                   chakra :: (B.C f (Kernel.Prelude.Maybe Lib.Yudhishthira.Types.Chakra)),
                   tagType :: (B.C f Lib.Yudhishthira.Types.NammaTagV2.TagType),
                   merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
                   name :: (B.C f Kernel.Prelude.Text),
                   rangeEnd :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double)),
                   rangeStart :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double)),
                   tags :: (B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text])),
                   llmContext :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                   ruleEngine :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
                   validity :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Hours)),
                   createdAt :: (B.C f Kernel.Prelude.UTCTime),
                   updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table NammaTagV2T
    where data PrimaryKey NammaTagV2T f = NammaTagV2Id (B.C f Kernel.Prelude.Text) (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = NammaTagV2Id <$> merchantOperatingCityId <*> name
type NammaTagV2 = NammaTagV2T Identity

$(enableKVPG (''NammaTagV2T) [('merchantOperatingCityId), ('name)] [])

$(mkTableInstancesGenericSchema (''NammaTagV2T) "namma_tag_v2")

