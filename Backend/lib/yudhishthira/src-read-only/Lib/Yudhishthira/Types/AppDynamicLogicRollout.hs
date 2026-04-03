{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Lib.Yudhishthira.Types.AppDynamicLogicRollout where
import Kernel.Prelude
import Data.Aeson
import qualified Lib.Yudhishthira.Types
import qualified Kernel.Types.Id
import qualified Data.Text
import qualified Tools.Beam.UtilsTH



data AppDynamicLogicRollout
    = AppDynamicLogicRollout {domain :: Lib.Yudhishthira.Types.LogicDomain,
                              experimentStatus :: Kernel.Prelude.Maybe Lib.Yudhishthira.Types.ExperimentStatus,
                              isBaseVersion :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
                              merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Lib.Yudhishthira.Types.Merchant),
                              merchantOperatingCityId :: Kernel.Types.Id.Id Lib.Yudhishthira.Types.MerchantOperatingCity,
                              modifiedBy :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Lib.Yudhishthira.Types.Person),
                              percentageRollout :: Kernel.Prelude.Int,
                              timeBounds :: Data.Text.Text,
                              version :: Kernel.Prelude.Int,
                              versionDescription :: Kernel.Prelude.Maybe Data.Text.Text,
                              createdAt :: Kernel.Prelude.UTCTime,
                              updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



