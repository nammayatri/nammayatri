{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Lib.Yudhishthira.Types.NammaTagTriggerV2 where
import Kernel.Prelude
import Data.Aeson
import qualified Lib.Yudhishthira.Types
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH



data NammaTagTriggerV2
    = NammaTagTriggerV2 {createdAt :: Kernel.Prelude.UTCTime,
                         event :: Lib.Yudhishthira.Types.ApplicationEvent,
                         merchantOperatingCityId :: Kernel.Types.Id.Id Lib.Yudhishthira.Types.MerchantOperatingCity,
                         tagName :: Kernel.Prelude.Text,
                         updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



