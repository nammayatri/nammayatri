{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Lib.Yudhishthira.Types.NammaTagTrigger where
import Kernel.Prelude
import Data.Aeson
import qualified Lib.Yudhishthira.Types
import qualified Tools.Beam.UtilsTH



data NammaTagTrigger
    = NammaTagTrigger {createdAt :: Kernel.Prelude.UTCTime, event :: Lib.Yudhishthira.Types.ApplicationEvent, tagName :: Kernel.Prelude.Text, updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



