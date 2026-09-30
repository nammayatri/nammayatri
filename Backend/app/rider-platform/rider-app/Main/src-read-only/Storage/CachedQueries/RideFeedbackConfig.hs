{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.CachedQueries.RideFeedbackConfig (module Storage.CachedQueries.RideFeedbackConfig, module ReExport) where

import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Utils.Common
import Storage.CachedQueries.RideFeedbackConfigExtra as ReExport
