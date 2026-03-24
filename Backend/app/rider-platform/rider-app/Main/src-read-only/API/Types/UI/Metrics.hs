{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Types.UI.Metrics where
import EulerHS.Prelude hiding (id)
import Servant
import Tools.Auth
import Data.OpenApi (ToSchema)
import qualified Data.Text



data MetricCounterReq
    = MetricCounterReq {message :: Data.Text.Text, metricName :: Data.Text.Text}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)



