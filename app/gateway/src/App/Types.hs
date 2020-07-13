module App.Types
  ( AppEnv (..),
    AppFlow,
  )
where

import Beckn.Types.Common
import qualified Data.Cache as C
import EulerHS.Prelude

newtype AppEnv = AppEnv {cache :: C.Cache Text Text}

type AppFlow = FlowR AppEnv
