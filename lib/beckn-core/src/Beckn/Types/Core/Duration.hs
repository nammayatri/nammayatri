module Beckn.Types.Core.Duration where

import Beckn.Utils.Common
import Data.Text

type Duration = Text -- duration as per ISO8601 format

instance Example Duration where
  example = "P1D" -- 1 day
