{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

module Lib.IncentiveJourney.Reward
  ( RewardDispatcher (..),
  )
where

import Kernel.Prelude
import Lib.IncentiveJourney.Types

class Monad m => RewardDispatcher m where
  dispatchReward :: RewardDispatchCtx -> RewardSpec -> m AwardResult
