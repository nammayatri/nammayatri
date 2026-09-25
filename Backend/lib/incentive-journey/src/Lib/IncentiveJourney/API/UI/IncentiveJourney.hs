{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

module Lib.IncentiveJourney.API.UI.IncentiveJourney where

import qualified Lib.IncentiveJourney.Common.UI.IncentiveJourney as Common
import Servant

type IncentiveJourneyAPI =
  "list" :> Common.IncentiveJourneyListAPI
    :<|> "history" :> Common.IncentiveJourneyHistoryAPI
