{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Extra.ShuttleInterest where

-- The ShuttleInterest API types expose [DayOfWeek]; its ToSchema instance is the orphan in
-- Domain.Types.Extra.NyRegularSubscription. Importing it here puts it in scope for every module that imports
-- Domain.Types.ShuttleInterest.
import Domain.Types.Extra.NyRegularSubscription ()
