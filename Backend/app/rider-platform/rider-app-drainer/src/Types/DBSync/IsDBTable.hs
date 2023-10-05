{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.DBSync.IsDBTable where

-- -- for reusing code between rider and driver app
-- data App = RIDER_APP | DRIVER_APP

-- class IsDbTable' (app :: App) table  where

-- type IsDbTable = IsDbTable' 'RIDER_APP

-- separate class for rider and driver drainer for avoiding orphan instances
-- class IsDbTable table where
