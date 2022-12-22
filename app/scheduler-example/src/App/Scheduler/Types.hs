{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Scheduler.Types where

-- FIXME: This entire module is just for example
-- TODO: move it to the integration tests when real usage of the scheduler library appears.

import Beckn.Prelude
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.GenericPretty (PrettyShow, Showable (..))
import Data.Singletons.TH
import Lib.Scheduler

data SchedulerJobType
  = PrintBananasCount
  | PrintCurrentTimeWithErrorProbability
  | IncorrectDataJobType
  | FakeJobType
  | TestTermination
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass (FromJSON, ToJSON, FromDhall)
  deriving (PrettyShow) via Showable SchedulerJobType

genSingletons [''SchedulerJobType]
singEqInstances [''SchedulerJobType]
singOrdInstances [''SchedulerJobType]
showSingInstances [''SchedulerJobType]

-----------------
data BananasCount = BananasCount
  { count :: Int,
    createdAt :: UTCTime
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON, PrettyShow)

type instance JobContent 'PrintBananasCount = BananasCount

-----------------

type instance JobContent 'PrintCurrentTimeWithErrorProbability = ()

-----------------
type instance JobContent 'FakeJobType = ()

-----------------
data IncorrectlySerializable = IncSer
  { foo :: Int,
    bar :: Text
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, PrettyShow)
  deriving (FromJSON) via JSONfail IncorrectlySerializable

type instance JobContent 'IncorrectDataJobType = IncorrectlySerializable

newtype JSONfail a = JSONfail a

instance FromJSON (JSONfail a) where
  parseJSON _ = fail "fake fail"

-----------------

type instance JobContent 'TestTermination = ()
