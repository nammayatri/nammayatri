{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Scheduler.Types where

-- FIXME: This entire module is just for example
-- TODO: move it to the integration tests when real usage of the scheduler library appears.

import Data.Singletons.Base.TH
import Kernel.Prelude
import Kernel.Utils.Dhall (FromDhall)
import Kernel.Utils.GenericPretty (PrettyShow, Showable (..))
import Lib.Scheduler

data SchedulerJobType
  = PrintBananasCount
  | PrintCurrentTimeWithErrorProbability
  | IncorrectDataJobType
  | FakeJobType
  | TestTermination
  deriving stock (Generic, Show, Read, Eq, Ord)
  deriving anyclass (FromJSON, ToJSON, FromDhall)
  deriving (PrettyShow) via Showable SchedulerJobType

genSingletons [''SchedulerJobType]
singEqInstances [''SchedulerJobType]
singOrdInstances [''SchedulerJobType]
showSingInstances [''SchedulerJobType]

instance JobProcessor SchedulerJobType where
  restoreAnyJobInfo :: Sing (e :: SchedulerJobType) -> Text -> Maybe (AnyJobInfo SchedulerJobType)
  restoreAnyJobInfo SPrintBananasCount jobData = AnyJobInfo <$> restoreJobInfo SPrintBananasCount jobData
  restoreAnyJobInfo SPrintCurrentTimeWithErrorProbability jobData = AnyJobInfo <$> restoreJobInfo SPrintCurrentTimeWithErrorProbability jobData
  restoreAnyJobInfo SIncorrectDataJobType jobData = AnyJobInfo <$> restoreJobInfo SIncorrectDataJobType jobData
  restoreAnyJobInfo SFakeJobType jobData = AnyJobInfo <$> restoreJobInfo SFakeJobType jobData
  restoreAnyJobInfo STestTermination jobData = AnyJobInfo <$> restoreJobInfo STestTermination jobData

-----------------
data BananasCount = BananasCount
  { count :: Int,
    createdAt :: UTCTime
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON, PrettyShow)

instance JobInfoProcessor 'PrintBananasCount

type instance JobContent 'PrintBananasCount = BananasCount

-----------------

instance JobInfoProcessor 'PrintCurrentTimeWithErrorProbability

type instance JobContent 'PrintCurrentTimeWithErrorProbability = ()

-----------------
instance JobInfoProcessor 'FakeJobType

type instance JobContent 'FakeJobType = ()

-----------------
data IncorrectlySerializable = IncSer
  { foo :: Int,
    bar :: Text
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, PrettyShow)
  deriving (FromJSON) via JSONfail IncorrectlySerializable

instance JobInfoProcessor 'IncorrectDataJobType

type instance JobContent 'IncorrectDataJobType = IncorrectlySerializable

newtype JSONfail a = JSONfail a

instance FromJSON (JSONfail a) where
  parseJSON _ = fail "fake fail"

-----------------

instance JobInfoProcessor 'TestTermination

type instance JobContent 'TestTermination = ()
