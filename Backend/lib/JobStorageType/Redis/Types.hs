module Lib.Scheduler.JobStorageType.Redis.Types where

import Data.String.Conversions
import Data.Text
import Prelude

type MBInteger = Maybe Integer

newtype Count = Count MBInteger

newtype BlockTime = BlockTime MBInteger

type GroupName = Text

type ConsumerName = Text

data Consumer = Consumer GroupName ConsumerName

data EntryId = AutoId | EntryId Text Text | NewId -- not all types of ids are included here.

instance Show EntryId where
  show AutoId = "*"
  show (EntryId ts ind) = cs $ ts <> "-" <> ind
  show NewId = ">"
