{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Beckn.Types.Common
  ( module Beckn.Types.Common,
    module Common,
  )
where

import Beckn.External.Encryption as Common (EncFlow)
import Beckn.External.FCM.Types as Common (FCMFlow)
import Beckn.Storage.DB.Config as Common (DBFlow)
import Beckn.Types.App as Common
import Beckn.Types.Flow as Common
import Beckn.Types.Forkable as Common
import Beckn.Types.GuidLike as Common
import Beckn.Types.Logging as Common
import Beckn.Types.MonadGuid as Common
import Beckn.Types.Time as Common
import Data.Aeson
import Data.Generics.Labels ()
import EulerHS.Prelude hiding (id)

newtype IdObject = IdObject
  { id :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

class FromBeckn a b where
  fromBeckn :: a -> b

class ToBeckn a b where
  toBeckn :: b -> a

-- This type class is not strictly an isomorphism. We use the name 'Iso' to
-- denote that it is always expected that a type from the beckn spec should
-- have a corresponding type defined by us allowing conversion (which may be
-- lossy) between the two, when defined as an instance of this typeclass.
class (FromBeckn a b, ToBeckn a b) => BecknSpecIso a b
