{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Types.Storage.OnSearchEvent where

import Beckn.Types.Id
import Data.Time (UTCTime)
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)

data OnSearchEventT f = OnSearchEvent
  { id :: B.C f (Id OnSearchEvent),
    bppId :: B.C f Text,
    transactionId :: B.C f Text,
    errorCode :: B.C f (Maybe Text),
    errorType :: B.C f (Maybe Text),
    errorMessage :: B.C f (Maybe Text),
    createdAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

type OnSearchEvent = OnSearchEventT Identity

type SearchEventPrimaryKey = B.PrimaryKey OnSearchEventT Identity

instance B.Table OnSearchEventT where
  data PrimaryKey OnSearchEventT f = SearchEventPrimaryKey (B.C f (Id OnSearchEvent))
    deriving (Generic, B.Beamable)
  primaryKey = SearchEventPrimaryKey . (.id)

deriving instance Show OnSearchEvent

deriving instance Eq OnSearchEvent

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity OnSearchEventT)
fieldEMod =
  B.setEntityName "on_search_event"
    <> B.modifyTableFields
      B.tableModification
        { bppId = "bpp_id",
          transactionId = "transaction_id",
          errorCode = "error_code",
          errorType = "error_type",
          errorMessage = "error_message",
          createdAt = "created_at"
        }
