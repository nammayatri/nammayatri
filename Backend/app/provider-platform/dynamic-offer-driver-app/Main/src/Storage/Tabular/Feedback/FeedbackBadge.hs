{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Feedback.FeedbackBadge where

import qualified Domain.Types.Feedback.Feedback as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Storage.Tabular.Person (PersonTId)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    FeedbackBadgeT sql=feedback_badge
      id Text
      driverId PersonTId
      badge Text
      badgeCount Int
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey FeedbackBadgeT where
  type DomainKey FeedbackBadgeT = Id Domain.FeedbackBadge
  fromKey (FeedbackBadgeTKey _id) = Id _id
  toKey (Id id) = FeedbackBadgeTKey id

instance FromTType FeedbackBadgeT Domain.FeedbackBadge where
  fromTType FeedbackBadgeT {..} = do
    return $
      Domain.FeedbackBadge
        { id = Id id,
          driverId = fromKey driverId,
          ..
        }

instance ToTType FeedbackBadgeT Domain.FeedbackBadge where
  toTType Domain.FeedbackBadge {..} =
    FeedbackBadgeT
      { id = getId id,
        driverId = toKey driverId,
        ..
      }
