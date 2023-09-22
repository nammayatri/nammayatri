{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IssueManagement.Storage.Queries.Issue.IssueTranslation where

import Control.Applicative
import Data.Function hiding (id)
import Data.Maybe
import IssueManagement.Domain.Types.Issue.IssueTranslation
import qualified IssueManagement.Storage.Beam.Issue.IssueTranslation as BeamIT
import Kernel.Beam.Functions
import Kernel.Types.Id

instance FromTType' BeamIT.IssueTranslation IssueTranslation where
  fromTType' BeamIT.IssueTranslationT {..} = do
    pure $
      Just
        IssueTranslation
          { id = Id id,
            sentence = sentence,
            translation = translation,
            language = language
          }

instance ToTType' BeamIT.IssueTranslation IssueTranslation where
  toTType' IssueTranslation {..} = do
    BeamIT.IssueTranslationT
      { id = getId id,
        sentence = sentence,
        translation = translation,
        language = language
      }
