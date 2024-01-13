{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DeriveDataTypeable #-}

module BecknV2.OnDemand.Utils.Common where

import qualified BecknV2.OnDemand.Types as Spec
import Data.Data (Data, gmapQ)
import Data.Generics.Aliases (ext1Q)
import EulerHS.Prelude

allNothing :: (Data d) => d -> Bool
allNothing = not . or . gmapQ (const True `ext1Q` isJust)

type TagGroupCode = Text

type TagCode = Text

getTagV2 :: TagGroupCode -> TagCode -> [Spec.TagGroup] -> Maybe Text
getTagV2 tagGroupCode tagCode tagGroups = do
  tagGroup <- find (\tagGroup -> descriptorCode tagGroup.tagGroupDescriptor == Just tagGroupCode) tagGroups
  case tagGroup.tagGroupList of
    Nothing -> Nothing
    Just tagGroupList -> do
      tag <- find (\tag -> descriptorCode tag.tagDescriptor == Just tagCode) tagGroupList
      tag.tagValue
  where
    descriptorCode :: Maybe Spec.Descriptor -> Maybe Text
    descriptorCode (Just desc) = desc.descriptorCode
    descriptorCode Nothing = Nothing
