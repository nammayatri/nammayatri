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
import qualified Data.Aeson as A
import Data.Data (Data, gmapQ)
import Data.Generics.Aliases (ext1Q)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.UUID as UUID
import EulerHS.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common

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

getStartLocation :: [Spec.Stop] -> Maybe Spec.Stop
getStartLocation = find (\stop -> stop.stopType == Just "START")

getDropLocation :: [Spec.Stop] -> Maybe Spec.Stop
getDropLocation = find (\stop -> stop.stopType == Just "END")

getTransactionId :: (MonadFlow m) => Spec.Context -> m Text
getTransactionId context = context.contextTransactionId <&> UUID.toText & fromMaybeM (InvalidRequest "Transaction Id not found")

decodeReq :: (MonadFlow m, A.FromJSON v1, A.FromJSON v2) => ByteString -> m (Either v1 v2)
decodeReq reqBS =
  case A.eitherDecodeStrict reqBS of
    Right reqV2 -> pure $ Right reqV2
    Left _ ->
      case A.eitherDecodeStrict reqBS of
        Right reqV1 -> pure $ Left reqV1
        Left err -> throwError . InvalidRequest $ "Unable to parse request: " <> T.pack err <> T.decodeUtf8 reqBS
