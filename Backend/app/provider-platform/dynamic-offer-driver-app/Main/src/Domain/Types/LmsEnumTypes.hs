{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your optionText) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.LmsEnumTypes where

import qualified Data.ByteString.Char8 as C8
import Data.Text as T
import Data.Text.Encoding as TE
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.Postgres
import Database.PostgreSQL.Simple.FromField (FromField (fromField))
import qualified Database.PostgreSQL.Simple.FromField as DPSF
import Kernel.Prelude

data QuizQuestion = TextQuestion Text | ImageQuestion Text Text Int Int
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

instance FromField QuizQuestion where
  fromField f mbValue =
    case mbValue of
      Nothing -> DPSF.returnError UnexpectedNull f mempty
      Just value' ->
        case readMaybe $ C8.unpack value' of
          Just (TextQuestion val) -> pure $ TextQuestion $ TE.decodeUtf8 $ C8.pack $ T.unpack val
          Just (ImageQuestion question url width height) -> pure $ ImageQuestion (TE.decodeUtf8 $ C8.pack $ T.unpack question) url width height
          Nothing -> DPSF.returnError ConversionFailed f ("Could not 'read'" <> Kernel.Prelude.show value')

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be QuizQuestion where
  sqlValueSyntax (TextQuestion question) = sqlValueSyntax $ "TextQuestion " <> "\"" <> question <> "\""
  sqlValueSyntax (ImageQuestion question url int1 int2) = sqlValueSyntax $ "ImageQuestion " <> "\"" <> question <> "\" \"" <> url <> "\" " <> Kernel.Prelude.show int1 <> " " <> Kernel.Prelude.show int2

instance BeamSqlBackend be => B.HasSqlEqualityCheck be QuizQuestion

instance FromBackendRow Postgres QuizQuestion
