{-
 Copyright 2022-23, Juspay India Pvt Ltd
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Type-safe disability tag ADT for ONDC v2.1.0.
--
-- This module defines the canonical list of ONDC disability types used across
-- the search/select/booking pipeline.  Using an ADT instead of raw @Maybe Text@
-- ensures that any new disability type must be added here first, which in turn
-- forces the integration tests and Beckn serialisation helpers to be updated.
--
-- __CI guard:__ any change to 'DisabilityTag' requires updating the roundtrip
-- tests in @beckn-spec-test@ (see @BecknV2.OnDemand.TagsSpec@).
module SharedLogic.DisabilityGuard
  ( DisabilityTag (..),
    allDisabilityTags,
    disabilityTagToText,
    parseDisabilityTag,
    disabilityTagFromText,
  )
where

import Data.Aeson (Value (..), withText)
import qualified Data.Text as T
import Database.Beam.Backend ()
import Database.PostgreSQL.Simple.FromField ()
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnum)
import Kernel.Prelude

-- | All 13 ONDC v2.1.0 disability types.
--
-- If you add or remove a constructor here you __must__ also update:
--
--   1. 'disabilityTagToText' / 'disabilityTagFromText' in this module
--   2. 'disabilityTagToGroup' in "BecknV2.OnDemand.Tags"
--   3. The roundtrip property tests in @BecknV2.OnDemand.TagsSpec@
--   4. The @disability@ seed data in @dev/sql-seed/…@
data DisabilityTag
  = BLIND_LOW_VISION
  | HEAR_IMPAIRMENT
  | LOCOMOTOR_DISABILITY
  | COGNITIVE_DISABILITY
  | LEPROSY_CURED
  | SPEECH_LANGUAGE
  | INTELLECTUAL_DISABILITY
  | MENTAL_ILLNESS
  | BLOOD_DISORDER
  | DWARFISM
  | ACID_ATTACK_SURVIVOR
  | MULTIPLE_DISABILITIES
  | OTHER_DISABILITY
  deriving (Eq, Ord, Show, Read, Generic, Enum, Bounded)

-- | Exhaustive list — used in tests to ensure full coverage.
allDisabilityTags :: [DisabilityTag]
allDisabilityTags = [minBound .. maxBound]

-- | Canonical text representation matching the values stored in the database
--   and sent over the Beckn protocol.
disabilityTagToText :: DisabilityTag -> T.Text
disabilityTagToText = \case
  BLIND_LOW_VISION -> "BLIND_LOW_VISION"
  HEAR_IMPAIRMENT -> "HEAR_IMPAIRMENT"
  LOCOMOTOR_DISABILITY -> "LOCOMOTOR_DISABILITY"
  COGNITIVE_DISABILITY -> "COGNITIVE_DISABILITY"
  LEPROSY_CURED -> "LEPROSY_CURED"
  SPEECH_LANGUAGE -> "SPEECH_LANGUAGE"
  INTELLECTUAL_DISABILITY -> "INTELLECTUAL_DISABILITY"
  MENTAL_ILLNESS -> "MENTAL_ILLNESS"
  BLOOD_DISORDER -> "BLOOD_DISORDER"
  DWARFISM -> "DWARFISM"
  ACID_ATTACK_SURVIVOR -> "ACID_ATTACK_SURVIVOR"
  MULTIPLE_DISABILITIES -> "MULTIPLE_DISABILITIES"
  OTHER_DISABILITY -> "OTHER_DISABILITY"

-- | Strict parser: returns 'Just' only for recognised ONDC disability tags,
--   'Nothing' otherwise.  Callers should use this for any new code so that
--   unrecognised values are surfaced explicitly instead of silently mapping
--   to 'OTHER_DISABILITY'.
parseDisabilityTag :: T.Text -> Maybe DisabilityTag
parseDisabilityTag = \case
  "BLIND_LOW_VISION" -> Just BLIND_LOW_VISION
  "HEAR_IMPAIRMENT" -> Just HEAR_IMPAIRMENT
  "LOCOMOTOR_DISABILITY" -> Just LOCOMOTOR_DISABILITY
  "COGNITIVE_DISABILITY" -> Just COGNITIVE_DISABILITY
  "LEPROSY_CURED" -> Just LEPROSY_CURED
  "SPEECH_LANGUAGE" -> Just SPEECH_LANGUAGE
  "INTELLECTUAL_DISABILITY" -> Just INTELLECTUAL_DISABILITY
  "MENTAL_ILLNESS" -> Just MENTAL_ILLNESS
  "BLOOD_DISORDER" -> Just BLOOD_DISORDER
  "DWARFISM" -> Just DWARFISM
  "ACID_ATTACK_SURVIVOR" -> Just ACID_ATTACK_SURVIVOR
  "MULTIPLE_DISABILITIES" -> Just MULTIPLE_DISABILITIES
  "OTHER_DISABILITY" -> Just OTHER_DISABILITY
  _ -> Nothing

-- | Lossy parser kept for backwards-compatibility with legacy database rows.
--   Unknown strings fall back to 'OTHER_DISABILITY'.
--   Prefer 'parseDisabilityTag' in new code.
disabilityTagFromText :: T.Text -> DisabilityTag
disabilityTagFromText t = fromMaybe OTHER_DISABILITY (parseDisabilityTag t)

-- JSON instances -----------------------------------------------------------

instance ToJSON DisabilityTag where
  toJSON = String . disabilityTagToText

instance FromJSON DisabilityTag where
  parseJSON = withText "DisabilityTag" $ \t ->
    case parseDisabilityTag t of
      Just tag -> pure tag
      Nothing -> fail $ "Unknown DisabilityTag: " <> T.unpack t

-- Beam / PostgreSQL instances ----------------------------------------------

$(mkBeamInstancesForEnum ''DisabilityTag)
