{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Code-owned text keys.
--
-- Every user-facing string the backend looks up by name is one constructor of a key type.
-- The constructor is @Domain_Field Value@ and its 'show' is the stored form @Domain.Field.Value@,
-- so the database, the dashboard and the code all spell a key the same way. A key that replaced
-- an older free-text name keeps that name as its 'legacyKeyName', and a lookup asks for both.
module Domain.Types.TranslationKey
  ( IsTranslationKey (..),
    keyName,
    lookupNames,
    legacyOrKeyName,
    dotted,
    dottedConstructorName,
    TranslatedText,
    translatedTextToText,
    textReadByKey,
    textAsReceived,
  )
where

import qualified Data.Text as T
import GHC.Generics
import Kernel.Prelude

class Show k => IsTranslationKey k where
  -- | Every key the code can produce: the registry and 'parseKey' read it.
  allKeys :: [k]

  -- | The name the same lookup used before the key became an enum.
  legacyKeyName :: k -> Maybe Text
  legacyKeyName _ = Nothing

  -- | Reads the stored form or the legacy form of a key in 'allKeys'.
  parseKey :: Text -> Maybe k
  parseKey t = find (\k -> keyName k == t || legacyKeyName k == Just t) allKeys

-- | The key's stored name.
keyName :: Show k => k -> Text
keyName = show

-- | The stored names a lookup asks for, the key's own name first.
lookupNames :: IsTranslationKey k => k -> [Text]
lookupNames k = keyName k : maybe [] pure (legacyKeyName k)

-- | The name the code used before the key became an enum, else the key's own: for text that
-- leaves the backend (app payloads, Redis entries) and so must not change with the key.
legacyOrKeyName :: IsTranslationKey k => k -> Text
legacyOrKeyName k = fromMaybe (keyName k) (legacyKeyName k)

dotted :: [Text] -> String
dotted = T.unpack . T.intercalate "."

-- | The stored form of a constructor without arguments: @Domain_Field_Value@ printed as
-- @Domain.Field.Value@. Constructors with arguments spell their own 'show' case.
dottedConstructorName :: (Generic a, GConstructorName (Rep a)) => a -> String
dottedConstructorName = map (\c -> if c == '_' then '.' else c) . gConstructorName . from

class GConstructorName f where
  gConstructorName :: f p -> String

instance GConstructorName f => GConstructorName (D1 d f) where
  gConstructorName (M1 x) = gConstructorName x

instance (GConstructorName f, GConstructorName g) => GConstructorName (f :+: g) where
  gConstructorName (L1 x) = gConstructorName x
  gConstructorName (R1 x) = gConstructorName x

instance Constructor c => GConstructorName (C1 c f) where
  gConstructorName = conName

-- | Translatable text, in the reader's language. An API field of this type is filled by reading
-- a translation key through the app's "Storage.CachedQueries.Translations"; a string literal does
-- not fit it. It is sent as the same plain string as 'Text'.
newtype TranslatedText = TranslatedText Text
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToSchema)

translatedTextToText :: TranslatedText -> Text
translatedTextToText (TranslatedText text) = text

-- | The text a key lookup returned. Only "Storage.CachedQueries.Translations" of each app calls it;
-- the @translation-keys-only@ pre-commit hook keeps it there.
textReadByKey :: Text -> TranslatedText
textReadByKey = TranslatedText

-- | For text that comes from outside the code and is shown as given: typed by ops (a gate's own name),
-- typed by a user, or sent by another service. Never a string literal: translatable text belongs to a key.
textAsReceived :: Text -> TranslatedText
textAsReceived = TranslatedText
