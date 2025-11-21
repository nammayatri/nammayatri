module Storage.Queries.Transformers.DigilockerVerification where

import Kernel.External.Encryption
import Kernel.Prelude

-- Since we're using EncryptedField (not EncryptedHashedField),
-- we only need the encrypted field, no hash
mkFieldEncrypted :: Maybe (EncryptedField 'AsEncrypted Text) -> Maybe Text
mkFieldEncrypted = fmap unEncrypted

-- This function is no longer needed since we don't have hash field,
-- but keeping it for backward compatibility during transition
mkFieldHash :: Maybe (EncryptedField 'AsEncrypted Text) -> Maybe DbHash
mkFieldHash = const Nothing

-- This function is no longer needed since we don't use EncryptedHashedField,
-- but keeping it for backward compatibility during transition
mkEncryptedItem :: Maybe Text -> Maybe DbHash -> Maybe (EncryptedField 'AsEncrypted Text)
mkEncryptedItem (Just encrypted) _ = Just (Encrypted encrypted)
mkEncryptedItem Nothing _ = Nothing
