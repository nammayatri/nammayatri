module Storage.Queries.Transformers.FleetOwnerInformation where

import Kernel.External.Encryption
import Kernel.Prelude

mkEncryptedItem :: Maybe Text -> Maybe DbHash -> Maybe (EncryptedHashed Text)
mkEncryptedItem fieldEncrypted fieldHash = do
  EncryptedHashed <$> (Encrypted <$> fieldEncrypted) <*> fieldHash

mkFieldEncrypted :: Maybe (EncryptedHashed Text) -> Maybe Text
mkFieldEncrypted item = item <&> unEncrypted . (.encrypted)

mkFieldHash :: Maybe (EncryptedHashed Text) -> Maybe DbHash
mkFieldHash item = item <&> (.hash)
