module Tools.Encryption
  ( module Reexport,
    decryptWithDefault,
    encryptWithDefault,
  )
where

import Kernel.External.Encryption as Reexport
import Kernel.Prelude

decryptWithDefault :: EncFlow m r => Maybe (EncryptedHashed Text) -> Maybe Text -> m (Maybe Text)
decryptWithDefault mbFieldEncrypted fieldDefaultDec = case mbFieldEncrypted of
  Just fieldEncrypted -> Just <$> decrypt fieldEncrypted
  Nothing -> pure fieldDefaultDec

encryptWithDefault :: EncFlow m r => Maybe (EncryptedHashed Text) -> Maybe Text -> m (Maybe (EncryptedHashed Text))
encryptWithDefault mbFieldEncrypted fieldDefaultDec = case mbFieldEncrypted of
  Just fieldEncrypted -> pure (Just fieldEncrypted)
  Nothing -> forM fieldDefaultDec encrypt
