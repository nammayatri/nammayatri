{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Utilities for encrypting secret and personal data with external service.
module Beckn.External.Encryption
  ( DbHash (..),
    DbHashable (..),
    EncKind (..),
    Encrypted (..),
    EncFlow,
    EncryptedHashed (..),
    EncryptedHashedField,
    EncryptedBase64 (..),
    EncTools (..),
    HashSalt,
    EncryptedItem' (..),
    encrypt,
    decrypt,
    getDbHash,

    -- * Re-exports
    EncryptedItem (..),
    genericEncryptItem,
    genericDecryptItem,
  )
where

import Beckn.Storage.Esqueleto (PersistField, PersistFieldSql)
import Beckn.Types.App
import Beckn.Types.Field
import Beckn.Utils.Dhall (FromDhall)
import qualified Crypto.Hash as Hash
import Crypto.Hash.Algorithms (SHA256)
import qualified Data.Aeson as Aeson
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Lazy as LBS
import EulerHS.Prelude
import Passetto.Client (EncryptedBase64 (..), EncryptedItem (..), PassettoContext, cliDecrypt, cliEncrypt, genericDecryptItem, genericEncryptItem, mkDefPassettoContext, throwLeft)
import Passetto.Client.EncryptedItem (Encrypted (..))
import Text.Hex (decodeHex, encodeHex)

-- * Encrypted fields

-- | Specifies whether field is encrypted or not.
--
-- Esqueleto table schemas which have some fields encrypted are assumed to be
-- polymorphic over type of this kind; it then should be included into
-- 'EncryptedField' to affect a particular field.
data EncKind
  = AsEncrypted
  | AsUnencrypted

-- ** Encrypted and hashed fields

-- | Hash which we store in the database in order to be able to lookup on a
-- value.
--
-- NOTE: we have decided on this temporary solution, in no way it is assumed
-- to be secure.
-- Implementation of this hash, as well as the overall need in it is to be
-- revised later.
newtype DbHash = DbHash {unDbHash :: ByteString}
  deriving stock (Show, Eq)
  deriving newtype (PersistField, PersistFieldSql)

-- These json instances are necessary for Euler's ART system only
instance ToJSON DbHash where
  toJSON = toJSON . encodeHex . unDbHash

instance FromJSON DbHash where
  parseJSON = maybe (fail "Bad hex") (pure . DbHash) . decodeHex <=< parseJSON

type HashAlgo = SHA256

type HashSalt = Text

-- | Typeclass for values which can be hashed.
class DbHashable a where
  evalDbHash :: (a, HashSalt) -> DbHash
  default evalDbHash :: ToJSON a => (a, HashSalt) -> DbHash
  evalDbHash = evalDbHash . first toJSON

instance DbHashable ByteString where
  evalDbHash = evalDbHash . first LBS.fromStrict

instance DbHashable LByteString where
  evalDbHash (a, salt) =
    DbHash . BA.convert @(Hash.Digest HashAlgo) $ Hash.hashlazy (encodeUtf8 salt <> a)

instance DbHashable Text where
  evalDbHash = evalDbHash . first (encodeUtf8 @_ @ByteString)

instance DbHashable Aeson.Value where
  evalDbHash = evalDbHash . first Aeson.encode

data EncryptedHashed a = EncryptedHashed
  { encrypted :: Encrypted a,
    hash :: DbHash
  }
  deriving stock (Generic)

instance
  (ToJSON a, FromJSON a, DbHashable a) =>
  EncryptedItem (EncryptedHashed a)
  where
  type Unencrypted (EncryptedHashed a) = (a, HashSalt)
  encryptItem value = do
    let hash = evalDbHash value
    encrypted <- encryptItem $ fst value
    return EncryptedHashed {..}
  decryptItem mvalue = (,"") <$> decryptItem mvalue.encrypted

-- | Mark a field as encrypted with hash or not, depending on @e@ argument.
--
-- The same considerations as for 'EncryptedField' apply here.
type family EncryptedHashedField (e :: EncKind) (a :: Type) :: Type where
  EncryptedHashedField 'AsUnencrypted a = a
  EncryptedHashedField 'AsEncrypted a = EncryptedHashed a

class (EncryptedItem e) => EncryptedItem' e where
  type UnencryptedItem e :: Type
  toUnencrypted :: UnencryptedItem e -> HashSalt -> Unencrypted e
  fromUnencrypted :: Unencrypted e -> UnencryptedItem e

instance (ToJSON a, FromJSON a, DbHashable a) => EncryptedItem' (EncryptedHashed a) where
  type UnencryptedItem (EncryptedHashed a) = a
  toUnencrypted a salt = (a, salt)
  fromUnencrypted a = fst a

-- * Encryption methods

data EncTools = EncTools
  { hashSalt :: HashSalt,
    service :: (String, Word16)
  }
  deriving (Generic, FromDhall)

-- FIXME! Modify passetto to use BaseUrl and use it too!
type EncFlow m r = (HasFlowEnv m r '["encTools" ::: EncTools])

-- Helper which allows running passetto client operations in our monad.
withPassettoCtx :: MonadIO m => (String, Word16) -> ReaderT PassettoContext IO a -> m a
withPassettoCtx (host, port) action =
  liftIO (mkDefPassettoContext host port) >>= liftIO . runReaderT action

-- | Encrypt given value.
--
-- Note: this performs not more than one call to server, so try to avoid using
-- multiple subsequent invocations of this method in favor of passing complex
-- structures (e.g. tuples) through it.
encrypt ::
  forall (m :: Type -> Type) r e.
  (EncFlow m r, EncryptedItem' e) =>
  UnencryptedItem e ->
  m e
encrypt payload = do
  encTools <- asks (.encTools)
  let unencrypted = toUnencrypted @e payload encTools.hashSalt
  withPassettoCtx encTools.service $ throwLeft =<< cliEncrypt unencrypted

-- | Decrypt given value.
decrypt ::
  forall (m :: Type -> Type) r e.
  (EncFlow m r, EncryptedItem' e) =>
  e ->
  m (UnencryptedItem e)
decrypt encrypted = do
  encTools <- asks (.encTools)
  item <- withPassettoCtx encTools.service $ throwLeft =<< cliDecrypt encrypted
  return $ fromUnencrypted @e item

getDbHash ::
  (EncFlow m r, DbHashable a) =>
  a ->
  m DbHash
getDbHash a = do
  salt <- asks (.encTools.hashSalt)
  return $ evalDbHash (a, salt)
