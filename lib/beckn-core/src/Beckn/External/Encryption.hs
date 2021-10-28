{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Utilities for encrypting secret and personal data with external service.
module Beckn.External.Encryption
  ( DbHash (..),
    DbHashable (..),
    EncKind (..),
    Encrypted (..),
    EncFlow,
    EncryptedField,
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

import Beckn.Types.App
import Beckn.Types.Field
import Beckn.Utils.Dhall (FromDhall)
import qualified Crypto.Hash as Hash
import Crypto.Hash.Algorithms (SHA256)
import qualified Data.Aeson as Aeson
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Lazy as LBS
import Database.Beam as B (Beamable, Columnar, HasSqlEqualityCheck (..), Nullable)
import Database.Beam.Backend (FromBackendRow (..), HasSqlValueSyntax (..))
import Database.Beam.Postgres (Postgres)
import Database.Beam.Postgres.Syntax (PgValueSyntax)
import EulerHS.Prelude
import GHC.TypeLits (ErrorMessage (..), TypeError)
import Passetto.Client (EncryptedBase64 (..), EncryptedItem (..), PassettoContext, cliDecrypt, cliEncrypt, genericDecryptItem, genericEncryptItem, mkDefPassettoContext, throwLeft)
import Passetto.Client.EncryptedItem (Encrypted (..))
import Text.Hex (decodeHex, encodeHex)

-- * Encrypted fields

-- | Specifies whether field is encrypted or not.
--
-- Beam table schemas which have some fields encrypted are assumed to be
-- polymorphic over type of this kind; it then should be included into
-- 'EncryptedField' to affect a particular field.
data EncKind
  = AsEncrypted
  | AsUnencrypted

-- | Mark a field as encrypted or not, depending on @e@ argument.
--
-- This always relies on 'ToJSON' and 'FromJSON' instances to serialize the value
-- under the hood. If this does not suit you, use some other type family.
type family EncryptedField (e :: EncKind) (f :: Type -> Type) (a :: Type) :: Type where
  EncryptedField 'AsUnencrypted f a = Columnar f a
  EncryptedField 'AsEncrypted f a = Columnar f (Encrypted a)

-- | 'Encrypted' always corresponds to a textual SQL type.
-- Adjust the size to be @4/3@ times greater than JSON encoded plaintext value
-- (because of base64 encoding) + few extra bytes as reserve.
deriving newtype instance HasSqlValueSyntax PgValueSyntax (Encrypted a)

deriving newtype instance FromBackendRow Postgres (Encrypted a)

-- | This instance is prohibited because encryption is not a deterministic
-- operation (different encryption keys can be used each time), so
-- matching on encrypted data does not actually give you any information.
--
-- If you need to lookup by encrypted data, put 'EncryptedHashedField'
-- into table field, and in query match against field hash, e.g.
-- @ myfield.hash ==. val_ (evalDbHash seekedValue) @
instance
  TypeError
    ( 'Text "Matching on encrypted data is not allowed"
        ':$$: 'Text "Match on hash instead"
    ) =>
  HasSqlEqualityCheck Postgres (Encrypted a)

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
  deriving anyclass (HasSqlEqualityCheck Postgres)

-- These json instances are necessary for Euler's ART system only
instance ToJSON DbHash where
  toJSON = toJSON . encodeHex . unDbHash

instance FromJSON DbHash where
  parseJSON = maybe (fail "Bad hex") (pure . DbHash) . decodeHex <=< parseJSON

-- | Corresponds to @bytea@ type in database.
deriving newtype instance HasSqlValueSyntax PgValueSyntax DbHash

deriving newtype instance FromBackendRow Postgres DbHash

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

-- | A field which appears encrypted in database along with hash.
--
-- In database this occupies two columns.
--
-- If you need to mark a field as optional, pass @Nullable f@ as
-- the last type argument.
data EncryptedHashed a f = EncryptedHashed
  { encrypted :: Columnar f (Encrypted a),
    hash :: Columnar f DbHash
  }
  deriving stock (Generic)
  deriving anyclass (Beamable)

instance
  (ToJSON a, FromJSON a, DbHashable a) =>
  EncryptedItem (EncryptedHashed a Identity)
  where
  type Unencrypted (EncryptedHashed a Identity) = Identity (a, HashSalt)
  encryptItem (Identity value) = do
    let hash = evalDbHash value
    encrypted <- encryptItem $ fst value
    return EncryptedHashed {..}
  decryptItem mvalue = Identity . (,"") <$> decryptItem (encrypted mvalue)

instance
  (ToJSON a, FromJSON a, DbHashable a) =>
  EncryptedItem (EncryptedHashed a (B.Nullable Identity))
  where
  type Unencrypted (EncryptedHashed a (B.Nullable Identity)) = Maybe (a, HashSalt)
  encryptItem mvalue = do
    let hash = evalDbHash <$> mvalue
    encrypted <- encryptItem $ fst <$> mvalue
    return EncryptedHashed {..}
  decryptItem mvalue = fmap (,"") <$> decryptItem (encrypted mvalue)

class (EncryptedItem e) => EncryptedItem' e where
  type UnencryptedItem e :: Type
  toUnencrypted :: UnencryptedItem e -> HashSalt -> Unencrypted e
  fromUnencrypted :: Unencrypted e -> UnencryptedItem e

instance (ToJSON a, FromJSON a, DbHashable a) => EncryptedItem' (EncryptedHashed a Identity) where
  type UnencryptedItem (EncryptedHashed a Identity) = a
  toUnencrypted a salt = Identity (a, salt)
  fromUnencrypted a = fst $ runIdentity a

instance (ToJSON a, FromJSON a, DbHashable a) => EncryptedItem' (EncryptedHashed a (B.Nullable Identity)) where
  type UnencryptedItem (EncryptedHashed a (B.Nullable Identity)) = Maybe a
  toUnencrypted a salt = (,salt) <$> a
  fromUnencrypted a = fst <$> a

-- | Mark a field as encrypted with hash or not, depending on @e@ argument.
--
-- The same considerations as for 'EncryptedField' apply here.
type family EncryptedHashedField (e :: EncKind) (f :: Type -> Type) (a :: Type) :: Type where
  EncryptedHashedField 'AsUnencrypted f a = Columnar f a
  EncryptedHashedField 'AsEncrypted f a = EncryptedHashed a f

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
