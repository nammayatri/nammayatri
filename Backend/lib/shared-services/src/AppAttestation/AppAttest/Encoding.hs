{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module AppAttestation.AppAttest.Encoding
  ( AttestationObject (..),
    AssertionObject (..),
    AuthenticatorData (..),
    AttestedCredential (..),
    decodeAttestationObject,
    decodeAssertionObject,
    parseAuthenticatorData,
  )
where

import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Term as CBOR
import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Lazy as TL
import Kernel.Prelude

data AttestationObject = AttestationObject
  { format :: Text,
    certificateChain :: [ByteString],
    receipt :: Maybe ByteString,
    authData :: ByteString
  }
  deriving (Show, Eq)

data AssertionObject = AssertionObject
  { signature :: ByteString,
    authenticatorData :: ByteString
  }
  deriving (Show, Eq)

data AuthenticatorData = AuthenticatorData
  { rpIdHash :: ByteString,
    signCount :: Int,
    attestedCredential :: Maybe AttestedCredential
  }
  deriving (Show, Eq)

data AttestedCredential = AttestedCredential
  { aaguid :: ByteString,
    credentialId :: ByteString
  }
  deriving (Show, Eq)

decodeAttestationObject :: ByteString -> Maybe AttestationObject
decodeAttestationObject raw = do
  root <- decodeTerm raw
  format <- lookupKey "fmt" root >>= asText
  statement <- lookupKey "attStmt" root
  certificateChain <- lookupKey "x5c" statement >>= asList >>= traverse asBytes
  authData <- lookupKey "authData" root >>= asBytes
  pure AttestationObject {receipt = lookupKey "receipt" statement >>= asBytes, ..}

decodeAssertionObject :: ByteString -> Maybe AssertionObject
decodeAssertionObject raw = do
  root <- decodeTerm raw
  signature <- lookupKey "signature" root >>= asBytes
  authenticatorData <- lookupKey "authenticatorData" root >>= asBytes
  pure AssertionObject {..}

parseAuthenticatorData :: ByteString -> Maybe AuthenticatorData
parseAuthenticatorData raw
  | BS.length raw < headerLength = Nothing
  | otherwise =
    Just
      AuthenticatorData
        { rpIdHash = BS.take 32 raw,
          signCount = bigEndian . BS.take 4 $ BS.drop 33 raw,
          attestedCredential = parseAttestedCredential $ BS.drop headerLength raw
        }
  where
    headerLength = 37

parseAttestedCredential :: ByteString -> Maybe AttestedCredential
parseAttestedCredential raw = do
  guard $ BS.length raw >= 18
  let aaguid = BS.take 16 raw
      credentialIdLength = bigEndian . BS.take 2 $ BS.drop 16 raw
      credentialId = BS.take credentialIdLength $ BS.drop 18 raw
  guard $ BS.length credentialId == credentialIdLength
  pure AttestedCredential {..}

bigEndian :: ByteString -> Int
bigEndian = BS.foldl' (\acc byte -> (acc `shiftL` 8) .|. fromIntegral byte) 0

decodeTerm :: ByteString -> Maybe CBOR.Term
decodeTerm raw = case CBOR.deserialiseFromBytes CBOR.decodeTerm (BSL.fromStrict raw) of
  Right (rest, term) | BSL.null rest -> Just term
  _ -> Nothing

lookupKey :: Text -> CBOR.Term -> Maybe CBOR.Term
lookupKey key term = do
  entries <- case term of
    CBOR.TMap entries -> Just entries
    CBOR.TMapI entries -> Just entries
    _ -> Nothing
  snd <$> find ((== Just key) . asText . fst) entries

asText :: CBOR.Term -> Maybe Text
asText = \case
  CBOR.TString text -> Just text
  CBOR.TStringI text -> Just $ TL.toStrict text
  _ -> Nothing

asBytes :: CBOR.Term -> Maybe ByteString
asBytes = \case
  CBOR.TBytes bytes -> Just bytes
  CBOR.TBytesI bytes -> Just $ BSL.toStrict bytes
  _ -> Nothing

asList :: CBOR.Term -> Maybe [CBOR.Term]
asList = \case
  CBOR.TList items -> Just items
  CBOR.TListI items -> Just items
  _ -> Nothing
