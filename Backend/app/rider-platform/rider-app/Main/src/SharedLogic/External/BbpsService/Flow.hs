{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}

module SharedLogic.External.BbpsService.Flow where

import Data.Aeson
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as C8
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Domain.Types.BBPSConfig
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Utils.Common
import Web.JWT

-- data BbbpsConfig = BbbpsConfig {
--     agentId :: Text,
--     serverUrl :: Text,
--     signatureKey :: EncryptedField 'AsEncrypted Text
--   }

createBBPSJWT :: (MonadFlow m, EncFlow m r) => BBPSConfig -> Text -> Text -> m (Either Text Text)
createBBPSJWT BBPSConfig {..} mobileNumber deviceId = do
  now <- getCurrentTime
  let joseHeader = JOSEHeader {typ = Just "JWT", alg = Just RS256, cty = Nothing, kid = Just bbpsAgentId}
      claims' =
        mempty
          { iat = numericDate $ utcTimeToPOSIXSeconds now,
            unregisteredClaims =
              ClaimsMap $
                M.fromList
                  [ ("mobile", String mobileNumber),
                    ("device_id", String deviceId)
                  ]
          }
  bbpsSignatureKeyDecrypted <- decrypt bbpsSignatureKey
  pure $
    (readRsaSecret . Base64.decodeLenient . C8.pack . T.unpack $ bbpsSignatureKeyDecrypted) & \case
      Just rsaSecret -> Right $ encodeSigned (EncodeRSAPrivateKey rsaSecret) joseHeader claims'
      Nothing -> Left "Invalid private key"
