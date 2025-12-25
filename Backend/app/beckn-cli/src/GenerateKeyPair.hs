{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module GenerateKeyPair where

import qualified EulerHS.Language as L
import EulerHS.Prelude
import Kernel.Types.Credentials
import qualified Kernel.Utils.SignatureAuth as HttpSig

data GenerateKeyPairResponse = GenerateKeyPairResponse
  { privateKey :: PrivateKey,
    publicKey :: PublicKey
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

generateKeyPair :: L.Flow GenerateKeyPairResponse
generateKeyPair = do
  L.logInfo @Text "GenerateKeyPair" "Generating random key pair."
  L.runIO HttpSig.generateKeyPair <&> uncurry GenerateKeyPairResponse
