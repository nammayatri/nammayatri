 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
 
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program 
 
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of 
 
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Crypto.Saltine.Core.Box (newKeypair)
import Crypto.Saltine.Core.SecretBox (newKey)
import qualified Data.Aeson as A
import Data.Binary (Binary, encode)
import qualified Data.Vector as V (fromList)
import Kernel.External.Encryption (DbHash (..), DbHashable (..))
import Passetto.Crypto (encryptKey, encryptMasterKey, passwordFromText, sodiumInit)
import Passetto.JsonEncryption (encryptPayload)
import Passetto.KeysContext (mkKeysContext)
import System.Environment (getArgs)
import Text.Hex (encodeHex, strictByteString)
import Universum

main :: IO ()
main = getArgs >>= doit
  where
    doit [ns, ps, pid, pmobile]
      | Just n <- readMaybe ns = main_ n (fromString ps) (fromString pid) (fromString pmobile)
    doit _ = putStrLn @Text "Usage: geninis num_of_keys password person_id person_mobile_number"

main_ :: Int -> Text -> Text -> Text -> IO ()
main_ n p pid pmo = do
  sodiumInit
  m <- newKey
  em <- encryptMasterKey (passwordFromText p) m
  ks <- replicateM n newKeypair
  eks <- mapM (encryptKey m) ks
  writeFile "passetto_init_data.sql" $
    unlines $
      ("INSERT INTO \"Passetto\".\"Master\" (key) values (decode('" <> b2hexs em <> "', 'hex'));") :
      map (\k -> "INSERT INTO \"Passetto\".\"Keys\" (encryptedKeyPair) values(decode('" <> b2hexs k <> "', 'hex'));") eks
  ek <- mkKeysContext (V.fromList ks) >>= runReaderT (encryptPayload $ "S" <> renderJSon (A.String pmo))
  let genAddEnc table =
        writeFile (toString table <> "_add_encrypted_phone.sql") $
          "UPDATE atlas_" <> table <> ".person SET mobile_number_encrypted = '" <> ek
            <> "', mobile_number_hash = decode('"
            <> encodeHex (unDbHash $ evalDbHash pmo)
            <> "', 'hex') where id = '"
            <> pid
            <> "';"
  genAddEnc "app"
  genAddEnc "transporter"
  where
    b2hexs :: Binary a => a -> Text
    b2hexs = encodeHex . strictByteString . encode
    renderJSon = decodeUtf8 . A.encode
