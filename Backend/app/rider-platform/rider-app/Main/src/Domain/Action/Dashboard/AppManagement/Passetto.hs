module Domain.Action.Dashboard.AppManagement.Passetto
  ( postPassettoEncrypt,
    postPassettoDecrypt,
  )
where

import qualified API.Types.Dashboard.AppManagement.Passetto
import qualified Domain.Types.Merchant
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption (EncFlow, decrypt, encrypt)
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Passetto.Client.EncryptedItem (Encrypted (..))

postPassettoEncrypt ::
  (EncFlow m r) =>
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  API.Types.Dashboard.AppManagement.Passetto.PassettoEncryptReq ->
  m API.Types.Dashboard.AppManagement.Passetto.PassettoEncryptResp
postPassettoEncrypt _merchantShortId _opCity req = do
  (encrypted :: Encrypted Text) <- encrypt req.value
  pure API.Types.Dashboard.AppManagement.Passetto.PassettoEncryptResp {encryptedValue = unEncrypted encrypted}

postPassettoDecrypt ::
  (EncFlow m r) =>
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  API.Types.Dashboard.AppManagement.Passetto.PassettoDecryptReq ->
  m API.Types.Dashboard.AppManagement.Passetto.PassettoDecryptResp
postPassettoDecrypt _merchantShortId _opCity req = do
  (decrypted :: Text) <- decrypt (Encrypted req.encryptedValue :: Encrypted Text)
  pure API.Types.Dashboard.AppManagement.Passetto.PassettoDecryptResp {value = decrypted}
