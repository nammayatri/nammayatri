{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module FRFS.CMRLV2.EncryptionSpec
  ( testEncryptMockPayload,
    testDecryption,
  )
where

import qualified Crypto.Cipher.AES as AES
import qualified Crypto.Cipher.Types as CT
import qualified Crypto.Error as CE
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import EulerHS.Prelude
import ExternalBPP.ExternalAPI.Metro.CMRL.V2.Encryption (decryptPayload)

data MockOperatorData = MockOperatorData
  { operatorNameId :: Int,
    merchantOrderId :: T.Text,
    bankTransactionRefNumber :: T.Text,
    merchantId :: T.Text,
    ticketTypeId :: Int,
    paymentMode :: Int,
    paymentChannelId :: Int,
    transTypeId :: Int,
    zoneNumber :: Int,
    fareQuoteId :: T.Text
  }
  deriving (Generic, Show)

instance ToJSON MockOperatorData

instance FromJSON MockOperatorData

data MockTicketInfoPayload = MockTicketInfoPayload
  { grp_Size :: T.Text,
    src_Stn :: T.Text,
    dest_Stn :: T.Text,
    activation_Date :: T.Text,
    product_Id :: T.Text,
    service_Id :: T.Text,
    tkt_Fare :: T.Text,
    validity :: T.Text,
    duration :: T.Text,
    operatorData :: MockOperatorData
  }
  deriving (Generic, Show)

instance ToJSON MockTicketInfoPayload

instance FromJSON MockTicketInfoPayload

data MockOperator = MockOperator
  { opID :: T.Text,
    noOfTickets :: T.Text,
    validator_Info :: T.Text,
    ticketInfo :: [MockTicketInfoPayload]
  }
  deriving (Generic, Show)

instance ToJSON MockOperator

instance FromJSON MockOperator

data MockDynamicBlock = MockDynamicBlock
  { operators :: [MockOperator]
  }
  deriving (Generic, Show)

instance ToJSON MockDynamicBlock

instance FromJSON MockDynamicBlock

data MockTicketBlock = MockTicketBlock
  { dynamic_Block :: MockDynamicBlock
  }
  deriving (Generic, Show)

instance ToJSON MockTicketBlock

instance FromJSON MockTicketBlock

data MockGenerateTicketPayload = MockGenerateTicketPayload
  { requester_ID :: T.Text,
    language :: T.Text,
    txn_Type :: T.Text,
    txn_Ref_No :: T.Text,
    txn_Date :: T.Text,
    pSP_Specific_Data :: T.Text,
    total_Fare :: T.Text,
    customer_Mobile :: T.Text,
    ticketBlock :: MockTicketBlock
  }
  deriving (Generic, Show)

instance ToJSON MockGenerateTicketPayload

instance FromJSON MockGenerateTicketPayload

zeroIV :: BS.ByteString
zeroIV = BS.replicate 16 0

pkcs7Pad :: BS.ByteString -> BS.ByteString
pkcs7Pad input =
  let blockSize = 16
      padLen = blockSize - (BS.length input `mod` blockSize)
   in input <> BS.replicate padLen (fromIntegral padLen)

initCipher :: Text -> Either String AES.AES256
initCipher clientKey = do
  let keyBytes = TE.encodeUtf8 clientKey
  if BS.length keyBytes /= 32
    then Left "AES-256 requires a 32-byte (256-bit) key"
    else case CT.cipherInit keyBytes :: CE.CryptoFailable AES.AES256 of
      CE.CryptoPassed cipher -> Right cipher
      CE.CryptoFailed err -> Left $ "Cipher init failed: " <> show err

testEncryptOnly :: Text -> Text -> Either String Text
testEncryptOnly key plaintext = do
  cipher <- initCipher key
  iv <- case CT.makeIV zeroIV of
    Nothing -> Left "Failed to create IV"
    Just v -> Right v
  let padded = pkcs7Pad (TE.encodeUtf8 plaintext)
      ciphertext = CT.cbcEncrypt cipher iv padded
      encrypted = TE.decodeUtf8 (B64.encode ciphertext)
  Right encrypted

testEncryptMockPayload :: Text -> Either String (Text, Text)
testEncryptMockPayload key = do
  let mockOperatorData =
        MockOperatorData
          { operatorNameId = 1000,
            merchantOrderId = "CUM2181178658",
            bankTransactionRefNumber = "5dbdf9b4-55f7-482d-9ae7-d39f44c5abec",
            merchantId = "7000886103351",
            ticketTypeId = 105,
            paymentMode = 102,
            paymentChannelId = 0,
            transTypeId = 0,
            zoneNumber = 20,
            fareQuoteId = ""
          }

      mockTicketInfo =
        MockTicketInfoPayload
          { grp_Size = "3",
            src_Stn = "0201",
            dest_Stn = "0117",
            activation_Date = "09012026073138",
            product_Id = "105",
            service_Id = "1",
            tkt_Fare = "30",
            validity = "100",
            duration = "180",
            operatorData = mockOperatorData
          }

      mockOperator =
        MockOperator
          { opID = "1000",
            noOfTickets = "1",
            validator_Info = "31",
            ticketInfo = [mockTicketInfo]
          }

      mockPayload =
        MockGenerateTicketPayload
          { requester_ID = "7000886103345",
            language = "0",
            txn_Type = "65",
            txn_Ref_No = "CUM2181178601",
            txn_Date = "09012026073138",
            pSP_Specific_Data = "Mode=UPI;ServiceFee=0%",
            total_Fare = "30",
            customer_Mobile = "7680018358",
            ticketBlock =
              MockTicketBlock
                { dynamic_Block =
                    MockDynamicBlock
                      { operators = [mockOperator]
                      }
                }
          }

      payloadText = TE.decodeUtf8 $ BL.toStrict $ encode mockPayload

  encrypted <- testEncryptOnly key payloadText
  Right (payloadText, encrypted)

testDecryption :: Text -> Text -> Either String Text
testDecryption key encryptedText = decryptPayload encryptedText key
