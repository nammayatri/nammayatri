module SharedLogic.DriverNumber
  ( getDriverMobileNumber,
    getDriverMobileNumberFromDecrypted,
  )
where

import qualified Domain.Types.Ride as DRide
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Utils.Common
import qualified Storage.Queries.Ride as QRide
import qualified Tools.Fields as Fields

getDriverMobileNumber :: (EncFlow m r, EsqDBFlow m r, MonadFlow m) => DRide.Ride -> m (Maybe Text)
getDriverMobileNumber ride = case (ride.driverPhoneNumber, ride.driverMobileNumber) of
  (Just encrypted, Nothing) -> Just <$> decrypt encrypted
  (Just encrypted, Just plaintext) -> do
    decrypted <- decrypt encrypted
    fork "DriverNumber:clearPlaintext" $ clearPlaintext ride encrypted decrypted plaintext
    pure $ Just decrypted
  (Nothing, Just plaintext) -> do
    fork "DriverNumber:backfill" $ backfill ride plaintext
    pure $ Just plaintext
  (Nothing, Nothing) -> pure Nothing

backfill :: (EncFlow m r, EsqDBFlow m r, MonadFlow m) => DRide.Ride -> Text -> m ()
backfill ride plaintext = do
  res <- withTryCatch "DriverNumber:backfill" $ do
    encryptedHashed <- encrypt plaintext
    QRide.updateDriverNumberFromPlaintext ride.id encryptedHashed
  case res of
    Left err -> logError $ "DriverNumber backfill failed for ride " <> ride.id.getId <> ": " <> show err
    Right _ -> pure ()

clearPlaintext :: (EncFlow m r, EsqDBFlow m r, MonadFlow m) => DRide.Ride -> EncryptedHashed Text -> Text -> Text -> m ()
clearPlaintext ride encrypted decrypted plaintext = do
  res <- withTryCatch "DriverNumber:clearPlaintext" $ do
    plaintextHash <- getDbHash plaintext
    if encrypted.hash == plaintextHash && decrypted == plaintext
      then QRide.clearPlaintextDriverNumber ride.id
      else logError $ "DriverNumber: stored encrypted/hash does not match plaintext for ride " <> ride.id.getId <> ", plaintext left untouched"
  case res of
    Left err -> logError $ "DriverNumber plaintext clear failed for ride " <> ride.id.getId <> ": " <> show err
    Right _ -> pure ()

getDriverMobileNumberFromDecrypted :: DRide.DecryptedRide -> Maybe Text
getDriverMobileNumberFromDecrypted ride = maybe ride.driverMobileNumber Just ride.driverPhoneNumber
