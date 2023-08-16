{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.DriverReferral where

import qualified Data.Text as T
import qualified Database.Beam as B
import Domain.Types.DriverReferral as DDR
import qualified Domain.Types.Person as SP
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Types.Logging
import qualified Sequelize as Se
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.DriverReferral as BeamDR

-- create :: DriverReferral -> SqlDB ()
-- create = Esq.create

create :: (L.MonadFlow m, Log m) => DDR.DriverReferral -> m ()
create = createWithKV

-- findByRefferalCode :: Transactionable m => Id DriverReferral -> m (Maybe DriverReferral)
-- findByRefferalCode = Esq.findById

findByRefferalCode ::
  (L.MonadFlow m, Log m) =>
  Id DriverReferral ->
  m (Maybe DriverReferral)
findByRefferalCode (Id referralId) = findOneWithKV [Se.Is BeamDR.referralCode $ Se.Eq referralId]

-- findById ::
--   Transactionable m =>
--   Id SP.Person ->
--   m (Maybe DriverReferral)
-- findById driverId = do
--   findOne $ do
--     driverReferral <- from $ table @DriverReferralT
--     where_ $ driverReferral ^. DriverReferralDriverId ==. val (toKey driverId)
--     return driverReferral

findById ::
  (L.MonadFlow m, Log m) =>
  Id SP.Person ->
  m (Maybe DriverReferral)
findById (Id driverId) = findOneWithKV [Se.Is BeamDR.driverId $ Se.Eq driverId]

getLastRefferalCode ::
  (L.MonadFlow m, Log m) =>
  m (Id DriverReferral)
getLastRefferalCode = do
  dbConf <- getMasterBeamConfig
  resp <-
    L.runDB dbConf $
      L.findRow $
        B.select $
          B.limit_ 1 $
            B.filter_' (\(BeamDR.DriverReferralT {referralCode}) -> B.sqlNot_ (B.sqlBool_ (B.in_ referralCode $ B.val_ <$> ["999999", "696969"]))) $
              B.orderBy_ (\driverReferral -> B.desc_ driverReferral.referralCode) do
                B.all_ (BeamCommon.driverReferral BeamCommon.atlasDB)
  case resp of
    Right (Just val) -> do
      mbDriverReferral <- fromTType' val
      case mbDriverReferral of
        Just driverReferral -> do
          let referralCode = trimLeadingZeros $ driverReferral.referralCode.getId
          pure (Id referralCode)
        Nothing -> pure "0"
    _ -> return "0"
  where
    trimLeadingZeros :: Text -> Text
    trimLeadingZeros text =
      case T.uncons text of
        Just ('0', rest) -> trimLeadingZeros rest
        _ -> text

instance FromTType' BeamDR.DriverReferral DriverReferral where
  fromTType' BeamDR.DriverReferralT {..} = do
    pure $
      Just
        DriverReferral
          { referralCode = Id referralCode,
            driverId = Id driverId,
            linkedAt = linkedAt
          }

instance ToTType' BeamDR.DriverReferral DriverReferral where
  toTType' DriverReferral {..} = do
    BeamDR.DriverReferralT
      { BeamDR.referralCode = getId referralCode,
        BeamDR.driverId = getId driverId,
        BeamDR.linkedAt = linkedAt
      }
