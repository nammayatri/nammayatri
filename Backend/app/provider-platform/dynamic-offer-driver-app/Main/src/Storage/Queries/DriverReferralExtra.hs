{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DriverReferralExtra where

import qualified Data.Text
import qualified Database.Beam as B
import qualified Domain.Types.DriverReferral
import qualified Domain.Types.Person
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.DriverReferral as Beam
import qualified Storage.Beam.DriverReferral as BeamDR
import Storage.Queries.OrphanInstances.DriverReferral

-- Extra code goes here --

getLastRefferalCode ::
  (KvDbFlow m r, Log m) =>
  m Integer
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
          pure $ read $ Data.Text.unpack referralCode
        Nothing -> pure 0
    _ -> return 0
  where
    trimLeadingZeros :: Text -> Text
    trimLeadingZeros text =
      case Data.Text.uncons text of
        Just ('0', rest) -> trimLeadingZeros rest
        _ -> text
