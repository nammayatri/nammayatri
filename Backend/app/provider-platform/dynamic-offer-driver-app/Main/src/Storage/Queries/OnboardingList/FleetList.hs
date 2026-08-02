-- | The fleet owner onboarding grid query.
--
--   Joins person so the caller does not have to re-read the same rows by id, and orders by
--   createdAt with fleetOwnerPersonId as tiebreaker so paging is stable.
module Storage.Queries.OnboardingList.FleetList (findFleetOwners) where

import Data.Text (toLower)
import qualified Database.Beam as B
import qualified Domain.Types.DocsVerificationStatus as DDVS
import qualified Domain.Types.FleetOwnerInformation
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.FleetOwnerInformation as Beam
import qualified Storage.Beam.Person as BeamP
import Storage.Queries.OrphanInstances.FleetOwnerInformation ()
import Storage.Queries.OrphanInstances.Person ()

findFleetOwners ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r, EncFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  Maybe Domain.Types.FleetOwnerInformation.FleetType ->
  Maybe DDVS.DocsVerificationStatus ->
  Maybe UTCTime ->
  Maybe Text ->
  Maybe Bool ->
  Maybe UTCTime ->
  Maybe Int ->
  Maybe Int ->
  Maybe Bool ->
  Maybe (Maybe Bool) ->
  Maybe Bool ->
  m [(Domain.Types.FleetOwnerInformation.FleetOwnerInformation, DP.Person)]
findFleetOwners merchantOperatingCityId mbFleetType mbDocsVerificationStatus mbFromDate mbSearchString mbBlocked mbToDate mbLimit mbOffset mbVerified mbApprovalFilter mbEnabled = do
  searchHash <- mapM getDbHash mbSearchString
  dbConf <- getReplicaBeamConfig
  res <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.limit_ (fromIntegral $ fromMaybe 10 mbLimit) $
            B.offset_ (fromIntegral $ fromMaybe 0 mbOffset) $
              B.orderBy_ (\(fleetOwnerInfo, _) -> (B.desc_ fleetOwnerInfo.createdAt, B.asc_ fleetOwnerInfo.fleetOwnerPersonId)) $
                B.filter_'
                  ( \(fleetOwnerInfo, person) ->
                      fleetOwnerInfo.merchantOperatingCityId B.==?. B.val_ (Just $ getId merchantOperatingCityId)
                        B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\fleetType -> fleetOwnerInfo.fleetType B.==?. B.val_ fleetType) mbFleetType
                        B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\docsVerificationStatus -> fleetOwnerInfo.docsVerificationStatus B.==?. B.val_ (Just docsVerificationStatus)) mbDocsVerificationStatus
                        B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\enabled -> fleetOwnerInfo.enabled B.==?. B.val_ enabled) mbEnabled
                        B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\blocked -> fleetOwnerInfo.blocked B.==?. B.val_ blocked) mbBlocked
                        B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\verified -> fleetOwnerInfo.verified B.==?. B.val_ verified) mbVerified
                        B.&&?. case mbApprovalFilter of
                          Nothing -> B.sqlBool_ $ B.val_ True
                          Just Nothing -> B.sqlBool_ (B.isNothing_ fleetOwnerInfo.approved)
                          Just (Just approved) -> fleetOwnerInfo.approved B.==?. B.val_ (Just approved)
                        B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\fromDate -> B.sqlBool_ $ fleetOwnerInfo.createdAt B.>=. B.val_ fromDate) mbFromDate
                        B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\toDate -> B.sqlBool_ $ fleetOwnerInfo.createdAt B.<=. B.val_ toDate) mbToDate
                        B.&&?. maybe
                          (B.sqlBool_ $ B.val_ True)
                          ( \searchString ->
                              B.sqlBool_ (B.lower_ (B.coalesce_ [person.email] (B.val_ "")) `B.like_` B.val_ ("%" <> toLower searchString <> "%"))
                                B.||?. B.sqlBool_ (B.lower_ person.firstName `B.like_` B.val_ ("%" <> toLower searchString <> "%"))
                                B.||?. B.sqlBool_ (B.lower_ (B.coalesce_ [person.lastName] (B.val_ "")) `B.like_` B.val_ ("%" <> toLower searchString <> "%"))
                                B.||?. B.sqlBool_ (B.lower_ (B.coalesce_ [fleetOwnerInfo.fleetName] (B.val_ "")) `B.like_` B.val_ ("%" <> toLower searchString <> "%"))
                                B.||?. maybe
                                  (B.sqlBool_ $ B.val_ False)
                                  (\hashedPhone -> person.mobileNumberHash B.==?. B.val_ (Just hashedPhone))
                                  searchHash
                          )
                          mbSearchString
                  )
                  do
                    fleetOwnerInfo <- B.all_ (BeamCommon.fleetOwnerInformation BeamCommon.atlasDB)
                    person <- B.join_ (BeamCommon.person BeamCommon.atlasDB) (\person -> Beam.fleetOwnerPersonId fleetOwnerInfo B.==. BeamP.id person)
                    pure (fleetOwnerInfo, person)
  case res of
    -- The join already produced the Person; returning it avoids the caller re-reading the same
    -- rows by id.
    Right fleetOwnerInfoList ->
      fmap catMaybes $
        forM fleetOwnerInfoList $ \(beamFoi, beamPerson) -> do
          mbFoi <- fromTType' beamFoi
          mbPerson <- fromTType' beamPerson
          pure $ (,) <$> mbFoi <*> mbPerson
    Left _ -> pure []
