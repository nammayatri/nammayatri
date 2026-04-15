{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.CommunicationExtra where

import qualified Data.Aeson as Aeson
import Data.Either (fromRight)
import qualified Data.Text as T
import qualified Database.Beam as B
import qualified Domain.Types.Communication
import qualified Domain.Types.Person
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.Communication as Beam
import Storage.Queries.OrphanInstances.Communication

findBySenderIdWithLimitOffset ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id Domain.Types.Person.Person ->
  Maybe Domain.Types.Communication.CommunicationStatus ->
  Maybe Domain.Types.Communication.CommunicationDomain ->
  Maybe Int ->
  Maybe Int ->
  m [Domain.Types.Communication.Communication]
findBySenderIdWithLimitOffset senderId mbStatus mbDomain mbLimit mbOffset = do
  let limitVal = min 50 $ fromMaybe 10 mbLimit
      offsetVal = fromMaybe 0 mbOffset
  findAllWithOptionsKV
    [ Se.And $
        [Se.Is Beam.senderId $ Se.Eq (Kernel.Types.Id.getId senderId)]
          <> maybe [] (\s -> [Se.Is Beam.status $ Se.Eq s]) mbStatus
          <> maybe [] (\d -> [Se.Is Beam.domain $ Se.Eq d]) mbDomain
    ]
    (Se.Desc Beam.createdAt)
    (Just limitVal)
    (Just offsetVal)

findBySenderIdWithSearchAndLimitOffset ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id Domain.Types.Person.Person ->
  Maybe Domain.Types.Communication.CommunicationStatus ->
  Maybe Domain.Types.Communication.CommunicationDomain ->
  Maybe Text ->
  Maybe Int ->
  Maybe Int ->
  m [Domain.Types.Communication.Communication]
findBySenderIdWithSearchAndLimitOffset senderId mbStatus mbDomain mbSearchString mbLimit mbOffset = do
  let limitVal = fromIntegral $ min 50 $ fromMaybe 10 mbLimit
      offsetVal = fromIntegral $ fromMaybe 0 mbOffset
  dbConf <- getReplicaBeamConfig
  res <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.limit_ limitVal $
            B.offset_ offsetVal $
              B.orderBy_ (\comm -> B.desc_ comm.createdAt) $
                B.filter_'
                  ( \comm ->
                      comm.senderId B.==?. B.val_ (Kernel.Types.Id.getId senderId)
                        B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\s -> comm.status B.==?. B.val_ s) mbStatus
                        B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\d -> comm.domain B.==?. B.val_ d) mbDomain
                        B.&&?. maybe
                          (B.sqlBool_ $ B.val_ True)
                          ( \searchStr ->
                              let q = B.val_ ("%" <> T.toLower searchStr <> "%")
                               in B.sqlBool_ (B.like_ (B.lower_ comm.title) q)
                                    B.||?. B.sqlBool_ (B.like_ (B.lower_ comm.body) q)
                                    B.||?. B.sqlBool_ (B.like_ (B.lower_ (B.coalesce_ [comm.senderDisplayName] (B.val_ ""))) q)
                          )
                          mbSearchString
                  )
                  $ B.all_ (BeamCommon.communication BeamCommon.atlasDB)
  catMaybes <$> mapM fromTType' (fromRight [] res)

updateCommunication ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id Domain.Types.Communication.Communication ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Domain.Types.Communication.CommunicationContentType ->
  Maybe [Domain.Types.Communication.ChannelType] ->
  Maybe Aeson.Value ->
  m ()
updateCommunication commId mbTitle mbBody mbHtmlBody mbContentType mbChannels mbMediaUrls = do
  now <- getCurrentTime
  updateOneWithKV
    ( [Se.Set Beam.updatedAt now]
        <> maybe [] (\t -> [Se.Set Beam.title t]) mbTitle
        <> maybe [] (\b -> [Se.Set Beam.body b]) mbBody
        <> maybe [] (\h -> [Se.Set Beam.htmlBody (Just h)]) mbHtmlBody
        <> maybe [] (\c -> [Se.Set Beam.contentType c]) mbContentType
        <> maybe [] (\ch -> [Se.Set Beam.channels (Just $ Aeson.toJSON ch)]) mbChannels
        <> maybe [] (\m -> [Se.Set Beam.mediaUrls (Just m)]) mbMediaUrls
    )
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId commId)]

deleteById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Kernel.Types.Id.Id Domain.Types.Communication.Communication -> m ()
deleteById commId = deleteWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId commId)]
