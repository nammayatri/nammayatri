{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.CommunicationEngine.Storage.Queries.CommunicationExtra where

import qualified Data.Aeson as Aeson
import Data.Either (fromRight)
import qualified Data.Text as T
import Data.Time (Day, UTCTime (..), addDays)
import qualified Database.Beam as B
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.CommunicationEngine.Domain.Types.Communication as Domain
import qualified Lib.CommunicationEngine.Storage.Beam.BeamFlow
import qualified Lib.CommunicationEngine.Storage.Beam.Common as CommDB
import qualified Lib.CommunicationEngine.Storage.Beam.Communication as Beam
import Lib.CommunicationEngine.Storage.Queries.OrphanInstances.Communication
import qualified Sequelize as Se

findBySenderIdWithLimitOffset ::
  (Lib.CommunicationEngine.Storage.Beam.BeamFlow.BeamFlow m r) =>
  Text ->
  Maybe Domain.CommunicationStatus ->
  Maybe Domain.CommunicationDomain ->
  Maybe Int ->
  Maybe Int ->
  m [Domain.Communication]
findBySenderIdWithLimitOffset senderId mbStatus mbDomain mbLimit mbOffset = do
  let limitVal = min 50 $ fromMaybe 10 mbLimit
      offsetVal = fromMaybe 0 mbOffset
  findAllWithOptionsKV
    [ Se.And $
        [Se.Is Beam.senderId $ Se.Eq senderId]
          <> maybe [] (\s -> [Se.Is Beam.status $ Se.Eq s]) mbStatus
          <> maybe [] (\d -> [Se.Is Beam.domain $ Se.Eq d]) mbDomain
    ]
    (Se.Desc Beam.createdAt)
    (Just limitVal)
    (Just offsetVal)

findBySenderIdWithFilters ::
  (Lib.CommunicationEngine.Storage.Beam.BeamFlow.BeamFlow m r) =>
  Text ->
  Maybe Domain.CommunicationStatus ->
  Maybe Domain.CommunicationDomain ->
  Maybe Text ->
  Maybe Int ->
  Maybe Int ->
  Maybe Domain.ChannelType ->
  Maybe Day ->
  Maybe Day ->
  m [Domain.Communication]
findBySenderIdWithFilters senderId mbStatus mbDomain mbSearchString mbLimit mbOffset mbChannel mbFromDate mbToDate = do
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
                      comm.senderId B.==?. B.val_ senderId
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
                        B.&&?. maybe
                          (B.sqlBool_ $ B.val_ True)
                          ( \ch ->
                              let channelStr = show ch
                                  sqlExpr = "COALESCE(channels::text, '[]') LIKE '%" <> "\"" <> channelStr <> "\"" <> "%'"
                               in B.sqlBool_ (B.customExpr_ sqlExpr)
                          )
                          mbChannel
                        B.&&?. maybe
                          (B.sqlBool_ $ B.val_ True)
                          (\fromDate -> B.sqlBool_ (comm.createdAt B.>=. B.val_ (UTCTime fromDate 0)))
                          mbFromDate
                        B.&&?. maybe
                          (B.sqlBool_ $ B.val_ True)
                          (\toDate -> B.sqlBool_ (comm.createdAt B.<. B.val_ (UTCTime (addDays 1 toDate) 0)))
                          mbToDate
                  )
                  $ B.all_ (CommDB.communication CommDB.communicationEngineDB)
  catMaybes <$> mapM fromTType' (fromRight [] res)

updateCommunication ::
  (Lib.CommunicationEngine.Storage.Beam.BeamFlow.BeamFlow m r) =>
  Kernel.Types.Id.Id Domain.Communication ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Domain.CommunicationContentType ->
  Maybe [Domain.ChannelType] ->
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

deleteById :: (Lib.CommunicationEngine.Storage.Beam.BeamFlow.BeamFlow m r) => Kernel.Types.Id.Id Domain.Communication -> m ()
deleteById commId = deleteWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId commId)]
