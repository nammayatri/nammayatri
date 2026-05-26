{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.CommunicationExtra where

import Data.Either (fromRight)
import qualified Data.Text as T
import qualified Database.Beam as B
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Lib.Communication.Domain.Types.Communication
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Storage.Beam.Common as BeamCommon
import qualified Lib.Communication.Storage.Beam.Communication as Beam
import Lib.Communication.Storage.Queries.OrphanInstances.Communication ()

findBySenderIdWithSearchAndLimitOffset ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Prelude.Text ->
  Maybe Lib.Communication.Domain.Types.Communication.CommunicationStatus ->
  Maybe Lib.Communication.Domain.Types.Communication.CommunicationDomain ->
  Maybe Text ->
  Maybe Int ->
  Maybe Int ->
  m [Lib.Communication.Domain.Types.Communication.Communication]
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
                  )
                  $ B.all_ (BeamCommon.communication BeamCommon.atlasDB)
  catMaybes <$> mapM fromTType' (fromRight [] res)
