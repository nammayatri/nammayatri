module Beckn.Product.PassApplication.Update where

import qualified Beckn.Data.Accessor                   as Accessor
import qualified Beckn.Storage.Queries.Pass            as Pass
import qualified Beckn.Storage.Queries.PassApplication as DB
import           Beckn.Types.API.PassApplication
import           Beckn.Types.App
import           Beckn.Types.Common
import qualified Beckn.Types.Common                    as Location (Location (..),
                                                                    LocationType)
import qualified Beckn.Types.Storage.Pass              as Pass
import           Beckn.Types.Storage.PassApplication
import qualified Beckn.Types.Storage.PassApplication   as PassApplication
import qualified Beckn.Types.Storage.RegistrationToken as RegistrationToken
import           Beckn.Utils.Common
import           Beckn.Utils.Routes
import           Beckn.Utils.Storage
import           Data.Aeson
import qualified EulerHS.Language                      as L
import           EulerHS.Prelude
import qualified EulerHS.Types                         as T
import           Servant

updatePassApplication ::
  Maybe Text ->
  PassApplicationId ->
  UpdatePassApplicationReq ->
  FlowHandler PassApplicationRes
updatePassApplication regToken passApplicationId UpdatePassApplicationReq{..} = withFlowHandler $ do
  verifyToken regToken
  pA <- ifNotFoundDbErr "Pass Application not found" =<< DB.findById passApplicationId
  verifyIfStatusUpdatable (PassApplication._status pA) _status
  approvedCount <- if (_status == REVOKED)
                    then Pass.revokeByPassApplicationId passApplicationId *> pure 0
                    else pure $ fromMaybe (PassApplication._count pA) _approvedCount
  eres <- DB.update passApplicationId _status approvedCount _remarks
  case eres of
    Left err -> L.throwException $ err500 {errBody = ("DBError: " <> show err)}
    Right _ -> do
      pA' <- ifNotFoundDbErr "Pass Application not found" =<< DB.findById passApplicationId
      createPassesOnApproval pA' approvedCount
      return $ PassApplicationRes pA'

verifyIfStatusUpdatable :: Status -> Status -> L.Flow ()
verifyIfStatusUpdatable currStatus newStatus =
  case (currStatus, newStatus) of
    (PENDING, APPROVED) -> return ()
    (PENDING, REJECTED) -> return ()
    (PENDING, EXPIRED) -> return ()
    (APPROVED, REJECTED) -> return ()
    (APPROVED, EXPIRED) -> return ()
    (APPROVED, REVOKED) -> return ()
    _ -> L.throwException $ err400 {errBody = "Invalid status update"}

ifNotFoundDbErr :: Text -> T.DBResult (Maybe a) -> L.Flow a
ifNotFoundDbErr errMsg dbres =
  case dbres of
    Left err -> L.throwException $ err500 {errBody = ("DBError: " <> show err)}
    Right Nothing -> L.throwException $ err400 {errBody = show errMsg}
    Right (Just v) -> return v

createPassesOnApproval :: PassApplication -> Int -> L.Flow ()
createPassesOnApproval pa@PassApplication {..} approvedCount =
  if _status /= APPROVED
    then return ()
    else void $ replicateM approvedCount (createPass pa)

createPass :: PassApplication -> L.Flow ()
createPass PassApplication{..} = do
  id <- generateGUID
  currTime <- getCurrTime
  let pass = Pass.Pass
              { _id = id
              , _ShortId = ""
              , _status = Pass.ACTIVE
              , _PassApplicationId = _id
              , _createdAt = currTime
              , _updatedAt = currTime
              , ..
              }
  Pass.create pass
