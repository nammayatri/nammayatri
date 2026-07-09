module Domain.Action.Dashboard.AppManagement.SeatLayout
  ( upsertSeatLayout,
    getSeatLayout,
    deleteSeatLayout,
    listSeatLayout,
  )
where

import qualified API.Types.Dashboard.AppManagement.SeatLayout as API
import Data.List (nub)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Domain.Types.Seat (Seat (..))
import qualified Domain.Types.Seat as DSeat
import Domain.Types.SeatLayout (SeatLayout (..))
import qualified Domain.Types.SeatLayout as DSeatLayout
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Id (Id, getId)
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Seat as CQSeat
import qualified Storage.Queries.Seat as QSeat
import qualified Storage.Queries.SeatLayout as QSeatLayout
import qualified Storage.Queries.VehicleSeatLayoutMapping as QVehicleSeatLayoutMapping
import Tools.Error

-- ====================================================================
-- Pure helpers
-- ====================================================================

inferSeatType :: Int -> Int -> [(Int, Int)] -> Maybe DSeat.SeatType
inferSeatType targetRow targetCol seatPositions = do
  cols <- NE.nonEmpty [col | (row, col) <- seatPositions, row == targetRow]
  let minCol = minimum cols
      maxCol = maximum cols
      hasLeft = (targetRow, targetCol - 1) `elem` seatPositions
      hasRight = (targetRow, targetCol + 1) `elem` seatPositions
  pure $
    if
        | targetCol == minCol || targetCol == maxCol -> DSeat.WINDOW
        | hasLeft && hasRight -> DSeat.MIDDLE
        | otherwise -> DSeat.AISLE

applyPatch :: UTCTime -> DSeat.Seat -> API.SeatDef -> DSeat.Seat
applyPatch now old u =
  old{seatLabel = fromMaybe old.seatLabel u.label,
      seatType = u.seatType <|> old.seatType,
      isBookable = fromMaybe old.isBookable u.isBookable,
      isLadiesOnly = u.isLadiesOnly <|> old.isLadiesOnly,
      directionDegrees = u.directionDegrees <|> old.directionDegrees,
      minStopsRequired = u.minStopsRequired <|> old.minStopsRequired,
      updatedAt = now
     }

-- A SeatDef whose create-required fields (row, col, label) have been extracted.
type ExtractedCreate = (Int, Int, Text, API.SeatDef)

extractCreate :: API.SeatDef -> Environment.Flow ExtractedCreate
extractCreate s = do
  row <- s.row & fromMaybeM (InvalidRequest "row is required on create")
  col <- s.col & fromMaybeM (InvalidRequest "col is required on create")
  label <- s.label & fromMaybeM (InvalidRequest "label is required on create")
  return (row, col, label, s)

extractUpdateId :: API.SeatDef -> Environment.Flow (Id DSeat.Seat, API.SeatDef)
extractUpdateId s = do
  i <- s.id & fromMaybeM (InvalidRequest "id is required on update")
  return (i, s)

buildSeat ::
  Id DSeatLayout.SeatLayout ->
  Id Domain.Types.Merchant.Merchant ->
  Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  [(Int, Int)] ->
  UTCTime ->
  ExtractedCreate ->
  DSeat.Seat
buildSeat layoutId merchantId merchantOperatingCityId allPositions now (row, col, label, s) =
  let inferredType = inferSeatType row col allPositions
      finalType = s.seatType <|> inferredType
   in DSeat.Seat
        { id = Kernel.Types.Id.Id $ getId layoutId <> "-" <> show row <> "-" <> show col,
          seatLabel = label,
          rowNo = row,
          colNo = col,
          seatType = finalType,
          isBookable = fromMaybe True s.isBookable,
          isLadiesOnly = s.isLadiesOnly,
          seatLayoutId = layoutId,
          merchantId = merchantId,
          merchantOperatingCityId = merchantOperatingCityId,
          directionDegrees = s.directionDegrees,
          minStopsRequired = s.minStopsRequired,
          createdAt = now,
          updatedAt = now
        }

-- All violations of a given kind are collected so the frontend can highlight
-- every offending seat at once instead of fixing one at a time.
findDuplicates :: Ord a => [a] -> [a]
findDuplicates xs =
  Map.keys $ Map.filter (> (1 :: Int)) $ Map.fromListWith (+) [(x, 1) | x <- xs]

validateMergedState ::
  DSeatLayout.SeatLayout ->
  [DSeat.Seat] ->
  Environment.Flow ()
validateMergedState layout seats = do
  when (layout.rows < 1 || layout.columns < 1) $ throwError InvalidSeatLayoutDimensions
  let positions = [(s.rowNo, s.colNo) | s <- seats]
      labels = [s.seatLabel | s <- seats]
      positionSet = Set.fromList positions
      outOfBounds =
        [ (s.rowNo, s.colNo)
          | s <- seats,
            s.rowNo < 1 || s.rowNo > layout.rows || s.colNo < 1 || s.colNo > layout.columns
        ]
      dupPositions = findDuplicates positions
      dupLabels = findDuplicates labels
      sleeperViolations =
        [ (s.rowNo, s.colNo)
          | s <- seats,
            s.seatType == Just DSeat.SLEEPER_LOWER || s.seatType == Just DSeat.SLEEPER_UPPER,
            s.rowNo + 1 > layout.rows || Set.member (s.rowNo + 1, s.colNo) positionSet
        ]
  unless (null outOfBounds) $ throwError (InvalidSeatPosition outOfBounds)
  unless (null dupPositions) $ throwError (DuplicateSeatPosition dupPositions)
  unless (null dupLabels) $ throwError (DuplicateSeatLabel dupLabels)
  unless (null sleeperViolations) $ throwError (InvalidSleeperSeatPosition sleeperViolations)

verifyOwnership ::
  DSeatLayout.SeatLayout ->
  Id Domain.Types.Merchant.Merchant ->
  Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  Environment.Flow ()
verifyOwnership layout merchantId merchantOperatingCityId = do
  when (layout.merchantId /= merchantId) $ throwError SeatLayoutNotFound
  when (layout.merchantOperatingCityId /= merchantOperatingCityId) $ throwError SeatLayoutNotFound

guardNotInUse :: Id DSeatLayout.SeatLayout -> Environment.Flow ()
guardNotInUse layoutId = do
  mappings <- QVehicleSeatLayoutMapping.findAllBySeatLayoutId (Just 1) (Just 0) layoutId
  when (not $ null mappings) $ throwError SeatLayoutInUseCannotModify

shrinks :: DSeatLayout.SeatLayout -> DSeatLayout.SeatLayout -> Bool
shrinks old new = new.rows < old.rows || new.columns < old.columns

-- ====================================================================
-- Per-bucket validation
-- ====================================================================

-- Single combined existence check so the frontend gets all stale ids
-- from update + delete buckets in one error, not bucket-by-bucket.
validateSeatIdsExist ::
  Map.Map (Id DSeat.Seat) DSeat.Seat ->
  [(Id DSeat.Seat, API.SeatDef)] ->
  [Id DSeat.Seat] ->
  Environment.Flow ()
validateSeatIdsExist oldById updates deleteIds = do
  let missingFromUpdates = [i | (i, _) <- updates, Map.notMember i oldById]
      missingFromDeletes = [i | i <- deleteIds, Map.notMember i oldById]
      missing = nub (missingFromUpdates ++ missingFromDeletes)
  unless (null missing) $ throwError (SeatNotFound (map getId missing))

validateUpdateBucket ::
  [Id DSeat.Seat] ->
  [(Id DSeat.Seat, API.SeatDef)] ->
  Environment.Flow ()
validateUpdateBucket deleteIds updates = do
  let updateIds = [i | (i, _) <- updates]
  when (length (nub updateIds) /= length updateIds) $
    throwError (InvalidRequest "duplicate update for same seat id")
  let deleteSet = Set.fromList deleteIds
  forM_ updates $ \(i, _) ->
    when (Set.member i deleteSet) $
      throwError (InvalidRequest "seat appears in both update and delete buckets")

-- ====================================================================
-- Create path
-- ====================================================================

doCreate ::
  Domain.Types.Merchant.Merchant ->
  Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  API.SeatLayoutUpsertReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
doCreate merchant merchantOperatingCity req = do
  when (not (null req.seatsToUpdate) || not (null req.seatsToDelete)) $
    throwError (InvalidRequest "update/delete buckets not allowed on create")
  name <- req.name & fromMaybeM (InvalidRequest "name is required on create")
  rows <- req.rows & fromMaybeM (InvalidRequest "rows is required on create")
  columns <- req.columns & fromMaybeM (InvalidRequest "columns is required on create")
  when (rows < 1 || columns < 1) $ throwError InvalidSeatLayoutDimensions
  mbExisting <- QSeatLayout.findById req.seatLayoutId
  when (isJust mbExisting) $ throwError SeatLayoutAlreadyExists
  now <- getCurrentTime
  let seatLayout =
        DSeatLayout.SeatLayout
          { id = req.seatLayoutId,
            name = name,
            rows = rows,
            columns = columns,
            merchantId = merchant.id,
            merchantOperatingCityId = merchantOperatingCity.id,
            createdAt = now,
            updatedAt = now
          }
  extractedCreates <- mapM extractCreate req.seatsToCreate
  let allPositions = [(row, col) | (row, col, _, _) <- extractedCreates]
  let seats = map (buildSeat seatLayout.id merchant.id merchantOperatingCity.id allPositions now) extractedCreates
  validateMergedState seatLayout seats
  result <- withTryCatch "upsertSeatLayout-create" $ do
    QSeatLayout.create seatLayout
    QSeat.createMany seats
    return Kernel.Types.APISuccess.Success
  case result of
    Left err -> do
      logError $ "SeatLayout create failed for " <> getId req.seatLayoutId <> ", cleaning up: " <> show err
      QSeat.deleteBySeatLayoutId req.seatLayoutId
      QSeatLayout.deleteById req.seatLayoutId
      CQSeat.invalidateCacheByLayoutId req.seatLayoutId
      throwError (InvalidRequest $ "Failed to create seat layout: " <> show err)
    Right res -> return res

-- ====================================================================
-- Update path (delta)
-- ====================================================================

doUpdate ::
  Domain.Types.Merchant.Merchant ->
  Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  DSeatLayout.SeatLayout ->
  API.SeatLayoutUpsertReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
doUpdate merchant merchantOperatingCity existingLayout req = do
  verifyOwnership existingLayout merchant.id merchantOperatingCity.id
  now <- getCurrentTime
  let allBucketsEmpty = null req.seatsToCreate && null req.seatsToUpdate && null req.seatsToDelete
      finalLayout =
        existingLayout
          { name = fromMaybe existingLayout.name req.name,
            rows = fromMaybe existingLayout.rows req.rows,
            columns = fromMaybe existingLayout.columns req.columns,
            updatedAt = now
          }
      shrinking = shrinks existingLayout finalLayout
  if allBucketsEmpty && not shrinking
    then do
      when (finalLayout.rows < 1 || finalLayout.columns < 1) $ throwError InvalidSeatLayoutDimensions
      QSeatLayout.updateByPrimaryKey finalLayout
      return Kernel.Types.APISuccess.Success
    else do
      oldSeats <- QSeat.findAllByLayoutId existingLayout.id
      let oldById = Map.fromList [(s.id, s) | s <- oldSeats]
      extractedCreates <- mapM extractCreate req.seatsToCreate
      extractedUpdates <- mapM extractUpdateId req.seatsToUpdate
      validateSeatIdsExist oldById extractedUpdates req.seatsToDelete
      validateUpdateBucket req.seatsToDelete extractedUpdates
      let patchedUpdates = map (\(i, s) -> applyPatch now (oldById Map.! i) s) extractedUpdates
          patchedIds = Set.fromList [i | (i, _) <- extractedUpdates]
          deletedIds = Set.fromList req.seatsToDelete
          createPositions = [(row, col) | (row, col, _, _) <- extractedCreates]
          keptOldPositions = [(s.rowNo, s.colNo) | s <- oldSeats, Set.notMember s.id deletedIds]
          allPositions = keptOldPositions ++ createPositions
          newSeats = map (buildSeat existingLayout.id merchant.id merchantOperatingCity.id allPositions now) extractedCreates
          keptOld =
            [ s
              | s <- oldSeats,
                Set.notMember s.id deletedIds,
                Set.notMember s.id patchedIds
            ]
          finalSeats = keptOld ++ patchedUpdates ++ newSeats
      validateMergedState finalLayout finalSeats
      when (not (null req.seatsToDelete) || shrinking) $
        guardNotInUse existingLayout.id
      result <- withTryCatch "upsertSeatLayout-update" $ do
        forM_ req.seatsToDelete CQSeat.invalidateCacheBySeatId
        forM_ patchedUpdates (CQSeat.invalidateCacheBySeatId . (.id))
        forM_ req.seatsToDelete QSeat.deleteById
        forM_ patchedUpdates QSeat.updateByPrimaryKey
        QSeat.createMany newSeats
        QSeatLayout.updateByPrimaryKey finalLayout
        CQSeat.invalidateCacheByLayoutId existingLayout.id
        return Kernel.Types.APISuccess.Success
      case result of
        Left err -> do
          logError $ "SeatLayout update failed for " <> getId existingLayout.id <> ", rolling back: " <> show err
          QSeat.deleteBySeatLayoutId existingLayout.id
          QSeatLayout.updateByPrimaryKey existingLayout
          QSeat.createMany oldSeats
          CQSeat.invalidateCacheByLayoutId existingLayout.id
          forM_ req.seatsToDelete CQSeat.invalidateCacheBySeatId
          forM_ patchedUpdates (CQSeat.invalidateCacheBySeatId . (.id))
          throwError (InvalidRequest $ "Failed to update seat layout: " <> show err)
        Right res -> return res

-- ====================================================================
-- API handlers
-- ====================================================================

upsertSeatLayout ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  API.SeatLayoutUpsertReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
upsertSeatLayout merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOperatingCity <-
    CQMOC.findByMerchantShortIdAndCity merchantShortId opCity
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  mbExisting <- QSeatLayout.findById req.seatLayoutId
  case mbExisting of
    Nothing -> doCreate merchant merchantOperatingCity req
    Just existingLayout -> doUpdate merchant merchantOperatingCity existingLayout req

getSeatLayout ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id DSeatLayout.SeatLayout ->
  Environment.Flow API.SeatLayoutDetailResp
getSeatLayout merchantShortId opCity seatLayoutId = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOperatingCity <-
    CQMOC.findByMerchantShortIdAndCity merchantShortId opCity
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  seatLayout <- QSeatLayout.findById seatLayoutId >>= fromMaybeM SeatLayoutNotFound
  verifyOwnership seatLayout merchant.id merchantOperatingCity.id
  seats <- QSeat.findAllByLayoutId seatLayoutId
  return $
    API.SeatLayoutDetailResp
      { id = seatLayout.id,
        name = seatLayout.name,
        rows = seatLayout.rows,
        columns = seatLayout.columns,
        seats = seats
      }

deleteSeatLayout ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id DSeatLayout.SeatLayout ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
deleteSeatLayout merchantShortId opCity seatLayoutId = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOperatingCity <-
    CQMOC.findByMerchantShortIdAndCity merchantShortId opCity
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  seatLayout <- QSeatLayout.findById seatLayoutId >>= fromMaybeM SeatLayoutNotFound
  verifyOwnership seatLayout merchant.id merchantOperatingCity.id
  guardNotInUse seatLayoutId
  QSeat.deleteBySeatLayoutId seatLayoutId
  QSeatLayout.deleteById seatLayoutId
  CQSeat.invalidateCacheByLayoutId seatLayoutId
  return Kernel.Types.APISuccess.Success

listSeatLayout ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Maybe Int ->
  Maybe Int ->
  Environment.Flow [DSeatLayout.SeatLayout]
listSeatLayout merchantShortId opCity limit offset = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOperatingCity <-
    CQMOC.findByMerchantShortIdAndCity merchantShortId opCity
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  QSeatLayout.findAllByMerchantIdAndMerchantOperatingCityId limit offset merchant.id merchantOperatingCity.id
