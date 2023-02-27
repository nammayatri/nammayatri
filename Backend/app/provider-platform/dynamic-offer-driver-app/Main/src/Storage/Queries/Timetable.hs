{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.Queries.Timetable where

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import Database.Esqueleto.Internal.Internal as E
import Database.Persist
import Database.Persist.SqlBackend
import Domain.Types.Timetable (Timetable)
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Storage.Tabular.Timetable

insertTimetables :: [Timetable] -> SqlDB ()
insertTimetables timetables = do
  SqlDB $
    lift $
      insertManyOnConflict
        (fmap toTType timetables)
        (UniqueBookingPickupDate undefined undefined)
        DoNothing

insertManyOnConflict ::
  forall a backend m.
  ( PersistEntity a,
    MonadIO m,
    BackendCompatible SqlBackend backend
  ) =>
  [a] ->
  Unique a ->
  OnConflict a ->
  ReaderT backend m ()
insertManyOnConflict values conflictKey onConflict = do
  let insertionProxy = undefined :: SqlExpr (Insertion a)
  sqlBackend <- asks projectBackend
  let info = (sqlBackend, initialIdentState)

      insertIntoFragment = sqlInsertInto info insertionProxy

      (valuesBuilders, valuesValues) = unzip $ fmap mkSqlEntityFragment values

      valuesFragment =
        ( "VALUES " <> uncommas valuesBuilders <> "\n",
          concat valuesValues
        )

      uniqueFieldName =
        uncommas $
          toList $
            TLB.fromText . flip getEscapedRawName sqlBackend . unFieldNameDB . snd
              <$> persistUniqueToFieldNames conflictKey

      onConflictFragment =
        ("ON CONFLICT (" <> uniqueFieldName <> ") ", []) <> renderOnConflict onConflict

      (queryBuilder, queryValues) =
        mconcat [insertIntoFragment, valuesFragment, onConflictFragment]
  rawExecute (TL.toStrict $ TLB.toLazyText queryBuilder) queryValues

mkSqlEntityFragment :: (PersistEntity a) => a -> (TLB.Builder, [PersistValue])
mkSqlEntityFragment entityVal =
  let vals = fmap toPersistValue $ toPersistFields entityVal
      builder = parens $ uncommas $ replicate (length vals) "?"
   in (builder, vals)

renderOnConflict :: OnConflict a -> (TLB.Builder, [PersistValue])
renderOnConflict onConflict =
  case onConflict of
    DoNothing -> ("DO NOTHING", [])

data OnConflict a
  = DoNothing
