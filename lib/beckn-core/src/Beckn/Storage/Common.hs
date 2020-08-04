{-# LANGUAGE NamedWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Beckn.Storage.Common where

import qualified Database.Beam as B
import Database.Beam.Postgres

insertExpression ::
  _ =>
  table B.Identity ->
  B.SqlInsertValues Postgres (table (B.QExpr Postgres s))
insertExpression value = B.insertValues [value]
