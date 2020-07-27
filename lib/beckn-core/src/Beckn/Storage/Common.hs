module Beckn.Storage.Common where

import qualified Database.Beam as B

insertExpression values = B.insertValues [values]
