{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.SeatManagement where

import qualified Data.Time.Calendar as Data.Time.Calendar
import qualified Domain.Types.ServiceCategory as Domain.Types.ServiceCategory
import Kernel.Prelude
import qualified Kernel.Types.Id as Kernel.Types.Id

data SeatManagement = SeatManagement
  { blocked :: Kernel.Prelude.Int,
    booked :: Kernel.Prelude.Int,
    date :: Data.Time.Calendar.Day,
    id :: Kernel.Types.Id.Id Domain.Types.SeatManagement.SeatManagement,
    ticketServiceCategoryId :: Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory
  }
  deriving (Generic, Show)
