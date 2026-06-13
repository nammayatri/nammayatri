module Main (main) where

import BookingConfirm (bookingConfirmTests)
import Kernel.Prelude
import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "Rider App Tests" [bookingConfirmTests]
