module Common.Resources.Constants where
import Engineering.Helpers.Commons (os)
import Prelude ((==))

zoomLevel :: Number
zoomLevel = if (os == "IOS") then 19.0 else 17.0

pickupZoomLevel :: Number
pickupZoomLevel = 17.0
