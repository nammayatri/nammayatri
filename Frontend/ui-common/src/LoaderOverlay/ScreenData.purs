module LoaderOverlay.ScreenData where

initData :: LoaderOverlayState
initData = {
    data : {
        title : ""
    ,   subTitle : ""
    }
}

type LoaderOverlayState
  = { data :: LoaderStateData
    }

type LoaderStateData
  = { title :: String
    , subTitle :: String
    }