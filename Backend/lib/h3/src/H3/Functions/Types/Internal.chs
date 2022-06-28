{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

{-# OPTIONS_GHC -Wno-all #-}
{-# OPTIONS_GHC -Wno-compat #-}
{-# OPTIONS_GHC -Wno-identities #-}
module H3.Functions.Types.Internal where

import Foreign
import GHC.Generics hiding (from, to)
import Control.Monad
import Control.Monad.IO.Class
import Prelude hiding (last)

#include <h3/h3api.h>

{#context lib="h3api"#}

type Radians = Double

data H3LatLng = H3LatLng --LatLng
  { lat :: Radians,
    lng :: Radians
  }
  deriving stock (Generic, Show)

{#pointer *LatLng as H3LatLngPtr -> H3LatLng #}

instance Storable H3LatLng where
  sizeOf _ = {#sizeof LatLng #}
  alignment _ = {#alignof LatLng#}
  peek p = H3LatLng
    <$> liftM realToFrac ({#get LatLng->lat #} p)
    <*> liftM realToFrac ({#get LatLng->lng #} p)
  poke p x = do
    {#set LatLng.lat #} p (realToFrac $ lat x)
    {#set LatLng.lng #} p (realToFrac $ lng x)

----------------------------------------------------------------------------------------------------------------------------------------------------

{#enum H3ErrorCodes as H3ResultCode {underscoreToCase}
    deriving (Show, Eq, Ord)#}

----------------------------------------------------------------------------------------------------------------------------------------------------

newtype H3Cell = H3Cell Word64  --H3Index
  deriving stock (Generic)
  deriving newtype (Show, Integral, Real, Num, Ord, Eq, Enum, Storable)

----------------------------------------------------------------------------------------------------------------------------------------------------

newtype H3Edge = H3Edge Word64  --H3Index
  deriving stock (Generic)
  deriving newtype (Show, Integral, Real, Num, Ord, Eq, Enum, Storable)

----------------------------------------------------------------------------------------------------------------------------------------------------

newtype H3Vertex = H3Vertex Word64  --H3Index
  deriving stock (Generic)
  deriving newtype (Show, Integral, Real, Num, Ord, Eq, Enum, Storable)

----------------------------------------------------------------------------------------------------------------------------------------------------

data H3CellBoundaryC = H3CellBoundaryC
  { numVerts :: Int,
    verts :: H3LatLngPtr
  }
  deriving (Generic)

{#pointer *CellBoundary as H3CellBoundaryCPtr -> H3CellBoundaryC #}

instance Storable H3CellBoundaryC where
  sizeOf _ = {#sizeof CellBoundary #}
  alignment _ = {#alignof CellBoundary#}
  peek p = H3CellBoundaryC
    <$> liftM fromIntegral ({#get CellBoundary->numVerts #} p)
    <*> {#get CellBoundary->verts #} p
  poke p x = do
    {#set CellBoundary.numVerts #} p (fromIntegral $ numVerts (x :: H3CellBoundaryC))
    {#set CellBoundary.verts #} p (verts (x :: H3CellBoundaryC))

data H3CellBoundary = H3CellBoundary
  { numVerts :: Int,
    verts :: [H3LatLng]
  }
  deriving (Generic, Show)

fromH3CellBoundaryC :: H3CellBoundaryCPtr -> IO H3CellBoundary
fromH3CellBoundaryC cellPtr = do
  H3CellBoundaryC {..} <- liftIO $ peek cellPtr
  v <- liftIO $ peekArray numVerts verts
  return $
    H3CellBoundary
      { numVerts = numVerts,
        verts = v
      }

----------------------------------------------------------------------------------------------------------------------------------------------------

data H3CoordIJ = H3CoordIJ -- CoordIJ
  { i :: Int,
    j :: Int
  }
  deriving stock (Generic, Show)

{#pointer *CoordIJ as H3CoordIJPtr -> H3CoordIJ #}

instance Storable H3CoordIJ where
  sizeOf _ = {#sizeof CoordIJ #}
  alignment _ = {#alignof CoordIJ#}
  peek p = H3CoordIJ
    <$> liftM fromIntegral ({#get CoordIJ->i #} p)
    <*> liftM fromIntegral ({#get CoordIJ->j #} p)
  poke p x = do
    {#set CoordIJ.i #} p (fromIntegral $ i x)
    {#set CoordIJ.j #} p (fromIntegral $ j x)

----------------------------------------------------------------------------------------------------------------------------------------------------

data H3GeoLoopC = H3GeoLoopC -- GeoLoop
  { numVerts :: Int,
    verts :: H3LatLngPtr
  }
  deriving (Generic)

{#pointer *GeoLoop as H3GeoLoopCPtr -> H3GeoLoopC #}

instance Storable H3GeoLoopC where
  sizeOf _ = {#sizeof GeoLoop #}
  alignment _ = {#alignof GeoLoop#}
  peek p = H3GeoLoopC
    <$> liftM fromIntegral ({#get GeoLoop->numVerts #} p)
    <*> {#get GeoLoop->verts #} p
  poke p x = do
    {#set GeoLoop.numVerts #} p (fromIntegral $ numVerts (x :: H3GeoLoopC))
    {#set GeoLoop.verts #} p (verts (x :: H3GeoLoopC))

data H3GeoLoop = H3GeoLoop
  { numVerts :: Int,
    verts :: [H3LatLng]
  }
  deriving (Generic, Show)


fromH3GeoLoopCPtr :: H3GeoLoopCPtr -> IO H3GeoLoop
fromH3GeoLoopCPtr cellPtr = do
  loop <- liftIO $ peek cellPtr
  fromH3GeoLoopC loop

fromH3GeoLoopC :: H3GeoLoopC -> IO H3GeoLoop
fromH3GeoLoopC H3GeoLoopC {..} = do
  v <- liftIO $ peekArray numVerts verts
  return $
    H3GeoLoop
      { numVerts = numVerts,
        verts = v
      }

toH3GeoLoopC :: H3GeoLoop -> (H3GeoLoopC -> IO b) -> IO b
toH3GeoLoopC H3GeoLoop {..} f = do
  liftIO $ allocaArray numVerts $ \vertsArray -> do
    let geoLoop = H3GeoLoopC {
              numVerts = numVerts,
              verts = vertsArray
            }
    f geoLoop

toH3GeoLoopCPtr :: H3GeoLoop -> (H3GeoLoopCPtr -> IO b) -> IO b
toH3GeoLoopCPtr geoloop f = do
  toH3GeoLoopC geoloop $ \geoloopC ->
    with geoloopC f

----------------------------------------------------------------------------------------------------------------------------------------------------

data H3GeoPolygonC = H3GeoPolygonC -- GeoPolygon
  { geoloop :: H3GeoLoopC,
    numHoles :: Int,
    holes :: H3GeoLoopCPtr
  }
  deriving stock (Generic)

{#pointer *GeoPolygon as H3GeoPolygonCPtr -> H3GeoPolygonC #}

instance Storable H3GeoPolygonC where
  sizeOf _ = {#sizeof GeoPolygon #}
  alignment _ = {#alignof GeoPolygon#}
  peek p = H3GeoPolygonC
    <$> peekByteOff p 0
    <*> liftM fromIntegral ({#get GeoPolygon->numHoles #} p)
    <*> {#get GeoPolygon->holes #} p
  poke p x = do
    pokeByteOff p 0 (geoloop (x :: H3GeoPolygonC))
    {#set GeoPolygon.numHoles #} p (fromIntegral $ numHoles (x :: H3GeoPolygonC))
    {#set GeoPolygon.holes #} p (holes (x :: H3GeoPolygonC))

data H3GeoPolygon = H3GeoPolygon
  { geoloop :: H3GeoLoop,
    numHoles :: Int,
    holes :: [H3GeoLoop]
  }
  deriving (Generic, Show)

fromH3GeoPolygonCPtr :: H3GeoPolygonCPtr -> IO H3GeoPolygon
fromH3GeoPolygonCPtr polyPtr = do
  poly <- liftIO $ peek polyPtr
  g <- fromH3GeoLoopC (geoloop (poly :: H3GeoPolygonC))
  h <- liftIO $ peekArray (numHoles (poly :: H3GeoPolygonC)) (holes (poly :: H3GeoPolygonC))
  h' <- fromH3GeoLoopC `mapM` h
  return $
    H3GeoPolygon
      { geoloop = g,
        numHoles = numHoles (poly :: H3GeoPolygonC),
        holes = h'
      }

toH3GeoPolygonCPtr :: H3GeoPolygon -> (H3GeoPolygonCPtr -> IO b) -> IO b
toH3GeoPolygonCPtr H3GeoPolygon {..} f =
  toH3GeoLoopC geoloop $ \g ->
    liftIO $ allocaArray numHoles $ \holesArray ->
      withList toH3GeoLoopC holes $ \holesC -> do
        pokeArray holesArray holesC
        let geoPolyC = H3GeoPolygonC {
                  geoloop = g,
                  numHoles = numHoles,
                  holes = holesArray
                }
        with geoPolyC f

----------------------------------------------------------------------------------------------------------------------------------------------------

data H3LinkedLatLngC = H3LinkedLatLngC -- LinkedLatLng
  { vertex :: H3LatLng,
    next :: H3LinkedLatLngCPtr
  }
  deriving stock (Generic)

{#pointer *LinkedLatLng as H3LinkedLatLngCPtr -> H3LinkedLatLngC #}

instance Storable H3LinkedLatLngC where
  sizeOf _ = {#sizeof LinkedLatLng #}
  alignment _ = {#alignof LinkedLatLng#}
  peek p = H3LinkedLatLngC
    <$> peekByteOff p 0
    <*> {#get LinkedLatLng->next #} p
  poke p x = do
    pokeByteOff p 0 (vertex (x :: H3LinkedLatLngC))
    {#set LinkedLatLng.next #} p (next (x :: H3LinkedLatLngC))

data H3LinkedLatLng = H3LinkedLatLng
  { vertex :: H3LatLng,
    next :: Maybe H3LinkedLatLng,
    -- Pointers
    nextPtr :: H3LinkedLatLngCPtr
  }
  deriving stock (Generic, Show)

fromH3LinkedLatLngCPtr :: H3LinkedLatLngCPtr -> IO H3LinkedLatLng
fromH3LinkedLatLngCPtr linkedLatLngPtr = do
  linkedLatLng <- liftIO $ peek linkedLatLngPtr
  let nextPtr = next (linkedLatLng :: H3LinkedLatLngC)
  mbNext <- if nextPtr == nullPtr
    then return Nothing
    else do
      Just <$> (fromH3LinkedLatLngCPtr $ next (linkedLatLng :: H3LinkedLatLngC))
  return $
    H3LinkedLatLng
      { vertex = vertex (linkedLatLng :: H3LinkedLatLngC),
        next = mbNext,
        -- Pointers
        nextPtr = nextPtr
      }

toH3LinkedLatLngCPtr :: H3LinkedLatLng -> (H3LinkedLatLngCPtr -> IO b) -> IO b
toH3LinkedLatLngCPtr H3LinkedLatLng {..} f = do
  let linkedLatLngC = H3LinkedLatLngC {
            vertex = vertex,
            next = nextPtr
          }
  with linkedLatLngC f

----------------------------------------------------------------------------------------------------------------------------------------------------

data H3LinkedGeoLoopC = H3LinkedGeoLoopC -- LinkedGeoLoop
  { first :: H3LinkedLatLngCPtr,
    last :: H3LinkedLatLngCPtr,
    next :: H3LinkedGeoLoopCPtr
  }
  deriving stock (Generic)

{#pointer *LinkedGeoLoop as H3LinkedGeoLoopCPtr -> H3LinkedGeoLoopC #}

instance Storable H3LinkedGeoLoopC where
  sizeOf _ = {#sizeof LinkedGeoLoop #}
  alignment _ = {#alignof LinkedGeoLoop#}
  peek p = H3LinkedGeoLoopC
    <$> {#get LinkedGeoLoop->first #} p
    <*> {#get LinkedGeoLoop->last #} p
    <*> {#get LinkedGeoLoop->next #} p
  poke p x = do
    {#set LinkedGeoLoop.first #} p (first (x :: H3LinkedGeoLoopC))
    {#set LinkedGeoLoop.last #} p (last (x :: H3LinkedGeoLoopC))
    {#set LinkedGeoLoop.next #} p (next (x :: H3LinkedGeoLoopC))

data H3LinkedGeoLoop = H3LinkedGeoLoop
  { first :: H3LinkedLatLng,
    last :: H3LinkedLatLng,
    next :: (Maybe H3LinkedGeoLoop),
    -- Pointers
    firstPtr :: H3LinkedLatLngCPtr,
    lastPtr :: H3LinkedLatLngCPtr,
    nextPtr :: H3LinkedGeoLoopCPtr
  }
  deriving stock (Generic, Show)

fromH3LinkedGeoLoopCPtr :: H3LinkedGeoLoopCPtr -> IO H3LinkedGeoLoop
fromH3LinkedGeoLoopCPtr linkedGeoLoopPtr = do
  linkedGeoLoop <- liftIO $ peek linkedGeoLoopPtr
  let firstPtr = first (linkedGeoLoop :: H3LinkedGeoLoopC)
      lastPtr = last (linkedGeoLoop :: H3LinkedGeoLoopC)
      nextPtr = next (linkedGeoLoop :: H3LinkedGeoLoopC)
  f <- fromH3LinkedLatLngCPtr firstPtr
  l <- fromH3LinkedLatLngCPtr lastPtr
  mbNext <- if  nextPtr == nullPtr
    then return Nothing
    else do
      Just <$> (fromH3LinkedGeoLoopCPtr nextPtr)
  return $
    H3LinkedGeoLoop
      { first = f,
        last = l,
        next = mbNext,
        -- Pointers
        firstPtr = firstPtr,
        lastPtr = lastPtr,
        nextPtr = nextPtr
      }

toH3LinkedGeoLoopCPtr :: H3LinkedGeoLoop -> (H3LinkedGeoLoopCPtr -> IO b) -> IO b
toH3LinkedGeoLoopCPtr H3LinkedGeoLoop {..} f = do
  let linkedGeoLoopC = H3LinkedGeoLoopC {
            first = firstPtr,
            last = lastPtr,
            next = nextPtr
          }
  with linkedGeoLoopC f

----------------------------------------------------------------------------------------------------------------------------------------------------

data H3LinkedGeoPolygonC = H3LinkedGeoPolygonC -- LinkedGeoPolygon
  { first :: H3LinkedGeoLoopCPtr,
    last :: H3LinkedGeoLoopCPtr,
    next :: H3LinkedGeoPolygonCPtr
  }
  deriving stock (Generic)

{#pointer *LinkedGeoPolygon as H3LinkedGeoPolygonCPtr -> H3LinkedGeoPolygonC #}

instance Storable H3LinkedGeoPolygonC where
  sizeOf _ = {#sizeof LinkedGeoPolygon #}
  alignment _ = {#alignof LinkedGeoPolygon#}
  peek p = H3LinkedGeoPolygonC
    <$> {#get LinkedGeoPolygon->first #} p
    <*> {#get LinkedGeoPolygon->last #} p
    <*> {#get LinkedGeoPolygon->next #} p
  poke p x = do
    {#set LinkedGeoPolygon.first #} p (first (x :: H3LinkedGeoPolygonC))
    {#set LinkedGeoPolygon.last #} p (last (x :: H3LinkedGeoPolygonC))
    {#set LinkedGeoPolygon.next #} p (next (x :: H3LinkedGeoPolygonC))

data H3LinkedGeoPolygon = H3LinkedGeoPolygon
  { first :: H3LinkedGeoLoop,
    last :: H3LinkedGeoLoop,
    next :: Maybe H3LinkedGeoPolygon,
    -- Pointers
    firstPtr :: H3LinkedGeoLoopCPtr,
    lastPtr :: H3LinkedGeoLoopCPtr,
    nextPtr :: H3LinkedGeoPolygonCPtr
  }
  deriving stock (Generic, Show)

fromH3LinkedGeoPolygonCPtr :: H3LinkedGeoPolygonCPtr -> IO H3LinkedGeoPolygon
fromH3LinkedGeoPolygonCPtr linkedPolyPtr = do
  linkedPoly <- liftIO $ peek linkedPolyPtr
  let firstPtr = first (linkedPoly :: H3LinkedGeoPolygonC)
      lastPtr = last (linkedPoly :: H3LinkedGeoPolygonC)
      nextPtr = next (linkedPoly :: H3LinkedGeoPolygonC)
  f <- fromH3LinkedGeoLoopCPtr firstPtr
  l <- fromH3LinkedGeoLoopCPtr lastPtr
  mbNext <- if nextPtr == nullPtr
    then return Nothing
    else do
      Just <$> (fromH3LinkedGeoPolygonCPtr nextPtr)
  return $
    H3LinkedGeoPolygon
      { first = f,
        last = l,
        next = mbNext,
        -- Pointers
        firstPtr = firstPtr,
        lastPtr = lastPtr,
        nextPtr = nextPtr
      }

toH3LinkedGeoPolygonCPtr :: H3LinkedGeoPolygon -> (H3LinkedGeoPolygonCPtr -> IO b) -> IO b
toH3LinkedGeoPolygonCPtr H3LinkedGeoPolygon {..} f = do
  let linkedGeoPolygonC = H3LinkedGeoPolygonC {
            first = firstPtr,
            last = lastPtr,
            next = nextPtr
          }
  with linkedGeoPolygonC f

----------------------------------------------------------------------------------------------------------------------------------------------------

withList :: (a -> (b -> IO c) -> IO c) -> [a] -> ([b] -> IO c) -> IO c
withList converter mas f = withList' mas []
  where
    withList' (el : mas') masC = do
      converter el $ \elemC ->
        withList' mas' (elemC : masC)
    withList' [] masC = do
      f (reverse masC)