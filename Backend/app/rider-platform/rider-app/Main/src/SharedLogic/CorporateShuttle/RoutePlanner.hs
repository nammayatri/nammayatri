-- | Corporate shuttle planning, the pure part: where employees are picked up, and how the pickup points are driven.
--
--   pickup points  DBSCAN splits the employees' homes into neighbourhoods (two homes in one when they could share a point
--                  between them); in each, the points are chosen from the road points near the homes (never the homes or
--                  a cluster centre: the caller gives metro stations, bus stands and main roads): a greedy cover, the
--                  point the most still-unplaced riders can walk to first (stations and stands weigh more than main
--                  roads), then fewer points (a point whose riders can all walk to another one is
--                  closed), then each point moved to the best road everyone on it can still walk to. Everyone walks at
--                  most walkMeters; a home no road point is near is left out, for a stop of its own
--   routes         Clarke-Wright savings join the pickup points into routes, the same in every slot (each route within
--                  the seats in every slot and the stop, length and ride-time limits), relocate and swap moves between routes improve them, and VND (or-opt, swap,
--                  2-opt) orders each route on the road travel times, both ways round (one-way streets, bridges and
--                  signals make the two directions differ)
--   loop/straight  a route whose stops lie around the office runs as one loop (office, stops, office) serving both
--                  directions when that loop is hardly longer than driving the stops one way; a route along a corridor
--                  runs straight: to the office in the morning and back from it in the evening
module SharedLogic.CorporateShuttle.RoutePlanner
  ( Home (..),
    Site (..),
    walkMeters,
    metersBetween,
    walkEstimate,
    mainRoadClass,
    hubClass,
    seats,
    homeClearanceMeters,
    dbscan,
    chooseSites,
    Matrix (..),
    secsOf,
    metersOf,
    RouteKind (..),
    routeKindText,
    nodesOf,
    pathSecs,
    planRoutes,
  )
where

import Control.Applicative ((<|>))
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)

-- Pickup points -------------------------------------------------------------------------------------------------------

-- | Riders who live at one spot, and how many.
data Home = Home {homeAt :: LatLong, homeWeight :: Int}

-- | A point on a road where a shuttle can stop, the road's name (for the stop's name only) and its class from the road
-- network: 4 a metro station or bus stand, 3 a main road, 2 an ordinary road, 1 a lane or side street (see the caller's
-- road classification).
data Site = Site {siteAt :: LatLong, siteRoad :: Maybe Text, siteClass :: Int}

mainRoadClass, hubClass :: Int
mainRoadClass = 3
-- a metro station or a bus stand: a place everyone knows, on a main road
hubClass = 4

-- | Seats on one shuttle.
seats :: Int
seats = 6

-- | A shared point on anything but a main road is never this close to someone's home (it would be their doorstep).
homeClearanceMeters :: Double
homeClearanceMeters = 30

-- | The furthest anyone walks to their pickup point: a kilometre (fewer, better placed stops make shorter routes).
walkMeters :: Double
walkMeters = 1000

-- | Walking along streets is about this much longer than the straight line.
walkDetour :: Double
walkDetour = 1.3

metersBetween :: LatLong -> LatLong -> Double
metersBetween a b = realToFrac (distanceBetweenInMeters a b)

-- | The walk between two points along the streets, estimated from the straight line.
walkEstimate :: LatLong -> LatLong -> Double
walkEstimate a b = metersBetween a b * walkDetour

rankFactor :: Int -> Double
rankFactor = \case
  4 -> 1.15
  3 -> 1.0
  2 -> 0.85
  1 -> 0.6
  _ -> 0.45

-- | DBSCAN over the homes, weighted (a home of two riders is dense on its own), with eps the furthest two homes can be
-- apart and still share a point between them. Every home is in one result: a cluster, or alone (noise).
dbscan :: [Home] -> [[Int]]
dbscan homes = reverse clusters <> [[i] | i <- IM.keys homeArr, not (IS.member i assigned)]
  where
    homeArr = IM.fromList (zip [0 ..] homes)
    eps = 2 * walkMeters
    minPts = 2
    lat0 = maybe 0 (\h -> h.homeAt.lat) (listToMaybe homes)
    -- grid cells at least eps (as a straight line) wide, so a home's neighbours are all in the cells around it
    cellLat = eps / walkDetour / 111000 * 1.05
    cellLon = cellLat / max 0.2 (cos (lat0 * pi / 180))
    cell p = (floor (p.lat / cellLat) :: Int, floor (p.lon / cellLon) :: Int)
    byCell = Map.fromListWith (<>) [(cell h.homeAt, [i]) | (i, h) <- IM.toList homeArr]
    nbrs = IM.mapWithKey (\_ h -> let (y, x) = cell h.homeAt in [j | dy <- [-1, 0, 1], dx <- [-1, 0, 1], j <- Map.findWithDefault [] (y + dy, x + dx) byCell, walkEstimate h.homeAt (homeArr IM.! j).homeAt <= eps]) homeArr
    core i = sum [(homeArr IM.! j).homeWeight | j <- nbrs IM.! i] >= minPts
    (assigned, clusters) = foldl grow (IS.empty, []) (IM.keys homeArr)
    grow (done, acc) i
      | IS.member i done || not (core i) = (done, acc)
      | otherwise = let (done', members) = expand (IS.insert i done) [i] [i] in (done', reverse members : acc)
    -- only core homes reach further; a border home joins the first cluster that reaches it
    expand done members [] = (done, members)
    expand done members (q : qs) =
      let new = [j | j <- nbrs IM.! q, not (IS.member j done)]
       in expand (foldr IS.insert done new) (reverse new <> members) (qs <> filter core new)

-- | One neighbourhood's pickup points, chosen from the road points near it: (site, homes walking to it) for the sites used,
-- and the homes no road point is within walking distance of.
chooseSites :: [Home] -> [Site] -> ([(Site, [Int])], [Int])
chooseSites homes allSites = ([(sArr IM.! s, hs) | (s, hs) <- IM.toList (groupsOf final)], unreachable)
  where
    hArr = IM.fromList (zip [0 ..] homes)
    -- off a main road, a point at someone's door is no shared pickup
    sites = [st | st <- allSites, st.siteClass >= mainRoadClass || all (\h -> metersBetween h.homeAt st.siteAt > homeClearanceMeters) homes]
    sArr = IM.fromList (zip [0 ..] sites)
    walk h s = walkEstimate (hArr IM.! h).homeAt (sArr IM.! s).siteAt
    weight h = (hArr IM.! h).homeWeight
    factor s = rankFactor (sArr IM.! s).siteClass
    reach = IM.map (\st -> [h | (h, hm) <- IM.toList hArr, walkEstimate hm.homeAt st.siteAt <= walkMeters]) sArr
    canUse = IM.fromListWith (<>) [(h, [s]) | (s, hs) <- IM.toList reach, h <- hs]
    reachable = IS.fromList (IM.keys canUse)
    unreachable = [h | h <- IM.keys hArr, not (IS.member h reachable)]
    weightedWalk s hs = sum [walk h s * fromIntegral (weight h) | h <- hs]
    groupsOf a = IM.fromListWith (flip (<>)) [(s, [h]) | (h, s) <- IM.toList a]
    load g s = sum (map weight (IM.findWithDefault [] s g))
    final = consolidate (reassign (recentre (consolidate (greedy reachable IM.empty))))
    -- 1. the site the most still-unplaced riders can walk to (a main road counts for more), until everyone is placed
    greedy unplaced acc
      | IS.null unplaced = acc
      | otherwise = case listToMaybe (sortOn fst [((negate (fromIntegral (sum (map weight hs)) * factor s), weightedWalk s hs), (s, hs)) | (s, all') <- IM.toList reach, let hs = filter (`IS.member` unplaced) all', not (null hs)]) of
        Nothing -> acc
        Just (_, (s, hs)) -> greedy (foldr IS.delete unplaced hs) (foldr (`IM.insert` s) acc hs)
    -- 2. fewer points: the lightest point whose riders can all walk to other open points is closed, its riders going to
    -- the busiest of those
    consolidate a0 = foldl close a0 (sortOn (load (groupsOf a0)) (IM.keys (groupsOf a0)))
    close a s =
      let g = groupsOf a
          hs = IM.findWithDefault [] s g
          alt h = listToMaybe (sortOn (\t -> (negate (load g t), walk h t)) [t | t <- IM.findWithDefault [] h canUse, t /= s, IM.member t g])
          moves = [(h, t) | h <- hs, Just t <- [alt h]]
       in if not (null hs) && length moves == length hs then foldr (\(h, t) -> IM.insert h t) a moves else a
    -- 3. each point moves to the best road all its riders can walk to: a main road first, then the least walking
    recentre a =
      IM.fromList
        [ (h, best)
          | (s, hs) <- IM.toList (groupsOf a),
            let common = foldr (IS.intersection . IS.fromList . (\h -> IM.findWithDefault [] h canUse)) (IS.fromList (IM.keys sArr)) hs
                best = maybe s snd (listToMaybe (sortOn fst [((negate (factor t), weightedWalk t hs), t) | t <- IS.toList common])),
            h <- hs
        ]
    -- 4. everyone to the best open point they can walk to (points that empty are dropped)
    reassign a =
      let open = IM.keys (groupsOf a)
       in IM.mapWithKey (\h s -> maybe s snd (listToMaybe (sortOn fst [((negate (factor t), walk h t), t) | t <- IM.findWithDefault [] h canUse, t `elem` open]))) a

-- Routes ---------------------------------------------------------------------------------------------------------------

-- | Road travel between the planning nodes (0 the office, the rest pickup points), each way: seconds and metres.
data Matrix = Matrix {travelSecs :: Map.Map (Int, Int) Double, travelMeters :: Map.Map (Int, Int) Double}

secsOf :: Matrix -> Int -> Int -> Double
secsOf m i j
  | i == j = 0
  | otherwise = Map.findWithDefault 1e7 (i, j) m.travelSecs

metersOf :: Matrix -> Int -> Int -> Double
metersOf m i j
  | i == j = 0
  | otherwise = Map.findWithDefault 1e7 (i, j) m.travelMeters

-- | TO_OFFICE: the pickups then the office; FROM_OFFICE: the office then the drops; LOOP: from the office round the stops
-- and back, both directions' riders on it.
data RouteKind = ToOffice | FromOffice | Loop deriving (Eq, Ord, Show)

routeKindText :: RouteKind -> Text
routeKindText = \case
  ToOffice -> "TO_OFFICE"
  FromOffice -> "FROM_OFFICE"
  Loop -> "LOOP"

-- | The route's nodes in driving order, the office (0) included.
nodesOf :: RouteKind -> [Int] -> [Int]
nodesOf ToOffice ps = ps <> [0]
nodesOf FromOffice ps = 0 : ps
nodesOf Loop ps = 0 : ps <> [0]

dwellSecs, maxRouteSecs, maxDetour, maxExtraSecs, loopFactor, routeFixedSecs :: Double
dwellSecs = 60
maxRouteSecs = 75 * 60
maxDetour = 2.0
maxExtraSecs = 20 * 60
-- a loop serves both directions when it is at most this much longer than driving the stops one way
loopFactor = 1.3
-- what one more route costs, in driving seconds, when routes are joined or stops moved between them
routeFixedSecs = 30 * 60

maxStops :: Int
maxStops = 12

-- | Driving the nodes in order, with a minute at every pickup.
pathSecs :: Matrix -> [Int] -> Double
pathSecs m ns = sum (zipWith (secsOf m) ns (drop 1 ns)) + dwellSecs * fromIntegral (length (filter (/= 0) ns))

-- | Arrival times at each node, from the first.
arrivals :: Matrix -> [Int] -> [(Double, Int)]
arrivals m ns = zip (scanl (\t (a, b) -> t + secsOf m a b + (if a /= 0 then dwellSecs else 0)) 0 (zip ns (drop 1 ns))) ns

-- | Nobody rides much longer than the direct drive: twice it, or it and 20 minutes.
rideOk :: Double -> Double -> Bool
rideOk direct onboard = onboard <= max (maxDetour * direct) (direct + maxExtraSecs)

-- | Within the stop and length limits, and every rider's ride within rideOk.
feasible :: Matrix -> (Int -> Bool) -> (Int -> Bool) -> RouteKind -> [Int] -> Bool
feasible m boardsTo leavesFrom kind ps =
  length ps <= maxStops && pathSecs m ns <= maxRouteSecs && okTo && okFrom
  where
    ns = nodesOf kind ps
    times = arrivals m ns
    endT = maybe 0 fst (lastMay times)
    stops = [(t, p) | (t, p) <- times, p /= 0]
    okTo = kind == FromOffice || and [rideOk (secsOf m p 0) (endT - t) | (t, p) <- stops, boardsTo p]
    okFrom = kind == ToOffice || and [rideOk (secsOf m 0 p) t | (t, p) <- stops, leavesFrom p]

-- | Variable neighbourhood descent on a stop order: move one to three consecutive stops elsewhere (or-opt, also
-- reversed), swap two stops, reverse a stretch (2-opt); the first improving move of the first neighbourhood that has one,
-- until none does. An infeasible order takes any feasible one first.
vnd :: ([Int] -> Double) -> ([Int] -> Bool) -> [Int] -> [Int]
vnd cost ok = go (300 :: Int)
  where
    go 0 xs = xs
    go k xs = maybe xs (go (k - 1)) (improving xs)
    improving xs =
      let c = cost xs
          okx = ok xs
          better ys = ys /= xs && ok ys && (not okx || cost ys < c - 1e-6)
       in find better (orOpt xs) <|> find better (swaps xs) <|> find better (twoOpt xs)
    orOpt xs =
      [ take j rest <> s <> drop j rest
        | len <- [1, 2, 3],
          i <- [0 .. length xs - len],
          let seg = take len (drop i xs)
              rest = take i xs <> drop (i + len) xs,
          j <- [0 .. length rest],
          s <- if len > 1 then [seg, reverse seg] else [seg]
      ]
    swaps xs =
      let a = IM.fromList (zip [0 ..] xs)
       in [IM.elems (IM.insert i (a IM.! j) (IM.insert j (a IM.! i) a)) | i <- IM.keys a, j <- IM.keys a, i < j]
    twoOpt xs = [take i xs <> reverse (take (j - i + 1) (drop i xs)) <> drop (j + 1) xs | i <- [0 .. length xs - 2], j <- [i + 1 .. length xs - 1]]

-- | The company's routes over its pickup points, the same in every slot and on every day: boardWaves gives, per slot and
-- day, the riders to the office at each point, leaveWaves the riders home. A route of two or more points never has more
-- riders in one slot than a shuttle's seats (one point busier than that runs several shuttles of its own route); within
-- that, routes are joined while it saves driving, so the seats fill. Each route is (kind, pickup points in driving order).
planRoutes :: Matrix -> [IM.IntMap Int] -> [IM.IntMap Int] -> [Int] -> [(RouteKind, [Int])]
planRoutes m boardWaves leaveWaves points = concatMap shapeGroup (improve (savings points))
  where
    boardsTo p = any (\w -> IM.findWithDefault 0 p w > 0) boardWaves
    leavesFrom p = any (\w -> IM.findWithDefault 0 p w > 0) leaveWaves
    fits g = length g <= 1 || all (\w -> sum [IM.findWithDefault 0 p w | p <- g] <= seats) (boardWaves <> leaveWaves)
    -- the two directions' average: a group serves both
    sym i j = (secsOf m i j + secsOf m j i) / 2
    okAs kind xs = null xs || feasible m boardsTo leavesFrom kind xs || feasible m boardsTo leavesFrom kind (reverse xs)
    groupOk g = fits g && okAs ToOffice (filter boardsTo g) && okAs FromOffice (filter leavesFrom g)
    symPath g = sum (zipWith sym g (drop 1 g)) + maybe 0 (`sym` 0) (lastMay g) + maybe 0 (sym 0) (listToMaybe g)
    groupCost [] = 0
    groupCost g = routeFixedSecs + symPath g

    -- Clarke-Wright: every point its own route, then joined end to end in order of the savings
    savings pts = IM.elems (fst (foldl merge (IM.fromList [(p, [p]) | p <- pts], IM.fromList [(p, p) | p <- pts]) candidates))
      where
        candidates = map snd (sortOn fst [(negate s, (i, j)) | i <- pts, j <- pts, i < j, let s = sym i 0 + sym 0 j - sym i j, s > 0])
        merge (routes, owner) (i, j) =
          let ri = owner IM.! i
              rj = owner IM.! j
           in fromMaybe (routes, owner) $ do
                guard (ri /= rj)
                a <- endingAt i (routes IM.! ri)
                b <- startingAt j (routes IM.! rj)
                guard (groupOk (a <> b))
                pure (IM.insert ri (a <> b) (IM.delete rj routes), foldr (`IM.insert` ri) owner b)
        endingAt i r
          | lastMay r == Just i = Just r
          | listToMaybe r == Just i = Just (reverse r)
          | otherwise = Nothing
        startingAt j r = reverse <$> endingAt j r

    -- between routes: move a stop to the best place in another route, or swap two stops of two routes, while it saves
    improve gs0 = go (40 :: Int) gs0
      where
        go 0 gs = gs
        go k gs = maybe gs (go (k - 1) . filter (not . null)) (relocateMove gs <|> swapMove gs)
        withIx gs = zip [0 :: Int ..] gs
        replace2 gs (ia, a') (ib, b') = [if i == ia then a' else if i == ib then b' else g | (i, g) <- withIx gs]
        gain a b a' b' = groupCost a + groupCost b - groupCost a' - groupCost b' > 1
        relocateMove gs =
          listToMaybe
            [ replace2 gs (ia, a') (ib, b')
              | (ia, a) <- withIx gs,
                x <- a,
                let a' = filter (/= x) a,
                null a' || groupOk a',
                (ib, b) <- withIx gs,
                ib /= ia,
                pos <- [0 .. length b],
                let b' = take pos b <> [x] <> drop pos b,
                gain a b a' b',
                groupOk b'
            ]
        swapMove gs =
          listToMaybe
            [ replace2 gs (ia, a') (ib, b')
              | (ia, a) <- withIx gs,
                (ib, b) <- withIx gs,
                ia < ib,
                x <- a,
                y <- b,
                let a' = map (\v -> if v == x then y else v) a
                    b' = map (\v -> if v == y then x else v) b,
                gain a b a' b',
                groupOk a',
                groupOk b'
            ]

    -- a group's routes: one loop, or straight to the office and back
    shapeGroup g =
      let ms = filter boardsTo g
          es = filter leavesFrom g
          cost kind = pathSecs m . nodesOf kind
          ordered kind xs = if null xs then [] else best kind [vnd (cost kind) (feasible m boardsTo leavesFrom kind) o | o <- [xs, reverse xs]]
          best kind = maybe [] snd . listToMaybe . sortOn fst . map (\o -> ((cost kind o, riderSecs kind o), o))
          to = ordered ToOffice ms
          from = ordered FromOffice es
          tour = ordered Loop g
          loopWins =
            not (null ms)
              && not (null es)
              && length g
              >= 3
              && feasible m boardsTo leavesFrom Loop tour
              && cost Loop tour
              <= loopFactor
              * max (cost ToOffice to) (cost FromOffice from)
       in if loopWins then [(Loop, tour)] else [(ToOffice, to) | not (null to)] <> [(FromOffice, from) | not (null from)]
    -- the riders' time on board, all added up (the tie-break between two equally long orders, e.g. a loop either way round)
    riderSecs kind ps =
      let times = arrivals m (nodesOf kind ps)
          endT = maybe 0 fst (lastMay times)
       in sum [(if boardsTo p && kind /= FromOffice then endT - t else 0) + (if leavesFrom p && kind /= ToOffice then t else 0) | (t, p) <- times, p /= 0]
