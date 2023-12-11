{-# LANGUAGE LambdaCase #-}
import Data.Map (Map, (!), fromList, foldrWithKey, keys, findWithDefault)
import Data.List (transpose)

type Coord = (Integer,Integer)

part1 :: IO ()
part1 = do
    inp <- lines <$> readFile "input.txt"
    let m = fromList $ concat [[((x,y), c) | (c,x) <- zip row [0..]] | (row,y) <- zip inp [0..]]
        start@(sx,sy) = foldrWithKey (\k x acc -> if x == 'S' then k else acc) (0,0) m
        next
          | findWithDefault '?' (sx, sy-1) m `elem` "F7|" = (sx, sy-1)
          | m ! (sx+1, sy) `elem` "7J-" = (sx+1, sy)
          | m ! (sx, sy+1) `elem` "JL|" = (sx, sy+1)
          | otherwise = error "Did not find path from start"
        steps = length $ traversePipes m start next
    print $ steps `div` 2 + steps `rem` 2 -- Ceiling division

traversePipes :: Map Coord Char -> Coord -> Coord -> [Coord]
traversePipes m old curr = do
    let newCoords = translate old curr $ m ! curr
    if m ! newCoords == 'S' then [curr] else
        curr : traversePipes m curr newCoords

translate :: Coord -> Coord -> Char -> Coord
translate (oldx,oldy) (curx,cury) = \case
    '|' -> (curx, 2*cury-oldy)
    '-' -> (2*curx-oldx, cury)
    'L' -> if curx-oldx /= 0 then (curx, cury-1) else (curx+1, cury)
    'J' -> if curx-oldx /= 0 then (curx, cury-1) else (curx-1, cury)
    '7' -> if curx-oldx /= 0 then (curx, cury+1) else (curx-1, cury)
    'F' -> if curx-oldx /= 0 then (curx, cury+1) else (curx+1, cury)
    '.' -> error "Reached ground"
    'S' -> error "Reached start"

----------------------------------
part2 :: IO ()
part2 = do
    inp <- lines <$> readFile "input.txt"
    let coords = [[((x,y), c) | (c,x) <- zip row [0..]] | (row,y) <- zip inp [0..]]
        m = fromList $ concat coords
        start@(sx,sy) = foldrWithKey (\k x acc -> if x == 'S' then k else acc) (0,0) m
        next
          | findWithDefault '?' (sx, sy-1) m `elem` "F7|" = (sx, sy-1)
          | m ! (sx+1, sy) `elem` "7J-" = (sx+1, sy)
          | m ! (sx, sy+1) `elem` "JL|" = (sx, sy+1)
          | otherwise = error "Did not find path from start"
        path = start : traversePipes m start next
        (n,_,_,_,_) = foldr (uncurry countTiles) (0, False, 0, 0, path) $ reverse . concat $ transpose coords
    print n

countTiles :: Coord -> Char -> (Integer, Bool, Integer, Integer, [Coord]) -> (Integer, Bool, Integer, Integer, [Coord])
countTiles coord@(x,y) c z@(n,inside,l,r,path) = do
    let notOnPath = coord `notElem` path
        onPath = not notOnPath
    -- This is horrible
    case c of
        '.' -> (if inside then n+1 else n, inside, l, r, path)
        '|' -> (if inside && notOnPath then n+1 else n,inside,l,r,path)
        'J' -> (if inside && notOnPath then n+1 else n,
            not (inside && r==0 && onPath) && inside,
            if onPath then if l>0 then l-1 else l+1 else l, r, path)
        'L' -> (if inside && notOnPath then n+1 else n,
            not (inside && l==0 && onPath) && inside,
            l, if onPath then if r>0 then r-1 else r+1 else r, path)
        '7' -> (if inside && notOnPath then n+1 else n,
            inside || onPath,
            if onPath then if l>0 then l-1 else l+1 else l, r, path)
        'F' -> (if inside && notOnPath then n+1 else n,
            inside || onPath,
            l, if onPath then if r>0 then r-1 else r+1 else r, path)
        '-' -> (if inside && notOnPath then n+1 else n,
            if onPath then not inside else inside,
            if onPath then if l>0 && r>0 then l-1 else l+1 else l,
            if onPath then if l>0 && r>0 then r-1 else r+1 else r, path)
        'S' -> do
            let (sx,sy) = head $ tail path
                (ex,ey) = last path
            case (sx-ex,sy-ey,sx-x) of
                (2,0,_)   -> countTiles coord '-' z
                (0,-2,_)  -> countTiles coord '|' z
                (1,-1,0)  -> countTiles coord 'J' z
                (-1,-1,0) -> countTiles coord 'L' z
                (1,-1,1)  -> countTiles coord 'F' z
                (1,1,0)   -> countTiles coord '7' z
