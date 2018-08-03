{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Game where

import Protolude
import qualified Prelude as Prelude
import qualified GHC.Show as Show
import qualified System.Random as Random

data Position = Position Int Int deriving (Eq, Show, Ord)

data Focus = FocusPosition Position | NoFocus deriving (Eq, Show)
newtype EmptyTilePosition = EmptyTilePosition Position deriving (Eq, Show)
newtype ColumnCount = Columns Int deriving (Eq, Show)
newtype RowCount = Rows Int deriving (Eq, Show)


data State tileType = State { positions :: [(Position, Maybe tileType)]
                            , focus :: Focus
                            , emptyTilePosition :: EmptyTilePosition
                            , isPositionValid :: Position -> Bool
                            }

instance (Show tileType) => Show (Game.State tileType) where
  show Game.State {..} = "positions=\n" ++ foldl' (\acc elem -> acc ++ show elem ++ "\n") "" positions ++ ",focus=" ++ show focus ++ ",emptyTilePosition=" ++ show emptyTilePosition

data Direction = Up | Down | Left | Right deriving (Eq, Show, Ord, Bounded, Enum)

data Event = FocusMove Direction | FocusToggle Position | TileMove Direction deriving (Eq, Show)

-- | createState creates a game state with the specified amount of columns and rows.
-- The tiles are assumend in row order, there need to be as many as
-- rowCount * columnCount - 1 (the last game position will stay emtpy).
createState :: ColumnCount -> RowCount -> [tileType] -> Game.State tileType
createState columnCount@(Columns columns) rowCount@(Rows rows) tiles = Game.State { positions = zip [Position x y | y <- [ 0 .. (rows-1) ], x <- [ 0 .. (columns-1) ], x /= (rows-1) || y /= (columns-1)] (fmap Just tiles)
                                                             , focus = NoFocus
                                                             , emptyTilePosition = EmptyTilePosition (Position (columns-1) (rows-1))
                                                             , isPositionValid = \(Position x y) -> 0 <= x && x < columns && 0 <= y && y < rows
                                                             }

targetPosition :: Position -> Direction -> Position
targetPosition (Position x y) d = case d of
  Game.Up    -> Position x (y-1)   -- qualification on Up not strictly needed, but the code looks nicer that way.
  Game.Down  -> Position x (y+1)
  Game.Left  -> Position (x-1) y
  Game.Right -> Position (x+1) y

handleFocusMove :: Game.State tileType -> Direction -> Game.State tileType
handleFocusMove gs@(Game.State { focus = NoFocus }) _ =  gs
handleFocusMove gs@(Game.State { focus = FocusPosition pos, emptyTilePosition = EmptyTilePosition epos, .. }) d =
  let candidatePos = targetPosition pos d
      newFocusPosition = if candidatePos == epos               -- ^ if the candidate position for the new focus position is empty
                          then targetPosition candidatePos d   -- ^ try to jump over it
                          else candidatePos                    -- ^ otherwise use it
  in
   case isPositionValid newFocusPosition of
     True -> gs { focus = FocusPosition newFocusPosition }
     False -> gs

handleFocusToggle :: Game.State tileType -> Position -> Game.State tileType
handleFocusToggle gs@(Game.State { emptyTilePosition = EmptyTilePosition epos, .. }) p 
  -- | invalid position selected -> change nothing
  | not (isPositionValid p) =  gs
  -- | empty position selected -> change nothing
  | p == epos = gs  
  -- | position selected, which was already selected -> turn focus off
  | FocusPosition fpos <- focus, fpos == p = gs { focus = NoFocus } 
  -- | valid occupied position selected, which was not already focused -> focus selected position
  | otherwise = gs { focus = FocusPosition p }

getPossibleMoveDirection :: Position -> Position -> Maybe Direction
getPossibleMoveDirection (Position x1 y1) (Position x2 y2) = case () of
  _ | x1 == x2 && y1 > y2 -> Just Game.Up
    | x1 == x2 && y1 < y2 -> Just Game.Down
    | y1 == y2 && x1 < x2 -> Just Game.Right
    | y1 == y2 && x1 > x2 -> Just Game.Left
    | otherwise -> Nothing

transformPosition :: Position -> Direction -> Position -> Position -> Position
transformPosition (Position fx fy) d (Position ex ey) p@(Position x y) =
  let move = case d of
               Game.Up -> fx == ex && fx == x && fy >= y && y > ey 
               Game.Down -> fx == ex && fx == x && fy <= y && y < ey
               Game.Left -> fy == ey && fy == y && fx >= x && x > ex
               Game.Right -> fy == ey && fy == y && fx <= x && x < ex
  in if move then targetPosition p d else p

handleTileMove :: Game.State tileType -> Direction -> Game.State tileType
handleTileMove gs@(Game.State { focus = NoFocus , ..}) _ = gs
handleTileMove gs@(Game.State {emptyTilePosition = EmptyTilePosition epos@(Position ex ey)
                              , focus = FocusPosition fpos@(Position fx fy)
                              , ..}) d = case getPossibleMoveDirection fpos epos of
    Nothing -> gs
    Just dir
      | dir /= d -> gs
      | otherwise -> gs { focus = FocusPosition (targetPosition fpos d)
                        , emptyTilePosition = EmptyTilePosition fpos
                        , positions = map (swap . fmap (transformPosition fpos d epos) . swap) positions
                        }

handleEvent :: Game.State tileType -> Maybe Event -> Game.State tileType
handleEvent gs ev = case ev of
  Just (FocusMove d) -> handleFocusMove gs d
  Just (FocusToggle p) -> handleFocusToggle gs p
  Just (TileMove d) -> handleTileMove gs d
  Nothing -> gs

-- The following functions are all there to provide a randomized starting position

setFocus :: Game.State tileType -> Position -> Game.State tileType
setFocus gs@(Game.State { emptyTilePosition = EmptyTilePosition epos, .. }) pos
  | pos /= epos = gs{focus = FocusPosition pos}
  | otherwise = gs

getMovableTiles :: Game.State tileType -> [(Position, Direction)]
getMovableTiles gs@(Game.State { emptyTilePosition = EmptyTilePosition epos, .. }) =
  let pos     = map fst positions
      dirs    = map (flip getPossibleMoveDirection epos) pos
      posdirs = zip pos dirs
  in  [ (pos, dir) | (pos, Just dir) <- posdirs ]

randomizeState :: Game.State tileType -> Int -> IO (Game.State tileType)
randomizeState gs n = do
    gen <- Random.newStdGen
    return $ loop n gs gen
  where
    loop 0 gs _   = gs 
    loop m gs gen = let movable     = getMovableTiles gs
                        (ind, ngen) = Random.randomR (0, length movable - 1) gen
                        (pos, dir)  = movable Prelude.!! ind
                        ngs1 = flip handleTileMove dir . flip setFocus pos $ gs
                        ngs = ngs1 { focus = NoFocus }
                    in loop (m-1) ngs ngen

