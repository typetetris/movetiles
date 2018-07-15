{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import SDL
import SDL.Vect (Point(..))
import Linear (V4(..))
import Control.Monad (unless)
import Foreign.C.Types (CInt(..))
import qualified Debug.Trace as Debug (trace)
import Data.Word (Word8(..))
import qualified Data.Vector.Storable as Vector.Storable
import Protolude
import qualified Options.Applicative as Options
import qualified Data.Map as Map
import Data.Map (Map(..))
import qualified SDL.Image
import Prelude (Show(..))

newtype ColumnCount = Columns Int deriving (Show, Eq)
newtype RowCount = Rows Int deriving (Show, Eq)

data CLIOptions = CLIOptions { cliOptionsImagePath :: FilePath
                             , cliOptionsColumns :: ColumnCount
                             , cliOptionsRows :: RowCount
                             } deriving (Show, Eq)

cliOptionsParser :: Options.Parser CLIOptions
cliOptionsParser = CLIOptions <$> Options.strOption ( Options.long "image" <> Options.metavar "IMAGE" <> Options.help "image for the tile puzzle" )
                              <*>  (Options.option (Columns <$> Options.auto) ( Options.long "columns" <> Options.metavar "NUMCOLUMNS" <> Options.help "number of columns"))
                              <*>  (Options.option (Rows <$> Options.auto) ( Options.long "rows" <> Options.metavar "NUMROWS" <> Options.help "number of rows"))

cliOptionsP :: Options.ParserInfo CLIOptions
cliOptionsP =  Options.info (cliOptionsParser <**> Options.helper) (Options.fullDesc <> Options.progDesc "Solve a tile puzzle with an image you love!" <> Options.header "movetiles - Solve a tile puzzle!")

main :: IO ()
main = do
  cliOptions <- Options.execParser cliOptionsP
  initializeAll
  window <- createWindow "My SDL Application" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  (image, imageDimensions@(ImageDimensions d)) <- loadImage (cliOptionsImagePath cliOptions) renderer
  rendererLogicalSize renderer $= Just d
  appLoop renderer (mkInitialGameState (cliOptionsColumns cliOptions) (cliOptionsRows cliOptions) imageDimensions image)

newtype ImageDimensions = ImageDimensions (V2 CInt)
loadImage :: FilePath -> SDL.Renderer -> IO (SDL.Texture, ImageDimensions)
loadImage fp renderer = do
  image <- SDL.Image.loadTexture renderer fp
  imageInfo <- SDL.queryTexture image
  return ( image
         , ImageDimensions (V2 (SDL.textureWidth imageInfo) (SDL.textureHeight imageInfo))
         )

data GameState = GameState { board :: Board
                           , selectedKachel :: Maybe ImageTile
                           , image :: SDL.Texture
                           , tileWidth :: CInt
                           , tileHeight :: CInt
                           , columns :: ColumnCount
                           , rows :: RowCount
                           } deriving Show

instance Show SDL.Texture where
  show _ = "some texture"

newtype Board = Board { positions :: Map GridPosition (Maybe ImageTile) } deriving Show

-- A grid position determines a cell in a grid
-- +---------+---------+
-- |         |         |
-- |  (0,0)  |  (1,0)  |
-- |         |         |
-- +---------+---------+
-- |         |         |
-- |  (0,1)  |  (1,1)  |
-- |         |         |
-- +---------+---------+
data GridPosition = GridPosition Int Int deriving (Show, Eq, Ord)
data ImageTile = ImageTile (Rectangle CInt) deriving (Eq, Show)

mkInitialGameState :: ColumnCount -> RowCount -> ImageDimensions -> SDL.Texture -> GameState
mkInitialGameState c@(Columns columns) r@(Rows rows) (ImageDimensions (V2 width height)) image =
  let tw = width `div` fromIntegral columns
      th = height `div` fromIntegral rows
  in GameState { board = Board $ Map.fromList $ [(GridPosition x y, Just $ ImageTile (Rectangle (P (V2 (fromIntegral x*tw) (fromIntegral y*th))) (V2 tw th))) | x <- [0..columns-1], y <- [0..rows-1], (x /= columns - 1) || (y /= rows - 1)]
                                                ++ [(GridPosition (columns-1) (rows-1),Nothing)]
               , selectedKachel = Nothing
               , image = image
               , tileWidth = tw
               , tileHeight = th
               , columns = c
               , rows = r
               }

calculateTargetPos :: GridPosition -> Motion -> GridPosition
calculateTargetPos (GridPosition x y) Main.Up    = GridPosition x     (y-1)
calculateTargetPos (GridPosition x y) Main.Down  = GridPosition x     (y+1)
calculateTargetPos (GridPosition x y) Main.Left  = GridPosition (x-1) y
calculateTargetPos (GridPosition x y) Main.Right = GridPosition (x+1) y

moveTileIfPossible :: Board -> Maybe GridPosition -> Motion -> Board
moveTileIfPossible b Nothing _ = b
moveTileIfPossible b@(Board positions) (Just from) direction
  | target <- calculateTargetPos from direction,
              target `Map.member` positions,
              from `Map.member` positions,
              positions Map.! target == Nothing = b{ positions = Map.union (Map.fromList [(target, positions Map.! from), (from, Nothing)]) positions }
  | otherwise = b

appLoop :: Renderer -> GameState -> IO ()
appLoop renderer gamestate@(GameState _ _ _ tileWidth tileHeight _ _) = do
  event <- waitEvent
  let eventIsQPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
      qPressed = eventIsQPress event
  let newGameState = case eventPayload event of
           MouseButtonEvent (MouseButtonEventData _ Pressed _ ButtonLeft _ (P (V2 x y))) ->
               gridPositionSelected
                 gamestate 
                 (GridPosition (fromIntegral x `div` fromIntegral tileWidth) (fromIntegral y `div` fromIntegral tileHeight))
           KeyboardEvent (KeyboardEventData _ Pressed _ (Keysym _ code _)) ->
             motionPressed gamestate
               (case code of
                 KeycodeUp    -> Just Main.Up
                 KeycodeDown  -> Just Main.Down
                 KeycodeLeft  -> Just Main.Left
                 KeycodeRight -> Just Main.Right
                 _ -> Nothing)
           _ -> gamestate
  drawGameState renderer newGameState
  present renderer
  if not qPressed then (appLoop renderer newGameState) else SDL.quit

gridPositionSelected :: GameState -> GridPosition -> GameState
-- gridPositionSelected gs pos | Debug.trace ("gridPositionSelected " ++ Prelude.show gs ++ " " ++ Prelude.show pos) False = undefined 
gridPositionSelected gs@(GameState (Board positions) selected _ _ _ _ _) pos = 
  case positions Map.! pos of
    Nothing     -> gs
    (Just tile) -> gs { selectedKachel = case selected of
                                          (Just oldSelection) | tile == oldSelection -> Nothing
                                          _ -> Just tile
                      }

motionPressed :: GameState -> Maybe Motion -> GameState
motionPressed gs Nothing = gs
motionPressed gs@(GameState _ Nothing     _ _ _ _ _) _ = gs
motionPressed gs@(GameState b@(Board positions) (Just tile) _ _ _ _ _) (Just motion) =
  let pos = Map.foldlWithKey'
                 (\a k b -> if a == Nothing then (if b == (Just tile) then Just k else Nothing) else a) 
                 Nothing
                 positions
  in gs { board = moveTileIfPossible b pos motion }

drawGameState :: Renderer -> GameState -> IO ()
drawGameState renderer gs@(GameState _ _ _ _ _ (Columns cols) (Rows rows)) = do
  rendererDrawColor renderer $= V4 100 100 100 255
  clear renderer
  sequence_ [ drawGridPosition renderer gs (GridPosition x y) | y <- [0..rows-1], x <- [0..cols-1] ]

drawGridPosition :: SDL.Renderer -> GameState -> GridPosition -> IO ()
-- drawGridPosition _ gs pos | Debug.trace ("drawGridPosition " ++ Prelude.show gs ++ " " ++ Prelude.show pos) False = undefined 
drawGridPosition renderer gs@(GameState (Board positions) selectedKachel image tw th _ _) pos@(GridPosition x y) = do
  case positions Map.! pos of
    Nothing     -> return ()
    (Just (ImageTile r)) -> do
      let destRect = Rectangle (P (V2 (fromIntegral x*tw) (fromIntegral y*th))) (V2 tw th)
      copy renderer image (Just r) (Just destRect) 
      case selectedKachel of
        (Just (ImageTile s)) | s == r -> drawSelectionFrame renderer destRect
        _ -> return ()

calculateSelectionRects :: Rectangle CInt -> [Rectangle CInt]
calculateSelectionRects (Rectangle (P (V2 offx offy)) (V2 width height)) =
  let p5 x = (x * 5) `div` 100 -- get 5 per cent
      thickness = min (p5 width) (p5 height)
  in [(Rectangle (P (V2 offx offy))                        (V2 width     thickness))
     ,(Rectangle (P (V2 offx (offy + height - thickness))) (V2 width     thickness))
     ,(Rectangle (P (V2 offx offy))                        (V2 thickness height))
     ,(Rectangle (P (V2 (offx + width - thickness) offy))  (V2 thickness height))
     ]

drawSelectionFrame :: Renderer -> Rectangle CInt -> IO ()
drawSelectionFrame renderer r = do
    rendererDrawColor renderer $= V4 255 0 0 255
    fillRects renderer (Vector.Storable.fromList (calculateSelectionRects r))

data Motion = Up | Down | Left | Right deriving (Eq, Show)
