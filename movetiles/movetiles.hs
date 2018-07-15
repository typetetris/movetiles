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

data CLIOptions = CLIOptions {
   imagePath :: FilePath
   } deriving (Show, Eq)

cliOptionsParser :: Options.Parser CLIOptions
cliOptionsParser = CLIOptions <$> Options.strOption ( Options.long "image" <> Options.metavar "IMAGE" <> Options.help "image for the tile puzzle" )

cliOptionsP :: Options.ParserInfo CLIOptions
cliOptionsP =  Options.info (cliOptionsParser <**> Options.helper) (Options.fullDesc <> Options.progDesc "Solve a tile puzzle with an image you love!" <> Options.header "movetiles - Solve a tile puzzle!")

data Kachel = Kachel1 | Kachel2 | Kachel3 deriving (Show, Eq, Ord)

data Board = Board { leftUpper :: Maybe Kachel
                   , rightUpper :: Maybe Kachel
                   , leftBottom :: Maybe Kachel
                   , rightBottom :: Maybe Kachel }

data BoardRectangles = BoardRectangles { leftUpperR :: Rectangle CInt
                                       , rightUpperR :: Rectangle CInt
                                       , leftBottomR :: Rectangle CInt
                                       , rightBottomR :: Rectangle CInt }

data GameState = GameState { board :: Board
                           , boardRectangles :: Maybe BoardRectangles -- to determine which kachel was clicked
                           , selectedKachel :: Maybe Kachel
                           , imageTexture :: SDL.Texture
                           , imageTileMap :: Map Kachel (Rectangle CInt)
                           }

initialBoard :: Board
initialBoard = Board (Just Kachel1) (Just Kachel2) (Just Kachel3) Nothing

mkInitialGameState :: SDL.Texture -> [Rectangle (CInt)] -> GameState
mkInitialGameState image imageTiles = GameState {
    board = Board (Just Kachel1) (Just Kachel2) (Just Kachel3) Nothing
  , boardRectangles = Nothing
  , selectedKachel = Nothing
  , imageTexture = image
  , imageTileMap = Map.fromList . zip [Kachel1,Kachel2,Kachel3] $ imageTiles
  }

newtype Width = Width CInt deriving (Show)
newtype Height = Height CInt deriving (Show)

splitInRectangles :: Rectangle CInt -> BoardRectangles
-- splitInRectangles w h | Debug.trace ("splitInRectangles " ++ show w ++ " " ++ show h) False = undefined
splitInRectangles (Rectangle (P (V2 offx offy)) (V2 width height)) =
  let firstWidth = width `div` 2
      restWidth = width - firstWidth
      firstHeight = height `div` 2
      restHeight = height - firstHeight
      zero = (0::CInt)
  in BoardRectangles (Rectangle (P (V2 offx offy))                                (V2 firstWidth firstHeight))
                     (Rectangle (P (V2 (offx + firstWidth) offy))                 (V2 restWidth firstHeight))
                     (Rectangle (P (V2 offx (offy + firstHeight)))                (V2 firstWidth restHeight))
                     (Rectangle (P (V2 (offx + firstWidth) (offy + firstHeight))) (V2 restWidth restHeight))

colorForKachel :: Kachel -> V4 Word8
colorForKachel Kachel1 = V4 0 255 0   255 
colorForKachel Kachel2 = V4 0 0   255 255
colorForKachel Kachel3 = V4 0 255 255 255 

getRendererDimensions :: Renderer -> IO (Rectangle CInt)
getRendererDimensions renderer = do
   (Just r) <- SDL.get (rendererViewport renderer)
   return r

point32toC :: Point V2 Int32 -> Point V2 CInt
point32toC = fmap CInt

pointInRectangle :: Rectangle CInt -> Point V2 CInt -> Bool
pointInRectangle (Rectangle (P (V2 x y)) (V2 w h)) (P (V2 kx ky)) =
  let relx = kx - x
      rely = ky - y
  in    0    <= relx
     && relx <= w
     && 0    <= rely
     && rely <= h

executeMotion :: Board -> Maybe Motion -> Maybe Kachel -> Board
executeMotion b (Just m) (Just k) = case () of
  _ | leftUpper b == (Just k) -> case m of
       Main.Up    -> b
       Main.Left  -> b
       Main.Down  -> if leftBottom b == Nothing then b { leftUpper = Nothing, leftBottom = (Just k) } else b
       Main.Right -> if rightUpper b == Nothing then b { leftUpper = Nothing, rightUpper = (Just k) } else b
  _ | leftBottom b == (Just k) -> case m of
       Main.Up    -> if leftUpper b == Nothing then b { leftBottom = Nothing, leftUpper = (Just k) } else b
       Main.Left  -> b
       Main.Down  -> b
       Main.Right -> if rightBottom b == Nothing then b { leftBottom = Nothing, rightBottom = (Just k) } else b
  _ | rightUpper b == (Just k) -> case m of
       Main.Up    -> b
       Main.Left  -> if leftUpper b == Nothing then b { rightUpper = Nothing, leftUpper = (Just k) } else b
       Main.Down  -> if rightBottom b == Nothing then b { rightUpper = Nothing, rightBottom = (Just k) } else b
       Main.Right -> b
  _ | rightBottom b == (Just k) -> case m of
       Main.Up    -> if rightUpper b == Nothing then b { rightBottom = Nothing, rightUpper = (Just k) } else b
       Main.Left  -> if leftBottom b == Nothing then b { rightBottom = Nothing, leftBottom = (Just k) } else b
       Main.Down  -> b
       Main.Right -> b
  _ -> b
executeMotion b _ _ = b

calculateNextGameState :: GameState -> Rectangle CInt -> Maybe (Point V2 Int32) -> Maybe Motion -> GameState
calculateNextGameState gs@(GameState board _ selected _ _) r buttonPress motion =
  let actRects     = splitInRectangles r
      newSelection = case buttonPress of
        Nothing    -> selected
        (Just pos) ->
          let cpos = point32toC pos
              clickedKachel = case () of
                _ | pointInRectangle (leftUpperR actRects) cpos -> leftUpper board
                _ | pointInRectangle (leftBottomR actRects) cpos -> leftBottom board
                _ | pointInRectangle (rightBottomR actRects) cpos -> rightBottom board
                _ | pointInRectangle (rightUpperR actRects) cpos -> rightUpper board
                _ -> Nothing
          in case clickedKachel of
            Nothing -> selected
            _ | clickedKachel == selected -> Nothing
            _ -> clickedKachel
  in gs { board = executeMotion board motion newSelection, boardRectangles = Just actRects, selectedKachel = newSelection }

getRectangleForSelectedKachel :: GameState -> Maybe (Rectangle CInt)
getRectangleForSelectedKachel (GameState _ _ Nothing _ _) = Nothing
getRectangleForSelectedKachel (GameState _ Nothing _ _ _) = Nothing
getRectangleForSelectedKachel (GameState b (Just rs) k _ _) = case () of
  _ | leftUpper b == k   -> Just $ leftUpperR rs
  _ | leftBottom b == k  -> Just $ leftBottomR rs
  _ | rightBottom b == k -> Just $ rightBottomR rs
  _ | rightUpper b == k  -> Just $ rightUpperR rs
  _ -> Nothing

drawGameState :: Renderer -> GameState -> IO ()
drawGameState _ (GameState _ Nothing _ _ _) = return () -- without Rectangles, we can't draw anything.
drawGameState renderer gs@(GameState board (Just boardRectangles) selectedKachel image imageTiles) = do
    drawBoard renderer board boardRectangles image imageTiles
    case getRectangleForSelectedKachel gs of
      Nothing  -> return ()
      (Just r) -> drawSelectionFrame renderer r

drawBoard :: Renderer -> Board -> BoardRectangles -> SDL.Texture -> Map Kachel (Rectangle CInt) -> IO ()
drawBoard renderer board rectAngles image imageTiles = do
  rendererDrawColor renderer $= V4 100 100 100 255
  clear renderer
  drawKachel renderer (leftUpper   board) (leftUpperR   rectAngles) image imageTiles
  drawKachel renderer (rightUpper  board) (rightUpperR  rectAngles) image imageTiles
  drawKachel renderer (leftBottom  board) (leftBottomR  rectAngles) image imageTiles
  drawKachel renderer (rightBottom board) (rightBottomR rectAngles) image imageTiles

drawKachel :: Renderer -> Maybe Kachel -> Rectangle CInt -> SDL.Texture -> Map Kachel (Rectangle CInt) -> IO ()
-- drawKachel _ mk r | Debug.trace ("drawKachel " ++ show mk ++ " " ++ show r) False = undefined
drawKachel _ Nothing _ _ _ = return ()
drawKachel renderer (Just kachel) r image imageTiles = do
    copy renderer image (Just $ imageTiles Map.! kachel) (Just r)

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

turnToGrid :: V2 Int -> V2 CInt -> [Rectangle CInt]
turnToGrid (V2 tPR tPC) (V2 width height) =
  let tilesPerRow = fromIntegral tPR
      tilesPerColumn = fromIntegral tPC
      tileWidth  = width  `div` tilesPerRow
      tileHeight = height `div` tilesPerColumn
  in [ Rectangle (P (V2 (x*tileWidth) (y*tileHeight))) (V2 tileWidth tileHeight) | y <- [0 .. tilesPerColumn-1], x <- [0..tilesPerRow-1]]

loadAndTileImage :: FilePath -> Renderer -> V2 Int -> IO (Texture, [Rectangle CInt])
loadAndTileImage fp renderer tilingInfo = do
  texture <- SDL.Image.loadTexture renderer fp 
  tInfo <- queryTexture texture
  let grid = turnToGrid tilingInfo (V2 (textureWidth tInfo) (textureHeight tInfo))
  -- Debug.trace ("Image layout: " ++ show grid) (return ())
  return (texture, grid)

main :: IO ()
main = do
  cliOptions <- Options.execParser cliOptionsP
  initializeAll
  window <- createWindow "My SDL Application" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  (image, imageTiles) <- loadAndTileImage (imagePath cliOptions) renderer (V2 2 2)
  appLoop renderer (mkInitialGameState image imageTiles)

data Motion = Up | Down | Left | Right deriving (Eq, Show)

appLoop :: Renderer -> GameState -> IO ()
appLoop renderer gamestate = do
  event <- waitEvent
  let eventIsQPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
      qPressed = eventIsQPress event
  let buttonPressPos = case eventPayload event of
          MouseButtonEvent (MouseButtonEventData _ Pressed _ ButtonLeft _ pos) -> Just pos
          _ -> Nothing
  let motion = case eventPayload event of
                 KeyboardEvent (KeyboardEventData _ Pressed _ (Keysym _ code _)) ->
                   case code of
                     KeycodeUp    -> Just Main.Up
                     KeycodeDown  -> Just Main.Down
                     KeycodeLeft  -> Just Main.Left
                     KeycodeRight -> Just Main.Right
                     _ -> Nothing
                 _ -> Nothing
  viewPort <- getRendererDimensions renderer
  let newGameState = calculateNextGameState gamestate viewPort buttonPressPos motion
  drawGameState renderer newGameState
  present renderer
  if not qPressed then (appLoop renderer newGameState) else SDL.quit
