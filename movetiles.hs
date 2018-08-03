{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
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
import qualified Dialog as Dialog
import qualified Game as Game
import qualified Debug.Trace as Trace

data CLIOptions = CLIOptions { cliOptionsImagePath :: FilePath
                             , cliOptionsColumns :: Game.ColumnCount
                             , cliOptionsRows :: Game.RowCount
                             , cliOptionsRandom :: Maybe Int
                             } deriving (Show, Eq)

cliOptionsParser :: Options.Parser CLIOptions
cliOptionsParser = CLIOptions <$> Options.strOption ( Options.long "image" <> Options.metavar "IMAGE" <> Options.help "image for the tile puzzle" )
                              <*>  (Options.option (Game.Columns <$> Options.auto) ( Options.long "columns" <> Options.metavar "NUMCOLUMNS" <> Options.help "number of columns"))
                              <*>  (Options.option (Game.Rows <$> Options.auto) ( Options.long "rows" <> Options.metavar "NUMROWS" <> Options.help "number of rows"))
                              <*>  Options.optional (Options.option Options.auto ( Options.long "randomDrawCount" <> Options.metavar "RANDOMDRAWCOUNT" <> Options.help "number of random draws before start"))

cliOptionsP :: Options.ParserInfo CLIOptions
cliOptionsP =  Options.info (cliOptionsParser <**> Options.helper) (Options.fullDesc <> Options.progDesc "Solve a tile puzzle with an image you love!" <> Options.header "movetiles - Solve a tile puzzle!")

-- What is a good name for the combination of the Game State and the Renderer Env?
data ProgramState = ProgramState { tileWidth :: CInt
                                 , tileHeight :: CInt
                                 , state :: Game.State (Rectangle CInt)
                                 , image :: SDL.Texture
                                 , renderer :: SDL.Renderer
                                 }
instance Show ProgramState where
  show ProgramState{..} = "tw: " ++ Protolude.show tileWidth ++ ", th: " ++ Protolude.show tileHeight ++ ", state: " ++ Protolude.show state

createProgramState :: Game.ColumnCount -> Game.RowCount -> ImageDimensions -> SDL.Texture -> SDL.Renderer -> ProgramState
createProgramState cC@(Game.Columns cols) rC@(Game.Rows rows) (ImageDimensions (V2 width height)) image renderer =
  let c = fromIntegral cols
      r = fromIntegral rows
      tileWidth = width `div` c
      tileHeight = height `div` r
  in ProgramState { tileWidth = tileWidth
                  , tileHeight = tileHeight
                  , Main.state = Game.createState cC rC [Rectangle (P (V2 (x * tileWidth) (y * tileHeight))) (V2 tileWidth tileHeight) | y <- [ 0 .. (r-1) ], x <- [ 0 .. (c-1) ] ]
                  , image = image
                  , renderer = renderer
                  }
randomizeState :: ProgramState -> Maybe Int -> IO ProgramState
randomizeState s Nothing = return s
randomizeState s (Just count)
   | count <= 0 = return s
   | otherwise, ProgramState { .. } <- s = do
     gs <- Game.randomizeState state count
     return s { Main.state = gs }

main :: IO ()
main = do
  args <- getArgs
  cliOptions <- do
     if null args
       then do
         Dialog.init
         (fn, r, c) <- Dialog.run
         case fn of
           Nothing -> putText "You need to select an image!" >> exitFailure
           (Just f) -> return $ CLIOptions f (Game.Columns c) (Game.Rows r) Nothing
       else Options.execParser cliOptionsP
  initializeAll
  window <- createWindow "My SDL Application" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  (image, imageDimensions@(ImageDimensions d)) <- loadImage (cliOptionsImagePath cliOptions) renderer
  rendererLogicalSize renderer $= Just d
  let r  = (cliOptionsRows cliOptions)
      c  = (cliOptionsColumns cliOptions)
      s  = (createProgramState c r imageDimensions image renderer) 
  ns <- randomizeState s (cliOptionsRandom cliOptions)
  appLoop ns

newtype ImageDimensions = ImageDimensions (V2 CInt)

loadImage :: FilePath -> SDL.Renderer -> IO (SDL.Texture, ImageDimensions)
loadImage fp renderer = do
  image <- SDL.Image.loadTexture renderer fp
  imageInfo <- SDL.queryTexture image
  return ( image
         , ImageDimensions (V2 (SDL.textureWidth imageInfo) (SDL.textureHeight imageInfo))
         )

instance Show SDL.Texture where
  show _ = "some texture"

data Event = Quit | GameEvent (Maybe Game.Event)

transformKeyboardEvent :: Keycode -> KeyModifier -> Main.Event
transformKeyboardEvent KeycodeQ _ = Quit
transformKeyboardEvent code mods =
  let evType =  if keyModifierLeftShift mods || keyModifierRightShift mods then Game.FocusMove else Game.TileMove
      direction = case code of
        KeycodeUp    -> Just Game.Up
        KeycodeDown  -> Just Game.Down
        KeycodeLeft  -> Just Game.Left
        KeycodeRight -> Just Game.Right
        _            -> Nothing
  in GameEvent $ evType <$> direction

appLoop :: ProgramState -> IO ()
appLoop ps@ProgramState{tileWidth=tileWidth, tileHeight=tileHeight} = do
  event <- waitEvent
  let ev = case eventPayload event of
        KeyboardEvent (KeyboardEventData _ Pressed _ (Keysym _ code mods)) -> transformKeyboardEvent code mods
        MouseButtonEvent (MouseButtonEventData _ Pressed _ ButtonLeft _ (P (V2 x y))) -> GameEvent . Just . Game.FocusToggle
            $ Game.Position (fromIntegral x `div` fromIntegral tileWidth) (fromIntegral y `div` fromIntegral tileHeight)
        _ -> GameEvent Nothing
  case ev of
    Quit           -> SDL.quit
    (GameEvent ev) -> let newProgramState = ps{ Main.state = Game.handleEvent (Main.state ps) ev }
      in do
        drawGameState newProgramState
        present (renderer newProgramState)
        appLoop newProgramState

drawGameState :: ProgramState -> IO ()
drawGameState ps@ProgramState{Main.state=Game.State{Game.focus=focus, Game.positions=positions}, ..} = do
  SDL.rendererDrawColor renderer $= V4 100 100 100 255
  clear renderer
  sequence_ $ map (drawGridPosition ps) positions
  drawSelectionFrame ps focus

drawGridPosition :: ProgramState -> (Game.Position, Maybe (Rectangle CInt)) -> IO ()
drawGridPosition _ (_, Nothing) = return () 
drawGridPosition ProgramState{..} (Game.Position x y, Just r) =
  let destRect = Rectangle (P (V2 (fromIntegral x*tileWidth) (fromIntegral y*tileHeight))) (V2 tileWidth tileHeight)
  in copy renderer image (Just r) (Just destRect)

drawSelectionFrame :: ProgramState -> Game.Focus -> IO ()
drawSelectionFrame _ Game.NoFocus = return ()
drawSelectionFrame ProgramState{..} (Game.FocusPosition (Game.Position x y)) = do
  let r = Rectangle (P (V2 (fromIntegral x*tileWidth) (fromIntegral y*tileHeight))) (V2 tileWidth tileHeight)
  rendererDrawBlendMode renderer $= BlendAlphaBlend
  rendererDrawColor renderer $= V4 255 0 0 100 
  fillRect renderer (Just r)
