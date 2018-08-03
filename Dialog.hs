{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
module Dialog where

import Protolude
import Control.Concurrent (forkIO)
import Control.Monad (void)

import qualified GI.Gtk as Gtk
import qualified GI.Gtk.Objects as Objects
import qualified GI.Gtk.Objects.Dialog as Dialog
import qualified GI.Gtk.Objects.Widget as Widget
import qualified GI.Gtk.Objects.Window as Window
import qualified GI.Gtk.Objects.Scale as Scale
import qualified GI.Gtk.Objects.FileChooserButton as FileChooserButton
import qualified GI.Gtk.Interfaces.FileChooser as FileChooser
import qualified GI.Gtk.Objects.Box as Box
import qualified GI.Gtk.Objects.Label as Label
import qualified GI.Gtk.Objects.Grid as Grid
import qualified GI.Gtk.Objects.Range as Range
import qualified GI.Gtk.Objects.Adjustment as Adjustment
import Data.GI.Base
import Data.Text
import Data.Int
import GI.Gtk.Enums

scaleValue :: (Range.IsRange a) => a -> IO Int
scaleValue scale = do
  adjs <- Range.rangeGetAdjustment scale
  value <- Adjustment.adjustmentGetValue adjs
  return $ round value

init :: IO ()
init = Gtk.init Nothing >> return ()

run :: IO (Maybe FilePath, Int, Int)
run = do
  dialog <- new Dialog.Dialog []
  _ <- Dialog.dialogAddButton dialog "Ok" 21
  _ <- Dialog.dialogAddButton dialog "Quit" 22

  rowLabel <- Label.labelNew (Just "Rows:")
  rowCountScale <- Scale.scaleNewWithRange OrientationHorizontal 1 10 1
  Widget.widgetSetHexpand rowCountScale True

  columnLabel <- Label.labelNew (Just "Columns:")
  columnCountScale <- Scale.scaleNewWithRange OrientationHorizontal 1 10 1
  Widget.widgetSetHexpand columnCountScale True

  imageLabel <- Label.labelNew (Just "Image:")
  fileChooser <- FileChooserButton.fileChooserButtonNew "Select an image to puzzle" FileChooserActionOpen

  b <- Dialog.dialogGetContentArea dialog
  gr <- Grid.gridNew
  Grid.gridSetColumnSpacing gr 20
  Grid.gridSetRowSpacing gr 20
  Grid.gridAttach gr rowLabel         0 0 1 1
  Grid.gridAttach gr rowCountScale    1 0 1 1
  Grid.gridAttach gr columnLabel      0 1 1 1
  Grid.gridAttach gr columnCountScale 1 1 1 1
  Grid.gridAttach gr imageLabel       0 2 1 1
  Grid.gridAttach gr fileChooser      1 2 1 1

  Box.boxPackStart b gr True True 5

  Widget.widgetShowAll dialog
  Window.windowSetModal dialog True
  Dialog.onDialogResponse dialog $ \_ -> Widget.widgetHide dialog

  result <- Dialog.dialogRun dialog

  fileName <- FileChooser.fileChooserGetFilename fileChooser
  rows <- scaleValue rowCountScale
  columns <- scaleValue columnCountScale

  return (fileName, rows, columns)
