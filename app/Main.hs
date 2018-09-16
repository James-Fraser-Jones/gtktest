module Main where

import GUI

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk hiding (Action, backspace)
import Graphics.UI.Gtk.Layout.Grid

--------------------------------------------------------------------------------

rows = 10
cols = 5

main :: IO ()
main = do
  void initGUI

  window <- windowNew
  set window [ windowTitle         := "DB Viewer"
             , windowResizable     := False
             , windowDefaultWidth  := 1280
             , windowDefaultHeight := 720
             , windowWindowPosition := WinPosCenter]

  grid <- gridNew
  gridSetRowHomogeneous grid True
  --gridSetColumnHomogeneous grid True

  attachCorner [] grid "✕"
  attachRows [labelWidthChars := 5] grid $ rowNames rows
  attachCols [] grid $ colNames cols
  attachCells [entryWidthChars := 10] grid rows cols

  gridWindow <- scrolledWindowNew Nothing Nothing --adds scrolling for our grid
  containerAdd gridWindow grid

  containerAdd window gridWindow

  cells <- getCells grid
  let cell = getCell cells rows cols 1 1
  writeToCell cell "James"
  let cell = getCell cells rows cols 2 1
  writeToCell cell "James"
  let cell = getCell cells rows cols 2 2
  writeToCell cell "James"
  let cell = getCell cells rows cols 1 2
  writeToCell cell "James"

  window `on` deleteEvent $ do
   liftIO mainQuit
   return False

  widgetShowAll window
  mainGUI

{-
https://wiki.haskell.org/Gtk2Hs/
https://www.stackbuilders.com/tutorials/haskell/gui-application/

This bit talks about the various packages used in GTK2HS:
libglib2.0-dev (glib: object type system and data structures)
libcairo2-dev (cairo: a 2D vector graphics library)
libpango1.0-dev (pango: a Unicode-aware font rendering engine)
libgtk-3-dev  (gtk: the base GUI library)
glade (glade: gui creator for GTK)

(gtk2hs-buildtools: build tools, required to build from source)
(gio: an OS-agnostic file access API)
libcanberra-gtk-module (extra module that it complained about when I build it, try not to use this unless it complains)

Add "gtk3", "gtk2hs-buildtools" and "glib" to the "build-depends" in the cabal file
-}
