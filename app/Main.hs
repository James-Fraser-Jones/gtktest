module Main where

import Lib

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk hiding (Action, backspace)
import Graphics.UI.Gtk.Layout.Grid

--------------------------------------------------------------------------------

attachCell :: [AttrOp Entry] -> Grid -> Int -> Int -> IO ()
attachCell entrySettings grid x y = entryNew >>= (\entry -> set entry entrySettings *> gridAttach grid entry x y 1 1)

attachCol :: [AttrOp Label] -> Grid -> String -> Int -> IO ()
attachCol labelSettings grid name x = labelNew (Just name) >>= (\label -> set label labelSettings *> gridAttach grid label x 0 1 1)

attachRow :: [AttrOp Label] -> Grid -> String -> Int -> IO ()
attachRow labelSettings grid name y = labelNew (Just name) >>= (\label -> set label labelSettings *> gridAttach grid label 0 y 1 1)

--------------------------------------------------------------------------------

attachCells :: [AttrOp Entry] -> Grid -> Int -> Int -> IO ()
attachCells entrySettings grid rows cols = void $ sequenceA $ attachCell entrySettings grid <$> [1..cols] <*> [1..rows]

attachRows :: [AttrOp Label] -> Grid -> [String] -> IO ()
attachRows labelSettings grid names = void $ sequenceA $ zipWith ($) (attachRow labelSettings grid <$> names) [1..length names]

attachCols :: [AttrOp Label] -> Grid -> [String] -> IO ()
attachCols labelSettings grid names = void $ sequenceA $ zipWith ($) (attachCol labelSettings grid <$> names) [1..length names]

attachCorner :: [AttrOp Label] -> Grid -> String -> IO ()
attachCorner labelSettings grid name = labelNew (Just name) >>= (\label -> set label labelSettings *> gridAttach grid label 0 0 1 1)

rowNames :: Int -> [String]
rowNames rows = show <$> [1..rows]

colNames :: Int -> [String]
colNames cols = (pure.toEnum.(+64)) <$> [1..cols]

--------------------------------------------------------------------------------

rows = 30
cols = 20

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
  attachCells [entryWidthChars := 10] grid rows cols --there is a minimum width and height for entry boxes, not sure how to change this

  gridWindow <- scrolledWindowNew Nothing Nothing --adds scrolling for our grid
  containerAdd gridWindow grid

  containerAdd window gridWindow

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
