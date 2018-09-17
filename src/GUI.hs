module GUI where

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk hiding (Action, backspace)
import Graphics.UI.Gtk.Layout.Grid
import Data.List.Split(chunksOf)
import Data.List(transpose)

--------------------------------------------------------------------------------

attachCell :: [AttrOp Entry] -> Grid -> Int -> Int -> IO ()
attachCell entrySettings grid x y = entryNew >>= (\entry -> set entry entrySettings *> gridAttach grid entry x y 1 1 *> widgetSetName entry (show x <> " " <> show y))

attachCol :: [AttrOp Label] -> Grid -> String -> Int -> IO ()
attachCol labelSettings grid name x = labelNew (Just name) >>= (\label -> set label labelSettings *> gridAttach grid label x 0 1 1 *> widgetSetName label (show x <> " 0"))

attachRow :: [AttrOp Label] -> Grid -> String -> Int -> IO ()
attachRow labelSettings grid name y = labelNew (Just name) >>= (\label -> set label labelSettings *> gridAttach grid label 0 y 1 1 *> widgetSetName label ("0 " <> show y))

attachCorner :: [AttrOp Label] -> Grid -> String -> IO ()
attachCorner labelSettings grid name = labelNew (Just name) >>= (\label -> set label labelSettings *> gridAttach grid label 0 0 1 1 *> widgetSetName label "0 0")

--------------------------------------------------------------------------------

attachCells :: [AttrOp Entry] -> Grid -> Int -> Int -> IO ()
attachCells entrySettings grid rows cols = void $ sequenceA $ attachCell entrySettings grid <$> [1..cols] <*> [1..rows]

attachRows :: [AttrOp Label] -> Grid -> [String] -> IO ()
attachRows labelSettings grid names = void $ sequenceA $ zipWith ($) (attachRow labelSettings grid <$> names) [1..length names]

attachCols :: [AttrOp Label] -> Grid -> [String] -> IO ()
attachCols labelSettings grid names = void $ sequenceA $ zipWith ($) (attachCol labelSettings grid <$> names) [1..length names]

--------------------------------------------------------------------------------

rowNames :: Int -> [String]
rowNames rows = show <$> [1..rows]

colNames :: Int -> [String]
colNames cols = (pure.toEnum.(+64)) <$> [1..cols]

--------------------------------------------------------------------------------

getCell :: [Entry] -> Int -> Int -> Int -> Int -> Entry
getCell list rows cols x y = list !! ((rows - 1) * (cols - 1) - y - (x * rows))

writeToCell :: Entry -> String -> IO ()
writeToCell cell text = set cell [entryText := text]
