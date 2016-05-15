{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Tray icon with configurable popup menu that uses wxWidgets as
--               a backend.
-- Copyright:    (c) 2015 Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- Tray icon with configurable popup menu that uses wxWidgets as a backend.
module Main (main)
  where

import Control.Exception (SomeException, handle)
import Control.Monad (Monad((>>=), return), (=<<), mapM_, void)
import Data.Bool ((||), otherwise)
import Data.Eq (Eq((==)))
import Data.Function ((.), ($), flip)
import Data.Functor ((<$))
import Data.Monoid ((<>))
import Data.String (String)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (IO, hPrint, hPutStrLn, stderr)

import System.FilePath ((</>))
import System.Process (system)

import Control.Lens ((^.), foldMapOf)
import Graphics.UI.WX
    ( Menu
    , Prop((:=))
    , command
    , help
    , menuItem
    , menuPane
    , on
    , start
    , text
    )
import Graphics.UI.WXCore.Events
    ( EventTaskBarIcon(TaskBarIconLeftDown, TaskBarIconRightDown)
    , evtHandlerOnTaskBarIconEvent
    )
import Graphics.UI.WXCore.Image (iconCreateFromFile)
import Graphics.UI.WXCore.WxcClasses
    ( taskBarIconCreate
    , taskBarIconSetIcon
    , taskBarIconPopupMenu
    )
import Graphics.UI.WXCore.WxcClassTypes (Icon, TaskBarIcon)
import Graphics.UI.WXCore.WxcTypes (sizeNull)
import Options.Applicative (fullDesc)
import System.Environment.XDG.BaseDir (getUserConfigFile)

import Main.ConfigFile (getMenuItems, readConfigFile, readUserConfigFile)
import Main.Options (execParser, optionsParser)
import Main.Type.MenuItem (MenuItem)
import Main.Type.MenuItem.Lens (menuItems)
import qualified Main.Type.MenuItem.Lens as MenuItem
    ( command
    , description
    , name
    )
import qualified Main.Type.Options.Lens as Options (iconFile)

import Paths_toolbox (getDataFileName)


taskBarIcon
    :: Icon ()
    -> String
    -> (TaskBarIcon () -> EventTaskBarIcon -> IO ())
    -> IO ()
taskBarIcon icon str f = do
    tbi <- taskBarIconCreate
    _ <- taskBarIconSetIcon tbi icon str
    evtHandlerOnTaskBarIconEvent tbi $ f tbi

main :: IO ()
main = getArgs >>= execParser optionsParser fullDesc >>= \options -> start $ do
    menu <- getMenuItems options onConfigError
        [ readConfigFile getDataFileName
        , readUserConfigFile (getUserConfigFile "toolbox")
        ]

    icon <- flip iconCreateFromFile sizeNull
        =<< case options ^. Options.iconFile of
            fileName@(c : _)
              | c == '/'  -> return fileName
              | otherwise -> getDataFileName fileName
            ""            -> getDataFileName $ "icons" </> iconFileName

    toolboxMenu <- menuPane [text := "Toolbox"]
    foldMapOf menuItems (mapM_ $ addMenuItem toolboxMenu) menu
    addMenuItem' toolboxMenu "Exit toolbox" "Exit toolbox" exitSuccess

    taskBarIcon icon "Toolbox" $ \tbi evt -> case evt of
        _ | evt == TaskBarIconLeftDown || evt == TaskBarIconRightDown
            -> () <$ taskBarIconPopupMenu tbi toolboxMenu
          | otherwise
            -> return ()
  where
    iconFileName = "elegantthemes-tools-icon.png"
--  iconFileName = "elegantthemes-beautiful-flat-icons-tools-icon.png"

    handleExceptions = handle $ \e -> hPrint stderr (e :: SomeException)

    addMenuItem :: Menu a -> MenuItem -> IO ()
    addMenuItem m item = addMenuItem' m
        (item ^. MenuItem.name)
        (item ^. MenuItem.description)
        . handleExceptions . void . system $ item ^. MenuItem.command

    addMenuItem' :: Menu a -> String -> String -> IO () -> IO ()
    addMenuItem' m name desc action = () <$ menuItem m
        [ text       := name
        , help       := desc
        , on command := action
        ]

    onConfigError msg = do
        hPutStrLn stderr $ "Parsing configuration failed: " <> msg
        exitFailure
