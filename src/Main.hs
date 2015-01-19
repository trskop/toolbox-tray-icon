{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
-- |
-- Module:       $HEADER$
-- Description:  Tray icon with configurable popup menu.
-- Copyright:    (c) 2015, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- Tray icon with configurable popup menu.
module Main (main)
  where

import Control.Exception (SomeException, handle)
import Control.Monad (Monad(return, (>>=)), (=<<), mapM_, void)
import Data.Bool (otherwise)
import Data.Either (Either(Left, Right))
import Data.Eq (Eq((==)))
import Data.Function ((.), ($), flip)
import Data.Functor ((<$>))
import Data.List ((++), concatMap)
import Data.Maybe (Maybe(Nothing), fromJust)
import Data.String (String)
import System.Exit (exitFailure, exitSuccess)
import System.IO (IO, hPutStrLn, stderr)
import Text.Show (Show(show))

import System.FilePath ((</>))
import System.Process (system)

import Control.Lens ((^.), foldMapOf)
import Data.String.Here.Interpolated (i)
import Graphics.UI.Gtk (initGUI, mainGUI, on)
import Graphics.UI.Gtk.Display.StatusIcon
    ( statusIconNewFromFile
    , statusIconActivate
    , statusIconPopupMenu
    )
import Graphics.UI.Gtk.ActionMenuToolbar.Action (actionNew, actionActivated)
import Graphics.UI.Gtk.ActionMenuToolbar.ActionGroup
    ( ActionGroup
    , actionGroupNew
    , actionGroupAddAction
    )
import Graphics.UI.Gtk.ActionMenuToolbar.UIManager
    ( uiManagerNew
    , uiManagerAddUiFromString
    , uiManagerGetWidget
    , uiManagerInsertActionGroup
    )
import Graphics.UI.Gtk.MenuComboToolbar.Menu (castToMenu, menuPopup)
import Options.Applicative (fullDesc)

import Main.ConfigFile (readConfigFile)
import Main.Options (execParser, optionsParser)
import Main.Type.MenuItem (MenuItem(MenuItem), MenuItems)
import qualified Main.Type.MenuItem as MenuItem (MenuItem(id))
import Main.Type.MenuItem.Lens (menuItems)
import qualified Main.Type.MenuItem.Lens as MenuItem.Lens (command, id, name)
import qualified Main.Type.Options.Lens as Options (iconFile)

import Paths_toolbox (getDataFileName)


popupMenuXml :: MenuItems -> String
popupMenuXml items = [i|
    <popup>
      ${menuItemsXml}
      <menuitem action="exit" />
    </popup> |]
  where
    menuItemsXml = flip (foldMapOf menuItems) items . concatMap
        $ \MenuItem{MenuItem.id = itemId} ->
            [i| <menuitem action="${itemId}" /> |]

main :: IO()
main = do
    options <- initGUI >>= execParser optionsParser fullDesc

    menu <- readConfigFile getDataFileName options >>= \r -> case r of
        Right x  -> return x
        Left msg -> do
            hPutStrLn stderr $ "Parsing configuration failed: " ++ msg
            exitFailure

    icon <- statusIconNewFromFile =<< case options ^. Options.iconFile of
        fileName@(c : _)
          | c == '/'  -> return fileName
          | otherwise -> getDataFileName fileName
        ""            -> getDataFileName $ "icons" </> iconFileName
          where
            iconFileName = "elegantthemes-tools-icon.png"
--          iconFileName = "elegantthemes-beautiful-flat-icons-tools-icon.png"

    group <- actionGroupNew "PopupMenuActions"
    foldMapOf menuItems (mapM_ (createMenuItem group)) menu
    createMenuItem' group "exit" "Exit toolbox" exitSuccess

    manager <- uiManagerNew
    _ <- uiManagerAddUiFromString manager $ popupMenuXml menu
    uiManagerInsertActionGroup manager group 0

    popupMenu <- castToMenu . fromJust <$> uiManagerGetWidget manager "/popup"
    _ <- on icon statusIconActivate $ menuPopup' popupMenu
    _ <- on icon statusIconPopupMenu $ (\m _ _ -> menuPopup' m) popupMenu

    mainGUI
  where
    menuPopup' = flip menuPopup Nothing

    handleExceptions = handle $ \e ->
        hPutStrLn stderr $ show (e :: SomeException)

    createMenuItem :: ActionGroup -> MenuItem -> IO ()
    createMenuItem group item = createMenuItem' group
        (item ^. MenuItem.Lens.id)
        (item ^. MenuItem.Lens.name)
        . handleExceptions . void . system $ item ^. MenuItem.Lens.command

    createMenuItem' :: ActionGroup -> String -> String -> IO () -> IO ()
    createMenuItem' group itemId itemName itemAction = do
        action <- actionNew itemId itemName Nothing Nothing
        _ <- on action actionActivated itemAction
        actionGroupAddAction group action
