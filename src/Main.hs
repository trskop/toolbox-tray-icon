{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main)
  where

import Control.Monad (Monad((>>=)), mapM_, when)
import Data.Eq (Eq((/=)))
import Data.Function ((.), ($), flip)
import Data.Functor ((<$>))
import Data.List (concatMap)
import Data.Maybe (Maybe(Nothing), fromJust)
import Data.String (String)
import Data.Typeable (Typeable)
import System.Command (cmd)
import System.Exit (ExitCode(ExitSuccess), exitSuccess)
import System.IO (IO, print)

import System.FilePath ((</>))

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

import Paths_toolbox (getDataFileName)


data MenuItem = MenuItem
    { menuId     :: String
    , menuText   :: String
    , menuAction :: IO ()
    }
  deriving (Typeable)

menuItems :: [MenuItem]
menuItems =
    [ MenuItem "something" "Something"    (cmd "false" >>= printResult)
    , MenuItem "exit"      "Exit toolbox" exitSuccess
    ]
  where
    printResult ec = when (ec /= ExitSuccess) $ print ec

menuItemsXml :: String
menuItemsXml = concatMap menuItemToXml menuItems
  where
    menuItemToXml MenuItem{menuId} = [i| <menuitem action="${menuId}" /> |]

popupMenuXml :: String
popupMenuXml = [i| <popup>${menuItemsXml}</popup> |]

main :: IO()
main = do
    args <- initGUI
    print args -- TODO

    iconFile <- getDataFileName $ "icons" </> iconFileName
    icon <- statusIconNewFromFile iconFile

    group <- actionGroupNew "PopupMenuActions"
    mapM_ (createMenuItem group) menuItems

    manager <- uiManagerNew
    _ <- uiManagerAddUiFromString manager popupMenuXml
    uiManagerInsertActionGroup manager group 0

    popupMenu <- castToMenu . fromJust <$> uiManagerGetWidget manager "/popup"
    _ <- on icon statusIconActivate $ menuPopup' popupMenu
    _ <- on icon statusIconPopupMenu $ (\menu _ _ -> menuPopup' menu) popupMenu

    mainGUI
  where
    menuPopup' = flip menuPopup Nothing

    createMenuItem :: ActionGroup -> MenuItem -> IO ()
    createMenuItem group MenuItem{..} = do
        action <- actionNew menuId menuText Nothing Nothing
        _ <- on action actionActivated menuAction
        actionGroupAddAction group action

    iconFileName = "elegantthemes-tools-icon.png"
--  iconFileName = "elegantthemes-beautiful-flat-icons-tools-icon.png"
