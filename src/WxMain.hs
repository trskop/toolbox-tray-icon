{-# LANGUAGE NoImplicitPrelude #-}
module Main (main)
  where

import Control.Monad (Monad((>>=), return))
import Data.Bool ((||), otherwise)
import Data.Eq (Eq((==)))
import Data.Function (($), flip)
import Data.Functor ((<$))
import Data.String (String)
import System.Exit (exitSuccess)
import System.IO (IO)
import System.Process (system)

import Graphics.UI.WX
    ( Prop((:=))
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

import Paths_toolbox (getDataFileName)


taskBarIcon
    :: Icon ()
    -> String
    -> (TaskBarIcon () -> EventTaskBarIcon -> IO ())
    -> IO (TaskBarIcon ())
taskBarIcon icon str f = do
    tbi <- taskBarIconCreate
    _ <- taskBarIconSetIcon tbi icon str
    evtHandlerOnTaskBarIconEvent tbi $ f tbi
    return tbi

taskBarIcon_
    :: Icon ()
    -> String
    -> (TaskBarIcon () -> EventTaskBarIcon -> IO ())
    -> IO ()
taskBarIcon_ icon str f = () <$ taskBarIcon icon str f

main :: IO()
main = start $ do
    icon <- getDataFileName iconFile >>= flip iconCreateFromFile sizeNull
    taskBarIcon_ icon "Toolbox" $ \tbi evt -> case evt of
      _ |    evt == TaskBarIconLeftDown
            || evt == TaskBarIconRightDown -> do
                toolboxMenu <- menuPane [text := "Toolbox"]
                _ <- menuItem toolboxMenu
                    [ text := "Firefox ProfileManager (no remote)"
                    , help := "Something"
                    , on command :=
                        (() <$ system "firefox -no-remote -ProfileManager &")
                    ]
                _ <- menuItem toolboxMenu
                    [ text := "Exit toolbox"
                    , on command := exitSuccess
                    ]
                _ <- taskBarIconPopupMenu tbi toolboxMenu
                return ()

        | otherwise                      -> return ()
  where
--  iconFile = "icons/guillendesign-variations-3-tools-icon.png"
    iconFile = "icons/elegantthemes-tools-icon.png"
