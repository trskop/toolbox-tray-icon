{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Read and parse configuration files.
-- Copyright:    (c) 2015, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- Read and parse configuration files.
module Main.ConfigFile
    ( defaultConfigFileName
    , readConfigFile
    , readUserConfigFile
    , readConfigFile'
    , getMenuItems
    )
  where

import Control.Monad (Monad((>>=), return), (=<<))
import Data.Bool (otherwise)
import Data.Either (Either(Left, Right))
import Data.Eq (Eq((==)))
import Data.Foldable (Foldable, foldlM)
import Data.Function (const)
import Data.Functor ((<$>))
import Data.Monoid (Monoid(mempty), (<>))
import Data.String (String)
import System.IO (IO, FilePath)

import Data.ByteString (readFile)
import System.Directory (doesFileExist)

import Control.Lens ((^.))
import Data.Aeson (eitherDecodeStrict')

import Main.Type.Options (Options)
import Main.Type.Options.Lens (configFile)
import Main.Type.MenuItem (MenuItems)


-- | Default configuration file name.
--
-- @
-- 'defaultConfigFileName' = \"toolbox-menu.json\"
-- @
defaultConfigFileName :: FilePath
defaultConfigFileName = "toolbox-menu.json"

-- | Generalized implementation of 'readConfigFile' and 'readUserConfigFile'.
readConfigFileImpl
    :: (Options -> FilePath)
    -- ^ Get configuration file path supplied by the user. See e.g.
    -- 'configFile'.
    -> (FilePath -> IO FilePath)
    -- ^ Get absoulete file path for relative path to a config file. This is
    -- where 'Paths.toolbox.getDataFileName' would be used.
    -> Options
    -> IO (Either String MenuItems)
readConfigFileImpl g f opts = readConfigFile' =<< case g opts of
    fileName@(c : _)
      | c == '/'  -> return fileName
        -- Leave absolute path as it is.
      | otherwise -> f fileName
        -- Find config file using provided funcion in case of relative path.
    ""            -> f defaultConfigFileName
        -- Empty path means "use default configuration file."

-- | Read configuration file either one provided with the installation or one
-- supplied by the user on command line.
readConfigFile
    :: (FilePath -> IO FilePath)
    -- ^ Get absoulete file path for relative path to a config file. This is
    -- where 'Paths.toolbox.getDataFileName' would be used.
    -> Options
    -> IO (Either String MenuItems)
readConfigFile = readConfigFileImpl (^. configFile)

-- | Read user configuration file. This differs from system configuration file.
readUserConfigFile
    :: (FilePath -> IO FilePath)
    -> Options
    -> IO (Either String MenuItems)
readUserConfigFile = readConfigFileImpl (const "")

-- | Read and parse configuration file (JSON). If configuration file doesn't
-- exist then @Right 'mempty' :: Either String 'MenuItems'@ is returned.
readConfigFile' :: FilePath -> IO (Either String MenuItems)
readConfigFile' file = do
    fileExist <- doesFileExist file
    if fileExist
        then eitherDecodeStrict' <$> readFile file
        else return (Right mempty)

-- | Gather menu items from various side-effect sources and handle error cases.
getMenuItems
    :: Foldable f
    => Options
    -> (String -> IO MenuItems)
    -- ^ Error handling function.
    -> f (Options -> IO (Either String MenuItems))
    -> IO MenuItems
getMenuItems opts onError = foldlM go mempty
  where
    go :: MenuItems -> (Options -> IO (Either String MenuItems)) -> IO MenuItems
    go items f = f opts >>= processResult items

    processResult :: MenuItems -> Either String MenuItems -> IO MenuItems
    processResult items r = case r of
        Right x  -> return (items <> x)
        Left msg -> onError msg
