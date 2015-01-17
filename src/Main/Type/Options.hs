{-# LANGUAGE NoImplicitPrelude #-}
module Main.Type.Options (Options(..))
  where

import System.IO (FilePath)
import Text.Show (Show)


data Options = Options {configFile :: FilePath}
  deriving (Show)
