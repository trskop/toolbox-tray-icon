{-# LANGUAGE NoImplicitPrelude #-}
module Main.Type.Options.Lens (configFile)
  where

import Control.Lens (Lens', lens)
import System.IO (FilePath)

import Main.Type.Options (Options)
import qualified Main.Type.Options as Options (configFile)


configFile :: Lens' Options FilePath
configFile = Options.configFile `lens` \s b -> s{Options.configFile = b}
