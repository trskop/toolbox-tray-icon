{-# LANGUAGE NoImplicitPrelude #-}
module Main.Options
    (optionsParser, execParser)
  where

import Control.Applicative (Applicative((<*>)))
import Control.Monad.Trans.Identity (IdentityT(IdentityT, runIdentityT))
import Data.Either (Either(Left, Right))
import Data.Function ((.), ($))
import Data.Functor ((<$>))
import Data.List (null)
import Data.Maybe (Maybe)
import Data.Monoid (Monoid(mempty), (<>))
import Data.String (String)
import System.IO (IO)

import Control.Lens (set)
import Data.Monoid.Endo (E, Endo(appEndo))
import Data.Monoid.Endo.Fold ((<&$>), foldEndo)
import Options.Applicative
    ( InfoMod
    , Parser
    , eitherReader
    , execParserPure
    , handleParseResult
    , help
    , info
    , long
    , metavar
    , option
    , optional
    , prefs
    , short
    )

import Main.Type.Options (Options)
import Main.Type.Options.Lens (configFile)


type Parser' = IdentityT Parser

parser' :: Parser a -> Parser' a
parser' = IdentityT

fromParser' :: Parser' a -> Parser a
fromParser' = runIdentityT

execParser :: Parser a -> InfoMod a -> [String] -> IO a
execParser parser infoMod =
    handleParseResult . execParserPure (prefs mempty) (info parser infoMod)

optionsParser :: Parser (E Options)
optionsParser = fromParser' $ appEndo <&$> foldEndo
    <*> configFileOption

configFileOption :: Parser' (Maybe (E Options))
configFileOption = parser' . optional . option (set configFile <$> parseFilePath)
    $ short 'c' <> long "config" <> metavar "FILE"
    <> help "Use specified FILE instead of default configuration file."
  where
    parseFilePath = eitherReader $ \s ->
        if null s
            then Left "Option argument can not be empty file path."
            else Right s
