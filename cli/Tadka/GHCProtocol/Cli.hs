-- | Command-line parsing for the @tadka-ghc@ executable (audit B13).
--
-- Lives in its own internal component so the parser can be tested
-- without spawning the executable, and so @optparse-applicative@ is a
-- dependency of the command line only, never of the core library.
--
-- Everything after the first @--@ is split off before the option parser
-- ever sees it and is passed through verbatim as extra build-tool
-- arguments. So @--help@, @-h@ or @--timeout=-1@ after @--@ belong to
-- the build tool, not to this program.
module Tadka.GHCProtocol.Cli
  ( CliOptions (..)
  , parseCliArgs
  , getCliOptions
  ) where

import Control.Applicative ((<|>))
import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Version (showVersion)
import qualified Options.Applicative as Opt
import System.Environment (getArgs)
import Text.Read (readMaybe)
import qualified Tadka

import Paths_tadka_ghc (version)
import Tadka.GHCProtocol
  ( BuildTool (..)
  , Timeout
  , mkTimeout
  , renderTimeoutError
  )

data CliOptions = CliOptions
  { cliDir       :: FilePath
  , cliTool      :: Maybe BuildTool
  , cliTarget    :: Maybe Tadka.Target
  , cliTimeout   :: Maybe Timeout
  , cliVerbose   :: Bool
  , cliExtraArgs :: [Text]
  } deriving stock (Eq, Show)

-- | Parse an argument vector. Pure, so every outcome (success, usage
-- error, @--help@, @--version@) can be asserted on in tests.
parseCliArgs :: [String] -> Opt.ParserResult CliOptions
parseCliArgs args =
  Opt.execParserPure (Opt.prefs Opt.showHelpOnError) (cliParserInfo (map Text.pack extra)) own
  where
    (own, extra) = splitAtDoubleDash args

-- | Parse the real command line. On @--help@ / @--version@ this prints
-- and exits with status 0; on a usage error it prints the problem and
-- the usage text to stderr and exits with status 2.
getCliOptions :: IO CliOptions
getCliOptions = getArgs >>= Opt.handleParseResult . parseCliArgs

-- | Split at the /first/ @--@; the separator itself is dropped. Later
-- occurrences of @--@ belong to the pass-through part.
splitAtDoubleDash :: [String] -> ([String], [String])
splitAtDoubleDash args = case break (== "--") args of
  (own, _ : extra) -> (own, extra)
  (own, [])        -> (own, [])

cliParserInfo :: [Text] -> Opt.ParserInfo CliOptions
cliParserInfo extra =
  Opt.info (optionsParser extra Opt.<**> versionOption Opt.<**> Opt.helper) $
       Opt.fullDesc
    <> Opt.progDesc
         "Runs the project's build (cabal build / stack build) with GHC's \
         \diagnostics-as-json protocol always enabled, and renders every \
         \diagnostic through Tadka."
    <> Opt.footer
         "Everything after -- is passed straight to the build tool. \
         \Exit status: the build's own exit code; 124 if the timeout was \
         \reached; 127 if the build tool could not be started; 130 if it \
         \was killed by a signal; 2 for a usage error."
    <> Opt.failureCode 2

versionOption :: Opt.Parser (a -> a)
versionOption =
  Opt.infoOption
    ("tadka-ghc " <> showVersion version)
    (Opt.long "version" <> Opt.help "Show the version and exit")

optionsParser :: [Text] -> Opt.Parser CliOptions
optionsParser extra =
  CliOptions
    <$> Opt.strArgument
          (Opt.metavar "DIR" <> Opt.value "." <> Opt.showDefault
             <> Opt.help "Project directory")
    <*> toolParser
    <*> targetParser
    <*> Opt.optional
          (Opt.option timeoutReader
             (Opt.long "timeout" <> Opt.metavar "SECONDS"
                <> Opt.help "Stop the build after this many seconds (greater than 0, at most one year)"))
    <*> Opt.switch
          (Opt.long "verbose" <> Opt.short 'v'
             <> Opt.help "Show every captured build-tool line, not just decoded diagnostics and (on failure) opaque output")
    <*> pure extra

-- | At most one of the two; naming both is a usage error rather than a
-- silent last-one-wins.
toolParser :: Opt.Parser (Maybe BuildTool)
toolParser =
      Opt.flag' (Just Cabal) (Opt.long "cabal" <> Opt.help "Force cabal (default: auto-detect)")
  <|> Opt.flag' (Just Stack) (Opt.long "stack" <> Opt.help "Force stack (default: auto-detect)")
  <|> pure Nothing

-- | At most one render target; default is Tadka's terminal auto-detection.
targetParser :: Opt.Parser (Maybe Tadka.Target)
targetParser =
      Opt.flag' (Just Tadka.TGraphical) (Opt.long "graphical" <> Opt.help "Render graphically")
  <|> Opt.flag' (Just Tadka.TNarratable) (Opt.long "narratable" <> Opt.help "Render as plain narrated text")
  <|> Opt.flag' (Just Tadka.TJson) (Opt.long "json" <> Opt.help "Render as JSON")
  <|> pure Nothing

-- | Every failure mode of 'mkTimeout' (not a number, NaN, infinite, zero,
-- negative, too large) surfaces as a usage error naming the bad value;
-- nothing invalid reaches the runner.
timeoutReader :: Opt.ReadM Timeout
timeoutReader = Opt.eitherReader $ \raw ->
  case readMaybe raw :: Maybe Double of
    Nothing      -> Left ("not a number: " <> raw)
    Just seconds ->
      first
        (\e -> Text.unpack (renderTimeoutError e) <> " (got " <> raw <> ")")
        (mkTimeout seconds)
