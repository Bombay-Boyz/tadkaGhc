-- | Build-tool detection, diagnostics-flag injection, and chunk-to-line
-- framing (vision §3; spec Phase 6.2, 6.3, 6.5). All pure except
-- 'detectBuildTool', which only inspects a directory listing -- it does
-- not compile or run anything, and is the one function in this module
-- that touches IO at all.
module Tadka.GHCProtocol.BuildTool
  ( -- * Build tool identity
    BuildTool (..)
  , BuildToolDetectionError (..)
  , detectBuildTool
    -- * Flag injection
  , DiagnosticsJsonFlag (..)
  , classifyGhcFlag
  , extractGhcOptionTokens
  , diagnosticsJsonCurrentlyEnabled
  , injectDiagnosticsFlag
    -- * Chunk-to-line framing
  , FramerState
  , emptyFramerState
  , feedChunk
  , splitCompleteLines
  , flushFramer
  ) where

import qualified Data.ByteString as BS
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory (listDirectory)
import System.FilePath (takeExtension)

--------------------------------------------------------------------------------
-- Build tool identity and detection (§3, spec Phase 6.2).
--------------------------------------------------------------------------------

data BuildTool = Cabal | Stack
  deriving stock (Eq, Show)

data BuildToolDetectionError
  = NoRecognizedProjectFile
  | AmbiguousProjectFiles (NonEmpty FilePath)
  deriving stock (Eq, Show)

-- | 'Just' bypasses detection (and the filesystem check) entirely,
-- forcing the given tool; 'Nothing' performs the detection rule below.
-- Search root is exactly the 'FilePath' argument; search depth is that
-- one directory's immediate contents only, never a recursive walk or
-- an ancestor directory.
detectBuildTool :: Maybe BuildTool -> FilePath -> IO (Either BuildToolDetectionError BuildTool)
detectBuildTool (Just tool) _   = pure (Right tool)
detectBuildTool Nothing     dir = classifyEntries <$> listDirectory dir

-- | Total: presence of @stack.yaml@ (exact casing) selects 'Stack'
-- regardless of any @.cabal@/@cabal.project@ files also present, since
-- Stack projects routinely carry a generated @.cabal@ file too. More
-- than one @*.cabal@ file (and no @stack.yaml@) is ambiguous -- there is
-- no principled way to prefer one without caller input.
classifyEntries :: [FilePath] -> Either BuildToolDetectionError BuildTool
classifyEntries entries
  | hasStackYaml = Right Stack
  | otherwise = case cabalFiles of
      (c1 : c2 : cs) -> Left (AmbiguousProjectFiles (c1 :| (c2 : cs)))
      [_]            -> Right Cabal
      []
        | hasCabalProject -> Right Cabal
        | otherwise       -> Left NoRecognizedProjectFile
  where
    hasStackYaml    = "stack.yaml" `elem` entries
    hasCabalProject = "cabal.project" `elem` entries
    cabalFiles      = filter (\f -> takeExtension f == ".cabal") entries

--------------------------------------------------------------------------------
-- Flag injection (§3, spec Phase 6.3). A naive substring search is
-- unsound: "-fno-diagnostics-as-json" contains "-fdiagnostics-as-json"
-- as a substring. Real GHC option tokens are recognised instead.
--------------------------------------------------------------------------------

data DiagnosticsJsonFlag = EnableDiagnosticsJson | DisableDiagnosticsJson
  deriving stock (Eq, Show)

classifyGhcFlag :: Text -> Maybe DiagnosticsJsonFlag
classifyGhcFlag "-fdiagnostics-as-json"    = Just EnableDiagnosticsJson
classifyGhcFlag "-fno-diagnostics-as-json" = Just DisableDiagnosticsJson
classifyGhcFlag _                          = Nothing

-- | Total: a single @--ghc-options=<value>@ argument may itself contain
-- several whitespace-separated GHC flags, and cabal/stack both
-- accumulate (rather than override) repeated @--ghc-options@
-- occurrences, so every occurrence is expanded and concatenated, left
-- to right.
extractGhcOptionTokens :: [Text] -> [Text]
extractGhcOptionTokens = concatMap tokensOf
  where
    tokensOf arg = case Text.stripPrefix "--ghc-options=" arg of
      Just value -> Text.words value
      Nothing    -> []

-- | Total: GHC applies repeated boolean -f/-fno flags in the order
-- given on the command line, so the last occurrence of either form
-- determines the effective setting.
diagnosticsJsonCurrentlyEnabled :: [Text] -> Bool
diagnosticsJsonCurrentlyEnabled userArgs =
  foldl' step False (mapMaybe classifyGhcFlag (extractGhcOptionTokens userArgs))
  where
    step _ EnableDiagnosticsJson  = True
    step _ DisableDiagnosticsJson = False

-- | Pure, total, idempotent. Never deletes or edits a user-supplied
-- argument (§32 invariant #18: "preserved, not overridden") -- only
-- ever appends its own occurrence after everything the user supplied.
-- Because GHC's last-occurrence-wins rule then applies, appending after
-- the user's own arguments guarantees the flag is effectively enabled
-- even if the user's own configuration disabled it explicitly, without
-- textually removing anything the user wrote.
injectDiagnosticsFlag :: BuildTool -> [Text] -> [Text]
injectDiagnosticsFlag _tool userArgs
  | diagnosticsJsonCurrentlyEnabled userArgs = userArgs
  | otherwise = userArgs <> ["--ghc-options=-fdiagnostics-as-json"]

--------------------------------------------------------------------------------
-- Chunk-to-line framing (spec Phase 6.5). An OS pipe delivers arbitrary
-- byte chunks, not lines.
--------------------------------------------------------------------------------

-- | Bytes seen since the last '\n' for one stream, not yet a complete
-- line. Constructor deliberately not exported -- construct via
-- 'emptyFramerState', mutate only via 'feedChunk'.
newtype FramerState = FramerState BS.ByteString
  deriving stock (Show)

emptyFramerState :: FramerState
emptyFramerState = FramerState BS.empty

-- | Total: feeds one raw, non-newline-aligned chunk into the framer,
-- returning every complete line it now contains (excluding the
-- trailing '\n') and the updated pending state.
feedChunk :: FramerState -> BS.ByteString -> (FramerState, [BS.ByteString])
feedChunk (FramerState pending) chunk =
  let (complete, rest) = splitCompleteLines (pending <> chunk)
  in (FramerState rest, complete)

-- | Total, structural recursion on the '\n' positions found in the
-- buffer; terminates because each recursive call strictly shortens its
-- input.
splitCompleteLines :: BS.ByteString -> ([BS.ByteString], BS.ByteString)
splitCompleteLines bs = case BS.elemIndex 10 {- '\n' -} bs of
  Nothing -> ([], bs)
  Just i  ->
    let (line, rest)      = BS.splitAt i bs
        (more, remainder) = splitCompleteLines (BS.drop 1 rest)
    in (line : more, remainder)

-- | Total: called once a pipe reaches EOF, to flush a final
-- unterminated line rather than silently dropping it.
flushFramer :: FramerState -> [BS.ByteString]
flushFramer (FramerState pending)
  | BS.null pending = []
  | otherwise       = [pending]
