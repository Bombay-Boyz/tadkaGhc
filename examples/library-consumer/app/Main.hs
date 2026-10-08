-- | Using tadka-ghc as a library: decode one GHC diagnostic, bind it to
-- source text, and render it through Tadka.
--
-- Everything used here comes from the single supported import,
-- "Tadka.GHCProtocol".
module Main (main) where

import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as Text
import qualified Tadka

import Tadka.GHCProtocol

-- | One line of GHC's @-fdiagnostics-as-json@ output, exactly as GHC
-- 9.10.3 printed it for the program below (captured from a real Stack
-- build).
ghcOutputLine :: BS8.ByteString
ghcOutputLine = BS8.pack $ concat
  [ "{\"version\":\"1.0\",\"ghcVersion\":\"ghc-9.10.3\""
  , ",\"span\":{\"file\":\"src/Main.hs\",\"start\":{\"line\":3,\"column\":18},\"end\":{\"line\":3,\"column\":26}}"
  , ",\"severity\":\"Error\",\"code\":83865"
  , ",\"message\":[\"Couldn't match type `Int' with `[Char]'\\nExpected: String\\n  Actual: Int\""
  , ",\"In the first argument of `putStrLn', namely `(1 :: Int)'\\nIn the expression: putStrLn (1 :: Int)\\nIn an equation for `main': main = putStrLn (1 :: Int)\"]"
  , ",\"hints\":[]}"
  ]

-- | The source file GHC was compiling when it printed that line.
sourceText :: Text.Text
sourceText = Text.pack "module Main (main) where\nmain :: IO ()\nmain = putStrLn (1 :: Int)\n"

main :: IO ()
main = do
  -- 1. Decode. Total: malformed input is a typed error, never a crash.
  diagnostic <- case decodeDiagnosticLine ghcOutputLine of
    Right d -> pure d
    Left e  -> fail ("could not decode: " <> Text.unpack (renderDecodeError e))

  -- 2. Bind the span to real source text. Here the source comes from
  --    memory; 'projectSourceProvider' reads files from a project.
  let provider = SourceProvider (\_ -> pure (Right (Just (SourceText sourceText))))
  spanState <- bindSpan provider (ghcSpan diagnostic)

  -- 3. Render through Tadka (graphical, narratable, or JSON).
  Tadka.reportDiagnostic
    (Tadka.withTarget Tadka.TNarratable Tadka.defaultConfig)
    (BoundGhcDiagnostic diagnostic spanState)

  -- GHC's own facts stay available to the caller, unmodified.
  putStrLn ("GHC version: " <> Text.unpack (unGhcVersion (ghcVersion diagnostic)))
  putStrLn ("GHC code:    " <> maybe "none" (show . unGhcDiagnosticCode) (ghcCode diagnostic))
