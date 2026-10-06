-- | Resolving GHC-reported source paths inside a project (audit D3), and
-- saying why an excerpt is missing. The two-package layout in
-- 'capturedLayout' is the one captured from real GHC 9.14.1 + cabal 3.16
-- output: GHC reported the PACKAGE-RELATIVE path @src/Lib.hs@, and a
-- decoy file at the project root's @src/Lib.hs@ was displayed as the
-- excerpt for package b's error.
module SourceResolutionTests
  ( sourceResolutionTests
  ) where

import Control.Exception (finally)
import Control.Monad ((>=>), forM_)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Text as Text
import System.Directory
  ( createDirectory
  , createDirectoryIfMissing
  , createDirectoryLink
  , getTemporaryDirectory
  , removeDirectoryRecursive
  , removeFile
  )
import System.FilePath (takeDirectory, (</>))
import System.Info (os)
import System.IO (hClose, openTempFile)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit

import Tadka.GHCProtocol
import Tadka.GHCProtocol.Span (PathResolution (..), discoverPackageRoots, resolveSpanPath)

-- | Build a project tree in a fresh temporary directory, run the action
-- on its path, and remove the tree afterwards.
withProject :: [(FilePath, String)] -> (FilePath -> IO a) -> IO a
withProject files act = do
  tmp <- getTemporaryDirectory
  (path, h) <- openTempFile tmp "tadka-ghc-proj"
  hClose h
  removeFile path
  createDirectory path
  ( do forM_ files $ \(rel, content) -> do
         createDirectoryIfMissing True (takeDirectory (path </> rel))
         writeFile (path </> rel) content
       act path
    ) `finally` removeDirectoryRecursive path

-- | The project captured in the evidence run: package roots a and b, each
-- with src/Lib.hs, plus a decoy at the project root (which is not a
-- package root: it has no .cabal file).
capturedLayout :: [(FilePath, String)]
capturedLayout =
  [ ("cabal.project", "packages: a b\n")
  , ("a/a.cabal", ""),  ("a/src/Lib.hs", "module Lib (ok) where\n")
  , ("b/b.cabal", ""),  ("b/src/Lib.hs", "module Lib (x) where\n\nx :: Int\nx = \"s\"\n")
  , ("src/Lib.hs", "-- DECOY line 1\n-- DECOY line 2\n-- DECOY line 3\n-- DECOY line 4\n")
  ]

lookupIn :: FilePath -> FilePath -> IO (Either SourceLookupError (Maybe SourceText))
lookupIn dir path = do
  provider <- projectSourceProvider dir
  lookupSource provider path

spanAt :: FilePath -> Integer -> Integer -> Integer -> Integer -> IO GhcSpan
spanAt file sl sc el ec =
  either (assertFailure . show) pure $ do
    l1 <- mkLine sl
    c1 <- mkColumn sc
    l2 <- mkLine el
    c2 <- mkColumn ec
    mkGhcSpan file l1 c1 l2 c2

sourceResolutionTests :: [TestTree]
sourceResolutionTests =
  [ testCase "package roots: .cabal and package.yaml directories, sorted; build and hidden directories skipped" $
      withProject
        [ ("p2/package.yaml", ""), ("p1/p1.cabal", "")
        , ("dist-newstyle/build/x/ghost.cabal", ""), ("dist/ghost2.cabal", "")
        , (".hidden/h.cabal", ""), (".stack-work/s.cabal", "")
        , ("node_modules/n/n.cabal", "")
        ] $ \dir -> do
          roots <- discoverPackageRoots dir
          roots @?= [dir </> "p1", dir </> "p2"]

  , testCase "the project directory itself is a root when it holds a .cabal file" $
      withProject [("x.cabal", ""), ("src/Lib.hs", "")] $ \dir -> do
        roots <- discoverPackageRoots dir
        roots @?= [dir]

  , testCase "a directory named like a cabal file does not count as a package root" $
      withProject [("d.cabal/inside.txt", "")] $
        discoverPackageRoots >=> (@?= [])

  , testCase "roots are found down to the depth limit and not beyond it" $
      withProject
        [ ("d1/d2/d3/d4/d5/d6/p.cabal", "")          -- 6 levels down: found
        , ("e1/e2/e3/e4/e5/e6/e7/q.cabal", "")       -- 7 levels down: not found
        ] $ \dir -> do
          roots <- discoverPackageRoots dir
          roots @?= [dir </> "d1/d2/d3/d4/d5/d6"]

  , testCase "symbolic links to directories are not followed (no phantom roots from a loop)" $
      if os == "mingw32"
        then pure ()
        else withProject [("p.cabal", "")] $ \dir -> do
          createDirectoryLink "." (dir </> "loop")
          roots <- discoverPackageRoots dir
          roots @?= [dir]

  , testCase "D3 as captured: an ambiguous package-relative path is refused, and the root decoy is not a candidate" $
      withProject capturedLayout $ \dir ->
        lookupIn dir "src/Lib.hs"
          >>= (@?= Left (SourceAmbiguousPath "src/Lib.hs" ("a/src/Lib.hs" :| ["b/src/Lib.hs"])))

  , testCase "D3 as captured: the span is NOT bound to the decoy (SpanSourceUnavailable, never SpanBound)" $
      withProject capturedLayout $ \dir -> do
        provider <- projectSourceProvider dir
        sp       <- spanAt "src/Lib.hs" 4 5 4 8
        st       <- bindSpan provider (Just sp)
        case st of
          SpanSourceUnavailable _ (SourceAmbiguousPath _ _) -> pure ()
          SpanBound _ -> assertFailure "bound to a file although two packages have src/Lib.hs"
          other       -> assertFailure ("unexpected span state: " <> show other)

  , testCase "a path present in exactly one package resolves, even with a decoy at the root" $
      withProject
        [ ("a/a.cabal", ""), ("a/src/A.hs", "-- a\n")
        , ("b/b.cabal", ""), ("b/src/Lib.hs", "module Lib where\n")
        , ("src/Lib.hs", "-- DECOY\n")
        ] $ \dir ->
          lookupIn dir "src/Lib.hs" >>= (@?= Right (Just (SourceText "module Lib where\n")))

  , testCase "a single-package project resolves its own files" $
      withProject [("p.cabal", ""), ("src/Lib.hs", "module Lib where\n")] $ \dir ->
        lookupIn dir "src/Lib.hs" >>= (@?= Right (Just (SourceText "module Lib where\n")))

  , testCase "with no package roots at all, the project directory is used (old behaviour kept)" $
      withProject [("src/Lib.hs", "module Lib where\n")] $ \dir ->
        lookupIn dir "src/Lib.hs" >>= (@?= Right (Just (SourceText "module Lib where\n")))

  , testCase "a path that exists nowhere is 'no such source', not an error" $
      withProject [("p.cabal", "")] $ \dir ->
        lookupIn dir "src/Nope.hs" >>= (@?= Right Nothing)

  , testCase "pseudo file names are simply not found" $
      withProject [("p.cabal", "")] $ \dir ->
        lookupIn dir "<interactive>" >>= (@?= Right Nothing)

  , testCase "the same file reached from two packages through .. counts once, so it resolves" $
      withProject
        [ ("a/a.cabal", ""), ("b/b.cabal", ""), ("shared/Foo.hs", "module Foo where\n") ] $ \dir -> do
          r <- resolveSpanPath [dir </> "a", dir </> "b"] "../shared/Foo.hs"
          case r of
            Resolved _ -> pure ()
            other      -> assertFailure ("expected Resolved, got " <> show other)

  , testCase "an absolute path is used as is" $
      withProject [("p.cabal", ""), ("src/Lib.hs", "x\n")] $ \dir -> do
        resolveSpanPath [dir] (dir </> "src/Lib.hs") >>= (@?= Resolved (dir </> "src/Lib.hs"))
        resolveSpanPath [dir] (dir </> "src/None.hs") >>= (@?= NotFound)

  , testCase "an ambiguous-path error names the path and every candidate" $
      renderSourceLookupError (SourceAmbiguousPath "src/Lib.hs" ("a/src/Lib.hs" :| ["b/src/Lib.hs"]))
        @?= "\"src/Lib.hs\" exists in more than one package (a/src/Lib.hs, b/src/Lib.hs), so no file was chosen"

  , testCase "a missing excerpt is explained: one sentence per state, none when there is nothing to explain" $ do
      sp  <- spanAt "src/Lib.hs" 4 5 4 8
      sp' <- spanAt "<interactive>" 1 1 1 2
      l   <- either (assertFailure . show) pure (mkLine 4)
      renderSpanStateNote NoSpan @?= Nothing
      renderSpanStateNote (SpanNoSource sp')
        @?= Nothing
      renderSpanStateNote (SpanNoSource sp)
        @?= Just "source excerpt omitted for src/Lib.hs: the file was not found in the project"
      renderSpanStateNote (SpanSourceUnavailable sp (SourceAmbiguousPath "src/Lib.hs" ("a/src/Lib.hs" :| ["b/src/Lib.hs"])))
        @?= Just "source excerpt omitted for src/Lib.hs: \"src/Lib.hs\" exists in more than one package (a/src/Lib.hs, b/src/Lib.hs), so no file was chosen"
      case renderSpanStateNote (SpanSourceUnavailable sp (SourceIOError "permission denied")) of
        Just t  -> assertBool (Text.unpack t) ("permission denied" `Text.isInfixOf` t)
        Nothing -> assertFailure "expected a note for an IO failure"
      case renderSpanStateNote (SpanInvalidCoordinates sp (InvalidCoordinates sp (LineOutOfRange l))) of
        Just t  -> assertBool (Text.unpack t) ("line 4 is beyond the end of the source" `Text.isInfixOf` t)
        Nothing -> assertFailure "expected a note for invalid coordinates"
      case convertSpan (SourceText "a\nb\nc\nx = \"s\"\n") sp of
        Right ctx -> renderSpanStateNote (SpanBound ctx) @?= Nothing
        Left e    -> assertFailure (show e)
  ]
