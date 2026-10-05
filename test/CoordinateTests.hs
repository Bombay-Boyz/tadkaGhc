-- | GHC column conventions (audit B5), pinned by real GHC 9.14.1 output
-- (test/fixtures/coordinate/tab-real-*, see PROVENANCE-tab-real.txt):
-- columns count characters, a tab advances to the next multiple of 8
-- plus 1, and a span's end column is exclusive.
module CoordinateTests
  ( tabTests
  ) where

import Control.Monad (forM_, unless)
import Data.Bits (shiftL, shiftR)
import Data.Text (Text)
import qualified Data.Text as Text
import Hedgehog (assert, evalEither, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Tadka
import Test.Tasty (TestTree)
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog (testProperty)

import Tadka.GHCProtocol
import Tadka.GHCProtocol.Span (computeLineMetadata, coordinateToOffset)

orFail :: Show e => Either e a -> IO a
orFail = either (assertFailure . show) pure

-- | Offset of a (line, column) in the given source.
offsetAt :: Text -> Integer -> Integer -> IO (Either CoordinateError Int)
offsetAt src l c = do
  line <- orFail (mkLine l)
  col  <- orFail (mkColumn c)
  pure (coordinateToOffset (computeLineMetadata src) line col)

-- | A column strictly inside a tab's expansion?
insideTab :: Either CoordinateError Int -> Bool
insideTab (Left (ColumnInsideTab _ _)) = True
insideTab _                            = False

beyondLine :: Either CoordinateError Int -> Bool
beyondLine (Left (ColumnOutOfRange _ _)) = True
beyondLine _                             = False

-- | GHC's own tab-stop rule, written the way GHC writes it (shifts), as
-- an independent reference for the property below.
ghcAdvance :: Char -> Int -> Int
ghcAdvance '\t' c = ((((c - 1) `shiftR` 3) + 1) `shiftL` 3) + 1
ghcAdvance _    c = c + 1

tabTests :: [TestTree]
tabTests =
  [ testCase "a leading tab: the next character is at column 9, not 2" $ do
      offsetAt "\tx = 1" 1 1  >>= (@?= Right 0)
      offsetAt "\tx = 1" 1 9  >>= (@?= Right 1)
      offsetAt "\tx = 1" 1 10 >>= (@?= Right 2)

  , testCase "columns strictly inside a tab's expansion name no character" $
      forM_ [2 .. 8] $ \c -> do
        r <- offsetAt "\tx = 1" 1 c
        assertBool ("column " <> show c <> " should be inside the tab, got " <> show r) (insideTab r)

  , testCase "a tab after text advances to the next multiple of 8, plus 1" $ do
      offsetAt "ab\tc" 1 3 >>= (@?= Right 2)   -- the tab itself
      offsetAt "ab\tc" 1 9 >>= (@?= Right 3)   -- 'c'
      forM_ [4 .. 8] $ \c -> offsetAt "ab\tc" 1 c >>= \r -> assertBool (show c) (insideTab r)

  , testCase "a tab at column 8 advances to column 9 (no column to land inside)" $ do
      offsetAt "1234567\tx" 1 8 >>= (@?= Right 7)
      offsetAt "1234567\tx" 1 9 >>= (@?= Right 8)

  , testCase "a tab at column 9 advances to column 17" $ do
      offsetAt "12345678\tx" 1 9  >>= (@?= Right 8)
      offsetAt "12345678\tx" 1 17 >>= (@?= Right 9)

  , testCase "consecutive tabs each advance to the next stop" $
      offsetAt "\t\tx" 1 17 >>= (@?= Right 2)

  , testCase "the exclusive end of a line that ends in a tab is legal; past it is not" $ do
      offsetAt "\t" 1 9  >>= (@?= Right 1)
      offsetAt "\t" 1 10 >>= \r -> assertBool (show r) (beyondLine r)
      offsetAt "\t" 1 5  >>= \r -> assertBool (show r) (insideTab r)

  , testCase "columns count characters, not UTF-8 bytes" $ do
      offsetAt "\233\tx" 1 9  >>= (@?= Right 2)   -- e-acute, tab, x
      offsetAt "\233\tx" 1 10 >>= \r -> assertBool (show r) (not (insideTab r))

  , testCase "CRLF: the CR is never a column, tabs still expand" $ do
      offsetAt "\tx\r\n" 1 10 >>= (@?= Right 2)
      offsetAt "\tx\r\n" 1 11 >>= \r -> assertBool (show r) (beyondLine r)

  , testCase "tabs on a later line use that line's own text" $
      offsetAt "a\n\tb" 2 9 >>= (@?= Right 3)

  , testProperty "every character start maps back to its own index (reference: GHC's advance_tabstop)" $
      property $ do
        s <- forAll (Gen.list (Range.linear 0 40) (Gen.element ['a', 'b', ' ', '\t', '\233', 'x']))
        l1 <- evalEither (mkLine 1)
        let meta = computeLineMetadata (Text.pack s)
            cols = scanl (flip ghcAdvance) 1 s
        forM_ (zip [0 :: Int ..] cols) $ \(i, c) -> do
          col <- evalEither (mkColumn (fromIntegral c))
          coordinateToOffset meta l1 col === Right i

  , testProperty "every other column up to the end is inside a tab; beyond the end is out of range" $
      property $ do
        s <- forAll (Gen.list (Range.linear 0 40) (Gen.element ['a', 'b', ' ', '\t', '\233', 'x']))
        l1 <- evalEither (mkLine 1)
        let meta    = computeLineMetadata (Text.pack s)
            cols    = scanl (flip ghcAdvance) 1 s
            lastCol = maximum cols
        forM_ [1 .. lastCol] $ \c -> unless (c `elem` cols) $ do
          col <- evalEither (mkColumn (fromIntegral c))
          assert (insideTab (coordinateToOffset meta l1 col))
        past <- evalEither (mkColumn (fromIntegral lastCol + 1))
        assert (beyondLine (coordinateToOffset meta l1 past))

  , testCase "convertSpan: the column GHC reports for a tabbed token binds" $ do
      sp <- orFail (buildSpan' "Foo.hs" 1 9 1 10)   -- the 'x' in "\tx = 1"
      case convertSpan (SourceText "\tx = 1\n") sp of
        Right ctx -> case Tadka.contextLabelStates ctx of
          [Tadka.LabelOk _] -> pure ()
          other             -> assertFailure ("unexpected label states: " <> show other)
        Left e -> assertFailure ("expected Right, got " <> show e)

  , testCase "convertSpan: the old one-column answer (2) is now rejected, not silently wrong" $ do
      sp <- orFail (buildSpan' "Foo.hs" 1 2 1 3)
      case convertSpan (SourceText "\tx = 1\n") sp of
        Left (InvalidCoordinates _ (ColumnInsideTab _ _)) -> pure ()
        other -> assertFailure ("expected ColumnInsideTab, got " <> show other)

  , testCase "coordinate errors render with the real numbers, never as constructors" $ do
      sp <- orFail (buildSpan' "Foo.hs" 3 4 3 5)
      l  <- orFail (mkLine 3)
      c  <- orFail (mkColumn 4)
      let render e = renderSourceBindingError (InvalidCoordinates sp e)
      render (LineOutOfRange l)
        @?= "invalid coordinates in Foo.hs:3:4-3:5: line 3 is beyond the end of the source"
      render (ColumnOutOfRange l c)
        @?= "invalid coordinates in Foo.hs:3:4-3:5: column 4 is beyond the end of line 3"
      let tabMsg = render (ColumnInsideTab l c)
      assertBool (Text.unpack tabMsg) ("column 4 of line 3 falls inside a tab character" `Text.isInfixOf` tabMsg)
      mapM_ (\t -> assertBool (Text.unpack t)
                     (not (any (`Text.isInfixOf` t) ["LineOutOfRange", "ColumnOutOfRange", "ColumnInsideTab"])))
            [render (LineOutOfRange l), render (ColumnOutOfRange l c), tabMsg]
  ]
  where
    buildSpan' file sl sc el ec = do
      l1 <- mkLine sl
      c1 <- mkColumn sc
      l2 <- mkLine el
      c2 <- mkColumn ec
      mkGhcSpan file l1 c1 l2 c2
