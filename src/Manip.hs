-- | Utilities for applying string endomorphisms ("macros") within parts of textual documents. Two cases are supported: 1) text to text; and 2) text to 'StreamingBody'.
--
-- === Example
--
-- @
-- let input :: Text
--     input = "just text [hi|this input will be ignored|] and [upper|this input will be uppercase'd|]"

--     macroMap :: Applicative m => Map Text (Text -> Transform m Utf8Builder)
--     macroMap = [ ("hi", \_ send _ -> send "hi")
--                , ("upper", \arg send flush -> send (textToUtf8Builder $ T.toUpper arg) *> flush)
--                ]

--     onMissingMacro :: Monad m => Text -> Text -> Transform m Utf8Builder
--     onMissingMacro macroName remainder = \send flush -> send ("invalid macro: " <> textToUtf8Builder macroName) *> flush *> buildTransform onMissingMacro macroMap remainder send flush

--     transform :: Monad m => Transform m Utf8Builder
--     transform = buildTransform onMissingMacro macroMap input
-- @
-- >>> utf8BuilderToText . runIdentity . runTransform $ transform
-- "just text hi and THIS INPUT WILL BE UPPERCASE'D"
--
-- If we change the "hi" macro in input to "ho", which is not in the macro map, then we get the result "just text invalid macro: ho and THIS INPUT WILL BE UPPERCASE'D"
--
-- @flush@ is a no-op in this example because I'm not outputting to a file handle or socket or anything. That being said, @transform@ can be used for these purposes without being edited! (That's the gist of this module.)
module Manip where

import RIO
import RIO.Text (Text)
import qualified RIO.Text as T
import RIO.Map (Map)
import qualified RIO.Map as M
import Control.Monad.Trans.Writer.Strict
import Data.Attoparsec.Text

textToUtf8Builder :: Text -> Utf8Builder
textToUtf8Builder = Utf8Builder . encodeUtf8Builder

-- | Parses an input string and returns an output string in an applicative. A generalization of 'StreamingBody' which allows writing a streaming server route at the same time as a text transformer. This lessens work needed to keep dynamically and statically-served web routes consistent.
type Transform m s
    = (s -> m ()) -- ^ send function
    -> m () -- ^ flush function
    -> m () -- ^ output value

-- | Run a @Transform@ on text. You can convert the output to Text via 'utf8BuilderToText'.
runTransform :: (Monad m, w ~ Utf8Builder) => Transform (WriterT w m) w -> m w
runTransform t = fmap snd . runWriterT $ t tell (pure ())

-- | Output text to an IO 'Handle', as you would a 'StreamingBody'.
runTransformIO :: MonadIO m => Handle -> Transform m Utf8Builder -> m ()
runTransformIO h t = t (hPutBuilder h . getUtf8Builder) (hFlush h)

toStreamingBody :: Transform IO Utf8Builder -> Transform IO Builder
toStreamingBody t = \s f -> t (s . getUtf8Builder) f

-- | Create a Transform from macros in a body of text. The macro format is QuasiQuotation-like: @[macroName|input|]@. This will pass @input@ to the @macroName@ macro, then replace the whole quasi-quote block with its output.
--
-- * Preface the first '[' with a backslash to leave as a literal.
buildTransform :: forall m trans
                . (Monad m, trans ~ Transform m Utf8Builder)
               => (Text -> Text -> trans) -- ^ how to handle macros not being found. Arguments are macro name, then text following macro clause
               -> Map Text (Text -> trans) -- ^ macro identifiers and their corresponding functions to apply to their inputs
               -> Text -- ^ input text
               -> trans
buildTransform handleNoMacro !m = go
  where
    go :: Text -> trans
    go !s = \send flush -> case parse parser s of
        Partial _ -> send (textToUtf8Builder s) *> flush
        Done remainder (!before, mm) -> do
            send (textToUtf8Builder before)
            flush
            case mm of
                Nothing -> go remainder send flush
                Just (!macro, arg) -> case M.lookup macro m of
                    Nothing -> handleNoMacro macro remainder send flush
                    Just f -> f arg send flush *> go remainder send flush
        _ -> error "impossible parse fail in buildTransform" -- parser cannot fail; do not account for that case
      where
        parser :: Parser (Text, Maybe (Text, Text))
        parser = let next = skip (const True) in do
            before <- takeTill (\c -> c == '\\' || c == '[')
            anyChar >>= \case
                '\\' -> next *> pure (before, Nothing)
                '[' -> do
                    name <- takeTill (=='|')
                    next
                    input <- manyTill' anyChar "|]" -- surprised there's no provided more efficient Text-specific version
                    pure (before, Just (name, T.pack input))
                _ -> error "Impossible character encountered in buildTransform"

-- | Create links to footnodes for of the form:
--
-- @
-- Ordinary document like any other(1). Just *some(2) stuff*.
--
-- ---
--
-- (1) well, like any other, PLUS FOOTNOTES! :D
-- (2) not "any" stuff: *some* stuff!
-- @
--
-- Indented for markdown documents, but works on plaintext, too.
--
-- === Teachnical Details
-- * footnotes and their references both be in parenthesis
-- * footnotes must be each one line of plain text
-- * the footnotes section is defined as the section after the document's final "---"
-- * the footnotes do not need to start at 1. Any parenthesis-with-number ocurring before the footnotes section is looked-up in the footnotes section; thus having out of order or non-consecutive footnotes is fine.
-- * never should a footnote ocurr more than once in either the body or footnote lookup section. You'll get odd results otherwise (but it won't break anything.)
footnotes :: Text -> Text
footnotes _ = undefined -- TODO
