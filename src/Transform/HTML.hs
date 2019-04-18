-- | DOM manipulations
module Transform.HTML
( lineCode
, deriveTOC
, giveIdsToHeadings
, noIdHeadings
) where

import Data.Char (isDigit, ord, toLower, isSpace)
import Data.List (intercalate, (!!), head)
import NicLib.Tree (readIndentedGeneral)
import Data.Foldable (foldr')
import Data.Ix (inRange)
import RIO hiding (when)
import qualified RIO.Text as T
import Data.Tree.NTree.TypeDefs (NTree(..))
import qualified Data.Tree as Tree
import Text.XML.HXT.Core
import qualified Text.XML.HXT.DOM.ShowXml as SX

-- Assumes that the first line in such a block is the least indented. If not so, define leadingFirstSpaces as
-- min (length . takeWhile isSpace <$> ts)
-- | Transform \<pre\>\<code\> blocks into \<table\>s with line numbers.
--
-- Transformation only occurs when number of lines in code block is at least equal to threshold
lineCode :: ArrowXml a => Int -> a XmlTree XmlTree
lineCode threshold = processTopDown $ applyA (deep getText >. arr g) `when` ((isElem >>> hasName "pre") /> (isElem >>> hasName "code"))
    where
        g (foldMap (filter nonBlank . lines) -> ts) = 
            let leadingFirstSpaces = length (takeWhile isSpace (head ts))
            in if length ts >= threshold then
                   mkelem "table" [sattr "class" "linedCode"]
                       [ selem "tbody"
                           [ selem "tr" -- each row has two columns
                               [ selem "td" [txt $ show i]
                               , selem "td" [txt $ drop leadingFirstSpaces t]
                               ] | i <- [1..] :: [Int] -- determined by these comprehensions
                                 | t <- ts
                           ]
                       ]
               else this
        nonBlank = \case
            [] -> False
            s -> not $ all isSpace s

-- | Derive a table of contents from @\<h1\>@, @\<h2\>@ etc. elements with or without an @id@ attribute
--
-- If deriving all, use with 'giveIdsToHeadings' to ensure that the TOC links aren't broken!
--
-- This is not an automorphism! It returns the TOC element itself. If you want to insert the TOC back into the original document, you must do that yourself. Being an @ArrowList@ is a bit misleading here considering that the resulting list is a singleton.
deriveTOC :: (ArrowXml a, ArrowChoice a)
          => Bool -- ^ derive id attributes for heading tags that don't already have id's? If @True@, then we can include /all/ headings in the TOC; if @False@, only headings having an "id" attribute are included.
          -> Bool -- ^ ordered elements? @True@ => ol; @False@ => ul
          -> a XmlTree XmlTree
deriveTOC deriveAll (bool ("ul" :: String) "ol" -> listWrapper)
     = (deep isHeading >>> if deriveAll then deriveAddId `when` (neg $ hasAttr "id") else hasAttr "id")
    >>. toList . readIndentedGeneral
            (T.pack . SX.xshow . pure)
            (Right . headingToAnchorPair)
    >>> selem "li" [mkText <<^ T.unpack] ||| arr listFold
    where
        -- convert <h_n id="idValue">innerHtml</h_n> to (n, <a href="#idValue">innerHtml</a>)
        headingToAnchorPair :: XmlTree -> (Int, XmlTree)
        headingToAnchorPair (NTree (XTag h attrs) innerHtml) = (charToInt $ localPart h !! 1, NTree (XTag (mkName "a") (id2href <$> attrs)) innerHtml)
            where
                -- convert id to href, leaving other attrs as-is
                id2href :: XmlTree -> XmlTree
                id2href (NTree (XAttr (localPart -> "id")) [NTree (XText ref) []]) = NTree (XAttr $ mkName "href") [NTree (XText $ '#':ref) []]
                id2href x = x
        
                -- like read but for Char and faster
                charToInt :: Char -> Int
                charToInt c = ord c - 48
        headingToAnchorPair _ = error "Invalid assumption in deriveTOC"

        -- transform tree of anchor elements into a TOC nested list
        listFold :: Tree.Tree XmlTree -> XmlTree
        listFold t = flip Tree.foldTree t $ \(wrapInLi -> liAnchor) -> \case
            [] -> liAnchor
            ls -> wrapInLr [liAnchor, wrapInLr ls] {- BUG: if ls is of listWrapper, don't wrap it!
                                                      Not really a /problem/, though, so I'll leave it as-is. -}
            where
                -- <li>'s always have exactly one child, even if that child is an <ol> or <ul>
                wrapInLi :: XmlTree -> XmlTree
                wrapInLi = NTree (XTag (mkName "li") []) . pure

                -- <ol/ul>'s will always contain one or more <li>'s.
                wrapInLr :: XmlTrees -> XmlTree
                wrapInLr = NTree (XTag (mkName listWrapper) [])

-- HXT Helper Funcs

-- | Derive an id attribute (for a heading) from its innerHtml text content, such that the id has only alphanumerics and dashes whitelists.
deriveAddId :: ArrowXml a => a XmlTree XmlTree
deriveAddId = proc x -> do
    t <- deep getText >. intercalate " " -< x
    addAttr "id" (foldr' tf mempty t) -<< x
    where
        tf :: Char -> String -> String -- TODO: foldr' is...apparently inheriently dubious? idk. Use difference lists/codensity instead (prob. dstring package), whenever
        tf (toLower -> c) t =
            if | isSpace c -> '-':t
               | inRange ('a','z') c || inRange ('0','9') c || c == '-' -> c:t
               | otherwise -> t

-- | Ensure that all headings have the @id@ attributes referenced by 'deriveTOC'
giveIdsToHeadings :: ArrowXml a => a XmlTree XmlTree
giveIdsToHeadings = processTopDown $ deriveAddId `when` (isHeading >>> neg (hasAttr "id"))

-- | A little proofreading arrow: gets headings lacking the id attribute
noIdHeadings :: ArrowXml a => a XmlTree XmlTree
noIdHeadings = deep $ isHeading >>> neg (hasAttr "id")

-- defined as its own entity for easy re-use in deriveId and noIdHeadings
-- do not export
isHeading :: ArrowXml a => a XmlTree XmlTree
isHeading = isElem >>> hasNameWith ((\case ['h', n] -> isDigit n; _ -> False) . localPart)
