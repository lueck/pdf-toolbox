
-- | Font info contains information, extracted from font,
-- that may be needed when processing content stream

module Pdf.Content.FontInfo
(
  FontInfo(..),
  FISimple(..),
  FontBaseEncoding(..),
  SimpleFontEncoding(..),
  FIComposite(..),
  CIDFontWidths(..),
  FontDescriptor(..),
  makeCIDFontWidths,
  cidFontGetWidth,
  fontInfoDecodeGlyphs
)
where

import Pdf.Core
import Pdf.Core.Util
import Pdf.Core.Object.Util

import Pdf.Content.UnicodeCMap
import Pdf.Content.Transform
import Pdf.Content.Processor (Glyph(..))
import Pdf.Content.GlyphList
import Pdf.Content.TexGlyphList
import qualified Pdf.Content.Encoding.WinAnsi as WinAnsi
import qualified Pdf.Content.Encoding.MacRoman as MacRoman

import Data.List
import Data.Maybe
import Data.Word
import Data.Map (Map)
import qualified Data.Map as Map
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as Vector
import Control.Monad

-- | Font info
data FontInfo
  = FontInfoSimple FISimple
  | FontInfoComposite FIComposite
  deriving (Show)

-- | Font info for simple fonts
data FISimple = FISimple {
  fiSimpleUnicodeCMap :: Maybe UnicodeCMap,
  fiSimpleEncoding :: Maybe SimpleFontEncoding,
  fiSimpleWidths :: Maybe (Int, Int, [Double]),
  -- ^ FirstChar, LastChar, list of widths
  fiSimpleFontMatrix :: Transform Double,
  fiSimpleFontDescriptor :: Maybe FontDescriptor
  }
  deriving (Show)

-- | Standard encoding, other encodings are based on them
data FontBaseEncoding
  = FontBaseEncodingWinAnsi
  | FontBaseEncodingMacRoman
  deriving (Show)

-- | Encoding fo simple font
data SimpleFontEncoding = SimpleFontEncoding {
  simpleFontBaseEncoding :: FontBaseEncoding,
  -- | Mapping from glyph code to glyph name for cases when it is different
  -- from base encoding
  simpleFontDifferences :: [(Word8, ByteString)]
  }
  deriving (Show)

-- | Font info for Type0 font
data FIComposite = FIComposite {
  fiCompositeUnicodeCMap :: Maybe UnicodeCMap,
  fiCompositeWidths :: CIDFontWidths,
  fiCompositeDefaultWidth :: Double,
  fiCompositeFontDescriptor :: Maybe FontDescriptor
  }
  deriving (Show)

-- | Glyph widths for CID fonts
data CIDFontWidths = CIDFontWidths {
  cidFontWidthsChars :: Map Int Double,
  cidFontWidthsRanges :: [(Int, Int, Double)]
  }
  deriving (Show)

instance Monoid CIDFontWidths where
  mempty = CIDFontWidths {
    cidFontWidthsChars = mempty,
    cidFontWidthsRanges = mempty
    }
  w1 `mappend` w2 = CIDFontWidths {
    cidFontWidthsChars = cidFontWidthsChars w1
        `mappend` cidFontWidthsChars w2,
    cidFontWidthsRanges = cidFontWidthsRanges w1
        `mappend` cidFontWidthsRanges w2
    }

instance Semigroup CIDFontWidths where
  (<>) = mappend

data FontDescriptor = FontDescriptor {
  fdHeight :: Double -- height of bbox and default vertical displacement in vertical writing mode
  }
  deriving (Show)

-- | Returns the height for a glyph of a font, in text space
-- units. Defaults to 1.0 if a FontDescriptor is not present.
getFontHeight :: FontInfo -> Int -> Double
getFontHeight (FontInfoComposite fi) =
  const $ fromMaybe 1.0 $ fmap ((/1000) . fdHeight) $ fiCompositeFontDescriptor fi
getFontHeight (FontInfoSimple fi) =
  const $ fromMaybe 1.0 $ fmap ((/1000) . fdHeight) $ fiSimpleFontDescriptor fi


simpleFontEncodingDecode :: SimpleFontEncoding -> Word8 -> Maybe Text
simpleFontEncodingDecode enc code =
  case lookup code (simpleFontDifferences enc) of
    Nothing ->
      case simpleFontBaseEncoding enc of
        FontBaseEncodingWinAnsi -> Map.lookup code WinAnsi.encoding
        FontBaseEncodingMacRoman -> Map.lookup code MacRoman.encoding
    Just glyphName ->
      case Map.lookup glyphName adobeGlyphList of
        Just c -> Just $ Text.pack [c]
        Nothing ->
          case Map.lookup glyphName texGlyphList of
            Nothing-> Nothing
            Just c -> Just $ Text.pack [c]

-- | Make `CIDFontWidths` from value of \"W\" key in descendant font
makeCIDFontWidths :: Array -> Either String CIDFontWidths
makeCIDFontWidths vals = go mempty (Vector.toList vals)
  `notice` ("Can't parse CIDFont width " ++ show vals)
  where
  go res [] = return res
  go res (x1@Number{} : x2@Number{} : x3@Number{} : xs) = do
    n1 <- intValue x1
    n2 <- intValue x2
    n3 <- realValue x3
    go res {cidFontWidthsRanges = (n1, n2, n3) : cidFontWidthsRanges res} xs
  go res (x : Array arr : xs) = do
    n <- intValue x
    ws <- forM (Vector.toList arr) realValue
    go res {cidFontWidthsChars = Map.fromList (zip [n ..] ws)
        `mappend` cidFontWidthsChars res} xs
  go _ _ = Nothing

-- | Get glyph width by glyph code
cidFontGetWidth :: CIDFontWidths -> Int -> Maybe Double
cidFontGetWidth w code =
  case Map.lookup code (cidFontWidthsChars w) of
    Just width -> Just width
    Nothing -> case find (\(start, end, _) -> code >= start && code <= end)
                         (cidFontWidthsRanges w) of
                 Just (_, _, width) -> Just width
                 _ -> Nothing

-- | Decode string into list of glyphs and their widths
fontInfoDecodeGlyphs :: FontInfo -> ByteString -> [(Glyph, Double)]
fontInfoDecodeGlyphs fInfo@(FontInfoSimple fi) = \bs ->
  flip map (BS.unpack bs) $ \c ->
    let code = fromIntegral c
        txt =
          case fiSimpleUnicodeCMap fi of
            Nothing ->
              case fiSimpleEncoding fi of
                Nothing ->
                  case Text.decodeUtf8' (BS.pack [c]) of
                    Right t -> Just t
                    _ -> Nothing
                Just enc ->
                  case simpleFontEncodingDecode enc c of
                    Just t -> Just t
                    Nothing ->
                      case Text.decodeUtf8' (BS.pack [c]) of
                        Right t -> Just t
                        _ -> Nothing
            Just toUnicode ->
              case unicodeCMapDecodeGlyph toUnicode code of
                Just t -> Just t
                Nothing ->
                  case fiSimpleEncoding fi of
                    Nothing -> Nothing
                    Just enc ->
                      case simpleFontEncodingDecode enc c of
                        Just t -> Just t
                        Nothing ->
                          case Text.decodeUtf8' (BS.pack [c]) of
                            Right t -> Just t
                            _ -> Nothing
        width =
          case fiSimpleWidths fi of
            Nothing -> 0
            Just (firstChar, lastChar, widths) ->
              if code >= firstChar && code <= lastChar
                  && (code - firstChar) < length widths
                 then let Vector w _ = transform (fiSimpleFontMatrix fi) $
                            Vector (widths !! (code - firstChar)) 0
                      in w
                 else 0
        height = getFontHeight fInfo code
    in (Glyph {
      glyphCode = code,
      glyphTopLeft = Vector 0 0,
      glyphBottomRight = Vector width height,
      glyphText = txt
      }, width)
fontInfoDecodeGlyphs fInfo@(FontInfoComposite fi) = \bs ->
  case fiCompositeUnicodeCMap fi of
    Nothing ->  -- XXX: use encoding here
      tryDecode2byte $ BS.unpack bs
    Just toUnicode ->
      let getWidth = fromMaybe (fiCompositeDefaultWidth fi)
                   . cidFontGetWidth (fiCompositeWidths fi)
          getHeight = getFontHeight fInfo
      in cmapDecodeString getWidth getHeight toUnicode bs
  where
  -- Most of the time composite fonts have 2-byte encoding,
  -- so lets try that for now.
  tryDecode2byte (b1:b2:rest) =
    let code = fromIntegral b1 * 255 + fromIntegral b2
        width = (/ 1000) $ fromMaybe (fiCompositeDefaultWidth fi)
                         $ cidFontGetWidth (fiCompositeWidths fi) code
        txt =
          case Text.decodeUtf8' (BS.pack [b1, b2]) of
            Right t -> Just t
            _ -> Nothing
        height = getFontHeight fInfo code
        g = Glyph {
          glyphCode = code,
          glyphTopLeft = Vector 0 0,
          glyphBottomRight = Vector width height,
          glyphText = txt
          }
    in (g, width) : tryDecode2byte rest
  tryDecode2byte _ = []

cmapDecodeString
  :: (Int -> Double)
  -> (Int -> Double)
  -> UnicodeCMap
  -> ByteString
  -> [(Glyph, Double)]
cmapDecodeString getWidth getHeight cmap str = go str
  where
  go s =
    case unicodeCMapNextGlyph cmap s of
      Nothing -> []
      Just (g, rest) ->
        let width = getWidth g / 1000
            height = getHeight g
            glyph = Glyph {
          glyphCode = g,
          glyphTopLeft = Vector 0 0,
          glyphBottomRight = Vector width height,
          glyphText = unicodeCMapDecodeGlyph cmap g
          }
        in (glyph, width) : go rest
