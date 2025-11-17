module Render
    ( render
    ) where

import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy   as Text.Lazy
import           Diagrams.Prelude                   hiding (pre, loc, render)
import           Diagrams.Backend.SVG
import           DeltaQ
import qualified Graphics.Svg

{-----------------------------------------------------------------------------
    Rendering Logic
------------------------------------------------------------------------------}
render :: String -> String
render name =
    Text.unpack $ renderSvg $ renderDiagram $ scale 55 $ renderOutcomeDiagram d
  where
    d = var name .>>. loc "" .>>. var "y" :: O

{-----------------------------------------------------------------------------
    Render Diagrams and SVG
------------------------------------------------------------------------------}
-- | Render an SVG document.
-- This assumes that the argument is indeed a complete SVG document,
-- i.e. an @<SVG>@ tag.
renderSvg :: Graphics.Svg.Element -> Text
renderSvg = Text.Lazy.toStrict . Graphics.Svg.renderText

-- | Render a diagram via SVG.
--
-- Uses absolute units:
-- one unit in the diagram corresponds to @1px@ in the rendered image.
--
-- To change the scale in your diagram before rendering, use 'scale'.
renderDiagram :: QDiagram SVG V2 Double Any -> Graphics.Svg.Element
renderDiagram = renderDia SVG (SVGOptions sz Nothing (Text.pack "") [] True)
  where
    sz = mkSizeSpec2D Nothing Nothing
