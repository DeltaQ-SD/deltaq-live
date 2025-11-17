module Render.Diagram
    ( renderSvg
    , renderDiagram
    ) where

import           Data.Text (Text)
import           Data.Text            as Text
import qualified Data.Text.Lazy       as Text.Lazy
import qualified Diagrams.Prelude     as D
import qualified Diagrams.Backend.SVG as D
import qualified Graphics.Svg         as Svg

{-----------------------------------------------------------------------------
    Render Diagrams and SVG
------------------------------------------------------------------------------}
-- | Render an SVG document.
-- This assumes that the argument is indeed a complete SVG document,
-- i.e. an @<SVG>@ tag.
renderSvg :: Svg.Element -> Text
renderSvg = Text.Lazy.toStrict . Svg.renderText

-- | Render a diagram via SVG.
--
-- Uses absolute units:
-- one unit in the diagram corresponds to @1px@ in the rendered image.
--
-- To change the scale in your diagram before rendering, use 'scale'.
renderDiagram :: D.QDiagram D.SVG D.V2 Double D.Any -> Svg.Element
renderDiagram =
    D.renderDia D.SVG (D.SVGOptions sz Nothing (Text.pack "") [] True)
  where
    sz = D.mkSizeSpec2D Nothing Nothing
