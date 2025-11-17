module Render.Chart
    ( ChartEnv
    , mkChartEnv
    , loadChartEnv
    , renderChart
    ) where

import Control.Exception (evaluate)

import qualified Graphics.Rendering.Chart as Chart
import qualified Graphics.Rendering.Chart.Backend.Diagrams as Chart
import qualified Graphics.Svg as Svg

import qualified Diagrams.Prelude as Dia
import           Render.Chart.Fonts (loadSansSerifFonts)
import qualified Render.Diagram as Dia

{-----------------------------------------------------------------------------
    Rendering a 'Chart' with some default values.
------------------------------------------------------------------------------}
-- | Default sizes and scale.
chartX, chartY, chartScale :: Double
chartScale = 1.3
chartX = 640 / chartScale
chartY = 480 / chartScale

-- | An environment in which 'Chart' can be rendered.
type ChartEnv = Chart.DEnv Double

-- | Create an environment for rendering 'Chart',
-- essentially by loading fonts.
mkChartEnv :: Chart.FontSelector Double -> ChartEnv
mkChartEnv =    
    Chart.createEnv Chart.vectorAlignmentFns chartX chartY

-- | Load a 'ChartEnv' by loading fonts.
loadChartEnv :: (String -> IO String) -> IO ChartEnv
loadChartEnv loadFile = do
    fonts <- loadSansSerifFonts loadFile
    evaluate $ mkChartEnv fonts

-- | Render a chart via SVG.
--
-- The rendered image has size 640 x 480 px.
renderChart
    :: Chart.ToRenderable a
    => Chart.DEnv Double -> a -> Svg.Element
renderChart env =
      Dia.renderDiagram
    . Dia.scale chartScale
    . fst . Chart.runBackend env
    . flip Chart.render (chartX, chartY)
    . Chart.toRenderable
