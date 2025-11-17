{-# LANGUAGE ScopedTypeVariables #-}
module Render.Chart.Fonts
    ( loadSansSerifFonts
    ) where

import Graphics.Rendering.Chart.Backend
    ( FontStyle (..), FontSlant (..), FontWeight(..)
    )
import Graphics.Rendering.Chart.Backend.Diagrams
    ( FontSelector
    )

import qualified Graphics.SVGFonts as F
import qualified Graphics.SVGFonts.CharReference as F
import qualified Graphics.SVGFonts.ReadFont as F

{-----------------------------------------------------------------------------
    Loading fonts needed for 'Chart'
------------------------------------------------------------------------------}

alterFontFamily :: String -> F.PreparedFont n -> F.PreparedFont n
alterFontFamily n (fd, om) = (fd { F.fontDataFamily = n }, om)

loadSansSerifFonts
    :: forall n. (Read n, RealFloat n)
    => (String -> IO String) -> IO (FontSelector n)
loadSansSerifFonts loadFile = do
    let loadFont dat = snd $ F.loadFont' "" dat

    sansR    <- loadFont <$> loadFile "fonts/SourceSansPro_R.svg"
    sansRB   <- loadFont <$> loadFile "fonts/SourceSansPro_RB.svg"
    sansRBI  <- loadFont <$> loadFile "fonts/SourceSansPro_RBI.svg"
    sansRI   <- loadFont <$> loadFile "fonts/SourceSansPro_RI.svg"

    let selectFont :: FontStyle -> F.PreparedFont n
        selectFont fs = case (_font_name fs, _font_slant fs, _font_weight fs) of
            (_, FontSlantNormal , FontWeightNormal) -> alterFontFamily "sans-serif" sansR
            (_, FontSlantNormal , FontWeightBold  ) -> alterFontFamily "sans-serif" sansRB
            (_, FontSlantItalic , FontWeightNormal) -> alterFontFamily "sans-serif" sansRI
            (_, FontSlantOblique, FontWeightNormal) -> alterFontFamily "sans-serif" sansRI
            (_, FontSlantItalic , FontWeightBold  ) -> alterFontFamily "sans-serif" sansRBI
            (_, FontSlantOblique, FontWeightBold  ) -> alterFontFamily "sans-serif" sansRBI

    return selectFont
