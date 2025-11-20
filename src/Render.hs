module Render
    ( renderOutcomeExpression
    , Chart.ChartEnv
    , Chart.loadChartEnv
    ) where

import           Control.Monad (when)
import           Data.Maybe (fromMaybe)
import           Data.List (find, group, intersperse, sort)
import           Data.Map (Map)
import qualified Data.Map               as Map
import           Data.Set (Set)
import qualified Data.Set               as Set
import qualified Data.Text              as Text
import           DeltaQ
import qualified Graphics.Rendering.Chart.Easy as G
import qualified Diagrams.Prelude       as D
import qualified Diagrams.Backend.SVG   as D
import           Parser
    ( Name (..)
    , ObservationLocation (..)
    , parseOutcomeExpressions
    , groupIndentedLines
    , parseVariableAssignments
    , parseObservationLocations
    )
import qualified Parser as Expr
import qualified Render.Chart           as Chart
import qualified Render.Diagram         as D
import           Text.Read (readMaybe)

{-----------------------------------------------------------------------------
    Rendering Logic
------------------------------------------------------------------------------}
type Err = String
type Dia = String
type Chart = String

-- | Render an outcome expression with variable assignments.
renderOutcomeExpression
    :: Chart.ChartEnv -> String -> String -> String -> Either Err (Dia, Chart)
renderOutcomeExpression env expr vars loc = do
    es <- parseOutcomeExpressions expr
    when (null es) $ Left "Empty outcome expression"
    v <- parseVariableAssignments vars
    l <- parseObservationLocations loc
    let os =
            [ (name, outcomeFromExpr $ patchObservationLocations l e)
            | (name, e) <- es
            ]
    pure
        ( renderDiagramFromOutcomes os
        , renderChartFromOutcomes env os v
        )

{-----------------------------------------------------------------------------
    Render Expression
------------------------------------------------------------------------------}
-- | Render a list of named diagrams.
renderDiagramFromOutcomes :: [(Name, O)] -> String
renderDiagramFromOutcomes = concat . map render
  where
    render (name, o) =
        "<div>"
            <> "<div>" <> mkName name <> "</div>"
            <> "<div style='margin-left:10px;'>" <>
                renderDiagramFromOutcome o <> "</div>"
        <> "</div>"
    mkName (Name name) = name <> " = "
    mkName Anonymous   = ""

type Diagram = D.QDiagram D.SVG D.V2 Double D.Any

-- | Render a diagram for an outcome expression.
--
-- Abbreviates variable names and adds a legend for them.
renderDiagramFromOutcome :: O -> String
renderDiagramFromOutcome o =
    Text.unpack . D.renderSvg
    $ D.renderDiagram $ D.scale 55
    $ outcomeDia D.=== D.strutY 0.3 D.=== legendDia
  where
    outcomeDia = renderOutcomeDiagram o'
    legendDia = D.scale 0.3 $ renderLegend legend

    legend = everything (<>) abbreviate $ termFromOutcome o
    o' = outcomeFromTerm $ everywhere reference $ termFromOutcome o

    mkEllipsis n long =
        let short = ellipsis n long
        in  if long == short then Map.empty else Map.singleton long short

    abbreviate (Var name) = mkEllipsis 7 name
    abbreviate (Loc name) = mkEllipsis 10 name
    abbreviate _ = Map.empty

    reference (Var name)
        | Just name' <- Map.lookup name legend = Var name'
        | otherwise = Var name
    reference (Loc name)
        | Just name' <- Map.lookup name legend = Loc name'
        | otherwise = Loc name
    reference x = x

-- | Replace part of the string with an ellipsis
-- if the initial string exceeds a given number of characters.
ellipsis :: Int -> String -> String
ellipsis n s
    | length s > n = take (n-2) s <> "..." <> (drop (length s - 2) s)
    | otherwise = s

-- | Render the legend for a diagram.
renderLegend :: Map String String -> Diagram
renderLegend xs =
    D.translate (D.r2 (-1.5,0))
    $ D.cat (D.r2 (0,-1)) . map renderOne $ Map.toList xs
  where
    renderOne :: (String, String) -> Diagram
    renderOne (long,short) = text' $ short <> " = " <> long

    -- Text with a bounding box
    text' :: String -> Diagram
    text' t =
        D.alignedText 0 0.5 t
        <> D.strutY 1.3
        <> (D.strutX (0.6 * fromIntegral (length t)) D.# D.alignL)

{-----------------------------------------------------------------------------
    Render Observation Locations
------------------------------------------------------------------------------}
-- | Add the given observation locations to the outcome expression.
patchObservationLocations :: [ObservationLocation] -> Expr.Expr -> Expr.Expr
patchObservationLocations locs = Expr.everywhere patch
  where
    matches ex (LocationBefore loc ey) = ex == ey
    matches ex (LocationAfter  ey loc) = ex == ey

    adjust (LocationAfter  _ loc) expr
        = Expr.Seq expr (Expr.Loc loc)
    adjust (LocationBefore loc _) expr
        = Expr.Seq (Expr.Loc loc) expr

    patch expr =
        foldr adjust expr [l | l <- locs, matches expr l]

outcomeFromExpr :: Expr.Expr -> O
outcomeFromExpr = from
  where
    from Expr.Never = never
    from (Expr.Var v) = var v
    from (Expr.Wait t) = wait t
    from (Expr.Loc s) = loc s
    from (Expr.Seq x y) = from x .>>. from y
    from (Expr.FirstToFinish x y) = from x .\/. from y
    from (Expr.LastToFinish  x y) = from x ./\. from y
    from (Expr.Choice p x y) = choice p (from x) (from y)

{-----------------------------------------------------------------------------
    Render Chart
------------------------------------------------------------------------------}
-- | Render multiple named outcome expressions in a single chart.
renderChartFromOutcomes
    :: Chart.ChartEnv
    -> [(Name, O)]
    -> [(String, DQ)]
    -> String
renderChartFromOutcomes env outcomes assignments
    | Left e <- allVariablesAssigned (map snd outcomes) assignments =
        renderErrorHtml e
    | otherwise =
        Text.unpack $ D.renderSvg $ Chart.renderChart env
        $ plotDQs
        $ [(mkName name, toDeltaQ toDQ o) | (name, o) <- outcomes]
  where
    toDQ :: String -> DQ
    toDQ v = fromMaybe (wait 0) $ lookup v assignments
    mkName (Name name) = name
    mkName Anonymous   = ""

    plotDQs :: [(String, DQ)] -> G.Layout Double Double
    plotDQs [(name, dq)] = plotCDF name dq
    plotDQs xs           = plotCDFs "" xs

-- | Check whether all variables in the outcomes are assigned.
allVariablesAssigned :: [O] -> [(String, DQ)] -> Either String ()
allVariablesAssigned os assignments
    | Set.null missingVars && null duplicateVars = Right ()
    | otherwise = Left $ unlines $
        (if not (Set.null missingVars) then errMissing <> [""] else [])
        <> (if not (null duplicateVars) then errDuplicate <> [""] else [])
  where
    renderVarList = concat . intersperse ", "
    givenVars     = map fst assignments
    neededVars    = mconcat (map variables os)
    missingVars   = Set.difference neededVars (Set.fromList givenVars)
    duplicateVars = duplicates givenVars

    isOk = Set.null missingVars && null duplicateVars
    errMissing =
        [ "Cannot generate chart because some variables are unassigned!"
        , "The following variables are unassigned: " <> 
            renderVarList (Set.toList missingVars)
        ]
    errDuplicate =
        [ "Cannot generate chart because some variables have duplicate assignments!"
        , "The following variables have been assigned multiple times: " <> 
            renderVarList duplicateVars
        ]

-- | Render an error message as HTML.
renderErrorHtml :: String -> String
renderErrorHtml s = "<pre>Error:\n" <> s <> "</pre>"

-- | Set of variables in an outcome expression.
variables :: O -> Set String
variables = everything Set.union varname . termFromOutcome
  where
    varname (Var name) = Set.singleton name
    varname _ = Set.empty

-- | Return all duplicates in a list.
duplicates :: Ord a => [a] -> [a]
duplicates xs =
    [x | (x:xs') <- group (sort xs), not (null xs')]

