module Render
    ( renderOutcomeExpression
    , Chart.ChartEnv
    , Chart.loadChartEnv
    ) where

import           Data.Maybe (fromMaybe)
import           Data.List (group, intersperse, sort)
import           Data.Set (Set)
import qualified Data.Set               as Set
import qualified Data.Text              as Text
import           DeltaQ
import qualified Diagrams.Prelude       as D
import           Parser (parseOutcomeExpression, parseVariableAssignments)
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
    :: Chart.ChartEnv -> String -> String -> Either Err (Dia, Chart)
renderOutcomeExpression env expr vars = do
    o <- parseOutcomeExpression expr
    v <- parseVariableAssignments vars
    pure (renderDiagramFromExpr o, renderChartFromExpr env o v)

-- | Render a diagram.
renderDiagramFromExpr :: O -> String
renderDiagramFromExpr =
    Text.unpack . D.renderSvg . D.renderDiagram
    . D.scale 55 . renderOutcomeDiagram

-- | Render a chart.
renderChartFromExpr :: Chart.ChartEnv -> O -> [(String, DQ)] -> String
renderChartFromExpr env o assignments
    | not isOk = renderErrorHtml $ unlines $
        (if not (Set.null missingVars) then errMissing <> [""] else [])
        <> (if not (null duplicateVars) then errDuplicate <> [""] else [])
    | otherwise =
        Text.unpack $ D.renderSvg $ Chart.renderChart env
        $ plotCDF "" $ toDeltaQ toDQ o
  where
    toDQ v = fromMaybe (wait 0) $ lookup v assignments

    renderVarList = concat . intersperse ", "
    vars = map fst assignments
    missingVars = Set.difference (variables o) (Set.fromList vars)
    duplicateVars = duplicates vars

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

