module Render
    ( renderOutcomeExpression
    , Chart.ChartEnv
    , Chart.loadChartEnv
    ) where

import           Control.Monad (when)
import           Data.Maybe (fromMaybe)
import           Data.List (find, group, intersperse, sort)
import           Data.Set (Set)
import qualified Data.Set               as Set
import qualified Data.Text              as Text
import           DeltaQ
import qualified Diagrams.Prelude       as D
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
        ( renderDiagramFromExprs os
        , renderChartFromExprs env os v
        )

{-----------------------------------------------------------------------------
    Render Expression
------------------------------------------------------------------------------}
-- | Render a list of named diagrams.
renderDiagramFromExprs :: [(Name, O)] -> String
renderDiagramFromExprs = concat . map render
  where
    render (name, o) =
        "<div>"
            <> "<div>" <> mkName name <> "</div>"
            <> "<div style='margin-left:10px;'>" <> renderDiagramFromExpr o <> "</div>"
        <> "</div>"
    mkName (Name name) = name <> " = "
    mkName Anonymous   = ""

-- | Render a diagram.
renderDiagramFromExpr :: O -> String
renderDiagramFromExpr =
    Text.unpack . D.renderSvg . D.renderDiagram
    . D.scale 55 . renderOutcomeDiagram

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
renderChartFromExprs
    :: Chart.ChartEnv
    -> [(Name, O)]
    -> [(String, DQ)]
    -> String
renderChartFromExprs env outcomes assignments
    | Left e <- allVariablesAssigned (map snd outcomes) assignments =
        renderErrorHtml e
    | otherwise =
        Text.unpack $ D.renderSvg $ Chart.renderChart env
        $ plotCDFs ""
        $ [(mkName name, toDeltaQ toDQ o) | (name, o) <- outcomes]
  where
    toDQ v = fromMaybe (wait 0) $ lookup v assignments
    mkName (Name name) = name
    mkName Anonymous   = ""

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

