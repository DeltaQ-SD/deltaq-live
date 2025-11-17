module Main where

import Control.Concurrent
import Control.Exception (evaluate)
import GHC.Wasm.Prim

import Render

{-----------------------------------------------------------------------------
    JavaScript Imports
------------------------------------------------------------------------------}
foreign import javascript unsafe "document.getElementById($1)"
  js_document_getElementById :: JSString -> IO JSVal

foreign import javascript unsafe "$1.addEventListener($2, $3)"
  js_addEventListener :: JSVal -> JSString -> JSVal -> IO ()

foreign import javascript "wrapper"
  asEventListener :: (JSVal -> IO ()) -> IO JSVal

foreign import javascript unsafe "$1.target.value"
  js_event_target_value :: JSVal -> IO Double

foreign import javascript unsafe "$1.value"
  js_input_value :: JSVal -> IO JSString

foreign import javascript unsafe "$1.innerHTML = $2"
  js_element_setInnerHtml :: JSVal -> JSString -> IO ()

foreign import javascript safe "const r = await fetch($1); return r.text();"
  js_fetch :: JSString -> IO JSString

jsFetch :: String -> IO String
jsFetch = fmap fromJSString . js_fetch . toJSString

main :: IO ()
main = error "main is unused"

{-----------------------------------------------------------------------------
    Web page
------------------------------------------------------------------------------}
foreign export javascript "setup" setup :: IO ()

-- | Main entrypoint.
setup :: IO ()
setup = do
    -- Load data files
    chartEnv <- loadChartEnv jsFetch

    -- Register callback for button click.
    evalButton <- js_document_getElementById (toJSString "eval")
    onEvalButtonCallback <- asEventListener $ onEvalButtonClick chartEnv
    js_addEventListener evalButton (toJSString "click") onEvalButtonCallback

-- | Handle button clicks.
onEvalButtonClick :: ChartEnv -> JSVal -> IO ()
onEvalButtonClick chartEnv event = do
    exprInput  <- js_document_getElementById (toJSString "expr")
    exprString <- fromJSString <$> js_input_value exprInput

    varsInput  <- js_document_getElementById (toJSString "vars")
    varsString <- fromJSString <$> js_input_value varsInput

    let eexpr = renderOutcomeExpression chartEnv exprString varsString
    case eexpr of
        Left e -> do
            out <- js_document_getElementById (toJSString "out-error")
            js_element_setInnerHtml out (toJSString e)

        Right (dia, times) -> do
            out <- js_document_getElementById (toJSString "out-error")
            js_element_setInnerHtml out (toJSString "")

            outDia     <- js_document_getElementById (toJSString "out-diagram")
            js_element_setInnerHtml outDia (toJSString dia)

            outTimes   <- js_document_getElementById (toJSString "out-times")
            js_element_setInnerHtml outTimes (toJSString times)
