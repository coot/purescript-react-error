module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Element, ElementId(..), documentToNonElementParentNode)
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)
import React (ReactClass, ReactProps, ReactRefs, ReactSpec, ReactState, ReadOnly, ReadWrite, createClass, createElement, getProps, readState, spec, writeState)
import React.DOM as D
import React.DOM.Props as P
import ReactDOM (render)
import Unsafe.Coerce (unsafeCoerce)

type InputProps a eff =
  { value :: a
  , onUpdate :: String -> Eff (props :: ReactProps, refs :: ReactRefs ReadOnly, state :: ReactState ReadWrite | eff) Unit
  }

class AsString a where
  asString :: a -> String

instance asStringInt :: AsString Int where
  asString = show

instance asStringString :: AsString String where
  asString s = s

inputSpec :: forall e a. AsString a => ReactSpec (InputProps a e) Unit e
inputSpec = debugSpec $ ((spec unit renderFn) { displayName = "Input" })
  where
    handleChange this ev =
      let v :: String
          v = (unsafeCoerce ev).target.value
      in do
        { onUpdate } <- getProps this
        onUpdate v

    renderFn this = do
      { value } <- getProps this
      pure $ D.input [ P.value (asString value), P.onChange (handleChange this) ] []


inputCls :: forall e a. AsString a => ReactClass (InputProps a e)
inputCls = createClass inputSpec

cls :: ReactClass Unit
cls = createClass (debugSpec $ ((spec { value: "Hello World!" } renderFn) { displayName = "Parent" } ))
  where
    handleUpdate this value = void $ writeState this { value }

    renderFn this = do
      { value } <- readState this
      pure $ D.div'
        [ createElement
            inputCls
            { value, onUpdate: handleUpdate this }
            []
        ]

findElmById :: forall e. ElementId -> Eff (dom :: DOM | e) Element
findElmById _id = do
  el <- window >>= document >>= getElementById _id <<< castDocument
  pure $ unsafePartial fromJust el

 where
    castDocument = documentToNonElementParentNode <<< htmlDocumentToDocument

main :: forall e. Eff (console :: CONSOLE, dom :: DOM | e) Unit
main = do
  el <- findElmById (ElementId "app")
  void $ render (createElement cls unit []) el
