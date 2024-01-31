module CustomProps.CustomPrestoProps where

import Prelude
import PrestoDOM.Types.Core (ElemName(..), VDom(..), Namespace, PropName(..)) 
import PrestoDOM.Elements.Elements
import Halogen.VDom.DOM.Prop (Prop)


accordionLayout :: forall i p. Node (Prop i) p
accordionLayout = element (ElemName "accordionLayout")