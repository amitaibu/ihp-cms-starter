module Web.Controller.StyleGuide where

import Web.Controller.Prelude
import Web.View.StyleGuide.Index

instance Controller StyleGuideController where
    action StyleGuideAction = do
        render IndexView { .. }

