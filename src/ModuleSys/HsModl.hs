module ModuleSys.HsModl
    ( topLevelModl
    , mainModl
) where

import Module
import PrelNames

topLevel :: String
topLevel = "top-level"

mkMainModl :: ModuleName -> Module
mkMainModl = mkModule mainUnitId

topLevelModl :: Module
topLevelModl =
    let modlName = mkModuleName topLevel in
        mkMainModl modlName

mainModl :: Module
mainModl = mAIN
