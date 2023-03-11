module ModuleSys.HsModl
    ( topLevelModl
    , mAIN
    , mainModl
) where

import Module
import PrelNames

topLevel :: String
topLevel = "TopLevel"

mkMainModl :: ModuleName -> Module
mkMainModl = mkModule mainUnitId

topLevelModl :: Module
topLevelModl =
    let modlName = mkModuleName topLevel in
        mkMainModl modlName

mainModl :: Module
mainModl = topLevelModl
