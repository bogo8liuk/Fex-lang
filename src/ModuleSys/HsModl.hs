module ModuleSys.HsModl
    ( topLevelModl
) where

import Module

topLevel :: String
topLevel = "top-level"

mkMainModl :: ModuleName -> Module
mkMainModl = mkModule mainUnitId

topLevelModl :: Module
topLevelModl =
    let modlName = mkModuleName topLevel in
        mkMainModl modlName
