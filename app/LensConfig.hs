module LensConfig where

import Language.Haskell.TH.Syntax (Name, mkName, nameBase)
import Lens.Micro ((.~), (&))
import Lens.Micro.TH


-- Invert the convention and have field foo -> lens _foo
myRules :: LensRules
myRules = lensRules
        & lensField .~ mkFieldName
  where
    mkFieldName :: Name -> [Name] -> Name -> [DefName]
    mkFieldName _ _ n = [TopName (mkName ('_' : nameBase n))]
