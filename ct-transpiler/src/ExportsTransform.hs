module ExportsTransform(transformExport) where

import Language.Haskell.Exts
import Language.Haskell.Names (Scoped(Scoped), NameInfo(GlobalSymbol, Export), Symbol(..))

import qualified GeneratedNames as Names
import TransformUtils

import Data.Default


transformExport :: ExportSpec (Scoped ()) -> Transform [ExportSpec (Scoped ())]
-- Category symbols do not exist after the transform, and should thus be removed
transformExport (EAbs _ (NoNamespace _) con) | isCategory (ann con) = return []
    where
        isCategory (Scoped (GlobalSymbol (PieceCategory {}) _) _) = True
        isCategory _ = False
-- If all symbols from one module are reexported, then we should already be exporting everything we want to
transformExport e@(EModuleContents _ _) = return [e]
transformExport e = do
      newSpecs <- mapM (extraSpecs q) symbols
      return $ e : (concat newSpecs)
    where
        Scoped (Export symbols) _ = ann e
        q = copyQualifier (qNameFromSpec e)

extraSpecs :: Default l => (Name l -> QName l) -> Symbol -> Transform [ExportSpec l]
extraSpecs q (PieceConstructor {symbolName = name}) = do
    smartCon <- Names.qSmartCon (q $ fmap def name)
    return [EVar def smartCon]
extraSpecs q (ExtFunction {symbolName = name}) = do
    let qName = q $ fmap def name
    innerClass <- Names.qInnerClass qName
    outerClass <- Names.qOuterClass qName
    return [EAbs def (NoNamespace def) innerClass, EAbs def (NoNamespace def) outerClass]
extraSpecs _ _ = return []

qNameFromSpec :: ExportSpec l -> QName l
qNameFromSpec (EVar _ qn) = qn
qNameFromSpec (EAbs _ _ qn) = qn
qNameFromSpec (EThingWith _ _ qn _) = qn
qNameFromSpec (EModuleContents _ _) = undefined

copyQualifier :: Default l => QName l -> Name l -> QName l
copyQualifier (UnQual _ _) name = UnQual def name
copyQualifier (Qual _ mn _) name = Qual def mn name
copyQualifier (Special _ _) _ = undefined
