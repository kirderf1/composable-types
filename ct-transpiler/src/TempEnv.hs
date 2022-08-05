module TempEnv where
-- This entire module is here to smoothen the transition from our Env to haskell-names' Environment
import qualified Data.Map as Map
import qualified Data.Set as Set

import Language.Haskell.Exts
import Language.Haskell.Names

import TransformUtils

toEnv :: Environment -> Env
toEnv env = (sig, constrs)
  where
    sig = Map.fromSet findPieces categories
    categories = Set.fromList $ asCategory =<< symbols
    asCategory PieceCategory{symbolName = name} = [name]
    asCategory _                                = []
    findPieces category = Set.fromList $ asPiece category =<< symbols
    asPiece searchedCat Piece{symbolName = name, categoryName = pieceCat}
        | searchedCat == pieceCat = [name]
    asPiece _ _                   = []
    constrs = Set.fromList $ asConstr =<< symbols
    asConstr PieceConstructor{symbolName = name} = [name]
    asConstr _                                   = []
    symbols = concat $ Map.elems env

