module TempEnv {-# DEPRECATED "" #-} where
-- This entire module is here to smoothen the transition from our Env to haskell-names' Environment
import           Data.Map   (Map)
import qualified Data.Map as Map
import           Data.Set   (Set)
import qualified Data.Set as Set

import Language.Haskell.Exts
import Language.Haskell.Names

import TransformUtils

-- | Map of category names to pieces
type Sig = Map (Name ()) (Set (Name ()))

-- | Set of all piece constructors
type Constrs = Set (Name ())

type Env = (Sig, Constrs)

emptyEnv :: Env 
emptyEnv = (Map.empty, Set.empty)

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

