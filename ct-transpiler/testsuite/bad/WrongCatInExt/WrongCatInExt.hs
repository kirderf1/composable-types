{-# LANGUAGE ComposableTypes #-}

module WrongCatInExt where

piececategory A
piececategory B

f -: A -> Int

data piece B ==> P = P

ext f for P where
    f P = 0
