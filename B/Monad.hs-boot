{-# LANGUAGE KindSignatures #-}

module B.Monad where

data BuildRule (m :: * -> *) (a :: *)
