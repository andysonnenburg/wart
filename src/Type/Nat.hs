{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Type.Nat
       ( Nat (..)
       , (:+), (:-), (:>)
       , N0, N1, N2, N3, N4, N5, N6, N7, N8
       ) where

data Nat = Z | S Nat

#ifndef HLINT
type family (x :: Nat) :+ (y :: Nat) :: Nat
#endif
type instance Z :+ y = y
type instance S x :+ y = S (x :+ y)

#ifndef HLINT
type family (x :: Nat) :- (y :: Nat) :: Nat
#endif
type instance Z :- x = x
type instance S x :- S y = x :- y

#ifndef HLINT
type family (x :: Nat) :> (y :: Nat) :: Bool
#endif
type instance Z :> x = False
type instance S x :> Z = True
type instance S x :> S y = x :> y

type N0 = Z
type N1 = S N0
type N2 = S N1
type N3 = S N2
type N4 = S N3
type N5 = S N4
type N6 = S N5
type N7 = S N6
type N8 = S N7
