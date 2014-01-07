module GHC.Generics.Lens.Extras (rep, rep1) where

import Control.Lens
import GHC.Generics (Generic, Generic1, Rep, Rep1)
import qualified GHC.Generics as Generics

rep :: (Generic s, Generic t) => Iso s t (Rep s x) (Rep t x)
{-# INLINE rep #-}
rep = iso Generics.from Generics.to

rep1 :: (Generic1 s, Generic1 t) => Iso (s a) (t a) (Rep1 s a) (Rep1 t a)
{-# INLINE rep1 #-}
rep1 = iso Generics.from1 Generics.to1
