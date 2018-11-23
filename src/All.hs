{-# language TypeOperators, DataKinds, TypeFamilies, KindSignatures, PolyKinds #-}
module All where

type family All (f :: k -> j) (xs :: [k]) :: [j] where
  All f '[] = '[]
  All f (x ': xs) = f x ': All f xs