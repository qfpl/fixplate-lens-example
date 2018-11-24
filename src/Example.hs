{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language FunctionalDependencies, FlexibleInstances, MultiParamTypeClasses #-}
{-# language LambdaCase #-}
{-# language UndecidableInstances #-}
{-# language ScopedTypeVariables, TypeApplications #-}
{-# language FlexibleContexts #-}
{-# language TemplateHaskell #-}
{-# language DataKinds, TypeOperators #-}
module Example where

import All

import Data.Generics.Fixplate.Base (ShowF(..))
import Data.Generics.Fixplate.Functor ((:+:)(..))
import Data.Generics.Fixplate.Morphisms (cata)
import Data.Generics.Fixplate.Traversals (restructure)
import Data.Generics.Fixplate.Lens
import Data.Variant1.Lens
import Data.Record1.Lens

import Control.Applicative ((<|>))
import Control.Lens.Fold ((^?))
import Control.Lens.Iso (from)
import Control.Lens.Plated (rewriteOf)
import Control.Lens.Prism (Prism', prism')
import Control.Lens.Review ((#))
import Control.Lens.TH (makeClassyPrisms)
import Data.Deriving (deriveShow1)
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)
import Data.Monoid (Alt(..))



data AddF a = AddF a a
  deriving (Eq, Show, Functor, Foldable, Traversable)
deriveShow1 ''AddF
instance ShowF AddF where; showsPrecF = showsPrec

class AsAddF s a | s -> a where
  _AddF :: Prism' s (a, a)

instance AsAddF (AddF a) a where
  {-# inline _AddF #-}
  _AddF = prism' (uncurry AddF) (\(AddF a b) -> Just (a, b))

instance Ctor1 s AddF => AsAddF (Variant1 s a) a where
  {-# inline _AddF #-}
  _AddF = _Ctor1 @AddF . _AddF

instance Ctor1 s AddF => AsAddF (Mu (Variant1 s)) (Mu (Variant1 s)) where
  {-# inline _AddF #-}
  _AddF = from _Mu . _AddF



data MultF a = MultF a a
  deriving (Eq, Show, Functor, Foldable, Traversable)
deriveShow1 ''MultF
instance ShowF MultF where; showsPrecF = showsPrec

class AsMultF s a | s -> a where
  _MultF :: Prism' s (a, a)

instance AsMultF (MultF a) a where
  {-# inline _MultF #-}
  _MultF = prism' (uncurry MultF) (\(MultF a b) -> Just (a, b))

instance Ctor1 s MultF => AsMultF (Variant1 s a) a where
  {-# inline _MultF #-}
  _MultF = _Ctor1 @MultF . _MultF

instance Ctor1 s MultF => AsMultF (Mu (Variant1 s)) (Mu (Variant1 s)) where
  {-# inline _MultF #-}
  _MultF = from _Mu . _MultF



newtype IntF a = IntF Int
  deriving (Eq, Show, Functor, Foldable, Traversable)
deriveShow1 ''IntF
instance ShowF IntF where; showsPrecF = showsPrec

class AsIntF s a | s -> a where
  _IntF :: Prism' s Int

instance AsIntF (IntF a) a where
  {-# inline _IntF #-}
  _IntF = prism' IntF (\(IntF a) -> Just a)

instance Ctor1 s IntF => AsIntF (Variant1 s a) a where
  {-# inline _IntF #-}
  _IntF = _Ctor1 @IntF . _IntF

instance Ctor1 s IntF => AsIntF (Mu (Variant1 s)) (Mu (Variant1 s)) where
  {-# inline _IntF #-}
  _IntF = from _Mu . _IntF



data ExprF a
  = VarF String
  | LamF String a
  | AppF a a
  deriving (Eq, Show, Functor, Foldable, Traversable)
deriveShow1 ''ExprF
instance ShowF ExprF where; showsPrecF = showsPrec
makeClassyPrisms ''ExprF

instance Ctor1 s ExprF => AsExprF (Variant1 s a) a where
  {-# inline _ExprF #-}
  _ExprF = _Ctor1 @ExprF

instance Ctor1 s ExprF => AsExprF (Mu (Variant1 s)) (Mu (Variant1 s)) where
  {-# inline _ExprF #-}
  _ExprF = from _Mu . _ExprF

  {-# inline _LamF #-}
  _LamF = from _Mu . _LamF

  {-# inline _VarF #-}
  _VarF = from _Mu . _VarF

  {-# inline _AppF #-}
  _AppF = from _Mu . _AppF



{-# inline elimPlusZero #-}
elimPlusZero :: (AsAddF s s, AsIntF s s) => s -> Maybe s
elimPlusZero s = do
  (a, b) <- s ^? _AddF
  asum
    [ do
        n <- a ^? _IntF
        if n == 0 then Just b else Nothing
    , do
        n <- b ^? _IntF
        if n == 0 then Just a else Nothing
    ]

{-# inline elimMultOne #-}
elimMultOne :: (AsMultF s s, AsIntF s s) => s -> Maybe s
elimMultOne s = do
  (a, b) <- s ^? _MultF
  asum
    [ do
        n <- a ^? _IntF
        if n == 1 then Just b else Nothing
    , do
        n <- b ^? _IntF
        if n == 1 then Just a else Nothing
    ]

type Expr = Mu (Variant1 '[ExprF, AddF, MultF, IntF])

optimize :: Expr -> Expr
optimize = rewriteOf plateMu (getAlt . foldMap (Alt .) opts)
  where
    opts :: [Expr -> Maybe Expr]
    opts =
      [ elimMultOne
      , elimPlusZero
      ]



newtype PlaceholderF b a = PlaceholderF b
  deriving (Eq, Show, Functor, Foldable, Traversable)
deriveShow1 ''PlaceholderF
instance Show b => ShowF (PlaceholderF b) where; showsPrecF = showsPrec

class AsPlaceholderF s b a | s -> b a where
  _PlaceholderF :: Prism' s b

instance AsPlaceholderF (PlaceholderF b a) b a where
  {-# inline _PlaceholderF #-}
  _PlaceholderF = prism' PlaceholderF (\(PlaceholderF a) -> Just a)

instance Ctor1 s (PlaceholderF b) => AsPlaceholderF (Variant1 s a) b a where
  {-# inline _PlaceholderF #-}
  _PlaceholderF = _Ctor1 @(PlaceholderF b) . _PlaceholderF

instance
  Ctor1 s (PlaceholderF b) =>
  AsPlaceholderF (Mu (Variant1 s)) b (Mu (Variant1 s)) where

  {-# inline _PlaceholderF #-}
  _PlaceholderF = from _Mu . _PlaceholderF

type ExprP b = Mu (Variant1 '[ExprF, AddF, MultF, PlaceholderF b, IntF])

fillPlaceHolders :: (b -> Expr) -> ExprP b -> Expr
fillPlaceHolders f =
  restructure $
  elimV1 (_Ctor1 @ExprF #) $
  elimV1 (_Ctor1 @AddF #) $
  elimV1 (_Ctor1 @MultF #) $
  elimV1 (\(PlaceholderF b) -> unFix $ f b) $
  elimV1 (_Ctor1 @IntF #) $
  absurdV1



-- | We don't get any value from inlining subst
subst :: AsExprF s s => String -> s -> s -> s
subst n e ex =
  fromMaybe ex $
  (\(n', body) -> _LamF # (n', if n == n' then body else subst n e body)) <$> (ex ^? _LamF) <|>
  (\(f, x) -> _AppF # (subst n e f, subst n e x)) <$> (ex ^? _AppF) <|>
  (\n' -> if n == n' then e else _VarF # n') <$> (ex ^? _VarF)

{-# inline stepIntF #-}
stepIntF :: AsIntF s a => (s -> Maybe s) -> s -> Maybe s
stepIntF _ s = Nothing

{-# inline stepAddF #-}
stepAddF :: (AsAddF s s, AsIntF s s) => (s -> Maybe s) -> s -> Maybe s
stepAddF step s = do
  (a, b) <- s ^? _AddF
  asum
    [ (\x -> _AddF # (x, b)) <$> step a
    , (\x -> _AddF # (a, x)) <$> step b
    , do
        a' <- a ^? _IntF
        b' <- b ^? _IntF
        pure $ _IntF # (a' + b')
    ]

{-# inline stepMultF #-}
stepMultF :: (AsMultF s s, AsIntF s s) => (s -> Maybe s) -> s -> Maybe s
stepMultF step s = do
  (a, b) <- s ^? _MultF
  asum
    [ (\x -> _MultF # (x, b)) <$> step a
    , (\x -> _MultF # (a, x)) <$> step b
    , do
        a' <- a ^? _IntF
        b' <- b ^? _IntF
        pure $ _IntF # (a' * b')
    ]

{-# inline stepExprF_lazy #-}
stepExprF_lazy :: AsExprF s s => (s -> Maybe s) -> s -> Maybe s
stepExprF_lazy step s = do
  (a, b) <- s ^? _AppF
  asum
    [ (\x -> _AppF # (x, b)) <$> step a
    , do
        (n, body) <- a ^? _LamF
        pure (subst n b body)
    ]

{-# inline stepExprF_strict #-}
stepExprF_strict :: AsExprF s s => (s -> Maybe s) -> s -> Maybe s
stepExprF_strict step s = do
  (a, b) <- s ^? _AppF
  asum
    [ (\x -> _AppF # (x, b)) <$> step a
    , (\x -> _AppF # (a, x)) <$> step b
    , do
        (n, body) <- a ^? _LamF
        pure (subst n b body)
    ]

{-# inline buildEval #-}
buildEval :: [(s -> Maybe s) -> s -> Maybe s] -> s -> Maybe s
buildEval steps = go
  where
    go = getAlt . foldMap (\f -> Alt . f go) steps

eval :: Expr -> Maybe Expr
eval =
  buildEval
  [ stepIntF
  , stepAddF
  , stepMultF
  , stepExprF_lazy
  ]