{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# HLINT ignore "Use unwords" #-}

module Lib
  ( Showable(..)
  ) where

import Relude hiding (State)
import GHC.Generics

data State = Inf String | Pref | Rec | Tup

class Showable a where
  show' :: a -> String
  default show' :: (Generic a, GShow (Rep a)) => a -> String
  show' = gshow Pref . from

class GShow f where
  gshow :: State -> f p -> String

instance GShow V1 where
  gshow _ = error "gshow: V1"

instance GShow U1 where
  gshow _ _ = ""

instance (GShow a, GShow b) => GShow (a :*: b) where
  gshow t (a :*: b) = case t of
    Inf c -> intercalate " " [gshow t a, c, gshow t b]
    Pref -> gshow t a <> " " <> gshow t b
    Rec -> gshow t a <> ", " <> gshow t b
    Tup -> gshow t a <> ", " <> gshow t b

instance (GShow a, GShow b) => GShow (a :+: b) where
  gshow t (L1 x) = gshow t x
  gshow t (R1 x) = gshow t x

instance (Showable a) => GShow (K1 i a) where
  gshow _t (K1 x) = show' x


-- >>> data X = X Int deriving (Generic, Showable)
-- >>> show' (X 1)
-- "1"
instance (Datatype meta, GShow f) => GShow (D1 meta f) where
  gshow t (M1 x) = gshow t x


-- >>> data X = X { a :: Int, b :: Int } deriving (Generic, Showable)
-- >>> show' (X 1 2)
-- "X { a = 1, b = 2 }"
-- >>> data Y = Y Int Int deriving (Generic, Showable)
-- >>> show' (Y 1 2)
-- "(Y 1 2)"
instance (Selector meta, GShow f) => GShow (S1 meta f) where
  gshow t s@(M1 x) = case selName s of
    "" -> gshow t x
    name -> name <> " = " <> gshow t x


defShow :: (Generic a, GShow (Rep a)) => a -> String
defShow = gshow Pref . from

instance (Constructor meta, GShow f) => GShow (C1 meta f) where
  gshow _t c@(M1 x) = if
    | conIsRecord c -> name <> " { " <> gshow Rec x <> " }"
    | conIsTuple -> "(" <> gshow Tup x <> ")"
    | otherwise -> case conFixity c of
        Prefix -> "(" <> name <> " " <> gshow Pref x <> ")"
        Infix _as _n -> "(" <> gshow (Inf name) x <> ")"
    where
      name = conName c
      conIsTuple = case name of
        ('(':',' :_) -> True
        _ -> False

instance Showable Int where
  show' = show

instance {-# OVERLAPPABLE #-} Showable a => Showable [a] where
  show' ls = "[" <> intercalate "," (map show' . toList $ ls) <> "]"

instance {-# OVERLAPPING  #-} Showable String where
  show' = show

instance {-# OVERLAPPING  #-} Showable Bool where
  show' = defShow

instance (Showable a, Showable b) => Showable (a, b) where
  show' = defShow

instance (Showable a, Showable b, Showable c) => Showable (a, b, c) where
  show' = defShow

instance (Showable a, Showable b, Showable c, Showable d) => Showable (a, b, c, d) where
  show' = defShow
