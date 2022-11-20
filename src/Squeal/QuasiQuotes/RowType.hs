{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{- |
  Description: Monomorphic squeal row types.

  This module creates a type family that converts (via a type family)
  a squeal row type into a specific, monomorphic tuple representation
  meant to be consumed by the user. The purpose of this so that the squeal
  quasiquoter won't produce polymorphic types, though it will produce
  a *different* monomorphic type depending on the columns returned by
  the query. The reason we want that is to help type inference as much
  as possible. Squeal already has some problems with type inference,
  and burden on the user of navigating them is only likely to increase
  when a lot of the squeal "code" itself is hidden behind a quasiquoter.
-}
module Squeal.QuasiQuotes.RowType (
  RowType,
  query,
  Field(..),
) where

import Data.Text (Text)
import GHC.OverloadedLabels (fromLabel)
import GHC.TypeLits (Symbol)
import Generics.SOP (SListI)
import Squeal.PostgreSQL (NullType(NotNull, Null), (:::))
import qualified Squeal.PostgreSQL as Squeal


type family RowType a where
  RowType (fld ::: 'NotNull typ ': more) =
    (Field fld (PGToHaskell typ), RowType more)
  RowType (fld ::: 'Null typ ': more) =
    (Field fld (Maybe (PGToHaskell typ)), RowType more)
  RowType ('[]) = ()

type family PGToHaskell (a :: Squeal.PGType) where
  PGToHaskell 'Squeal.PGbool = Bool
  PGToHaskell 'Squeal.PGtext = Text


data Field (name :: Symbol) a = Field
  { unField :: a
  }
instance (Squeal.FromValue pg hask) => Squeal.FromValue pg (Field name hask) where
  fromValue mbs = Field @name <$> Squeal.fromValue @pg mbs


query
  :: forall db params input row ignored.
     ( HasRowDecoder row (RowType row)
     , SListI row
     , Squeal.GenericParams db params input ignored
     )
  => Squeal.Query '[] '[] db params row
  -> Squeal.Statement db input (RowType row)
query q =
  Squeal.Query
    Squeal.genericParams
    getRowDecoder
    q


class HasRowDecoder row x where
  getRowDecoder :: Squeal.DecodeRow row x

instance (Squeal.FromValue typ t, HasRowDecoder spec more) => HasRowDecoder ((fld ::: typ) ': spec)  (Field fld t, more) where
  getRowDecoder = do
    Squeal.consRow
      (,)
      (fromLabel @fld)
      (getRowDecoder @spec @more)
instance () => HasRowDecoder '[] () where
  getRowDecoder = pure ()

-- class DecodeField pgtype a where
--   decodeField


