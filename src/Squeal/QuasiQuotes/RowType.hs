{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
  monoQuery,
  Field(..),
) where

import Data.Kind (Type)
import Data.Text (Text)
import Data.UUID (UUID)
import GHC.OverloadedLabels (fromLabel)
import GHC.TypeLits (Symbol)
import Generics.SOP (SListI)
import Squeal.PostgreSQL (NullType(NotNull, Null), (:::))
import qualified Squeal.PostgreSQL as Squeal


{- |
  Given an Squeal row specification, produce a corresponding haskell
  tuple type of the form:

  > (Field name1 type1,
  > (Field name2 type2,
  > (Field name3 type3,
  > <continue nesting>,
  > ()))...)


  where the "name<N>" are phantom types of kind `Symbol`, which provide
  the name of the corresponding column, and types "type<N>" are whatever
  appropriate haskell type represents the postgres column type.
-}
type family RowType a where
  RowType (fld ::: 'NotNull typ ': more) =
    (Field fld (UnPG typ), RowType more)
  RowType (fld ::: 'Null typ ': more) =
    (Field fld (Maybe (UnPG typ)), RowType more)
  RowType '[] = ()


{- | Converts PGTypes to Haskell types. This is the inverse of 'Squeal.PG'. -}
type family UnPG (a :: Squeal.PGType) :: Type where
  UnPG 'Squeal.PGbool = Bool
  UnPG 'Squeal.PGtext = Text
  UnPG 'Squeal.PGuuid = UUID
  {- Not complete ...  -}


newtype Field (name :: Symbol) a = Field
  { unField :: a
  }
instance
    ( Squeal.FromValue pg hask
    )
  =>
    Squeal.FromValue pg (Field name hask)
  where
    fromValue mbs = Field @name <$> Squeal.fromValue @pg mbs


{- |
  Like 'Squeal.query', but use the monomorphizing 'RowType' family to
  fully specify the output rows. This is mainly a convenience to the
  template haskell code so it can simply quote this function instead of
  having to basically inline it directly in TH.
-}
monoQuery
  :: forall db params input row ignored.
     ( HasRowDecoder row (RowType row)
     , SListI row
     , Squeal.GenericParams db params input ignored
     )
  => Squeal.Query '[] '[] db params row
  -> Squeal.Statement db input (RowType row)
monoQuery = Squeal.Query Squeal.genericParams getRowDecoder


class HasRowDecoder row x where
  getRowDecoder :: Squeal.DecodeRow row x
instance
    ( HasRowDecoder moreRow moreFields
    , Squeal.FromValue typ t
    )
  =>
    HasRowDecoder ((fld ::: typ) ': moreRow)  (Field fld t, moreFields)
  where
    getRowDecoder =
      Squeal.consRow
        (,)
        (fromLabel @fld)
        (getRowDecoder @moreRow @moreFields)
instance () => HasRowDecoder '[] () where
  getRowDecoder = pure ()


