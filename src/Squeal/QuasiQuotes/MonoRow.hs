{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
  Description: Monomorphic squeal row types.

  This module provides a type family that converts a squeal row type into
  a specific, monomorphic tuple representation meant to be consumed by the
  user. The purpose of this so that the squeal quasiquoter won't produce
  polymorphic types, though it will produce a *different* monomorphic
  type depending on the columns returned by the query. The reason we want
  this is to help type inference as much as possible. Squeal already
  has some problems with type inference, and the burden on the user of
  navigating them is only likely to increase when a lot of the squeal
  "code" itself is hidden behind a quasiquoter.
-}
module Squeal.QuasiQuotes.MonoRow (
  MonoRow,
  monoQuery,
  monoManipulation,
  Field (..),
) where

import Data.Aeson (Value)
import Data.Int (Int32, Int64)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Time (Day, UTCTime)
import Data.UUID (UUID)
import GHC.TypeLits (Symbol)
import Generics.SOP (SListI)
import Prelude (Applicative(pure), (<$>), Bool, Maybe)
import Squeal.PostgreSQL
  ( FromValue(fromValue), IsLabel(fromLabel), NullType(NotNull, Null)
  , PGType
    ( PGbool, PGdate, PGint4, PGint8, PGjson, PGjsonb, PGnumeric, PGtext
    , PGtimestamptz, PGuuid
    )
  , (:::), Json, Jsonb
  )
import qualified Squeal.PostgreSQL as Squeal




{- FOURMOLU_DISABLE -}
type family MonoRow a = b | b -> a where
  {-
    It would be more convenient to use a helper type family here that
    would map PGtypes to Haskell types. But if we did that, we would
    not be able to make this type family injective.

    This is Because GHC would not be able to tell that the right-hand
    side of this type family did not overlap even if that the helper
    type family were itself injective. Injectivity of the helper is not
    enough. The specific helper definition must happen to not produce
    instances that overlap with any Maybe type when used here.

    For instance, this example helper type family is injective, but when
    used here it would produce overlapping values for the `NotNull PGint4`
    and `Null PGbool` equations.

    type family Helper x = a | a -> x where
      Helper Int32 = Maybe Bool
      Helper Bool = Bool
  -}
  MonoRow (fld ::: 'NotNull PGbool ': more) = (Field fld Bool, MonoRow more)
  MonoRow (fld ::: 'NotNull PGint4 ': more) = (Field fld Int32, MonoRow more)
  MonoRow (fld ::: 'NotNull PGint8 ': more) = (Field fld Int64, MonoRow more)
  MonoRow (fld ::: 'NotNull PGtext ': more) = (Field fld Text, MonoRow more)
  MonoRow (fld ::: 'NotNull PGuuid ': more) = (Field fld UUID, MonoRow more)
  MonoRow (fld ::: 'NotNull PGdate ': more) = (Field fld Day, MonoRow more)
  MonoRow (fld ::: 'NotNull PGtimestamptz ': more) = (Field fld UTCTime, MonoRow more)
  MonoRow (fld ::: 'NotNull PGjsonb ': more) = (Field fld (Jsonb Value), MonoRow more)
  MonoRow (fld ::: 'NotNull PGjson ': more) = (Field fld (Json Value), MonoRow more)
  MonoRow (fld ::: 'NotNull PGnumeric ': more) = (Field fld Scientific, MonoRow more)

  MonoRow (fld ::: 'Null PGbool ': more) = (Field fld (Maybe Bool), MonoRow more)
  MonoRow (fld ::: 'Null PGint4 ': more) = (Field fld (Maybe Int32), MonoRow more)
  MonoRow (fld ::: 'Null PGint8 ': more) = (Field fld (Maybe Int64), MonoRow more)
  MonoRow (fld ::: 'Null PGtext ': more) = (Field fld (Maybe Text), MonoRow more)
  MonoRow (fld ::: 'Null PGuuid ': more) = (Field fld (Maybe UUID), MonoRow more)
  MonoRow (fld ::: 'Null PGdate ': more) = (Field fld (Maybe Day), MonoRow more)
  MonoRow (fld ::: 'Null PGtimestamptz ': more) = (Field fld (Maybe UTCTime), MonoRow more)
  MonoRow (fld ::: 'Null PGjsonb ': more) = (Field fld (Maybe (Jsonb Value)), MonoRow more)
  MonoRow (fld ::: 'Null PGjson ': more) = (Field fld (Maybe (Json Value)), MonoRow more)
  MonoRow (fld ::: 'Null PGnumeric ': more) = (Field fld (Maybe Scientific), MonoRow more)
  MonoRow '[] = ()
{- FOURMOLU_ENABLE -}


newtype Field (name :: Symbol) a = Field
  { unField :: a
  }
instance
    (Squeal.FromValue pg hask)
  =>
    Squeal.FromValue pg (Field name hask)
  where
    fromValue mbs = Field @name <$> Squeal.fromValue @pg mbs


{- |
  Like 'Squeal.query', but use the monomorphizing 'MonoRow' family to
  fully specify the output rows. This is mainly a convenience to the
  template haskell code so it can simply quote this function instead of
  having to basically inline it directly in TH.
-}
monoQuery
  :: forall db params input row ignored.
     ( HasRowDecoder row (MonoRow row)
     , SListI row
     , Squeal.GenericParams db params input ignored
     )
  => Squeal.Query '[] '[] db params row
  -> Squeal.Statement db input (MonoRow row)
monoQuery = Squeal.Query Squeal.genericParams getRowDecoder


monoManipulation
  :: forall db params input row ignored.
     ( HasRowDecoder row (MonoRow row)
     , SListI row
     , Squeal.GenericParams db params input ignored
     )
  => Squeal.Manipulation '[] db params row
  -> Squeal.Statement db input (MonoRow row)
monoManipulation = Squeal.Manipulation Squeal.genericParams getRowDecoder


class HasRowDecoder row x where
  getRowDecoder :: Squeal.DecodeRow row x
instance
    ( HasRowDecoder moreRow moreFields
    , Squeal.FromValue typ t
    )
  =>
    HasRowDecoder ((fld ::: typ) ': moreRow) (Field fld t, moreFields)
  where
    getRowDecoder =
      Squeal.consRow
        (,)
        (fromLabel @fld)
        (getRowDecoder @moreRow @moreFields)
instance HasRowDecoder '[] () where
  getRowDecoder = pure ()


