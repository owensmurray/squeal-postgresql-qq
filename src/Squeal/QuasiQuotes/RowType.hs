{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wwarn #-}

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
  ParamsType,
  query,
  Statement(..),
  Field(..),
) where

import Data.Text (Text)
import GHC.OverloadedLabels (fromLabel)
import GHC.TypeLits
import Generics.SOP (All, SListI)
import Squeal.PostgreSQL (NullType(NotNull, Null), (:::))
import qualified Squeal.PostgreSQL as Squeal


type family RowType a where
  RowType (fld ::: 'NotNull typ ': more) =
    (Field fld (PGToHaskell typ), RowType more)
  RowType (fld ::: 'Null typ ': more) =
    (Field fld (Maybe (PGToHaskell typ)), RowType more)
  RowType ('[]) = ()

type family PGToHaskell (a :: Squeal.PGType) where
  PGToHaskell Squeal.PGbool = Bool
  PGToHaskell Squeal.PGtext = Text


data Field (name :: Symbol) a = Field
  { unField :: a
  }
instance (Squeal.FromValue pg hask) => Squeal.FromValue pg (Field name hask) where
  fromValue mbs = Field @name <$> Squeal.fromValue @pg mbs


type family ParamsType a where
  ParamsType '[] = ()


{- |
  A 'Squeal.Statement', but wrapped so that it can only be constructed
  by this package.
-}
newtype Statement db params row = Statement
  { unStatement :: Squeal.Statement db params row
  }


query
  :: forall db params row.
     ( All (Squeal.OidOfNull db) params
     , Decode row (RowType row)
     , Encode db params (ParamsType params)
     , SListI row
     )
  => Squeal.Query '[] '[] db params row
  -> Statement db (ParamsType params) (RowType row)
query q =
  Statement $
    Squeal.Query
      encodeParams
      decodeRow
      q


class Encode db (params :: [Squeal.NullType]) x | x -> params where
  encodeParams :: Squeal.EncodeParams db params x
instance Encode db '[] () where
  encodeParams = Squeal.nilParams

class Decode row x where
  decodeRow :: Squeal.DecodeRow row x

instance (Squeal.FromValue typ t, Decode spec more) => Decode ((fld ::: typ) ': spec)  (Field fld t, more) where
  decodeRow = do
    Squeal.consRow
      (,)
      (fromLabel @fld)
      (decodeRow @spec @more)
instance () => Decode '[] () where
  decodeRow = pure ()

-- class DecodeField pgtype a where
--   decodeField

