{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

{- | Description: Translate query expressions. -}
module Squeal.QuasiQuotes.Query (
  toSquealQuery,
) where

import Data.List (foldl')
import Language.Haskell.TH.Syntax (Exp(AppE, ConE, LabelE, VarE), Q)
import Language.SQL.SimpleSQL.Syntax (Name(Name), QueryExpr(Select,
  qeFrom, qeGroupBy, qeHaving, qeOffset, qeOrderBy, qeSelectList,
  qeSetQuantifier, qeWhere), ScalarExpr(BinOp, Iden, Star),
  SetQuantifier(SQDefault), TableRef(TRSimple))
import Prelude (Applicative(pure), Maybe(Just, Nothing), MonadFail(fail),
  Semigroup((<>)), Show(show), ($), error)
import Squeal.QuasiQuotes.RowType (monoQuery)
import qualified Squeal.PostgreSQL as S


toSquealQuery :: QueryExpr -> Q Exp
toSquealQuery = \case
  Select
      { qeSetQuantifier = SQDefault
      , qeSelectList    = selectionList
      , qeFrom          = [selectionFrom]
      , qeWhere         = Nothing
      , qeGroupBy       = []
      , qeHaving        = Nothing
      , qeOrderBy       = []
      , qeOffset        = Nothing
      }
    ->
      {-
        moral equivalent of:
        > monoQuery (select Star (from (table (#theTable `as` #theTable))))
      -}
      pure $
        VarE 'monoQuery
        `AppE`
          (
            VarE 'S.select
            `AppE` renderSelectionList selectionList
            `AppE`
              (
                VarE 'S.from
                `AppE`
                  renderTableRef selectionFrom
              )
          )
  unsupported ->
    fail $ "Unsupported: " <> show unsupported


renderTableRef :: TableRef -> Exp
renderTableRef = \case
  TRSimple [Name Nothing theTable] ->
    VarE 'S.table
    `AppE`
      (
        VarE 'S.as
        `AppE` LabelE theTable
        `AppE` LabelE theTable
      )
  unsupported ->
    error $ "Unsupported: " <> show unsupported


renderSelectionList :: [(ScalarExpr, Maybe Name)] -> Exp
renderSelectionList selectionList =
    case selectionList of
      [(field, alias)] ->
        renderAlias (renderScalarExpr field) alias
      (field, alias):more ->
        {-
          the order of these arguments to 'Also' seems backwards, but
          for whatever reason this produces a row type with the fields
          in the right order. I'm not sure if Squeal gets it backwards,
          or the sql parser does, or if we are reversing them somewhere.
        -}
        ConE 'S.Also
        `AppE` renderSelectionList more
        `AppE` renderAlias (renderScalarExpr field) alias
      [] -> error "Empty selection list not supported."
  where
    renderAlias :: Exp -> Maybe Name -> Exp
    renderAlias e = \case
      Nothing -> e
      Just (Name _ alias) ->
        VarE 'S.as
        `AppE` e
        `AppE` LabelE alias


renderScalarExpr :: ScalarExpr -> Exp
renderScalarExpr = \case
  Star -> ConE 'S.Star
  Iden (Name _ name:more) ->
    foldl'
      (\acc (Name _ n) ->
        VarE '(S.!) `AppE` acc `AppE` LabelE n
      )
      (LabelE name)
      more
  BinOp left [Name Nothing "."] Star ->
    ConE 'S.DotStar `AppE` renderScalarExpr left
  unsupported ->
    error $ "unsupported: " <> show unsupported


