{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

{-| Commonplace renderers shared by other modules. -}
module Squeal.QuasiQuotes.Common (
  renderTableRef,
  renderScalarExpr,
) where


import Data.List (foldl')
import Language.Haskell.TH.Syntax (Exp(AppE, ConE, LabelE, VarE))
import Language.SQL.SimpleSQL.Syntax (JoinCondition(JoinOn),
  JoinType(JLeft), Name(Name), ScalarExpr(BinOp, Iden, Star),
  TableRef(TRJoin, TRSimple))
import Prelude (Bool(False), Maybe(Just, Nothing), Semigroup((<>)),
  Show(show), ($), error)
import qualified Squeal.PostgreSQL as S


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
    TRJoin left False JLeft right (Just condition) ->
      VarE 'S.leftOuterJoin
      `AppE` renderTableRef right
      `AppE` renderJoinCondition condition
      `AppE` renderTableRef left
    unsupported ->
      error $ "Unsupported: " <> show unsupported
  where
    renderJoinCondition = \case
      (JoinOn expr) ->
        renderScalarExpr expr
      unsupported ->
        error $ "Unsupported: " <> show unsupported


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
  BinOp left [Name Nothing "="] right ->
    VarE '(S..==)
      `AppE` renderScalarExpr left
      `AppE` renderScalarExpr right
  unsupported ->
    error $ "unsupported: " <> show unsupported


