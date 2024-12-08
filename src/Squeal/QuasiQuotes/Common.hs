{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE ViewPatterns #-}

{-| Commonplace renderers shared by other modules. -}
module Squeal.QuasiQuotes.Common (
  renderTableRef,
  renderScalarExpr,
) where


import Data.List (foldl')
import Data.String (IsString(fromString))
import Language.Haskell.TH.Syntax (Exp(AppE, AppTypeE, ConE, LabelE,
  LitE, VarE), Lit(StringL), TyLit(NumTyLit), Type(LitT))
import Language.SQL.SimpleSQL.Syntax (JoinCondition(JoinOn),
  JoinType(JLeft), Name(Name), ScalarExpr(BinOp, Iden, NumLit,
  PositionalArg, Star, StringLit), TableRef(TRJoin, TRSimple))
import Prelude (Bool(False), Functor(fmap), Integral(toInteger),
  Maybe(Just, Nothing), Semigroup((<>)), Show(show), ($), error)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Char as Char
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
  Iden [Name Nothing (fmap Char.toLower -> "null")] ->
    VarE 'S.null_
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
  NumLit num ->
    ConE 'S.UnsafeExpression
      `AppE` (
          VarE 'BS8.pack
            `AppE` LitE (StringL num)
        )
  StringLit _ _ str ->
    VarE 'fromString `AppE` LitE (StringL str)
  PositionalArg n ->
    VarE 'S.param
      `AppTypeE` LitT (NumTyLit (toInteger n))
  unsupported ->
    error $ "unsupported: " <> show unsupported


