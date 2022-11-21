{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

{- | Description: Translate query expressions. -}
module Squeal.QuasiQuotes.Query (
  toSquealQuery,
) where

import Data.List (foldl')
import Language.Haskell.TH.Syntax (Exp(AppE, ConE, LabelE, VarE), Q)
import Squeal.PostgreSQL (Aliasable(as), IsQualified((!)), Selection(Also,
  DotStar, Star), from, select, table)
import Squeal.QuasiQuotes.RowType (monoQuery)
import qualified Language.SQL.SimpleSQL.Syntax as AST


toSquealQuery :: AST.QueryExpr -> Q Exp
toSquealQuery = \case
  AST.Select
      { AST.qeSetQuantifier = AST.SQDefault
      , AST.qeSelectList    = selectionList
      , AST.qeFrom          = [AST.TRSimple [AST.Name Nothing theTable]]
      , AST.qeWhere         = Nothing
      , AST.qeGroupBy       = []
      , AST.qeHaving        = Nothing
      , AST.qeOrderBy       = []
      , AST.qeOffset        = Nothing
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
            VarE 'select
            `AppE` renderSelectionList selectionList
            `AppE`
              (
                VarE 'from
                `AppE`
                  (
                    VarE 'table
                    `AppE`
                      (
                        VarE 'as
                        `AppE` LabelE theTable
                        `AppE` LabelE theTable
                      )
                  )
              )
          )
  unsupported ->
    fail $ "Unsupported: " <> show unsupported


renderSelectionList :: [(AST.ScalarExpr, Maybe AST.Name)] -> Exp
renderSelectionList selectionList =
    case selectionList of
      [(field, alias)] ->
        renderAlias (renderSelection field) alias
      (field, alias):more ->
        {-
          the order of these arguments to 'Also' seems backwards, but
          for whatever reason this produces a row type with the fields
          in the right order. I'm not sure if Squeal gets it backwards,
          or the sql parser does, or if we are reversing them somewhere.
        -}
        ConE 'Also
        `AppE` renderSelectionList more
        `AppE` renderAlias (renderSelection field) alias
      [] -> error "Empty selection list not supported."
  where
    renderAlias :: Exp -> Maybe AST.Name -> Exp
    renderAlias e = \case
      Nothing -> e
      Just (AST.Name _ alias) ->
        VarE 'as
        `AppE` e
        `AppE` LabelE alias

    renderSelection :: AST.ScalarExpr -> Exp
    renderSelection = \case
      AST.Star -> ConE 'Star
      AST.Iden (AST.Name _ name:more) ->
        foldl'
          (\acc (AST.Name _ n) ->
            VarE '(!) `AppE` acc `AppE` LabelE n
          )
          (LabelE name)
          more
      AST.BinOp (AST.Iden [AST.Name _ name]) [AST.Name Nothing "."] AST.Star ->
        ConE 'DotStar `AppE` LabelE name
      _ -> error $ "unsupported selection: " <> show selectionList


