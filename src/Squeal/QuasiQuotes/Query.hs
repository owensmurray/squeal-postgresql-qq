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
  Star), from, select, table)
import Squeal.QuasiQuotes.RowType (query)
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
        > query (select Star (from (table (#theTable `as` #theTable))))
      -}
      pure $
        VarE 'query
        `AppE`
          (
            VarE 'select
            `AppE` renderSelection selectionList
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


renderSelection :: [(AST.ScalarExpr, Maybe alias)] -> Exp
renderSelection selectionList =
  case selectionList of
    [(AST.Star, Nothing)] -> ConE 'Star
    [(AST.Iden (AST.Name _ name:more), Nothing)] ->
      foldl'
        (\acc (AST.Name _ n) ->
          VarE '(!) `AppE` acc `AppE` LabelE n
        )
        (LabelE name)
        more
    field:more ->
      {-
        the order of these arguments to 'Also' seems backwards, but
        for whatever reason this produces a row type with the fields
        in the right order. I'm not sure if Squeal gets the backwards,
        or the sql parser does, or if we are reversing them somewhere.
      -}
      ConE 'Also
      `AppE` renderSelection more
      `AppE` renderSelection [field]
    _ -> undefined


