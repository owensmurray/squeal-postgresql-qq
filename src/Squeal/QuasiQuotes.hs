{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Squeal.QuasiQuotes (
  ssql,
  Field(..),
) where


import Language.Haskell.TH.Quote (QuasiQuoter(QuasiQuoter, quoteDec,
  quoteExp, quotePat, quoteType))
import Language.Haskell.TH.Syntax (Exp(AppE, ConE, LabelE, VarE), Q)
import Language.SQL.SimpleSQL.Dialect (postgres)
import Language.SQL.SimpleSQL.Parse (ParseError, parseStatement)
import Squeal.PostgreSQL (Aliasable(as), Selection(Star), from, select,
  table)
import Squeal.QuasiQuotes.RowType (Field(Field, unField), query)
import qualified Language.SQL.SimpleSQL.Syntax as AST


ssql :: QuasiQuoter  
ssql =
  QuasiQuoter  
    { quoteExp = toSqueal . parseStatement postgres "" Nothing
    , quotePat = error "pattern quotes not supported"
    , quoteType = error "type quotes not supported"
    , quoteDec = error "declaration quotes not supported"
    }


toSqueal :: Either ParseError AST.Statement -> Q Exp
toSqueal = \case
  Left err -> fail (show err)
  Right statement -> toSquealStatement statement

toSquealStatement :: AST.Statement -> Q Exp
toSquealStatement = \case
  AST.SelectStatement theQuery -> toSquealQuery theQuery
  unsupported ->
    error $ "Unsupported: " <> show unsupported


toSquealQuery :: AST.QueryExpr -> Q Exp
toSquealQuery = \case
  {-
    For this prototype, we only match a _very_ limited subset of sql.. In
    particular, we match:
    > select * from <tablename>
  -}
  AST.Select
      { AST.qeSetQuantifier = AST.SQDefault
      , AST.qeSelectList    = [(AST.Star, Nothing)]
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
        >
          let
            theQuery = query (select Star (from (table (#theTable `as` #theTable))))
          in
            theQuery
      -}
      pure $
        VarE 'query
        `AppE`
          (
            VarE 'select
            `AppE` ConE 'Star
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
    error $ "Unsupported: " <> show unsupported


