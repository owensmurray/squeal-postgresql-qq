{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
  Description: quasiquoter understanding SQL and producing
  `squeal-postgresql` expressions.
-}
module Squeal.QuasiQuotes (
  ssql,
  Field (..),
) where

import Language.Haskell.TH.Quote
  ( QuasiQuoter(QuasiQuoter, quoteDec, quoteExp, quotePat, quoteType)
  )
import Language.Haskell.TH.Syntax (Exp, Q, runIO)
import Prelude
  ( Either(Left, Right), MonadFail(fail), Semigroup((<>)), Show(show), ($), (.)
  , String, error, print
  )
import Squeal.QuasiQuotes.Insert (toSquealInsert)
import Squeal.QuasiQuotes.Query (toSquealQuery)
import Squeal.QuasiQuotes.RowType (Field(Field, unField))
import qualified Data.Text as Text
import qualified PostgresqlSyntax.Ast as PGT_AST
import qualified PostgresqlSyntax.Parsing as PGT_Parse


ssql :: QuasiQuoter
ssql =
  QuasiQuoter
    { quoteExp =
        toSqueal . PGT_Parse.run PGT_Parse.preparableStmt . Text.strip . Text.pack
    , quotePat = error "pattern quotes not supported"
    , quoteType = error "type quotes not supported"
    , quoteDec = error "declaration quotes not supported"
    }


toSqueal :: Either String PGT_AST.PreparableStmt -> Q Exp
toSqueal = \case
  Left err -> fail err
  Right statement -> do
    runIO (print statement)
    toSquealStatement statement


toSquealStatement :: PGT_AST.PreparableStmt -> Q Exp
toSquealStatement = \case
  PGT_AST.SelectPreparableStmt theQuery -> toSquealQuery theQuery
  PGT_AST.InsertPreparableStmt stmt -> toSquealInsert stmt
  unsupported ->
    error $ "Unsupported statement: " <> show unsupported


