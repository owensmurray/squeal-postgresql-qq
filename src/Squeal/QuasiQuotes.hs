{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE LambdaCase #-}

{-|
  Description: quasiquoter understanding SQL and producing
  `squeal-postgresql` expressions.
-}
module Squeal.QuasiQuotes (
  ssql,
  Field(..),
) where


import Language.Haskell.TH.Quote (QuasiQuoter(QuasiQuoter, quoteDec,
  quoteExp, quotePat, quoteType))
import Language.Haskell.TH.Syntax (Exp, Q, runIO)
import Language.SQL.SimpleSQL.Dialect (postgres)
import Language.SQL.SimpleSQL.Parse (ParseError, parseStatement)
import Prelude (Applicative(pure), Either(Left, Right), Maybe(Nothing),
  MonadFail(fail), Semigroup((<>)), Show(show), ($), (.), error, print)
import Squeal.QuasiQuotes.Insert (toSquealInsert)
import Squeal.QuasiQuotes.Query (toSquealQuery)
import Squeal.QuasiQuotes.RowType (Field(Field, unField))
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
  Right statement -> do
    runIO (print statement)
    toSquealStatement statement


toSquealStatement :: AST.Statement -> Q Exp
toSquealStatement = \case
  AST.SelectStatement theQuery -> toSquealQuery theQuery
  AST.Insert into fields values ->
    pure $ toSquealInsert into fields values
  unsupported ->
    error $ "Unsupported: " <> show unsupported


