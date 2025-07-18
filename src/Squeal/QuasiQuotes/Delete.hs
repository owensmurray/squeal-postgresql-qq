{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE ViewPatterns #-}

-- | Description: Translate delete statements.
module Squeal.QuasiQuotes.Delete (
  toSquealDelete,
) where

import Control.Monad (when)
import Data.Foldable (Foldable(foldr), foldlM)
import Data.Maybe (isJust)
import Language.Haskell.TH.Syntax (Exp(AppE, ConE, LabelE, VarE), Q)
import Prelude
  ( Applicative(pure), Maybe(Just, Nothing), MonadFail(fail), Semigroup((<>))
  , ($), (<$>)
  )
import Squeal.QuasiQuotes.Query
  ( getIdentText, renderPGTAExpr, renderPGTTableRef, renderPGTTargetList
  , toSquealQuery
  )
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as Text
import qualified PostgresqlSyntax.Ast as PGT_AST
import qualified Squeal.PostgreSQL as S


renderPGTWithClause :: PGT_AST.WithClause -> Q ([Text.Text], Exp)
renderPGTWithClause (PGT_AST.WithClause recursive ctes) = do
    when recursive $ fail "Recursive WITH clauses are not supported yet."
    let
      cteList = NE.toList ctes
    (finalCteNames, renderedCtes) <-
      foldlM
        ( \(names, exps) cte -> do
            (name, exp) <- renderCte names cte
            pure (names <> [name], exps <> [exp])
        )
        ([], [])
        cteList

    let
      withExp =
        foldr
          (\cte acc -> ConE '(S.:>>) `AppE` cte `AppE` acc)
          (ConE 'S.Done)
          renderedCtes
    pure (finalCteNames, withExp)
  where
    renderCte :: [Text.Text] -> PGT_AST.CommonTableExpr -> Q (Text.Text, Exp)
    renderCte existingCteNames (PGT_AST.CommonTableExpr ident maybeColNames maybeMaterialized stmt) = do
      when (isJust maybeMaterialized) $
        fail "MATERIALIZED/NOT MATERIALIZED for CTEs is not supported yet."
      cteStmtExp <-
        case stmt of
          PGT_AST.SelectPreparableStmt selectStmt -> do
            queryExp <- toSquealQuery existingCteNames maybeColNames selectStmt
            pure $ VarE 'S.queryStatement `AppE` queryExp
          _ -> fail "Only SELECT statements are supported in CTEs for now."
      let
        cteName = getIdentText ident
      pure
        (cteName, VarE 'S.as `AppE` cteStmtExp `AppE` LabelE (Text.unpack cteName))


toSquealDelete :: PGT_AST.DeleteStmt -> Q Exp
toSquealDelete
  ( PGT_AST.DeleteStmt
      maybeWithClause
      relationExprOptAlias
      maybeUsingClause
      maybeWhereClause
      maybeReturningClause
    ) = do
    (cteNames, renderedWithClause) <-
      case maybeWithClause of
        Nothing -> pure ([], Nothing)
        Just withClause -> do
          (names, exp) <- renderPGTWithClause withClause
          pure (names, Just exp)
    -- whereOrCurrentClause with cursor is not supported yet

    targetTableExp <- renderPGTRelationExprOptAlias' relationExprOptAlias

    usingClauseExp <-
      case maybeUsingClause of
        Nothing -> pure $ ConE 'S.NoUsing
        Just usingClause -> AppE (ConE 'S.Using) <$> renderPGTTableRef cteNames usingClause

    whereConditionExp <-
      case maybeWhereClause of
        Nothing ->
          fail
            "DELETE statements must have a WHERE clause for safety. Use WHERE TRUE to delete all rows."
        Just (PGT_AST.ExprWhereOrCurrentClause whereAExpr) -> renderPGTAExpr cteNames whereAExpr
        Just (PGT_AST.CursorWhereOrCurrentClause _) -> fail "WHERE CURRENT OF is not supported."

    returningClauseExp <-
      case maybeReturningClause of
        Nothing -> pure $ ConE 'S.Returning_ `AppE` ConE 'S.Nil
        Just returningClause -> AppE (ConE 'S.Returning) <$> renderPGTTargetList cteNames returningClause

    let
      deleteBody =
        ( VarE 'S.deleteFrom
            `AppE` targetTableExp
            `AppE` usingClauseExp
            `AppE` whereConditionExp
            `AppE` returningClauseExp
        )
    let
      finalExp = case renderedWithClause of
        Nothing -> deleteBody
        Just withExp -> VarE 'S.with `AppE` withExp `AppE` deleteBody
    pure finalExp


renderPGTRelationExprOptAlias' :: PGT_AST.RelationExprOptAlias -> Q Exp
renderPGTRelationExprOptAlias' (PGT_AST.RelationExprOptAlias relationExpr maybeAlias) = do
  (tableName, schemaName) <-
    case relationExpr of
      PGT_AST.SimpleRelationExpr (PGT_AST.SimpleQualifiedName ident) _ ->
        pure (getIdentText ident, Nothing)
      PGT_AST.SimpleRelationExpr
        ( PGT_AST.IndirectedQualifiedName
            schemaIdent
            (PGT_AST.AttrNameIndirectionEl tableIdent NE.:| [])
          )
        _ ->
          pure (getIdentText tableIdent, Just (getIdentText schemaIdent))
      _ -> fail "Unsupported relation expression in DELETE statement."

  let
    aliasName = case maybeAlias of
      Just (_, colId) -> getIdentText colId
      Nothing -> tableName

  let
    qualifiedAlias = case schemaName of
      Nothing -> LabelE (Text.unpack tableName)
      Just schema ->
        VarE '(S.!)
          `AppE` LabelE (Text.unpack schema)
          `AppE` LabelE (Text.unpack tableName)

  pure $ VarE 'S.as `AppE` qualifiedAlias `AppE` LabelE (Text.unpack aliasName)


