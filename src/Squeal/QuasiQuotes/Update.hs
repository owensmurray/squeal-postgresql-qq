{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE ViewPatterns #-}

-- | Description: Translate update statements.
module Squeal.QuasiQuotes.Update (
  toSquealUpdate,
) where

import Control.Monad (when)
import Data.Foldable (Foldable(foldr), foldlM)
import Data.Maybe (isJust)
import Data.Text (Text)
import Language.Haskell.TH.Syntax (Exp(AppE, ConE, LabelE, VarE), Q)
import Prelude
  ( Applicative(pure), Maybe(Just, Nothing), MonadFail(fail), Semigroup((<>))
  , Traversable(mapM), ($), (<$>)
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


toSquealUpdate :: PGT_AST.UpdateStmt -> Q Exp
toSquealUpdate
  ( PGT_AST.UpdateStmt
      maybeWithClause
      relationExprOptAlias
      setClauseList
      maybeFromClause
      maybeWhereClause
      maybeReturningClause
    ) = do
    (cteNames, renderedWithClause) <-
      case maybeWithClause of
        Nothing -> pure ([], Nothing)
        Just withClause -> do
          (names, exp) <- renderPGTWithClause withClause
          pure (names, Just exp)
    targetTableExp <- renderPGTRelationExprOptAlias' relationExprOptAlias

    setClauseExp <- renderPGTSetClauseList cteNames setClauseList

    usingClauseExp <-
      case maybeFromClause of
        Nothing -> pure $ ConE 'S.NoUsing
        Just fromClause -> AppE (ConE 'S.Using) <$> renderPGTTableRef cteNames fromClause

    whereConditionExp <-
      case maybeWhereClause of
        Nothing ->
          fail
            "UPDATE statements must have a WHERE clause for safety. Use WHERE TRUE to update all rows."
        Just (PGT_AST.ExprWhereOrCurrentClause whereAExpr) -> renderPGTAExpr cteNames whereAExpr
        Just (PGT_AST.CursorWhereOrCurrentClause _) -> fail "WHERE CURRENT OF is not supported."

    returningClauseExp <-
      case maybeReturningClause of
        Nothing -> pure $ ConE 'S.Returning_ `AppE` ConE 'S.Nil
        Just returningClause -> AppE (ConE 'S.Returning) <$> renderPGTTargetList cteNames returningClause

    let
      updateBody =
        ( VarE 'S.update
            `AppE` targetTableExp
            `AppE` setClauseExp
            `AppE` usingClauseExp
            `AppE` whereConditionExp
            `AppE` returningClauseExp
        )
    let
      finalExp = case renderedWithClause of
        Nothing -> updateBody
        Just withExp -> VarE 'S.with `AppE` withExp `AppE` updateBody
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
      _ -> fail "Unsupported relation expression in UPDATE statement."

  let
    aliasName :: Text
    aliasName =
      case maybeAlias of
        Just (_, colId) -> getIdentText colId
        Nothing -> tableName

    qualifiedAlias :: Exp
    qualifiedAlias =
      case schemaName of
        Nothing -> LabelE (Text.unpack tableName)
        Just schema ->
          VarE '(S.!)
            `AppE` LabelE (Text.unpack schema)
            `AppE` LabelE (Text.unpack tableName)

  pure $ VarE 'S.as `AppE` qualifiedAlias `AppE` LabelE (Text.unpack aliasName)


renderPGTSetClauseList :: [Text.Text] -> PGT_AST.SetClauseList -> Q Exp
renderPGTSetClauseList cteNames setClauses = do
  renderedItems <- mapM (renderPGTSetClause cteNames) (NE.toList setClauses)
  pure $
    foldr
      (\item acc -> ConE '(S.:*) `AppE` item `AppE` acc)
      (ConE 'S.Nil)
      renderedItems


renderPGTSetClause :: [Text.Text] -> PGT_AST.SetClause -> Q Exp
renderPGTSetClause cteNames = \case
  PGT_AST.TargetSetClause (PGT_AST.SetTarget colId maybeIndirection) aExpr -> do
    when (isJust maybeIndirection) $
      fail "UPDATE SET with indirection (e.g., array access) is not supported."
    let
      colNameStr = Text.unpack (getIdentText colId)
    renderedExpr <- renderPGTAExpr cteNames aExpr
    pure $
      VarE 'S.as
        `AppE` (ConE 'S.Set `AppE` renderedExpr)
        `AppE` LabelE colNameStr
  PGT_AST.TargetListSetClause _ _ ->
    fail
      "UPDATE with multiple SET targets (e.g. (col1, col2) = (val1, val2)) is not yet supported."


