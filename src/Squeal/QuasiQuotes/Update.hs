{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE ViewPatterns #-}

-- | Description: Translate update statements.
module Squeal.QuasiQuotes.Update (
  toSquealUpdate,
) where

import Data.Text (Text)
import Control.Monad (when)
import Data.Maybe (isJust)
import Language.Haskell.TH.Syntax (Exp(AppE, ConE, LabelE, VarE), Q)
import Prelude
  ( Applicative(pure), Foldable(foldr), Maybe(Just, Nothing), MonadFail(fail)
  , Traversable(mapM), ($), (<$>)
  )
import Squeal.QuasiQuotes.Common
  ( getIdentText, renderPGTAExpr, renderPGTTableRef, renderPGTTargetList
  )
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as Text
import qualified PostgresqlSyntax.Ast as PGT_AST
import qualified Squeal.PostgreSQL as S


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
    when (isJust maybeWithClause) $
      fail "WITH clauses are not supported in UPDATE statements yet."
    targetTableExp <- renderPGTRelationExprOptAlias' relationExprOptAlias

    setClauseExp <- renderPGTSetClauseList setClauseList

    usingClauseExp <-
      case maybeFromClause of
        Nothing -> pure $ ConE 'S.NoUsing
        Just fromClause -> AppE (ConE 'S.Using) <$> renderPGTTableRef [] fromClause

    whereConditionExp <-
      case maybeWhereClause of
        Nothing ->
          fail
            "UPDATE statements must have a WHERE clause for safety. Use WHERE TRUE to update all rows."
        Just (PGT_AST.ExprWhereOrCurrentClause whereAExpr) -> renderPGTAExpr whereAExpr
        Just (PGT_AST.CursorWhereOrCurrentClause _) -> fail "WHERE CURRENT OF is not supported."

    returningClauseExp <-
      case maybeReturningClause of
        Nothing -> pure $ ConE 'S.Returning_ `AppE` ConE 'S.Nil
        Just returningClause -> AppE (ConE 'S.Returning) <$> renderPGTTargetList returningClause

    pure $
      ( VarE 'S.update
          `AppE` targetTableExp
          `AppE` setClauseExp
          `AppE` usingClauseExp
          `AppE` whereConditionExp
          `AppE` returningClauseExp
      )


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


renderPGTSetClauseList :: PGT_AST.SetClauseList -> Q Exp
renderPGTSetClauseList setClauses = do
  renderedItems <- mapM renderPGTSetClause (NE.toList setClauses)
  pure $
    foldr
      (\item acc -> ConE '(S.:*) `AppE` item `AppE` acc)
      (ConE 'S.Nil)
      renderedItems


renderPGTSetClause :: PGT_AST.SetClause -> Q Exp
renderPGTSetClause = \case
  PGT_AST.TargetSetClause (PGT_AST.SetTarget colId maybeIndirection) aExpr -> do
    when (isJust maybeIndirection) $
      fail "UPDATE SET with indirection (e.g., array access) is not supported."
    let colNameStr = Text.unpack (getIdentText colId)
    renderedExpr <- renderPGTAExpr aExpr
    pure $
      VarE 'S.as
        `AppE` (ConE 'S.Set `AppE` renderedExpr)
        `AppE` LabelE colNameStr
  PGT_AST.TargetListSetClause _ _ ->
    fail
      "UPDATE with multiple SET targets (e.g. (col1, col2) = (val1, val2)) is not yet supported."


