{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE ViewPatterns #-}

-- | Description: Translate delete statements.
module Squeal.QuasiQuotes.Delete (
  toSquealDelete,
) where

import Language.Haskell.TH.Syntax (Exp(AppE, ConE, LabelE, VarE), Q)
import Prelude
  ( Applicative(pure), Maybe(Just, Nothing), MonadFail(fail), ($), (<$>)
  )
import Squeal.QuasiQuotes.Common
  ( getIdentText, renderPGTAExpr, renderPGTTableRef, renderPGTTargetList
  )
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as Text
import qualified PostgresqlSyntax.Ast as PGT_AST
import qualified Squeal.PostgreSQL as S


toSquealDelete :: PGT_AST.DeleteStmt -> Q Exp
toSquealDelete
  ( PGT_AST.DeleteStmt
      _maybeWithClause
      relationExprOptAlias
      maybeUsingClause
      maybeWhereClause
      maybeReturningClause
    ) = do
    -- withClause is not supported yet
    -- whereOrCurrentClause with cursor is not supported yet

    targetTableExp <- renderPGTRelationExprOptAlias' relationExprOptAlias

    usingClauseExp <-
      case maybeUsingClause of
        Nothing -> pure $ ConE 'S.NoUsing
        Just usingClause -> AppE (ConE 'S.Using) <$> renderPGTTableRef usingClause

    whereConditionExp <-
      case maybeWhereClause of
        Nothing ->
          fail
            "DELETE statements must have a WHERE clause for safety. Use WHERE TRUE to delete all rows."
        Just (PGT_AST.ExprWhereOrCurrentClause whereAExpr) -> renderPGTAExpr whereAExpr
        Just (PGT_AST.CursorWhereOrCurrentClause _) -> fail "WHERE CURRENT OF is not supported."

    returningClauseExp <-
      case maybeReturningClause of
        Nothing -> pure $ ConE 'S.Returning_ `AppE` ConE 'S.Nil
        Just returningClause -> AppE (ConE 'S.Returning) <$> renderPGTTargetList returningClause

    pure $
      VarE 'S.manipulation
        `AppE` ( VarE 'S.deleteFrom
                   `AppE` targetTableExp
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


