{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE ViewPatterns #-}

-- | Description: Translate insert statements.
module Squeal.QuasiQuotes.Insert (
  toSquealInsert,
) where

import Control.Monad (MonadFail(fail), mapM, when, zipWithM)
import Data.Foldable (Foldable(foldr, length), foldlM)
import Data.Maybe (isJust)
import Language.Haskell.TH.Syntax (Exp(AppE, ConE, LabelE, ListE, VarE), Q)
import Prelude
  ( Applicative(pure), Either(Left), Eq((/=)), Maybe(Just, Nothing)
  , Semigroup((<>)), Show(show), ($), (.), error, otherwise
  )
import Squeal.QuasiQuotes.Query (getIdentText, renderPGTAExpr, toSquealQuery)
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


toSquealInsert :: PGT_AST.InsertStmt -> Q Exp
toSquealInsert
  ( PGT_AST.InsertStmt
      maybeWithClause
      insertTarget
      insertRest
      maybeOnConflict
      maybeReturningClause
    ) = do
    (cteNames, renderedWithClause) <-
      case maybeWithClause of
        Nothing -> pure ([], Nothing)
        Just withClause -> do
          (names, exp) <- renderPGTWithClause withClause
          pure (names, Just exp)

    when (isJust maybeOnConflict) $
      fail "ON CONFLICT clauses are not supported yet."

    insertBody <-
      case insertRest of
        PGT_AST.SelectInsertRest maybeInsertColumnList maybeOverrideKind selectStmt -> do
          when (isJust maybeOverrideKind) $
            fail "OVERRIDING clause is not supported yet."
          queryClauseExp <-
            case selectStmt of
              -- Case 1: INSERT ... VALUES ...
              Left
                (PGT_AST.SelectNoParens _ (Left (PGT_AST.ValuesSimpleSelect valuesClause)) _ _ _) ->
                  case maybeInsertColumnList of
                    Just colItems ->
                      renderPGTValueRows cteNames (NE.toList colItems) valuesClause
                    Nothing ->
                      fail
                        "INSERT INTO ... VALUES must specify column names for the Squeal-QQ translation."
              -- Case 2: INSERT ... SELECT ... (a general SELECT statement)
              _ ->
                -- selectStmt is not a ValuesSimpleSelect (i.e., it's a general query)
                case maybeInsertColumnList of
                  Just _ ->
                    fail
                      "INSERT INTO table (columns) SELECT ... is not yet supported by Squeal-QQ. Please use INSERT INTO table SELECT ... and ensure your SELECT statement provides all columns for the table, matching the table's column order and types."
                  Nothing -> do
                    squealQueryExp <- toSquealQuery cteNames Nothing selectStmt -- from Squeal.QuasiQuotes.Query
                    pure (ConE 'S.Subquery `AppE` squealQueryExp)
          let
            table = renderPGTInsertTarget insertTarget
          case maybeReturningClause of
            Nothing ->
              pure $ VarE 'S.insertInto_ `AppE` table `AppE` queryClauseExp
            Just (NE.toList -> [PGT_AST.AsteriskTargetEl]) -> do
              let
                returning = ConE 'S.Returning `AppE` ConE 'S.Star
              pure $
                VarE 'S.insertInto
                  `AppE` table
                  `AppE` queryClauseExp
                  `AppE` ConE 'S.OnConflictDoRaise
                  `AppE` returning
            Just targetList -> do
              returningProj <- renderTargetList cteNames (NE.toList targetList)
              let
                returning = ConE 'S.Returning `AppE` (ConE 'S.List `AppE` returningProj)
              pure $
                VarE 'S.insertInto
                  `AppE` table
                  `AppE` queryClauseExp
                  `AppE` ConE 'S.OnConflictDoRaise
                  `AppE` returning
        PGT_AST.DefaultValuesInsertRest ->
          fail "INSERT INTO ... DEFAULT VALUES is not yet supported by Squeal-QQ."

    let
      finalExp = case renderedWithClause of
        Nothing -> insertBody
        Just withExp -> VarE 'S.with `AppE` withExp `AppE` insertBody
    pure finalExp


renderTargetList :: [Text.Text] -> [PGT_AST.TargetEl] -> Q Exp
renderTargetList cteNames targetEls = do
  exps <- mapM (renderTargetEl cteNames) targetEls
  pure $
    foldr
      (\h t -> ConE '(S.:*) `AppE` h `AppE` t)
      (ConE 'S.Nil)
      exps


renderTargetEl :: [Text.Text] -> PGT_AST.TargetEl -> Q Exp
renderTargetEl cteNames = \case
  PGT_AST.ExprTargetEl expr -> do
    exprExp <- renderPGTAExpr cteNames expr
    case expr of
      PGT_AST.CExprAExpr (PGT_AST.ColumnrefCExpr (PGT_AST.Columnref ident Nothing)) ->
        let
          colName = getIdentText ident
        in
          pure $ VarE 'S.as `AppE` exprExp `AppE` LabelE (Text.unpack colName)
      _ ->
        fail
          "Returning expression without an alias is not supported for this expression type. Please add an alias."
  PGT_AST.AliasedExprTargetEl expr alias -> do
    exprExp <- renderPGTAExpr cteNames expr
    pure $
      VarE 'S.as `AppE` exprExp `AppE` LabelE (Text.unpack (getIdentText alias))
  PGT_AST.AsteriskTargetEl -> fail "should be handled by toSquealInsert"
  PGT_AST.ImplicitlyAliasedExprTargetEl{} ->
    fail "Implicitly aliased expressions in RETURNING are not supported"


getUnqualifiedNameFromAst :: PGT_AST.QualifiedName -> Text.Text
getUnqualifiedNameFromAst (PGT_AST.SimpleQualifiedName ident) = getIdentText ident
getUnqualifiedNameFromAst (PGT_AST.IndirectedQualifiedName _ (PGT_AST.AttrNameIndirectionEl ident NE.:| [])) = getIdentText ident
getUnqualifiedNameFromAst unsupported =
  error $
    "Unsupported qualified name structure for extracting unqualified name: "
      <> show unsupported


renderPGTInsertTarget :: PGT_AST.InsertTarget -> Exp
renderPGTInsertTarget (PGT_AST.InsertTarget qualifiedName maybeAsAlias) =
  let
    tableIdentifierExp = renderPGTQualifiedName qualifiedName
    targetAliasText = case maybeAsAlias of
      Nothing -> getUnqualifiedNameFromAst qualifiedName
      Just asAliasColId -> getIdentText asAliasColId
    targetAliasExp = LabelE (Text.unpack targetAliasText)
  in
    VarE 'S.as `AppE` tableIdentifierExp `AppE` targetAliasExp


renderPGTQualifiedName :: PGT_AST.QualifiedName -> Exp
renderPGTQualifiedName = \case
  PGT_AST.SimpleQualifiedName ident -> LabelE (Text.unpack (getIdentText ident)) -- Defaults to public schema
  PGT_AST.IndirectedQualifiedName
    schemaIdent
    (PGT_AST.AttrNameIndirectionEl colIdent NE.:| []) ->
      -- Assuming simple schema.table form
      VarE '(S.!)
        `AppE` LabelE (Text.unpack (getIdentText schemaIdent))
        `AppE` LabelE (Text.unpack (getIdentText colIdent))
  unsupported -> error $ "Unsupported qualified name structure: " <> show unsupported


renderPGTValueRows
  :: [Text.Text] -> [PGT_AST.InsertColumnItem] -> PGT_AST.ValuesClause -> Q Exp
renderPGTValueRows cteNames colItems (valuesClauseRows) =
  -- valuesClauseRows is NonEmpty (NonEmpty AExpr)
  case NE.toList valuesClauseRows of
    [] -> fail "Insert statement has no value rows."
    row : moreRows -> do
      firstRowExp <- renderPGTValueRow cteNames colItems (NE.toList row)
      moreRowsExp <- mapM (renderPGTValueRow cteNames colItems . NE.toList) moreRows
      pure $
        ConE 'S.Values
          `AppE` firstRowExp
          `AppE` ListE moreRowsExp


renderPGTValueRow :: [Text.Text] -> [PGT_AST.InsertColumnItem] -> [PGT_AST.AExpr] -> Q Exp
renderPGTValueRow cteNames colItems exprs
  | length colItems /= length exprs =
      fail "Mismatched number of column names and values in INSERT statement."
  | otherwise = do
      processedItems <- zipWithM processItem colItems exprs
      pure $ foldr nvpToCons (ConE 'S.Nil) processedItems
 where
  nvpToCons :: Exp -> Exp -> Exp
  nvpToCons item acc = ConE '(S.:*) `AppE` item `AppE` acc

  processItem :: PGT_AST.InsertColumnItem -> PGT_AST.AExpr -> Q Exp
  processItem (PGT_AST.InsertColumnItem colId maybeIndirection) expr = do
    when (isJust maybeIndirection) $
      fail "INSERT with indirection (e.g., array access) is not supported."
    let
      colNameStr = Text.unpack (getIdentText colId)
    case expr of
      PGT_AST.DefaultAExpr ->
        -- Check for DEFAULT keyword
        pure $
          VarE 'S.as
            `AppE` ConE 'S.Default
            `AppE` LabelE colNameStr
      _ -> do
        renderedExpr <- renderPGTAExpr cteNames expr
        pure $
          VarE 'S.as
            `AppE` (ConE 'S.Set `AppE` renderedExpr)
            `AppE` LabelE colNameStr


