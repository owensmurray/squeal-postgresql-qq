{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE ViewPatterns #-}

-- | Description: Translate query expressions.
module Squeal.QuasiQuotes.Query (
  toSquealQuery,
) where

import Control.Monad (unless, when)
import Data.Foldable (Foldable(foldl', foldr, null))
import Data.Maybe (fromMaybe, isJust, isNothing)
import Language.Haskell.TH.Syntax
  ( Exp(AppE, ConE, InfixE, LabelE, ListE, LitE, VarE), Lit(IntegerL), Q, mkName
  )
import Prelude
  ( Applicative(pure), Bool(False, True), Either(Left, Right)
  , Maybe(Just, Nothing), MonadFail(fail), Num((+)), Ord((>=)), Semigroup((<>))
  , Show(show), Traversable(mapM), ($), (&&), (<$>), Int, fromIntegral, zip
  )
import Squeal.QuasiQuotes.Common
  ( getIdentText, renderPGTAExpr, renderPGTTableRef, renderPGTTargeting
  )
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as Text
import qualified PostgresqlSyntax.Ast as PGT_AST
import qualified Squeal.PostgreSQL as S


toSquealQuery :: PGT_AST.SelectStmt -> Q Exp
toSquealQuery selectStmt = case selectStmt of
  Left selectNoParens -> toSquealSelectNoParens selectNoParens
  Right selectWithParens -> toSquealSelectWithParens selectWithParens


toSquealSelectWithParens :: PGT_AST.SelectWithParens -> Q Exp
toSquealSelectWithParens = \case
  PGT_AST.NoParensSelectWithParens snp -> toSquealSelectNoParens snp
  PGT_AST.WithParensSelectWithParens swp ->
    {- The AST structure itself should handle precedence.  Just recurse.  -}
    toSquealSelectWithParens swp


toSquealSelectNoParens :: PGT_AST.SelectNoParens -> Q Exp
toSquealSelectNoParens
  ( PGT_AST.SelectNoParens
      maybeWithClause
      selectClause
      maybeSortClause
      maybeSelectLimit
      maybeForLockingClause
    ) = do
    (cteNames, renderedWithClause) <-
      case maybeWithClause of
        Nothing -> pure ([], Nothing)
        Just withClause -> do
          (names, exp) <- renderPGTWithClause withClause
          pure (names, Just exp)

    squealQueryBody <-
      case selectClause of
        Left simpleSelect ->
          toSquealSimpleSelect
            cteNames
            simpleSelect
            maybeSortClause
            maybeSelectLimit
            maybeForLockingClause
        Right selectWithParens' -> toSquealSelectWithParens selectWithParens'

    case renderedWithClause of
      Nothing -> pure squealQueryBody
      Just withExp -> pure $ VarE 'S.with `AppE` withExp `AppE` squealQueryBody


renderPGTWithClause :: PGT_AST.WithClause -> Q ([Text.Text], Exp)
renderPGTWithClause (PGT_AST.WithClause recursive ctes) = do
  when recursive $ fail "Recursive WITH clauses are not supported yet."
  let cteList = NE.toList ctes
  cteNames <- mapM getCteName cteList
  renderedCtes <- mapM renderCte cteList
  let withExp =
        foldr
          (\cte acc -> ConE '(S.:>>) `AppE` cte `AppE` acc)
          (ConE 'S.Done)
          renderedCtes
  pure (cteNames, withExp)
  where
    getCteName (PGT_AST.CommonTableExpr ident _ _ _) = pure $ getIdentText ident
    renderCte (PGT_AST.CommonTableExpr ident maybeColNames maybeMaterialized stmt) = do
      when (isJust maybeColNames) $ fail "Column name lists in CTEs are not supported yet."
      when (isJust maybeMaterialized) $ fail "MATERIALIZED/NOT MATERIALIZED for CTEs is not supported yet."
      cteQueryExp <- case stmt of
        PGT_AST.SelectPreparableStmt selectStmt -> toSquealQuery selectStmt
        _ -> fail "Only SELECT statements are supported in CTEs."
      let cteName = getIdentText ident
      pure $ VarE 'S.as `AppE` cteQueryExp `AppE` LabelE (Text.unpack cteName)


toSquealSimpleSelect
  :: [Text.Text]
  -> PGT_AST.SimpleSelect
  -> Maybe PGT_AST.SortClause
  -> Maybe PGT_AST.SelectLimit
  -> Maybe PGT_AST.ForLockingClause
  -> Q Exp
toSquealSimpleSelect cteNames simpleSelect maybeSortClause maybeSelectLimit maybeForLockingClause =
  case simpleSelect of
    PGT_AST.ValuesSimpleSelect valuesClause -> do
      unless
        ( isNothing maybeSortClause
            && isNothing maybeSelectLimit
            && isNothing maybeForLockingClause
        )
        $ fail
        $ "ORDER BY / OFFSET / LIMIT / FOR UPDATE etc. not supported with VALUES clause "
          <> "in this translation yet."
      renderedValues <- renderValuesClauseToNP valuesClause
      pure $ VarE 'S.values_ `AppE` renderedValues
    PGT_AST.NormalSimpleSelect
      maybeTargeting
      maybeIntoClause
      maybeFromClause
      maybeWhereClause
      maybeGroupClause
      maybeHavingClause
      maybeWindowClause ->
        do
          targeting <-
            case maybeTargeting of
              Nothing ->
                fail "SELECT without a selection list is not supported."
              Just targeting -> pure targeting
          case
              ( maybeFromClause
              , maybeGroupClause
              , maybeHavingClause
              , maybeIntoClause
              , maybeSelectLimit
              , maybeWhereClause
              , maybeWindowClause
              )
            of
              ( Nothing
                , Nothing
                , Nothing
                , Nothing
                , Nothing
                , Nothing
                , Nothing
                ) ->
                  do
                    -- Case: SELECT <targeting_list> (no FROM, no other clauses)
                    -- Translate to S.values_
                    renderedTargetingForValues <-
                      renderPGTTargetingForValues targeting
                    pure $
                      VarE 'S.values_ `AppE` renderedTargetingForValues
              (Nothing, _, _, _, _, _, _) ->
                {-
                  Case: SELECT <targeting_list> (no FROM, but other
                  clauses are present)
                -}
                fail $
                  "SELECT with targeting but no FROM clause cannot have "
                    <> "other clauses like INTO, WHERE, GROUP BY, HAVING, "
                    <> "WINDOW, or LIMIT/OFFSET."
              (Just fromClause, _, _, _, _, _, _) -> do
                -- Case: SELECT ... FROM ... (original logic)

                when (isJust maybeIntoClause) $
                  fail "INTO clause is not yet supported in this translation."
                when (isJust maybeWindowClause) $
                  fail $
                    "WINDOW clause is not yet supported in this translation "
                      <> "for NormalSimpleSelect with FROM."

                renderedFromClauseExp <- renderPGTTableRef cteNames fromClause
                let
                  baseTableExpr = VarE 'S.from `AppE` renderedFromClauseExp

                tableExprWithWhere <-
                  case maybeWhereClause of
                    Nothing -> pure baseTableExpr
                    Just wc -> do
                      renderedWC <- renderPGTAExpr wc
                      pure $
                        InfixE
                          (Just baseTableExpr)
                          (VarE '(S.&))
                          (Just (AppE (VarE 'S.where_) renderedWC))

                tableExprWithGroupBy <-
                  applyPGTGroupBy tableExprWithWhere maybeGroupClause

                tableExprWithHaving <-
                  case maybeHavingClause of
                    Nothing -> pure tableExprWithGroupBy
                    Just hc -> do
                      when (isNothing maybeGroupClause) $
                        fail "HAVING clause requires a GROUP BY clause."
                      renderedHC <- renderPGTAExpr hc
                      pure $
                        InfixE
                          (Just tableExprWithGroupBy)
                          (VarE '(S.&))
                          (Just (AppE (VarE 'S.having) renderedHC))

                tableExprWithOrderBy <-
                  case maybeSortClause of
                    Nothing -> pure tableExprWithHaving
                    Just sortClause -> do
                      renderedSC <- renderPGTSortClause sortClause
                      pure $
                        InfixE
                          (Just tableExprWithHaving)
                          (VarE '(S.&))
                          (Just (AppE (VarE 'S.orderBy) renderedSC))

                (tableExprWithOffset, mTableExprWithLimit) <-
                  processSelectLimit tableExprWithOrderBy maybeSelectLimit

                let
                  baseFinalTableExpr =
                    fromMaybe tableExprWithOffset mTableExprWithLimit

                -- Apply FOR LOCKING clause if present
                finalTableExprWithPotentialLocking <-
                  case maybeForLockingClause of
                    Nothing -> pure baseFinalTableExpr
                    Just flc -> do
                      lockingClauseExps <- renderPGTForLockingClauseItems flc
                      pure $
                        foldl'
                          ( \accTableExpr lockingClauseExp ->
                              InfixE
                                (Just accTableExpr)
                                (VarE '(S.&))
                                (Just (AppE (VarE 'S.lockRows) lockingClauseExp))
                          )
                          baseFinalTableExpr
                          lockingClauseExps

                (selectionTargetExp, maybeDistinctOnExprs) <-
                  renderPGTTargeting targeting

                squealSelectFn <-
                  case maybeDistinctOnExprs of
                    Nothing ->
                      case targeting of
                        PGT_AST.DistinctTargeting Nothing _ ->
                          pure $ VarE 'S.selectDistinct
                        _ -> pure $ VarE 'S.select -- Normal or ALL
                    Just distinctOnAstExprs -> do
                      distinctOnSquealSortExps <-
                        renderPGTOnExpressionsClause distinctOnAstExprs
                      pure $
                        VarE 'S.selectDistinctOn
                          `AppE` distinctOnSquealSortExps

                pure $
                  squealSelectFn
                    `AppE` selectionTargetExp
                    `AppE` finalTableExprWithPotentialLocking
    unsupportedSimpleSelect ->
      fail $
        "Unsupported simple select statement: "
          <> show unsupportedSimpleSelect


-- Helper for VALUES clause: Assumes S.values_ for a single row of values.
-- PGT_AST.ValuesClause is NonEmpty (NonEmpty PGT_AST.AExpr)
renderValuesClauseToNP :: PGT_AST.ValuesClause -> Q Exp
renderValuesClauseToNP (firstRowExps NE.:| restRowExps) = do
    unless (null restRowExps) $
      fail $
        "Multi-row VALUES clause requires S.values, this translation "
          <> "currently supports single row S.values_."
    convertRowToNP firstRowExps
  where
    convertRowToNP :: NE.NonEmpty PGT_AST.AExpr -> Q Exp
    convertRowToNP exprs =
        go (NE.toList exprs) 1
      where
        go :: [PGT_AST.AExpr] -> Int -> Q Exp
        go [] _ = pure $ ConE 'S.Nil
        go (expr : fs) idx = do
          renderedExpr <- renderPGTAExpr expr
          let
            aliasText = "_column" <> show idx -- Default alias for VALUES
            aliasedExp = VarE 'S.as `AppE` renderedExpr `AppE` LabelE aliasText
          restExp <- go fs (idx + 1) -- restExp is Exp here
          -- Correct construction: aliasedExp :* restExp
          pure $ ConE '(S.:*) `AppE` aliasedExp `AppE` restExp


renderPGTForLockingClauseItems :: PGT_AST.ForLockingClause -> Q [Exp]
renderPGTForLockingClauseItems = \case
  PGT_AST.ReadOnlyForLockingClause ->
    fail $
      "FOR READ ONLY is not supported as a row-level locking "
        <> "clause by Squeal-QQ."
  PGT_AST.ItemsForLockingClause itemsNe ->
    mapM renderPGTForLockingItem (NE.toList itemsNe)


renderPGTForLockingItem :: PGT_AST.ForLockingItem -> Q Exp
renderPGTForLockingItem
  ( PGT_AST.ForLockingItem
      strength
      maybeTables
      waitingOpt
    ) = do
    squealStrength <- renderPGTForLockingStrength strength
    squealTables <-
      case maybeTables of
        Nothing ->
          {- Empty list for "OF" tables means all tables in query -}
          pure $ ConE 'S.Nil
        Just tablesNe -> do
          aliasExps <-
            mapM
              ( \qn -> case qn of
                  PGT_AST.SimpleQualifiedName ident ->
                    pure $ LabelE (Text.unpack $ getIdentText ident)
                  _ ->
                    fail $
                      "Qualified table names like schema.table in "
                        <> "FOR UPDATE/SHARE OF clauses are not yet "
                        <> "supported. Please use simple table aliases "
                        <> "that refer to tables in the FROM clause."
              )
              (NE.toList tablesNe)
          pure $
            foldr
              (\itemExp acc -> ConE '(S.:*) `AppE` itemExp `AppE` acc)
              (ConE 'S.Nil)
              aliasExps

    squealWaiting <- renderPGTWaiting waitingOpt

    pure $
      ConE 'S.For `AppE` squealStrength `AppE` squealTables `AppE` squealWaiting


renderPGTForLockingStrength :: PGT_AST.ForLockingStrength -> Q Exp
renderPGTForLockingStrength = \case
  PGT_AST.UpdateForLockingStrength -> pure $ ConE 'S.Update
  PGT_AST.NoKeyUpdateForLockingStrength -> pure $ ConE 'S.NoKeyUpdate
  PGT_AST.ShareForLockingStrength -> pure $ ConE 'S.Share
  PGT_AST.KeyForLockingStrength -> pure $ ConE 'S.KeyShare


renderPGTWaiting :: Maybe Bool -> Q Exp
renderPGTWaiting = \case
  Nothing -> pure $ ConE 'S.Wait -- Default (no NOWAIT or SKIP LOCKED)
  Just False -> pure $ ConE 'S.NoWait -- NOWAIT
  Just True -> pure $ ConE 'S.SkipLocked -- SKIP LOCKED


applyPGTGroupBy :: Exp -> Maybe PGT_AST.GroupClause -> Q Exp
applyPGTGroupBy currentTableExpr = \case
    Nothing -> pure currentTableExpr
    Just groupClause -> do
      renderedGB <- renderPGTGroupByClauseElements groupClause
      pure $
        InfixE
          (Just currentTableExpr)
          (VarE '(S.&))
          (Just (AppE (VarE 'S.groupBy) renderedGB))
  where
    renderPGTGroupByClauseElements :: PGT_AST.GroupClause -> Q Exp
    renderPGTGroupByClauseElements = \case
      PGT_AST.EmptyGroupingSetGroupByItem NE.:| [] ->
        pure $ ConE 'S.Nil
      groupByItems -> do
        renderedExprs <- mapM renderPGTGroupByItem (NE.toList groupByItems)
        pure $
          foldr
            (\expr acc -> ConE '(S.:*) `AppE` expr `AppE` acc)
            (ConE 'S.Nil)
            renderedExprs


renderPGTGroupByItem :: PGT_AST.GroupByItem -> Q Exp
renderPGTGroupByItem = \case
  PGT_AST.ExprGroupByItem scalarExpr -> renderPGTAExpr scalarExpr
  PGT_AST.EmptyGroupingSetGroupByItem -> pure (ConE 'S.Nil)
  unsupportedGroup ->
    fail $
      "Unsupported grouping expression: " <> show unsupportedGroup


processSelectLimit :: Exp -> Maybe PGT_AST.SelectLimit -> Q (Exp, Maybe Exp)
processSelectLimit tableExpr Nothing = pure (tableExpr, Nothing)
processSelectLimit tableExpr (Just selectLimit) = do
  let
    (maybeOffsetClause, maybeLimitClause) = case selectLimit of
      PGT_AST.LimitOffsetSelectLimit lim off -> (Just off, Just lim)
      PGT_AST.OffsetLimitSelectLimit off lim -> (Just off, Just lim)
      PGT_AST.LimitSelectLimit lim -> (Nothing, Just lim)
      PGT_AST.OffsetSelectLimit off -> (Just off, Nothing)

  tableExprWithOffset <-
    case maybeOffsetClause of
      Nothing -> pure tableExpr
      Just offsetVal -> do
        offsetExp <- renderPGTOffsetClause offsetVal
        pure $
          InfixE
            (Just tableExpr)
            (VarE '(S.&))
            (Just (AppE (VarE 'S.offset) offsetExp))

  case maybeLimitClause of
    Nothing -> pure (tableExprWithOffset, Nothing)
    Just limitVal -> do
      limitExp <- renderPGTLimitClause limitVal
      pure
        ( tableExprWithOffset
        , Just
            ( InfixE
                (Just tableExprWithOffset)
                (VarE '(S.&))
                (Just (AppE (VarE 'S.limit) limitExp))
            )
        )


renderPGTLimitClause :: PGT_AST.LimitClause -> Q Exp
renderPGTLimitClause = \case
  PGT_AST.LimitLimitClause slValue mOffsetVal -> do
    when (isJust mOffsetVal) $
      fail
        "LIMIT with comma (e.g. LIMIT x, y) is not supported. Use separate LIMIT and OFFSET clauses."
    case slValue of
      PGT_AST.ExprSelectLimitValue
        ( PGT_AST.CExprAExpr
            ( PGT_AST.FuncCExpr
                ( PGT_AST.ApplicationFuncExpr
                    ( PGT_AST.FuncApplication
                        (PGT_AST.TypeFuncName (PGT_AST.UnquotedIdent "inline"))
                        ( Just
                            ( PGT_AST.NormalFuncApplicationParams
                                Nothing
                                ( PGT_AST.ExprFuncArgExpr
                                    ( PGT_AST.CExprAExpr
                                        (PGT_AST.ColumnrefCExpr (PGT_AST.Columnref ident Nothing))
                                      )
                                    NE.:| []
                                  )
                                Nothing
                              )
                          )
                      )
                    Nothing
                    Nothing
                    Nothing
                  )
              )
          ) -> pure $ VarE (mkName (Text.unpack (getIdentText ident)))
      PGT_AST.ExprSelectLimitValue
        (PGT_AST.CExprAExpr (PGT_AST.AexprConstCExpr (PGT_AST.IAexprConst n))) ->
          if n >= 0
            then pure (LitE (IntegerL (fromIntegral n)))
            else fail $ "LIMIT value must be non-negative: " <> show n
      PGT_AST.AllSelectLimitValue ->
        fail "LIMIT ALL not supported in this translation."
      expr -> fail $ "Unsupported LIMIT expression: " <> show expr
  PGT_AST.FetchOnlyLimitClause{} ->
    fail "FETCH clause is not fully supported yet."


renderPGTOffsetClause :: PGT_AST.OffsetClause -> Q Exp
renderPGTOffsetClause = \case
  PGT_AST.ExprOffsetClause
    ( PGT_AST.CExprAExpr
        ( PGT_AST.FuncCExpr
            ( PGT_AST.ApplicationFuncExpr
                ( PGT_AST.FuncApplication
                    (PGT_AST.TypeFuncName (PGT_AST.UnquotedIdent "inline"))
                    ( Just
                        ( PGT_AST.NormalFuncApplicationParams
                            Nothing
                            ( PGT_AST.ExprFuncArgExpr
                                ( PGT_AST.CExprAExpr
                                    (PGT_AST.ColumnrefCExpr (PGT_AST.Columnref ident Nothing))
                                  )
                                NE.:| []
                              )
                            Nothing
                          )
                      )
                  )
                Nothing
                Nothing
                Nothing
              )
          )
      ) -> pure $ VarE (mkName (Text.unpack (getIdentText ident)))
  PGT_AST.ExprOffsetClause
    (PGT_AST.CExprAExpr (PGT_AST.AexprConstCExpr (PGT_AST.IAexprConst n))) ->
      if n >= 0
        then pure (LitE (IntegerL (fromIntegral n)))
        else fail $ "OFFSET value must be non-negative: " <> show n
  PGT_AST.ExprOffsetClause expr ->
    fail $
      "Unsupported OFFSET expression: " <> show expr
  PGT_AST.FetchFirstOffsetClause{} ->
    fail "OFFSET with FETCH FIRST clause is not supported yet."


-- Helper to render a single TargetEl for S.values_
-- Each expression must be aliased.
renderPGTTargetElForValues :: PGT_AST.TargetEl -> Int -> Q Exp
renderPGTTargetElForValues targetEl idx = do
  (exprAST, mUserAlias) <-
    case targetEl of
      PGT_AST.AliasedExprTargetEl e an -> pure (e, Just an)
      PGT_AST.ImplicitlyAliasedExprTargetEl e an -> pure (e, Just an)
      PGT_AST.ExprTargetEl e -> pure (e, Nothing)
      PGT_AST.AsteriskTargetEl ->
        fail "SELECT * is not supported unless there is a from clause."
  renderedScalarExp <- renderPGTAExpr exprAST
  let
    aliasLabelStr =
      case mUserAlias of
        Just ident -> Text.unpack $ getIdentText ident
        Nothing -> "_col" <> show idx -- Default alias for VALUES items
  pure $ VarE 'S.as `AppE` renderedScalarExp `AppE` LabelE aliasLabelStr


-- Helper to render a TargetList into an NP list for S.values_
renderPGTTargetListForValues :: PGT_AST.TargetList -> Q Exp
renderPGTTargetListForValues (item NE.:| items) = do
  renderedItems <-
    mapM
      (\(el, idx) -> renderPGTTargetElForValues el idx)
      (zip (item : items) [1 ..])
  -- Construct NP list: e1 :* e2 :* ... :* Nil
  -- Each element in renderedItems is an Exp.
  -- The result of the fold should be an Exp.
  -- Then pure the final Exp.
  pure $
    foldr
      (\hd acc -> ConE '(S.:*) `AppE` hd `AppE` acc)
      (ConE 'S.Nil)
      renderedItems


-- New function to render Targeting specifically for S.values_
renderPGTTargetingForValues :: PGT_AST.Targeting -> Q Exp
renderPGTTargetingForValues = \case
  PGT_AST.NormalTargeting targetList -> renderPGTTargetListForValues targetList
  PGT_AST.AllTargeting (Just targetList) ->
    renderPGTTargetListForValues targetList
  PGT_AST.AllTargeting Nothing ->
    fail $
      "SELECT * (ALL targeting without a list) is not supported "
        <> "with VALUES clause."
  PGT_AST.DistinctTargeting{} ->
    -- Handles both DISTINCT and DISTINCT ON
    fail $
      "DISTINCT and DISTINCT ON queries are not supported with VALUES clause in "
        <> "this translation."


renderPGTOnExpressionsClause :: [PGT_AST.AExpr] -> Q Exp
renderPGTOnExpressionsClause exprs = do
    renderedSortExps <- mapM renderToSortExpr exprs
    pure $ ListE renderedSortExps
  where
    renderToSortExpr :: PGT_AST.AExpr -> Q Exp
    renderToSortExpr astExpr = do
      squealExpr <- renderPGTAExpr astExpr
      -- For DISTINCT ON, the direction (ASC/DESC) and NULLS order
      -- are typically specified in the ORDER BY clause.
      -- Here, we default to ASC for the SortExpression constructor.
      pure $ ConE 'S.Asc `AppE` squealExpr


renderPGTSortClause :: PGT_AST.SortClause -> Q Exp
renderPGTSortClause sortBys = ListE <$> mapM renderPGTSortBy (NE.toList sortBys)


renderPGTSortBy :: PGT_AST.SortBy -> Q Exp
renderPGTSortBy = \case
  PGT_AST.AscDescSortBy aExpr maybeAscDesc maybeNullsOrder -> do
    squealExpr <- renderPGTAExpr aExpr
    let
      (asc, desc) = case maybeNullsOrder of
        Nothing -> ('S.Asc, 'S.Desc)
        Just PGT_AST.FirstNullsOrder -> ('S.AscNullsFirst, 'S.DescNullsFirst)
        Just PGT_AST.LastNullsOrder -> ('S.AscNullsLast, 'S.DescNullsLast)

    let
      constructor = case maybeAscDesc of
        Just PGT_AST.DescAscDesc -> desc
        _ -> asc -- default to ASC
    pure $ ConE constructor `AppE` squealExpr
  PGT_AST.UsingSortBy{} -> fail "ORDER BY USING is not supported"


