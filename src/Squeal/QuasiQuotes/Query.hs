{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE ViewPatterns #-}

-- | Description: Translate query expressions.
module Squeal.QuasiQuotes.Query (
  toSquealQuery,
  renderPGTTableRef,
  renderPGTTargeting,
  renderPGTTargetList,
  renderPGTAExpr,
  getIdentText,
) where

import Control.Applicative (Alternative((<|>)))
import Control.Monad (unless, when)
import Data.Foldable (Foldable(elem, foldl'), foldlM)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.String (IsString(fromString))
import Language.Haskell.TH.Syntax
  ( Exp(AppE, AppTypeE, ConE, InfixE, LabelE, ListE, LitE, TupE, VarE)
  , Lit(IntegerL, StringL), TyLit(NumTyLit), Type(LitT), Name, Q, mkName
  )
import Prelude
  ( Applicative(pure), Bool(False, True), Either(Left, Right), Eq((==))
  , Foldable(foldr, length, null), Functor(fmap), Maybe(Just, Nothing)
  , MonadFail(fail), Num((*), (+), (-), fromInteger), Ord((<), (>=))
  , Semigroup((<>)), Show(show), Traversable(mapM), ($), (&&), (.), (<$>), (||)
  , Int, Integer, any, either, error, fromIntegral, id, otherwise, zip
  )
import qualified Data.ByteString.Char8 as BS8
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as Text
import qualified PostgresqlSyntax.Ast as PGT_AST
import qualified Squeal.PostgreSQL as S


toSquealQuery
  :: [Text.Text]
  -> Maybe (NE.NonEmpty PGT_AST.Ident)
  -> PGT_AST.SelectStmt
  -> Q Exp
toSquealQuery cteNames maybeColAliases selectStmt = case selectStmt of
  Left selectNoParens -> toSquealSelectNoParens cteNames maybeColAliases selectNoParens
  Right selectWithParens -> toSquealSelectWithParens cteNames maybeColAliases selectWithParens


toSquealSelectWithParens
  :: [Text.Text]
  -> Maybe (NE.NonEmpty PGT_AST.Ident)
  -> PGT_AST.SelectWithParens
  -> Q Exp
toSquealSelectWithParens cteNames maybeColAliases = \case
  PGT_AST.NoParensSelectWithParens snp -> toSquealSelectNoParens cteNames maybeColAliases snp
  PGT_AST.WithParensSelectWithParens swp ->
    {- The AST structure itself should handle precedence.  Just recurse.  -}
    toSquealSelectWithParens cteNames maybeColAliases swp


toSquealSelectNoParens
  :: [Text.Text]
  -> Maybe (NE.NonEmpty PGT_AST.Ident)
  -> PGT_AST.SelectNoParens
  -> Q Exp
toSquealSelectNoParens
  initialCteNames
  maybeColAliases
  ( PGT_AST.SelectNoParens
      maybeWithClause
      selectClause
      maybeSortClause
      maybeSelectLimit
      maybeForLockingClause
    ) = do
    (cteNames, renderedWithClause) <-
      case maybeWithClause of
        Nothing -> pure (initialCteNames, Nothing)
        Just withClause -> do
          (names, exp) <- renderPGTWithClause initialCteNames withClause
          pure (names, Just exp)

    squealQueryBody <-
      case selectClause of
        Left simpleSelect ->
          toSquealSimpleSelect
            cteNames
            maybeColAliases
            simpleSelect
            maybeSortClause
            maybeSelectLimit
            maybeForLockingClause
        Right selectWithParens' -> toSquealSelectWithParens cteNames maybeColAliases selectWithParens'

    case renderedWithClause of
      Nothing -> pure squealQueryBody
      Just withExp -> pure $ VarE 'S.with `AppE` withExp `AppE` squealQueryBody


renderPGTWithClause :: [Text.Text] -> PGT_AST.WithClause -> Q ([Text.Text], Exp)
renderPGTWithClause initialCteNames (PGT_AST.WithClause recursive ctes) = do
    when recursive $ fail "Recursive WITH clauses are not supported yet."
    let
      cteList = NE.toList ctes
    (finalCteNames, renderedCtes) <-
      foldlM
        ( \(names, exps) cte -> do
            (name, exp) <- renderCte names cte
            pure (names <> [name], exps <> [exp])
        )
        (initialCteNames, [])
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
      when (isJust maybeColNames) $
        fail "Column name lists in CTEs are not supported yet."
      when (isJust maybeMaterialized) $
        fail "MATERIALIZED/NOT MATERIALIZED for CTEs is not supported yet."
      cteQueryExp <-
        case stmt of
          PGT_AST.SelectPreparableStmt selectStmt -> toSquealQuery existingCteNames Nothing selectStmt
          _ -> fail "Only SELECT statements are supported in CTEs."
      let
        cteName = getIdentText ident
      pure
        (cteName, VarE 'S.as `AppE` cteQueryExp `AppE` LabelE (Text.unpack cteName))


toSquealSimpleSelect
  :: [Text.Text]
  -> Maybe (NE.NonEmpty PGT_AST.Ident)
  -> PGT_AST.SimpleSelect
  -> Maybe PGT_AST.SortClause
  -> Maybe PGT_AST.SelectLimit
  -> Maybe PGT_AST.ForLockingClause
  -> Q Exp
toSquealSimpleSelect cteNames maybeColAliases simpleSelect maybeSortClause maybeSelectLimit maybeForLockingClause =
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
      renderedValues <- renderValuesClauseToNP maybeColAliases valuesClause
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
renderValuesClauseToNP
  :: Maybe (NE.NonEmpty PGT_AST.Ident) -> PGT_AST.ValuesClause -> Q Exp
renderValuesClauseToNP maybeColAliases (firstRowExps NE.:| restRowExps) = do
    unless (null restRowExps) $
      fail $
        "Multi-row VALUES clause requires S.values, this translation "
          <> "currently supports single row S.values_."
    convertRowToNP firstRowExps
  where
    colAliasTexts = fmap (fmap getIdentText . NE.toList) maybeColAliases

    convertRowToNP :: NE.NonEmpty PGT_AST.AExpr -> Q Exp
    convertRowToNP exprs = do
        let
          exprList = NE.toList exprs
        aliasTexts <-
          case colAliasTexts of
            Just aliases ->
              if length aliases == length exprList
                then pure aliases
                else
                  fail
                    "Number of column aliases in CTE does not match number of columns in VALUES clause."
            Nothing -> pure $ fmap (Text.pack . ("_column" <>) . show) [1 :: Int ..]
        go (zip exprList aliasTexts)
      where
        go :: [(PGT_AST.AExpr, Text.Text)] -> Q Exp
        go [] = pure $ ConE 'S.Nil
        go ((expr, aliasText) : fs) = do
          renderedExpr <- renderPGTAExpr expr
          let
            aliasedExp = VarE 'S.as `AppE` renderedExpr `AppE` LabelE (Text.unpack aliasText)
          restExp <- go fs
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


renderPGTTableRef :: [Text.Text] -> NE.NonEmpty PGT_AST.TableRef -> Q Exp
renderPGTTableRef cteNames tableRefs = do
  renderedTableRefs <- mapM (renderSingleTableRef cteNames) (NE.toList tableRefs)
  case renderedTableRefs of
    [] -> fail "Empty FROM clause" -- Should not happen with NonEmpty
    (firstTbl : restTbls) ->
      -- For FROM t1, t2, t3 Squeal uses: (table #t1) & also (table #t2) & also (table #t3)
      -- S.also takes new item first, then accumulated.
      -- So foldl' (\acc item -> VarE 'S.also `AppE` item `AppE` acc) firstTbl restTbls
      -- However, Squeal's FromClause Additional instance is `also right left`, meaning `also new current`.
      -- So `foldl (\current new -> VarE 'S.also `AppE` new `AppE` current) firstTbl restTbls` is correct.
      pure $ foldl' (\acc tbl -> VarE 'S.also `AppE` tbl `AppE` acc) firstTbl restTbls


renderSingleTableRef :: [Text.Text] -> PGT_AST.TableRef -> Q Exp
renderSingleTableRef cteNames = \case
  PGT_AST.RelationExprTableRef relationExpr maybeAliasClause sampleClause -> do
    when (isJust sampleClause) $ fail "TABLESAMPLE clause is not supported yet."
    renderPGTRelationExprTableRef cteNames relationExpr maybeAliasClause
  PGT_AST.JoinTableRef joinedTable maybeAliasClause ->
    -- If `maybeAliasClause` is Just, it means `(JOIN_TABLE) AS alias`.
    -- Squeal's direct join combinators don't alias the *result* of the join.
    -- This would require wrapping the join in a subquery.
    -- For now, we'll fail if an alias is applied to a complex join structure directly.
    -- Simple table references with aliases are handled by RelationExprTableRef.
    case maybeAliasClause of
      Just _ ->
        fail
          "Aliasing a JOIN clause directly is not supported. Consider a subquery: (SELECT * FROM ...) AS alias"
      Nothing -> renderPGTJoinedTable cteNames joinedTable
  -- PGT_AST.InParensTableRefTableRef was an incorrect pattern, removing it.
  -- Parenthesized joins are handled by PGT_AST.InParensJoinedTable within renderPGTJoinedTable.
  unsupported ->
    fail $ "Unsupported TableRef type in renderSingleTableRef: " <> show unsupported


renderPGTJoinedTable :: [Text.Text] -> PGT_AST.JoinedTable -> Q Exp
renderPGTJoinedTable cteNames = \case
  PGT_AST.InParensJoinedTable joinedTable -> renderPGTJoinedTable cteNames joinedTable
  PGT_AST.MethJoinedTable joinMeth leftRef rightRef -> do
    leftTableExp <- renderSingleTableRef cteNames leftRef
    rightTableExp <- renderSingleTableRef cteNames rightRef
    case joinMeth of
      PGT_AST.QualJoinMeth maybeJoinType joinQual ->
        case joinQual of
          PGT_AST.OnJoinQual onConditionAExpr -> do
            onConditionExp <- renderPGTAExpr onConditionAExpr
            squealJoinFn <-
              case maybeJoinType of
                Just (PGT_AST.LeftJoinType _) -> pure $ VarE 'S.leftOuterJoin
                Just (PGT_AST.RightJoinType _) -> pure $ VarE 'S.rightOuterJoin
                Just (PGT_AST.FullJoinType _) -> pure $ VarE 'S.fullOuterJoin
                Just PGT_AST.InnerJoinType -> pure $ VarE 'S.innerJoin
                Nothing -> pure $ VarE 'S.innerJoin -- SQL JOIN (no type) is INNER JOIN
                -- Change: Use S.& for join: leftTableExp & squealJoinFn rightTableExp onConditionExp
            pure $
              InfixE
                (Just leftTableExp)
                (VarE '(S.&))
                (Just (squealJoinFn `AppE` rightTableExp `AppE` onConditionExp))
          PGT_AST.UsingJoinQual _identsNE ->
            fail "USING join qualification not yet supported"
      PGT_AST.CrossJoinMeth ->
        -- Change: Use S.& for crossJoin: leftTableExp & S.crossJoin rightTableExp
        pure $
          InfixE
            (Just leftTableExp)
            (VarE '(S.&))
            (Just (VarE 'S.crossJoin `AppE` rightTableExp))
      PGT_AST.NaturalJoinMeth _naturalJoinType ->
        -- Squeal does not have direct high-level support for NATURAL JOIN.
        -- These would typically be rewritten as INNER JOINs with USING clauses
        -- or explicit ON conditions based on common column names.
        -- This is complex to implement correctly in the QQ and might be error-prone.
        fail "NATURAL JOIN is not supported by Squeal-QQ."


renderPGTRelationExprTableRef
  :: [Text.Text] -> PGT_AST.RelationExpr -> Maybe PGT_AST.AliasClause -> Q Exp
renderPGTRelationExprTableRef cteNames relationExpr maybeAliasClause = do
  (tableName, schemaName) <-
    case relationExpr of
      PGT_AST.SimpleRelationExpr (PGT_AST.SimpleQualifiedName ident) _ ->
        pure (getIdentText ident, Nothing)
      PGT_AST.SimpleRelationExpr
        ( PGT_AST.IndirectedQualifiedName
            schemaIdent
            (NE.last -> PGT_AST.AttrNameIndirectionEl tableIdent)
          )
        _ ->
          pure (getIdentText tableIdent, Just (getIdentText schemaIdent))
      _ ->
        fail $
          "Unsupported relation expression: " <> show relationExpr

  aliasStr <-
    case maybeAliasClause of
      Just (PGT_AST.AliasClause _ aliasIdent _) -> pure $ Text.unpack (getIdentText aliasIdent)
      Nothing -> case relationExpr of -- Infer default alias if none provided
        PGT_AST.SimpleRelationExpr (PGT_AST.SimpleQualifiedName ident) _ -> pure $ Text.unpack (getIdentText ident)
        PGT_AST.SimpleRelationExpr
          ( PGT_AST.IndirectedQualifiedName
              _
              (NE.last -> PGT_AST.AttrNameIndirectionEl ident)
            )
          _ -> pure $ Text.unpack (getIdentText ident)
        _ ->
          fail $
            "Cannot determine default alias for relation expression: " <> show relationExpr

  let
    isCte = tableName `elem` cteNames
    squealFn = if isCte then VarE 'S.common else VarE 'S.table

  tableExpr <-
    case schemaName of
      Nothing -> pure $ LabelE (Text.unpack tableName)
      Just schema ->
        if isCte
          then
            fail "CTEs cannot be schema-qualified."
          else
            pure $
              VarE '(S.!)
                `AppE` LabelE (Text.unpack schema)
                `AppE` LabelE (Text.unpack tableName)

  pure $ squealFn `AppE` (VarE 'S.as `AppE` tableExpr `AppE` LabelE aliasStr)


{- |
  Translates the `Targeting` clause of a SQL SELECT statement from the
  `postgresql-syntax` AST (`PGT_AST.Targeting`) into a Squeal representation.
  The `Targeting` clause defines the list of expressions or columns to be
  returned by the query (e.g., `*`, `col1`, `col2 AS alias`, `DISTINCT col3`).

  The function returns a Template Haskell `Q` computation that, when run,
  produces a pair:
  1. `Exp`: A Template Haskell expression representing the Squeal selection list.
     This could be `S.Star` for `SELECT *`, or a constructed Squeal expression
     for a list of target elements (e.g., `expression1 :* expression2 :* S.Nil`).
  2. `Maybe [PGT_AST.AExpr]`: This field is used to pass along the expressions
     from a `DISTINCT ON (expr1, expr2, ...)` clause. If the query uses
     `DISTINCT ON`, this will be `Just` containing the list of `PGT_AST.AExpr`
     nodes representing `expr1, expr2, ...`. For all other types of targeting
     (e.g., `SELECT DISTINCT col`, `SELECT col1, col2`, `SELECT *`), this
     will be `Nothing`.

  The function handles different kinds of targeting:
  - `PGT_AST.NormalTargeting`: Standard `SELECT col1, col2, ...`
  - `PGT_AST.AllTargeting`: `SELECT ALL ...` (often equivalent to normal select or `SELECT *`)
  - `PGT_AST.DistinctTargeting`: `SELECT DISTINCT ...` or `SELECT DISTINCT ON (...) ...`

  Returns (SquealSelectionListExp, Maybe DistinctOnAstExpressions)
-}
renderPGTTargeting
  :: PGT_AST.Targeting
  -> Q (Exp, Maybe [PGT_AST.AExpr])
renderPGTTargeting = \case
  PGT_AST.NormalTargeting targetList -> do
    selListExp <- renderPGTTargetList targetList
    pure (selListExp, Nothing)
  PGT_AST.AllTargeting maybeTargetList -> do
    selListExp <-
      case maybeTargetList of
        Nothing -> pure $ ConE 'S.Star -- SELECT ALL (which is like SELECT *)
        Just tl -> renderPGTTargetList tl
    pure (selListExp, Nothing)
  PGT_AST.DistinctTargeting maybeOnExprs targetList -> do
    selListExp <- renderPGTTargetList targetList
    pure (selListExp, fmap NE.toList maybeOnExprs)


renderPGTTargetEl :: PGT_AST.TargetEl -> Maybe PGT_AST.Ident -> Int -> Q Exp
renderPGTTargetEl targetEl mOuterAlias idx =
  let
    (exprAST, mInternalAlias) = case targetEl of
      PGT_AST.AliasedExprTargetEl e an -> (e, Just an)
      PGT_AST.ImplicitlyAliasedExprTargetEl e an -> (e, Just an)
      PGT_AST.ExprTargetEl e -> (e, Nothing)
      PGT_AST.AsteriskTargetEl ->
        ( PGT_AST.CExprAExpr
            ( PGT_AST.AexprConstCExpr
                PGT_AST.NullAexprConst
            )
        , Nothing -- Placeholder for Star, should be S.Star
        )
    finalAliasName = mOuterAlias <|> mInternalAlias
  in
    case targetEl of
      PGT_AST.AsteriskTargetEl -> pure $ ConE 'S.Star
      _ -> do
        renderedScalarExp <- renderPGTAExpr exprAST
        case exprAST of
          PGT_AST.CExprAExpr (PGT_AST.ColumnrefCExpr _)
            | Nothing <- finalAliasName ->
                pure renderedScalarExp
          _ -> do
            let
              aliasLabelStr =
                case finalAliasName of
                  Just ident -> Text.unpack $ getIdentText ident
                  Nothing -> "_col" <> show idx
            pure $
              VarE 'S.as
                `AppE` renderedScalarExp
                `AppE` LabelE aliasLabelStr


renderPGTTargetList :: PGT_AST.TargetList -> Q Exp
renderPGTTargetList (item NE.:| items) = go (item : items) 1
  where
    isAsterisk :: PGT_AST.TargetEl -> Bool
    isAsterisk PGT_AST.AsteriskTargetEl = True
    isAsterisk _ = False

    isDotStar :: PGT_AST.TargetEl -> Bool
    isDotStar
      ( PGT_AST.ExprTargetEl
          ( PGT_AST.CExprAExpr
              (PGT_AST.ColumnrefCExpr (PGT_AST.Columnref _ (Just indirection)))
            )
        ) =
        any isAllIndirectionEl (NE.toList indirection)
    isDotStar _ = False

    isAllIndirectionEl :: PGT_AST.IndirectionEl -> Bool
    isAllIndirectionEl PGT_AST.AllIndirectionEl = True
    isAllIndirectionEl _ = False

    renderPGTTargetElDotStar :: PGT_AST.TargetEl -> Q Exp
    renderPGTTargetElDotStar
      ( PGT_AST.ExprTargetEl
          ( PGT_AST.CExprAExpr
              ( PGT_AST.ColumnrefCExpr
                  ( PGT_AST.Columnref
                      qualName
                      _ -- indirectionOpt
                    )
                )
            )
        ) =
        pure $
          ConE 'S.DotStar
            `AppE` (LabelE (Text.unpack (getIdentText qualName)))
    renderPGTTargetElDotStar _ =
      fail "renderPGTTargetElDotStar called with unexpected TargetEl"

    go :: [PGT_AST.TargetEl] -> Int -> Q Exp
    go [] _ = fail "Empty selection list items in go."
    go [el] currentIdx = renderOne el currentIdx
    go (el : more) currentIdx = do
      renderedEl <- renderOne el currentIdx
      restRendered <- go more (currentIdx + 1)
      pure $ ConE 'S.Also `AppE` restRendered `AppE` renderedEl

    renderOne :: PGT_AST.TargetEl -> Int -> Q Exp
    renderOne el idx
      | isAsterisk el = pure $ ConE 'S.Star
      | isDotStar el = renderPGTTargetElDotStar el
      | otherwise = renderPGTTargetEl el Nothing idx


-- | Defines associativity of an operator.
data Associativity = LeftAssoc | RightAssoc | NonAssoc
  deriving stock (Eq, Show)


-- | Holds details for a binary operator relevant to precedence restructuring.
data OperatorDetails = OperatorDetails
  { odConstructor :: PGT_AST.AExpr -> PGT_AST.AExpr -> PGT_AST.AExpr
  , odPrecedence :: Int
  , odAssociativity :: Associativity
  }


{- | Extracts components if the expression is a recognized binary operator.
Higher precedence number means binds tighter.
Based on PostgreSQL operator precedence.
-}
getOperatorDetails
  :: PGT_AST.AExpr -> Maybe (PGT_AST.AExpr, OperatorDetails, PGT_AST.AExpr)
getOperatorDetails = \case
  PGT_AST.SymbolicBinOpAExpr l symOp r ->
    let
      details _op constr prec assoc = Just (l, OperatorDetails constr prec assoc, r)
      mathDetails mathOp prec assoc =
        details
          (PGT_AST.MathSymbolicExprBinOp mathOp)
          ( \l' r' -> PGT_AST.SymbolicBinOpAExpr l' (PGT_AST.MathSymbolicExprBinOp mathOp) r'
          )
          prec
          assoc
    in
      case symOp of
        PGT_AST.MathSymbolicExprBinOp PGT_AST.ArrowUpMathOp -> mathDetails PGT_AST.ArrowUpMathOp 8 LeftAssoc
        -- \^ (exponentiation)
        PGT_AST.MathSymbolicExprBinOp op
          | op `elem` [PGT_AST.AsteriskMathOp, PGT_AST.SlashMathOp, PGT_AST.PercentMathOp] ->
              mathDetails op 7 LeftAssoc
        -- \* / %
        PGT_AST.MathSymbolicExprBinOp op
          | op `elem` [PGT_AST.PlusMathOp, PGT_AST.MinusMathOp] ->
              mathDetails op 6 LeftAssoc -- binary + -
        PGT_AST.MathSymbolicExprBinOp op -- Comparisons
          | op
              `elem` [ PGT_AST.ArrowLeftMathOp
                     , PGT_AST.ArrowRightMathOp
                     , PGT_AST.EqualsMathOp
                     , PGT_AST.LessEqualsMathOp
                     , PGT_AST.GreaterEqualsMathOp
                     , PGT_AST.ArrowLeftArrowRightMathOp
                     , PGT_AST.ExclamationEqualsMathOp
                     ] ->
              mathDetails op 3 LeftAssoc -- < > = <= >= <> !=
        PGT_AST.QualSymbolicExprBinOp qualOp ->
          -- User-defined operators, bitwise, etc.
          details
            (PGT_AST.QualSymbolicExprBinOp qualOp)
            ( \l' r' -> PGT_AST.SymbolicBinOpAExpr l' (PGT_AST.QualSymbolicExprBinOp qualOp) r'
            )
            5
            LeftAssoc
        _ -> Nothing -- Should be exhaustive for PGT_AST.MathSymbolicExprBinOp if it's a binary op
  PGT_AST.AndAExpr l r -> Just (l, OperatorDetails PGT_AST.AndAExpr 2 LeftAssoc, r) -- AND (precedence 2 in PG docs example)
  PGT_AST.OrAExpr l r -> Just (l, OperatorDetails PGT_AST.OrAExpr 1 LeftAssoc, r) -- OR (precedence 1 in PG docs example)
  PGT_AST.VerbalExprBinOpAExpr l notOp verbalOp r mEscape ->
    -- LIKE, ILIKE, SIMILAR TO
    Just
      ( l
      , OperatorDetails
          (\l' r' -> PGT_AST.VerbalExprBinOpAExpr l' notOp verbalOp r' mEscape)
          3
          LeftAssoc
      , r -- Same as comparisons
      )
  PGT_AST.ReversableOpAExpr l notOp (PGT_AST.DistinctFromAExprReversableOp r) ->
    -- IS DISTINCT FROM
    Just
      ( l
      , OperatorDetails
          ( \l' r' ->
              PGT_AST.ReversableOpAExpr l' notOp (PGT_AST.DistinctFromAExprReversableOp r')
          )
          3
          LeftAssoc
      , r -- Same as =
      )
  _ -> Nothing


-- | Rearranges the AExpr syntax tree to account for operator precedence.
fixOperatorPrecedence :: PGT_AST.AExpr -> PGT_AST.AExpr
fixOperatorPrecedence = go
  where
    go expr =
      case getOperatorDetails expr of
        Just (l1, op1Details, r1) ->
          let
            l1Fixed = go l1
            r1Fixed = go r1
            currentOpConstructor = odConstructor op1Details
            currentPrecedence = odPrecedence op1Details
            currentAssociativity = odAssociativity op1Details
          in
            case getOperatorDetails r1Fixed of
              Just (l2, op2Details, r2) ->
                let
                  -- We have effectively: l1Fixed `op1` (l2 `op2` r2)
                  -- l2 is the left child of the (potentially restructured) r1Fixed
                  -- r2 is the right child of the (potentially restructured) r1Fixed
                  innerOpConstructor = odConstructor op2Details
                  innerPrecedence = odPrecedence op2Details
                in
                  -- innerAssociativity = odAssociativity op2Details -- Not used in this branch's logic directly

                  if currentPrecedence < innerPrecedence
                    || (currentPrecedence == innerPrecedence && currentAssociativity == RightAssoc)
                    then
                      -- op2 binds tighter, or op1 is right-associative with same precedence.
                      -- Structure l1Fixed `op1` (l2 `op2` r2) is correct.
                      currentOpConstructor l1Fixed r1Fixed
                    else
                      -- op1 binds tighter, or op1 is left-associative with same precedence.
                      -- We need to rotate to form: (l1Fixed `op1` l2) `op2` r2
                      let
                        newLeftChild = currentOpConstructor l1Fixed l2
                      in
                        go (innerOpConstructor newLeftChild r2) -- Recursively fix the new structure
              Nothing ->
                -- Right child r1Fixed is not a binary operator we're rebalancing.
                -- The structure l1Fixed `op1` r1Fixed is locally correct.
                currentOpConstructor l1Fixed r1Fixed
        Nothing ->
          -- Current expression `expr` is not a binary operator handled by getOperatorDetails,
          -- or it's an atom. Recursively fix its children.
          case expr of
            PGT_AST.CExprAExpr c -> PGT_AST.CExprAExpr c -- CExprs are atoms or structured (FuncCExpr, CaseCExpr etc.)
            PGT_AST.TypecastAExpr e t -> PGT_AST.TypecastAExpr (go e) t
            PGT_AST.CollateAExpr e c -> PGT_AST.CollateAExpr (go e) c
            PGT_AST.AtTimeZoneAExpr e1 e2 -> PGT_AST.AtTimeZoneAExpr (go e1) (go e2)
            PGT_AST.PlusAExpr e -> PGT_AST.PlusAExpr (go e) -- Unary plus
            -- MinusAExpr is handled by fixOperatorPrecedence if it's part of a binary op,
            -- otherwise it's a unary negate.
            PGT_AST.MinusAExpr e -> PGT_AST.MinusAExpr (go e)
            PGT_AST.PrefixQualOpAExpr op e -> PGT_AST.PrefixQualOpAExpr op (go e)
            PGT_AST.SuffixQualOpAExpr e op -> PGT_AST.SuffixQualOpAExpr (go e) op
            PGT_AST.NotAExpr e -> PGT_AST.NotAExpr (go e)
            PGT_AST.ReversableOpAExpr e notFlag revOp ->
              let
                eFixed = go e
              in
                case revOp of
                  PGT_AST.DistinctFromAExprReversableOp{} -> expr -- Should have been caught by getOperatorDetails
                  PGT_AST.BetweenAExprReversableOp symm bExpr aExpr ->
                    PGT_AST.ReversableOpAExpr
                      eFixed
                      notFlag
                      (PGT_AST.BetweenAExprReversableOp symm (goBExpr bExpr) (go aExpr))
                  PGT_AST.InAExprReversableOp inExpr ->
                    PGT_AST.ReversableOpAExpr
                      eFixed
                      notFlag
                      (PGT_AST.InAExprReversableOp (goInExpr inExpr))
                  _ -> PGT_AST.ReversableOpAExpr eFixed notFlag revOp -- For IS NULL, IS TRUE etc.
            PGT_AST.IsnullAExpr e -> PGT_AST.IsnullAExpr (go e)
            PGT_AST.NotnullAExpr e -> PGT_AST.NotnullAExpr (go e)
            PGT_AST.OverlapsAExpr row1 row2 -> PGT_AST.OverlapsAExpr (goRow row1) (goRow row2)
            PGT_AST.SubqueryAExpr e op st sub ->
              PGT_AST.SubqueryAExpr
                (go e)
                op
                st
                (either (Left . goSelectWithParens) (Right . go) sub)
            PGT_AST.UniqueAExpr s -> PGT_AST.UniqueAExpr (goSelectWithParens s)
            PGT_AST.DefaultAExpr -> PGT_AST.DefaultAExpr
            _ -> expr -- Leaf node or unhandled construct
    goBExpr :: PGT_AST.BExpr -> PGT_AST.BExpr
    goBExpr = \case
      PGT_AST.CExprBExpr c -> PGT_AST.CExprBExpr c
      PGT_AST.TypecastBExpr be t -> PGT_AST.TypecastBExpr (goBExpr be) t
      PGT_AST.PlusBExpr be -> PGT_AST.PlusBExpr (goBExpr be)
      PGT_AST.MinusBExpr be -> PGT_AST.MinusBExpr (goBExpr be)
      -- BExpr's own binary ops are typically higher precedence than AExpr's,
      -- but for completeness, one could define getOperatorDetails for BExpr too.
      -- For now, just recurse.
      PGT_AST.SymbolicBinOpBExpr l op r -> PGT_AST.SymbolicBinOpBExpr (goBExpr l) op (goBExpr r)
      PGT_AST.QualOpBExpr op be -> PGT_AST.QualOpBExpr op (goBExpr be)
      PGT_AST.IsOpBExpr be notFlag isOp ->
        let
          beFixed = goBExpr be
        in
          case isOp of
            PGT_AST.DistinctFromBExprIsOp b ->
              PGT_AST.IsOpBExpr beFixed notFlag (PGT_AST.DistinctFromBExprIsOp (goBExpr b))
            _ -> PGT_AST.IsOpBExpr beFixed notFlag isOp

    goRow :: PGT_AST.Row -> PGT_AST.Row
    goRow = \case
      PGT_AST.ExplicitRowRow mExprs -> PGT_AST.ExplicitRowRow (fmap (NE.map go) mExprs)
      PGT_AST.ImplicitRowRow (PGT_AST.ImplicitRow exprs aexpr) -> PGT_AST.ImplicitRowRow (PGT_AST.ImplicitRow (NE.map go exprs) (go aexpr))

    goSelectWithParens :: PGT_AST.SelectWithParens -> PGT_AST.SelectWithParens
    goSelectWithParens = id -- Placeholder: A full traversal would be needed.
    goInExpr :: PGT_AST.InExpr -> PGT_AST.InExpr
    goInExpr = \case
      PGT_AST.SelectInExpr s -> PGT_AST.SelectInExpr (goSelectWithParens s)
      PGT_AST.ExprListInExpr exprs -> PGT_AST.ExprListInExpr (NE.map go exprs)


renderPGTAExpr :: PGT_AST.AExpr -> Q Exp
renderPGTAExpr astExpr = case fixOperatorPrecedence astExpr of
  PGT_AST.CExprAExpr cExpr -> renderPGTCExpr cExpr
  PGT_AST.TypecastAExpr aExpr typename -> do
    tnExp <- renderPGTTypename typename
    aExp <- renderPGTAExpr aExpr
    pure $ VarE 'S.cast `AppE` tnExp `AppE` aExp
  PGT_AST.SymbolicBinOpAExpr left op right -> do
    lExp <- renderPGTAExpr left
    rExp <- renderPGTAExpr right
    squealOpExp <-
      case op of
        PGT_AST.MathSymbolicExprBinOp mathOp -> pure $ renderPGTMathOp mathOp
        PGT_AST.QualSymbolicExprBinOp qualOp -> pure $ renderPGTQualOp qualOp
    pure (squealOpExp `AppE` lExp `AppE` rExp)
  PGT_AST.PrefixQualOpAExpr op expr -> do
    let
      opExp' = renderPGTQualOp op
    eExp' <- renderPGTAExpr expr
    pure (opExp' `AppE` eExp')
  PGT_AST.AndAExpr left right -> do
    lExp' <- renderPGTAExpr left
    rExp' <- renderPGTAExpr right
    pure (VarE '(S..&&) `AppE` lExp' `AppE` rExp')
  PGT_AST.OrAExpr left right -> do
    lExp' <- renderPGTAExpr left
    rExp' <- renderPGTAExpr right
    pure (VarE '(S..||) `AppE` lExp' `AppE` rExp')
  PGT_AST.NotAExpr expr -> do
    eExp' <- renderPGTAExpr expr
    pure (VarE 'S.not_ `AppE` eExp')
  PGT_AST.VerbalExprBinOpAExpr left not op right mEscape -> do
    when (isJust mEscape) $ fail "LIKE with ESCAPE is not supported yet."
    lExp' <- renderPGTAExpr left
    rExp' <- renderPGTAExpr right
    baseOpExp <-
      case op of
        PGT_AST.LikeVerbalExprBinOp -> pure $ VarE 'S.like
        PGT_AST.IlikeVerbalExprBinOp -> pure $ VarE 'S.ilike
        _ -> fail $ "Unsupported verbal binary operator: " <> show op
    let
      finalOpExp = if not then VarE 'S.not_ `AppE` baseOpExp else baseOpExp
    pure (finalOpExp `AppE` lExp' `AppE` rExp')
  PGT_AST.ReversableOpAExpr expr not reversableOp -> do
    renderedExpr' <- renderPGTAExpr expr
    case reversableOp of
      PGT_AST.NullAExprReversableOp ->
        pure $ (if not then VarE 'S.isNotNull else VarE 'S.isNull) `AppE` renderedExpr'
      PGT_AST.BetweenAExprReversableOp _asymmetric bExpr andAExpr -> do
        bExp' <- renderPGTBExpr bExpr
        aExp' <- renderPGTAExpr andAExpr
        let
          opVar' = if not then VarE 'S.notBetween else VarE 'S.between
        pure $ opVar' `AppE` renderedExpr' `AppE` TupE [Just bExp', Just aExp']
      PGT_AST.InAExprReversableOp inExpr ->
        case inExpr of
          PGT_AST.ExprListInExpr exprList -> do
            let opVar' = if not then VarE 'S.notIn else VarE 'S.in_
            listExp' <- ListE <$> mapM renderPGTAExpr (NE.toList exprList)
            pure $ opVar' `AppE` renderedExpr' `AppE` listExp'
          PGT_AST.SelectInExpr selectWithParens -> do
            let
              (squealOp, squealFn) =
                if not
                  then (VarE '(S../=), VarE 'S.subAll)
                  else (VarE '(S..==), VarE 'S.subAny)
            subqueryExp <- toSquealSelectWithParens [] Nothing selectWithParens
            pure $ squealFn `AppE` renderedExpr' `AppE` squealOp `AppE` subqueryExp
      _ -> fail $ "Unsupported reversable operator: " <> show reversableOp
  PGT_AST.DefaultAExpr -> pure $ ConE 'S.Default
  PGT_AST.MinusAExpr expr -> do
    -- Unary minus
    eExp' <- renderPGTAExpr expr
    let
      zeroExp = AppE (VarE 'fromInteger) (LitE (IntegerL 0))
    pure (InfixE (Just zeroExp) (VarE '(-)) (Just eExp'))
  unsupported -> fail $ "Unsupported AExpr: " <> show unsupported


renderPGTBExpr :: PGT_AST.BExpr -> Q Exp
renderPGTBExpr = \case
  PGT_AST.CExprBExpr cExpr -> renderPGTCExpr cExpr
  PGT_AST.TypecastBExpr bExpr typename -> do
    tnExp <- renderPGTTypename typename
    bExp <- renderPGTBExpr bExpr
    pure $ VarE 'S.cast `AppE` tnExp `AppE` bExp
  PGT_AST.SymbolicBinOpBExpr left op right -> do
    lExp <- renderPGTBExpr left
    rExp <- renderPGTBExpr right
    squealOpExp <-
      case op of
        PGT_AST.MathSymbolicExprBinOp mathOp -> pure $ renderPGTMathOp mathOp
        PGT_AST.QualSymbolicExprBinOp qualOp -> pure $ renderPGTQualOp qualOp
    pure (squealOpExp `AppE` lExp `AppE` rExp)
  unsupported -> fail $ "Unsupported BExpr: " <> show unsupported


renderPGTCExpr :: PGT_AST.CExpr -> Q Exp
renderPGTCExpr = \case
  PGT_AST.AexprConstCExpr aexprConst -> pure $ renderPGTAexprConst aexprConst
  PGT_AST.ColumnrefCExpr columnref -> pure $ renderPGTColumnref columnref
  PGT_AST.ParamCExpr n maybeIndirection -> do
    when (isJust maybeIndirection) $
      fail "Parameters with indirection (e.g. $1[i]) are not supported."
    pure $ VarE 'S.param `AppTypeE` LitT (NumTyLit (fromIntegral n))
  PGT_AST.InParensCExpr expr maybeIndirection -> do
    when (isJust maybeIndirection) $
      fail "Parenthesized expressions with indirection are not supported."
    renderPGTAExpr expr -- Squeal's operator precedence should handle this
  PGT_AST.FuncCExpr funcExpr -> renderPGTFuncExpr funcExpr
  unsupported -> fail $ "Unsupported CExpr: " <> show unsupported


renderPGTFuncExpr :: PGT_AST.FuncExpr -> Q Exp
renderPGTFuncExpr = \case
  PGT_AST.ApplicationFuncExpr funcApp maybeWithinGroup maybeFilter maybeOver -> do
    when (isJust maybeWithinGroup) $ fail "WITHIN GROUP clause is not supported."
    when (isJust maybeFilter) $ fail "FILTER clause is not supported."
    when (isJust maybeOver) $ fail "OVER clause is not supported."
    renderPGTFuncApplication funcApp
  PGT_AST.SubexprFuncExpr funcCommonSubexpr -> renderPGTFuncExprCommonSubexpr funcCommonSubexpr


renderPGTFuncApplication :: PGT_AST.FuncApplication -> Q Exp
renderPGTFuncApplication (PGT_AST.FuncApplication funcName maybeParams) =
  case funcName of
    PGT_AST.IndirectedFuncName{} ->
      fail "Functions with indirection (e.g. schema.func) are not supported."
    PGT_AST.TypeFuncName fident ->
      let
        fnNameStr = Text.unpack (getIdentText fident)
      in
        case Text.toLower (Text.pack fnNameStr) of
          "inline" ->
            case maybeParams of
              Just (PGT_AST.NormalFuncApplicationParams _ args _) ->
                case NE.toList args of
                  [ PGT_AST.ExprFuncArgExpr
                      (PGT_AST.CExprAExpr (PGT_AST.ColumnrefCExpr (PGT_AST.Columnref ident Nothing)))
                    ] -> do
                      let
                        varName :: Name
                        varName = mkName . Text.unpack . getIdentText $ ident
                      pure $ VarE 'S.inline `AppE` VarE varName
                  _ -> fail "inline() function expects a single variable argument"
              _ -> fail "inline() function expects a single variable argument"
          "inline_param" ->
            case maybeParams of
              Just (PGT_AST.NormalFuncApplicationParams _ args _) ->
                case NE.toList args of
                  [ PGT_AST.ExprFuncArgExpr
                      (PGT_AST.CExprAExpr (PGT_AST.ColumnrefCExpr (PGT_AST.Columnref ident Nothing)))
                    ] -> do
                      let
                        varName :: Name
                        varName = mkName . Text.unpack . getIdentText $ ident
                      pure $ VarE 'S.inlineParam `AppE` VarE varName
                  _ -> fail "inline_param() function expects a single variable argument"
              _ -> fail "inline_param() function expects a single variable argument"
          otherFnName ->
            let
              squealFn :: Q Exp
              squealFn =
                case otherFnName of
                  "coalesce" -> pure $ VarE 'S.coalesce
                  "lower" -> pure $ VarE 'S.lower
                  "char_length" -> pure $ VarE 'S.charLength
                  "character_length" -> pure $ VarE 'S.charLength
                  "upper" -> pure $ VarE 'S.upper
                  "count" -> pure $ VarE 'S.count -- Special handling for count(*) might be needed
                  "now" -> pure $ VarE 'S.now
                  _ -> fail $ "Unsupported function: " <> fnNameStr
            in
              case maybeParams of
                Nothing -> squealFn -- No-argument function
                Just params -> case params of
                  PGT_AST.NormalFuncApplicationParams maybeAllOrDistinct args maybeSortClause -> do
                    when (isJust maybeAllOrDistinct) $
                      fail "DISTINCT in function calls is not supported."
                    when (isJust maybeSortClause) $
                      fail "ORDER BY in function calls is not supported."
                    fn <- squealFn
                    argExps <- mapM renderPGTFuncArgExpr (NE.toList args)
                    pure $ foldl' AppE fn argExps
                  PGT_AST.StarFuncApplicationParams ->
                    -- Specific for count(*)
                    if fnNameStr == "count"
                      then pure $ VarE 'S.countStar
                      else fail "Star argument only supported for COUNT"
                  _ -> fail $ "Unsupported function parameters structure: " <> show params


renderPGTFuncArgExpr :: PGT_AST.FuncArgExpr -> Q Exp
renderPGTFuncArgExpr = \case
  PGT_AST.ExprFuncArgExpr aExpr -> renderPGTAExpr aExpr
  _ -> fail "Named or colon-syntax function arguments not supported"


renderPGTFuncExprCommonSubexpr :: PGT_AST.FuncExprCommonSubexpr -> Q Exp
renderPGTFuncExprCommonSubexpr = \case
  PGT_AST.CurrentTimestampFuncExprCommonSubexpr (Just _) ->
    fail "CURRENT_TIMESTAMP with precision is not supported."
  PGT_AST.CurrentTimestampFuncExprCommonSubexpr Nothing -> pure $ VarE 'S.now -- Or S.currentTimestamp
  PGT_AST.CurrentDateFuncExprCommonSubexpr -> pure $ VarE 'S.currentDate
  PGT_AST.CoalesceFuncExprCommonSubexpr exprListNE -> do
    renderedInitExprs <- mapM renderPGTAExpr (NE.init exprListNE)
    renderedLastExpr <- renderPGTAExpr (NE.last exprListNE)
    pure $ VarE 'S.coalesce `AppE` ListE renderedInitExprs `AppE` renderedLastExpr
  e -> fail $ "Unsupported common function subexpression: " <> show e


renderPGTColumnref :: PGT_AST.Columnref -> Exp
renderPGTColumnref (PGT_AST.Columnref colId maybeIndirection) =
    case maybeIndirection of
      Nothing -> LabelE (Text.unpack (getIdentText colId))
      Just indirection ->
        let
          base = LabelE (Text.unpack (getIdentText colId))
        in
          foldl' applyIndirection base (NE.toList indirection)
  where
    applyIndirection acc = \case
      PGT_AST.AttrNameIndirectionEl attrName ->
        VarE '(S.!) `AppE` acc `AppE` LabelE (Text.unpack (getIdentText attrName))
      _ -> error "Unsupported column reference indirection"


renderPGTAexprConst :: PGT_AST.AexprConst -> Exp
renderPGTAexprConst = \case
  PGT_AST.IAexprConst n ->
    ConE 'S.UnsafeExpression
      `AppE` ( VarE 'BS8.pack
                 `AppE` LitE (StringL (show n))
             )
  PGT_AST.FAexprConst f ->
    ConE 'S.UnsafeExpression
      `AppE` ( VarE 'BS8.pack
                 `AppE` LitE (StringL (show f))
             )
  PGT_AST.SAexprConst s ->
    VarE 'fromString `AppE` LitE (StringL (Text.unpack s))
  PGT_AST.BoolAexprConst True -> VarE 'S.true
  PGT_AST.BoolAexprConst False -> VarE 'S.false
  PGT_AST.NullAexprConst -> VarE 'S.null_
  unsupported -> error $ "Unsupported AexprConst: " <> show unsupported


renderPGTMathOp :: PGT_AST.MathOp -> Exp
renderPGTMathOp = \case
  PGT_AST.PlusMathOp -> VarE '(+)
  PGT_AST.MinusMathOp -> VarE '(-)
  PGT_AST.AsteriskMathOp -> VarE '(*)
  PGT_AST.EqualsMathOp -> VarE '(S..==)
  PGT_AST.ArrowLeftArrowRightMathOp -> VarE '(S../=) -- <>
  PGT_AST.ExclamationEqualsMathOp -> VarE '(S../=) -- !=
  PGT_AST.ArrowRightMathOp -> VarE '(S..>)
  PGT_AST.GreaterEqualsMathOp -> VarE '(S..>=)
  PGT_AST.ArrowLeftMathOp -> VarE '(S..<)
  PGT_AST.LessEqualsMathOp -> VarE '(S..<=)
  _ -> error "Unsupported math operator"


renderPGTQualOp :: PGT_AST.QualOp -> Exp
renderPGTQualOp = \case
  PGT_AST.OpQualOp opText ->
    case Text.toLower opText of
      "+" -> VarE '(+)
      "-" -> VarE '(-)
      "*" -> VarE '(*)
      "=" -> VarE '(S..==)
      "<>" -> VarE '(S../=)
      "!=" -> VarE '(S../=)
      ">" -> VarE '(S..>)
      ">=" -> VarE '(S..>=)
      "<" -> VarE '(S..<)
      "<=" -> VarE '(S..<=)
      "and" -> VarE '(S..&&)
      "or" -> VarE '(S..||)
      "not" -> VarE 'S.not_
      "like" -> VarE 'S.like
      "ilike" -> VarE 'S.ilike
      _ -> error $ "Unsupported QualOp operator text: " <> Text.unpack opText
  PGT_AST.OperatorQualOp _anyOperator ->
    error "OPERATOR(any_operator) syntax not supported"


renderPGTTypename :: PGT_AST.Typename -> Q Exp
renderPGTTypename (PGT_AST.Typename setof simpleTypename nullable arrayInfo) = do
  when setof $ fail "SETOF type modifier is not supported."
  when nullable $ fail "Nullable type modifier '?' is not supported."
  baseTypeExp <- renderPGTSimpleTypename simpleTypename
  case arrayInfo of
    Nothing -> pure baseTypeExp
    Just (dims, nullableArray) -> do
      when nullableArray $ fail "Nullable array modifier '?' is not supported."
      renderPGTArrayDimensions baseTypeExp dims


renderPGTArrayDimensions :: Exp -> PGT_AST.TypenameArrayDimensions -> Q Exp
renderPGTArrayDimensions baseTypeExp = \case
  PGT_AST.BoundsTypenameArrayDimensions bounds ->
    -- Squeal's fixarray takes a type-level list of Nats for dimensions.
    -- This is hard to represent directly from parsed integer bounds.
    -- For now, we'll only support 1D arrays if bounds are provided.
    case NE.toList bounds of
      [Just dim] ->
        pure $
          VarE 'S.fixarray
            `AppTypeE` LitT (NumTyLit (fromIntegral dim))
            `AppE` baseTypeExp
      [_] -> pure $ VarE 'S.vararray `AppE` baseTypeExp -- e.g. int[]
      _ ->
        fail "Multidimensional arrays with explicit bounds not yet supported"
  PGT_AST.ExplicitTypenameArrayDimensions Nothing -> pure $ VarE 'S.vararray `AppE` baseTypeExp -- e.g. sometype ARRAY
  PGT_AST.ExplicitTypenameArrayDimensions (Just dim) ->
    pure $
      VarE 'S.fixarray
        `AppTypeE` LitT (NumTyLit (fromIntegral dim))
        `AppE` baseTypeExp -- e.g. sometype ARRAY[N]


renderPGTSimpleTypename :: PGT_AST.SimpleTypename -> Q Exp
renderPGTSimpleTypename = \case
  PGT_AST.GenericTypeSimpleTypename
    (PGT_AST.GenericType typeFnName attrs maybeModifiers) -> do
      when (isJust attrs) $
        fail "Qualified type names (e.g. schema.my_type) are not supported."
      let
        nameLower = Text.toLower (getIdentText typeFnName)
        extractLength :: Maybe PGT_AST.TypeModifiers -> Q Integer
        extractLength = \case
          Just
            ((PGT_AST.CExprAExpr (PGT_AST.AexprConstCExpr (PGT_AST.IAexprConst n))) NE.:| []) -> pure (fromIntegral n)
          Just other ->
            fail $
              "Unsupported type modifier for " <> Text.unpack nameLower <> ": " <> show other
          Nothing ->
            fail $
              "Type "
                <> Text.unpack nameLower
                <> " requires a length argument (e.g., "
                <> Text.unpack nameLower
                <> "(N))."

        extractLengthOrDefault :: Integer -> Maybe PGT_AST.TypeModifiers -> Q Integer
        extractLengthOrDefault def = \case
          Just
            ((PGT_AST.CExprAExpr (PGT_AST.AexprConstCExpr (PGT_AST.IAexprConst n))) NE.:| []) -> pure (fromIntegral n)
          Just other ->
            fail $
              "Unsupported type modifier for " <> Text.unpack nameLower <> ": " <> show other
          Nothing -> pure def
      case nameLower of
        "char" -> do
          len <- extractLengthOrDefault 1 maybeModifiers
          pure $ VarE 'S.char `AppTypeE` LitT (NumTyLit len)
        "character" -> do
          len <- extractLengthOrDefault 1 maybeModifiers
          pure $ VarE 'S.character `AppTypeE` LitT (NumTyLit len)
        "varchar" -> case maybeModifiers of
          Nothing -> pure $ VarE 'S.text -- varchar without length is text
          Just _ -> do
            len <- extractLength maybeModifiers
            pure $ VarE 'S.varchar `AppTypeE` LitT (NumTyLit len)
        "character varying" -> case maybeModifiers of
          Nothing -> pure $ VarE 'S.text -- character varying without length is text
          Just _ -> do
            len <- extractLength maybeModifiers
            pure $ VarE 'S.characterVarying `AppTypeE` LitT (NumTyLit len)
        "bool" -> pure $ VarE 'S.bool
        "int2" -> pure $ VarE 'S.int2
        "smallint" -> pure $ VarE 'S.smallint
        "int4" -> pure $ VarE 'S.int4
        "int" -> pure $ VarE 'S.int
        "integer" -> pure $ VarE 'S.integer
        "int8" -> pure $ VarE 'S.int8
        "bigint" -> pure $ VarE 'S.bigint
        "numeric" -> pure $ VarE 'S.numeric -- Ignoring precision/scale for now
        "float4" -> pure $ VarE 'S.float4 -- Ignoring precision for now
        "real" -> pure $ VarE 'S.real
        "float8" -> pure $ VarE 'S.float8
        "double precision" -> pure $ VarE 'S.doublePrecision
        "money" -> pure $ VarE 'S.money
        "text" -> pure $ VarE 'S.text
        "bytea" -> pure $ VarE 'S.bytea
        "timestamp" -> pure $ VarE 'S.timestamp
        "timestamptz" -> pure $ VarE 'S.timestamptz
        "timestamp with time zone" -> pure $ VarE 'S.timestampWithTimeZone
        "date" -> pure $ VarE 'S.date
        "time" -> pure $ VarE 'S.time
        "timetz" -> pure $ VarE 'S.timetz
        "time with time zone" -> pure $ VarE 'S.timeWithTimeZone
        "interval" -> pure $ VarE 'S.interval
        "uuid" -> pure $ VarE 'S.uuid
        "inet" -> pure $ VarE 'S.inet
        "json" -> pure $ VarE 'S.json
        "jsonb" -> pure $ VarE 'S.jsonb
        "tsvector" -> pure $ VarE 'S.tsvector
        "tsquery" -> pure $ VarE 'S.tsquery
        "oid" -> pure $ VarE 'S.oid
        "int4range" -> pure $ VarE 'S.int4range
        "int8range" -> pure $ VarE 'S.int8range
        "numrange" -> pure $ VarE 'S.numrange
        "tsrange" -> pure $ VarE 'S.tsrange
        "tstzrange" -> pure $ VarE 'S.tstzrange
        "daterange" -> pure $ VarE 'S.daterange
        "record" -> pure $ VarE 'S.record
        other -> fail $ "Unsupported generic type name: " <> Text.unpack other
  PGT_AST.NumericSimpleTypename numeric -> renderPGTNumeric numeric
  PGT_AST.BitSimpleTypename (PGT_AST.Bit _varying _maybeLength) ->
    -- PostgreSQL's BIT type without length is BIT(1). BIT VARYING without length is unlimited.
    -- Squeal's `char` and `varchar` are for text, not bit strings.
    -- Squeal does not have a direct equivalent for PG bit string types yet.
    -- Potentially map to bytea or text, or add new Squeal types. For now, error.
    fail
      "BIT and BIT VARYING types are not directly supported by Squeal's `char`/`varchar` like types. Consider using bytea or text, or a custom Squeal type."
  PGT_AST.CharacterSimpleTypename charTypeAst ->
    case charTypeAst of
      PGT_AST.CharacterCharacter False -> pure $ VarE 'S.character `AppTypeE` LitT (NumTyLit 1) -- SQL CHARACTER -> Squeal character(1)
      PGT_AST.CharacterCharacter True -> pure $ VarE 'S.text -- SQL CHARACTER VARYING -> Squeal text
      PGT_AST.CharCharacter False -> pure $ VarE 'S.char `AppTypeE` LitT (NumTyLit 1) -- SQL CHAR -> Squeal char(1)
      PGT_AST.CharCharacter True -> pure $ VarE 'S.text -- SQL CHAR VARYING -> Squeal text
      PGT_AST.VarcharCharacter -> pure $ VarE 'S.text -- SQL VARCHAR (no length) -> Squeal text
      -- National character types are often aliases for standard character types in PostgreSQL
      PGT_AST.NationalCharacterCharacter False -> pure $ VarE 'S.character `AppTypeE` LitT (NumTyLit 1) -- NCHAR -> character(1)
      PGT_AST.NationalCharacterCharacter True -> pure $ VarE 'S.text -- NCHAR VARYING -> text
      PGT_AST.NationalCharCharacter False -> pure $ VarE 'S.char `AppTypeE` LitT (NumTyLit 1) -- NATIONAL CHAR -> char(1)
      PGT_AST.NationalCharCharacter True -> pure $ VarE 'S.text -- NATIONAL CHAR VARYING -> text
      PGT_AST.NcharCharacter False -> pure $ VarE 'S.char `AppTypeE` LitT (NumTyLit 1) -- NCHAR (synonym for NATIONAL CHAR) -> char(1)
      PGT_AST.NcharCharacter True -> pure $ VarE 'S.text -- NCHAR VARYING -> text
  PGT_AST.ConstDatetimeSimpleTypename dt -> case dt of
    PGT_AST.TimestampConstDatetime precision maybeTimezone -> do
      when (isJust precision) $ fail "TIMESTAMP with precision is not supported."
      pure $ case maybeTimezone of
        Just False -> VarE 'S.timestampWithTimeZone -- WITH TIME ZONE
        _ -> VarE 'S.timestamp -- WITHOUT TIME ZONE or unspecified
    PGT_AST.TimeConstDatetime precision maybeTimezone -> do
      when (isJust precision) $ fail "TIME with precision is not supported."
      pure $ case maybeTimezone of
        Just False -> VarE 'S.timeWithTimeZone -- WITH TIME ZONE
        _ -> VarE 'S.time -- WITHOUT TIME ZONE or unspecified
  PGT_AST.ConstIntervalSimpleTypename (Left (Just _)) ->
    fail "INTERVAL with qualifiers is not supported."
  PGT_AST.ConstIntervalSimpleTypename (Left Nothing) ->
    pure $ VarE 'S.interval
  PGT_AST.ConstIntervalSimpleTypename (Right _) ->
    fail "INTERVAL with integer literal is not supported in this context."


renderPGTNumeric :: PGT_AST.Numeric -> Q Exp
renderPGTNumeric = \case
  PGT_AST.IntNumeric -> pure $ VarE 'S.int
  PGT_AST.IntegerNumeric -> pure $ VarE 'S.integer
  PGT_AST.SmallintNumeric -> pure $ VarE 'S.smallint
  PGT_AST.BigintNumeric -> pure $ VarE 'S.bigint
  PGT_AST.RealNumeric -> pure $ VarE 'S.real
  PGT_AST.FloatNumeric (Just _) -> fail "FLOAT with precision is not supported."
  PGT_AST.FloatNumeric Nothing -> pure $ VarE 'S.float4
  PGT_AST.DoublePrecisionNumeric -> pure $ VarE 'S.doublePrecision
  PGT_AST.DecimalNumeric (Just _) -> fail "DECIMAL with precision/scale is not supported."
  PGT_AST.DecimalNumeric Nothing -> pure $ VarE 'S.numeric
  PGT_AST.DecNumeric (Just _) -> fail "DEC with precision/scale is not supported."
  PGT_AST.DecNumeric Nothing -> pure $ VarE 'S.numeric
  PGT_AST.NumericNumeric (Just _) -> fail "NUMERIC with precision/scale is not supported."
  PGT_AST.NumericNumeric Nothing -> pure $ VarE 'S.numeric
  PGT_AST.BooleanNumeric -> pure $ VarE 'S.bool


getIdentText :: PGT_AST.Ident -> Text.Text
getIdentText = \case
  PGT_AST.QuotedIdent t -> t
  PGT_AST.UnquotedIdent t -> t


