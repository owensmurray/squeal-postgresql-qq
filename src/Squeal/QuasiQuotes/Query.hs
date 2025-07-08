{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE ViewPatterns #-}

-- | Description: Translate query expressions.
module Squeal.QuasiQuotes.Query (
  toSquealQuery,
) where

import Control.Applicative (Alternative((<|>)))
import Control.Monad (unless, when)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Language.Haskell.TH.Syntax
  ( Exp(AppE, ConE, InfixE, LabelE, LitE, VarE), Lit(IntegerL), Q, mkName
  )
import Prelude
  ( Applicative(pure), Bool(False, True), Either(Left, Right)
  , Foldable(foldl', foldr, null), Maybe(Just, Nothing), MonadFail(fail)
  , Num((+)), Ord((>=)), Semigroup((<>)), Show(show), Traversable(mapM), ($)
  , (&&), Int, any, fromIntegral, zip
  )
import Squeal.QuasiQuotes.Common
  ( getIdentText, renderPGTAExpr, renderPGTTableRef
  )
import Squeal.QuasiQuotes.RowType (monoQuery)
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
      _maybeWithClause
      selectClause
      _maybeSortClause
      maybeSelectLimit
      maybeForLockingClause
    ) =
    case selectClause of
      Left simpleSelect ->
        toSquealSimpleSelect
          simpleSelect
          maybeSelectLimit
          maybeForLockingClause
      Right selectWithParens' -> toSquealSelectWithParens selectWithParens'


toSquealSimpleSelect
  :: PGT_AST.SimpleSelect
  -> Maybe PGT_AST.SelectLimit
  -> Maybe PGT_AST.ForLockingClause
  -> Q Exp
toSquealSimpleSelect simpleSelect maybeSelectLimit maybeForLockingClause =
  case simpleSelect of
    PGT_AST.ValuesSimpleSelect valuesClause -> do
      unless (isNothing maybeSelectLimit && isNothing maybeForLockingClause) $
        fail $
          "OFFSET / LIMIT / FOR UPDATE etc. not supported with VALUES clause "
            <> "in this translation yet."
      renderedValues <- renderValuesClauseToNP valuesClause
      pure $ VarE 'monoQuery `AppE` (VarE 'S.values_ `AppE` renderedValues)
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
                      VarE 'monoQuery
                        `AppE` (VarE 'S.values_ `AppE` renderedTargetingForValues)
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
                when (isJust maybeHavingClause) $
                  fail $
                    "HAVING clause is not yet supported in this translation "
                      <> "for NormalSimpleSelect with FROM."
                when (isJust maybeWindowClause) $
                  fail $
                    "WINDOW clause is not yet supported in this translation "
                      <> "for NormalSimpleSelect with FROM."

                renderedFromClauseExp <- renderPGTTableRef fromClause
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

                (tableExprWithOffset, mTableExprWithLimit) <-
                  processSelectLimit tableExprWithGroupBy maybeSelectLimit

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

                selectionTargetExp <-
                  renderPGTTargeting targeting
                pure $
                  VarE 'monoQuery
                    `AppE` ( VarE 'S.select
                               `AppE` selectionTargetExp
                               `AppE` finalTableExprWithPotentialLocking
                           )
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


renderPGTTargeting :: PGT_AST.Targeting -> Q Exp
renderPGTTargeting = \case
  PGT_AST.NormalTargeting targetList -> do
    selListExp <- renderPGTTargetList targetList
    pure selListExp
  PGT_AST.AllTargeting maybeTargetList -> do
    selListExp <-
      case maybeTargetList of
        Nothing -> pure $ ConE 'S.Star -- SELECT ALL (which is like SELECT *)
        Just tl -> renderPGTTargetList tl
    pure selListExp
  PGT_AST.DistinctTargeting _ _ ->
    fail "DISTINCT queries are not supported by this quasi-quoter yet."


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


renderPGTTargetList :: PGT_AST.TargetList -> Q Exp
renderPGTTargetList (item NE.:| items) =
    if null items && isAsterisk item
      then
        pure $ ConE 'S.Star
      else
        if null items && isDotStar item
          then
            renderPGTTargetElDotStar item
          else
            go (item : items) 1
  where
    isAsterisk PGT_AST.AsteriskTargetEl = True
    isAsterisk _ = False

    isDotStar
      ( PGT_AST.ExprTargetEl
          ( PGT_AST.CExprAExpr
              (PGT_AST.ColumnrefCExpr (PGT_AST.Columnref _ (Just indirection)))
            )
        ) =
        any isAllIndirectionEl (NE.toList indirection)
    isDotStar _ = False

    isAllIndirectionEl PGT_AST.AllIndirectionEl = True
    isAllIndirectionEl _ = False

    renderPGTTargetElDotStar :: PGT_AST.TargetEl -> Q Exp
    renderPGTTargetElDotStar
      ( PGT_AST.ExprTargetEl
          ( PGT_AST.CExprAExpr
              ( PGT_AST.ColumnrefCExpr
                  ( PGT_AST.Columnref
                      qualName
                      indirectionOpt
                    )
                )
            )
        ) =
        case indirectionOpt of
          Just indirection
            | any isAllIndirectionEl (NE.toList indirection) ->
                pure $
                  ConE 'S.DotStar
                    `AppE` (LabelE (Text.unpack (getIdentText qualName)))
          _ ->
            fail $
              "renderPGTTargetElDotStar called with non-DotStar "
                <> "TargetEl structure"
    renderPGTTargetElDotStar _ =
      fail "renderPGTTargetElDotStar called with unexpected TargetEl"

    go :: [PGT_AST.TargetEl] -> Int -> Q Exp
    go [] _ =
      {- Should not happen with NonEmpty input to renderPGTTargetList -}
      fail "Empty selection list items in go."
    go [el] currentIdx = renderPGTTargetEl el Nothing currentIdx
    go (el : more) currentIdx = do
      renderedEl <- renderPGTTargetEl el Nothing currentIdx
      if null more
        then pure renderedEl
        else do
          restRendered <- go more (currentIdx + 1)
          pure $ ConE 'S.Also `AppE` restRendered `AppE` renderedEl


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
  PGT_AST.LimitLimitClause slValue _mOffsetVal -> case slValue of
    PGT_AST.ExprSelectLimitValue
      ( PGT_AST.CExprAExpr
          ( PGT_AST.FuncCExpr
              ( PGT_AST.ApplicationFuncExpr
                  ( PGT_AST.FuncApplication
                      (PGT_AST.TypeFuncName (PGT_AST.UnquotedIdent "haskell"))
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
  PGT_AST.FetchOnlyLimitClause _ mVal _ -> case mVal of
    Just (PGT_AST.NumSelectFetchFirstValue _ (Left n)) ->
      if n >= 0
        then pure (LitE (IntegerL (fromIntegral n)))
        else fail $ "FETCH FIRST value must be non-negative: " <> show n
    Just (PGT_AST.NumSelectFetchFirstValue _ (Right d)) ->
      fail $ "FETCH FIRST with float value not supported: " <> show d
    Just
      ( PGT_AST.ExprSelectFetchFirstValue
          (PGT_AST.AexprConstCExpr (PGT_AST.IAexprConst n))
        ) ->
        if n >= 0
          then pure (LitE (IntegerL (fromIntegral n)))
          else fail $ "FETCH FIRST value must be non-negative: " <> show n
    Just (PGT_AST.ExprSelectFetchFirstValue cexpr) ->
      fail $ "Unsupported FETCH FIRST expression: " <> show cexpr
    Nothing ->
      fail $
        "FETCH FIRST without value not supported (Squeal requires "
          <> "explicit value for limit)."


renderPGTOffsetClause :: PGT_AST.OffsetClause -> Q Exp
renderPGTOffsetClause = \case
  PGT_AST.ExprOffsetClause
    ( PGT_AST.CExprAExpr
        ( PGT_AST.FuncCExpr
            ( PGT_AST.ApplicationFuncExpr
                ( PGT_AST.FuncApplication
                    (PGT_AST.TypeFuncName (PGT_AST.UnquotedIdent "haskell"))
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
  PGT_AST.FetchFirstOffsetClause sffv _ -> case sffv of
    PGT_AST.NumSelectFetchFirstValue _ (Left n) ->
      if n >= 0
        then pure (LitE (IntegerL (fromIntegral n)))
        else fail $ "OFFSET value must be non-negative: " <> show n
    _ -> fail $ "Unsupported OFFSET FETCH FIRST value: " <> show sffv


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
  PGT_AST.DistinctTargeting _ _ ->
    fail $
      "DISTINCT queries are not supported with VALUES clause in "
        <> "this translation."


