{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE ViewPatterns #-}

-- | Commonplace renderers shared by other modules.
module Squeal.QuasiQuotes.Common (
  renderPGTTableRef,
  renderPGTAExpr,
  getIdentText,
) where

import Data.String (IsString(fromString))
import Language.Haskell.TH.Syntax
  ( Exp(AppE, AppTypeE, ConE, InfixE, LabelE, ListE, LitE, TupE, VarE)
  , Lit(IntegerL, StringL), TyLit(NumTyLit), Type(LitT), Q
  )
import Prelude
  ( Applicative(pure), Bool(False, True), Either(Left, Right), Eq((==))
  , Foldable(elem, foldl'), Functor(fmap), Maybe(Just, Nothing), MonadFail(fail)
  , Num((*), (+), (-), fromInteger), Ord((<)), Semigroup((<>)), Show(show)
  , Traversable(mapM), ($), (&&), (.), (<$>), (||), Int, Integer, String, either
  , error, fromIntegral, id
  )
import qualified Data.ByteString.Char8 as BS8
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as Text
import qualified PostgresqlSyntax.Ast as PGT_AST
import qualified Squeal.PostgreSQL as S


getIdentText :: PGT_AST.Ident -> Text.Text
getIdentText = \case
  PGT_AST.QuotedIdent t -> t
  PGT_AST.UnquotedIdent t -> t


renderPGTTableRef :: NE.NonEmpty PGT_AST.TableRef -> Q Exp
renderPGTTableRef tableRefs = do
  renderedTableRefs <- mapM renderSingleTableRef (NE.toList tableRefs)
  case renderedTableRefs of
    [] -> fail "Empty FROM clause" -- Should not happen with NonEmpty
    (firstTbl : restTbls) ->
      -- For FROM t1, t2, t3 Squeal uses: (table #t1) & also (table #t2) & also (table #t3)
      -- S.also takes new item first, then accumulated.
      -- So foldl' (\acc item -> VarE 'S.also `AppE` item `AppE` acc) firstTbl restTbls
      -- However, Squeal's FromClause Additional instance is `also right left`, meaning `also new current`.
      -- So `foldl (\current new -> VarE 'S.also `AppE` new `AppE` current) firstTbl restTbls` is correct.
      pure $ foldl' (\acc tbl -> VarE 'S.also `AppE` tbl `AppE` acc) firstTbl restTbls


renderSingleTableRef :: PGT_AST.TableRef -> Q Exp
renderSingleTableRef = \case
  PGT_AST.RelationExprTableRef relationExpr maybeAliasClause _sampleClause ->
    renderPGTRelationExprTableRef relationExpr maybeAliasClause
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
      Nothing -> renderPGTJoinedTable joinedTable
  -- PGT_AST.InParensTableRefTableRef was an incorrect pattern, removing it.
  -- Parenthesized joins are handled by PGT_AST.InParensJoinedTable within renderPGTJoinedTable.
  unsupported ->
    fail $ "Unsupported TableRef type in renderSingleTableRef: " <> show unsupported


renderPGTJoinedTable :: PGT_AST.JoinedTable -> Q Exp
renderPGTJoinedTable = \case
  PGT_AST.InParensJoinedTable joinedTable -> renderPGTJoinedTable joinedTable
  PGT_AST.MethJoinedTable joinMeth leftRef rightRef -> do
    leftTableExp <- renderSingleTableRef leftRef
    rightTableExp <- renderSingleTableRef rightRef
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
  :: PGT_AST.RelationExpr -> Maybe PGT_AST.AliasClause -> Q Exp
renderPGTRelationExprTableRef relationExpr maybeAliasClause = do
  tableExpr <-
    case relationExpr of
      PGT_AST.SimpleRelationExpr qualifiedName _isAsterisk ->
        renderPGTQualifiedName qualifiedName
      PGT_AST.OnlyRelationExpr qualifiedName _areParensPresent ->
        -- Squeal doesn't have a direct equivalent for ONLY, so we treat it as a normal table for now.
        -- This might need adjustment if ONLY semantics are critical.
        renderPGTQualifiedName qualifiedName

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

  pure $ VarE 'S.table `AppE` (VarE 'S.as `AppE` tableExpr `AppE` LabelE aliasStr)


renderPGTQualifiedName :: PGT_AST.QualifiedName -> Q Exp
renderPGTQualifiedName = \case
  PGT_AST.SimpleQualifiedName ident -> pure $ LabelE (Text.unpack (getIdentText ident))
  PGT_AST.IndirectedQualifiedName
    schemaIdent
    (PGT_AST.AttrNameIndirectionEl colIdent NE.:| []) ->
      -- Assuming schema.table.col
      pure $
        VarE '(S.!)
          `AppE` LabelE (Text.unpack (getIdentText schemaIdent))
          `AppE` LabelE (Text.unpack (getIdentText colIdent))
  unsupported ->
    fail $ "Unsupported qualified name for table reference: " <> show unsupported


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
  PGT_AST.VerbalExprBinOpAExpr left not op right _mEscape -> do
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
        let
          opVar' = if not then VarE 'S.notIn else VarE 'S.in_
        in
          case inExpr of
            PGT_AST.ExprListInExpr exprList -> do
              listExp' <- ListE <$> mapM renderPGTAExpr (NE.toList exprList)
              pure $ opVar' `AppE` renderedExpr' `AppE` listExp'
            _ -> fail "Unsupported IN subquery expression"
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
  PGT_AST.ParamCExpr n _maybeIndirection ->
    pure $ VarE 'S.param `AppTypeE` LitT (NumTyLit (fromIntegral n))
  PGT_AST.InParensCExpr expr _ -> renderPGTAExpr expr -- Squeal's operator precedence should handle this
  PGT_AST.FuncCExpr funcExpr -> renderPGTFuncExpr funcExpr
  unsupported -> fail $ "Unsupported CExpr: " <> show unsupported


renderPGTFuncExpr :: PGT_AST.FuncExpr -> Q Exp
renderPGTFuncExpr = \case
  PGT_AST.ApplicationFuncExpr funcApp _ _ _ -> renderPGTFuncApplication funcApp
  PGT_AST.SubexprFuncExpr funcCommonSubexpr -> renderPGTFuncExprCommonSubexpr funcCommonSubexpr


renderPGTFuncApplication :: PGT_AST.FuncApplication -> Q Exp
renderPGTFuncApplication (PGT_AST.FuncApplication funcName maybeParams) =
  let
    fnNameStr :: String
    fnNameStr =
      case funcName of
        PGT_AST.TypeFuncName ident -> Text.unpack (getIdentText ident)
        PGT_AST.IndirectedFuncName ident _ -> Text.unpack (getIdentText ident) -- Ignoring indirection for now
    squealFn :: Q Exp
    squealFn =
      case Text.toLower (Text.pack fnNameStr) of
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
        PGT_AST.NormalFuncApplicationParams _allOrDistinct args _sortClause -> do
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
renderPGTTypename (PGT_AST.Typename _setof simpleTypename _nullable arrayInfo) = do
  baseTypeExp <- renderPGTSimpleTypename simpleTypename
  case arrayInfo of
    Nothing -> pure baseTypeExp
    Just (dims, _nullableArray) -> renderPGTArrayDimensions baseTypeExp dims


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
    (PGT_AST.GenericType typeFnName _attrs maybeModifiers) ->
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
      in
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
  PGT_AST.NumericSimpleTypename numeric -> pure $ renderPGTNumeric numeric
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
    PGT_AST.TimestampConstDatetime _precision maybeTimezone ->
      pure $ case maybeTimezone of
        Just False -> VarE 'S.timestampWithTimeZone -- WITH TIME ZONE
        _ -> VarE 'S.timestamp -- WITHOUT TIME ZONE or unspecified
    PGT_AST.TimeConstDatetime _precision maybeTimezone ->
      pure $ case maybeTimezone of
        Just False -> VarE 'S.timeWithTimeZone -- WITH TIME ZONE
        _ -> VarE 'S.time -- WITHOUT TIME ZONE or unspecified
  PGT_AST.ConstIntervalSimpleTypename _ -> pure $ VarE 'S.interval -- Ignoring interval qualifiers for now


renderPGTNumeric :: PGT_AST.Numeric -> Exp
renderPGTNumeric = \case
  PGT_AST.IntNumeric -> VarE 'S.int
  PGT_AST.IntegerNumeric -> VarE 'S.integer
  PGT_AST.SmallintNumeric -> VarE 'S.smallint
  PGT_AST.BigintNumeric -> VarE 'S.bigint
  PGT_AST.RealNumeric -> VarE 'S.real
  PGT_AST.FloatNumeric _ -> VarE 'S.float4 -- Ignoring precision for float(p) for now
  PGT_AST.DoublePrecisionNumeric -> VarE 'S.doublePrecision
  PGT_AST.DecimalNumeric _ -> VarE 'S.numeric -- Ignoring precision/scale for decimal/numeric for now
  PGT_AST.DecNumeric _ -> VarE 'S.numeric
  PGT_AST.NumericNumeric _ -> VarE 'S.numeric
  PGT_AST.BooleanNumeric -> VarE 'S.bool


