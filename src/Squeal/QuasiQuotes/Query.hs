{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

{- | Description: Translate query expressions. -}
module Squeal.QuasiQuotes.Query (
  toSquealQuery,
) where

import Language.Haskell.TH.Syntax (Exp(AppE, ConE, LabelE, VarE), Q)
import Language.SQL.SimpleSQL.Syntax (Name(Name), QueryExpr(Select,
  qeFrom, qeGroupBy, qeHaving, qeOffset, qeOrderBy, qeSelectList,
  qeSetQuantifier, qeWhere), SetQuantifier(SQDefault), ScalarExpr)
import Prelude (Applicative(pure), Maybe(Just, Nothing), MonadFail(fail),
  Semigroup((<>)), Show(show), ($), error)
import Squeal.QuasiQuotes.Common (renderScalarExpr, renderTableRef)
import Squeal.QuasiQuotes.RowType (monoQuery)
import qualified Squeal.PostgreSQL as S


toSquealQuery :: QueryExpr -> Q Exp
toSquealQuery = \case
  Select
      { qeSetQuantifier = SQDefault
      , qeSelectList    = selectionList
      , qeFrom          = [selectionFrom]
      , qeWhere         = Nothing
      , qeGroupBy       = []
      , qeHaving        = Nothing
      , qeOrderBy       = []
      , qeOffset        = Nothing
      }
    ->
      {-
        moral equivalent of:
        > monoQuery (select Star (from (table (#theTable `as` #theTable))))
      -}
      pure $
        VarE 'monoQuery
        `AppE`
          (
            VarE 'S.select
            `AppE` renderSelectionList selectionList
            `AppE`
              (
                VarE 'S.from
                `AppE`
                  renderTableRef selectionFrom
              )
          )
  unsupported ->
    fail $ "Unsupported: " <> show unsupported


renderSelectionList :: [(ScalarExpr, Maybe Name)] -> Exp
renderSelectionList selectionList =
    case selectionList of
      [(field, alias)] ->
        renderAlias (renderScalarExpr field) alias
      (field, alias):more ->
        {-
          the order of these arguments to 'Also' seems backwards, but
          for whatever reason this produces a row type with the fields
          in the right order. I'm not sure if Squeal gets it backwards,
          or the sql parser does, or if we are reversing them somewhere.
        -}
        ConE 'S.Also
        `AppE` renderSelectionList more
        `AppE` renderAlias (renderScalarExpr field) alias
      [] -> error "Empty selection list not supported."
  where
    renderAlias :: Exp -> Maybe Name -> Exp
    renderAlias e = \case
      Nothing -> e
      Just (Name _ alias) ->
        VarE 'S.as
        `AppE` e
        `AppE` LabelE alias


