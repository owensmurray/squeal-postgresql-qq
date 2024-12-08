{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE ViewPatterns #-}

{-| Description: Translate insert statements.  -}
module Squeal.QuasiQuotes.Insert (
  toSquealInsert,
) where


import Data.List (foldl')
import Language.Haskell.TH.Syntax (Exp(AppE, ConE, LabelE, ListE, VarE))
import Language.SQL.SimpleSQL.Syntax (InsertSource(InsertQuery),
  Name(Name), QueryExpr(Values), ScalarExpr(Iden), Statement(Insert))
import Prelude (Functor(fmap), Maybe(Just, Nothing), Semigroup((<>)),
  Show(show), ($), (<$>), error)
import Squeal.QuasiQuotes.Common (renderScalarExpr)
import qualified Data.Char as Char
import qualified Squeal.PostgreSQL as S


toSquealInsert
  :: [Name]
  -> Maybe [Name]
  -> InsertSource
  -> Exp
toSquealInsert into fields values =
    VarE 'S.manipulation
    `AppE` (
      case (fields, values) of
        (Just names, InsertQuery (Values vals)) ->
            VarE 'S.insertInto_
              `AppE` renderQualifiedName into {- The table name -}
              `AppE` renderValueRows names vals {- The values -}
        _ ->
          error $ "Unspported: " <> show (Insert into fields values)
    )
  where
    renderQualifiedName :: [Name] -> Exp
    renderQualifiedName qualNames =
      case renderNameAsLabel <$> qualNames of
        [] -> error $ "Unspported: " <> show (Insert into fields values)
        first:more ->
          foldl'
            (\acc n ->
              VarE '(S.!)
                `AppE` acc
                `AppE` n
            )
            first
            more

    renderNameAsLabel :: Name -> Exp
    renderNameAsLabel = \case
      Name Nothing name -> LabelE name
      unsupported -> error $ "Unsupported: " <> show unsupported

    renderValueRows :: [Name] -> [[ScalarExpr]] -> Exp
    renderValueRows names vals =
      case vals of
        [] -> error "Insert statement has no value rows."
        row:more ->
          ConE 'S.Values
            `AppE` renderValueRow names row
            `AppE` ListE (renderValueRow names <$> more)

    renderValueRow :: [Name] -> [ScalarExpr] -> Exp
    renderValueRow =
      \cases
        [] [] -> ConE 'S.Nil
        ((Name Nothing name):names)
            (Iden [Name Nothing (fmap Char.toLower -> "default")]:vals)
          ->
            ConE '(S.:*)
            `AppE`
              (
                VarE 'S.as
                  `AppE` ConE 'S.Default
                  `AppE` LabelE name
              )
            `AppE`
              renderValueRow names vals
        ((Name Nothing name):names) (val:vals) ->
          ConE '(S.:*)
          `AppE`
            (
              VarE 'S.as
                `AppE` (ConE 'S.Set `AppE` renderScalarExpr val)
                `AppE` LabelE name
            )
          `AppE`
            renderValueRow names vals
        _ _ ->
          error "Mismatched number of names and values in insert statement."

