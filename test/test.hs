{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Squeal.QuasiQuotes (Field, ssql, withDB)
import Data.Text (Text)
import GHC.Generics (Generic)
import Squeal.PostgreSQL (type (:::), type (:=>), NullType(NotNull, Null),
  Optionality(Def, NoDef), PGType(PGint4, PGtext), RenderSQL(renderSQL),
  SchemumType(Table), TableConstraint(ForeignKey, PrimaryKey), Public,
  Statement)
import Squeal.QuasiQuotes (ssql, withDB)
import Test.Hspec (describe, hspec, it)
import qualified Generics.SOP as SOP (Generic, HasDatatypeInfo)


{- Copied (with one minor change) from the squeal documentation: -}
type UsersColumns =
  '[ "id"   :::   'Def :=> 'NotNull 'PGtext
   , "name" ::: 'NoDef :=> 'NotNull 'PGtext ]
type UsersConstraints = '[ "pk_users" ::: 'PrimaryKey '["id"] ]
type EmailsColumns =
  '[ "id" ::: 'Def :=> 'NotNull 'PGint4
   , "user_id" ::: 'NoDef :=> 'NotNull 'PGint4
   , "email" ::: 'NoDef :=> 'Null 'PGtext ]
type EmailsConstraints =
  '[ "pk_emails"  ::: 'PrimaryKey '["id"]
   , "fk_user_id" ::: 'ForeignKey '["user_id"] "public" "users" '["id"] ]
type Schema =
  '[ "users" ::: 'Table (UsersConstraints :=> UsersColumns)
   , "emails" ::: 'Table (EmailsConstraints :=> EmailsColumns) ]
type DB = Public Schema


data User = User
  {   id :: Text
  , name :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

data NotUser = NotUser
  {    notId :: Int
  ,  notName :: Text
  -- , whatever :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)


main :: IO ()
main =
  hspec $ do
    describe "thing" $ do
      it "works" $ do
        let
          {-
            This type signature is required in order to disambiguate the
            squeal expression. I'm thinking we might consider making the
            quasiquoter input and output row types totally monomorphic
            tuple-like types instead of polymorphic
            `(GenericRow row y ys) => y)`, then another layer can be added
            if the user wants to convert the tuple type into something
            else. I think this might greatly help the error messages
            by forcing the type checker to check against something
            static like `Field @"field-1" Int :> Field @"field-2" Text
            :> ...` instead of checking against a possibly ambiguous
            type variable.
          -}
          statement :: Statement DB () _
          statement = withDB @DB [ssql| select * from users |]
        print $ renderSQL statement


