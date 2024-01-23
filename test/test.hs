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

import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Prelude (($), (.), IO, Maybe, Show, putStrLn)
import Squeal.PostgreSQL (NullType(NotNull, Null), Optionality(Def,
  NoDef), PGType(PGint4, PGtext, PGuuid), RenderSQL(renderSQL),
  SchemumType(Table), TableConstraint(ForeignKey, PrimaryKey), (:::),
  (:=>), Public, Statement)
import Squeal.QuasiQuotes (Field, ssql)
import Test.Hspec (describe, hspec, it)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Generics.SOP as SOP


{- Copied (with one minor change) from the squeal documentation: -}
type UsersColumns =
  '[          "id" :::   'Def :=> 'NotNull 'PGtext
   ,        "name" ::: 'NoDef :=> 'NotNull 'PGtext
   , "employee_id" ::: 'NoDef :=> 'NotNull 'PGuuid
   ,         "bio" ::: 'NoDef :=> 'Null    'PGtext
   ]
type UsersConstraints = '[ "pk_users" ::: 'PrimaryKey '["id"] ]
type EmailsColumns =
  '[      "id" :::   'Def :=> 'NotNull 'PGint4
   , "user_id" ::: 'NoDef :=> 'NotNull 'PGtext
   ,   "email" ::: 'NoDef :=> 'Null 'PGtext ]
type EmailsConstraints =
  '[ "pk_emails"  ::: 'PrimaryKey '["id"]
   , "fk_user_id" ::: 'ForeignKey '["user_id"] "public" "users" '["id"] ]
type Schema =
  '[  "users" ::: 'Table (UsersConstraints :=> UsersColumns)
   , "emails" ::: 'Table (EmailsConstraints :=> EmailsColumns) ]
type DB = Public Schema


data User = User
  {   id :: Text
  , name :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)


main :: IO ()
main =
  hspec $ do
    describe "queries" $ do
      it "select * from users" $ do
        let
          statement
            :: Statement DB ()
                 (Field "id" Text,
                 (Field "name" Text,
                 (Field "employee_id" UUID,
                 (Field "bio" (Maybe Text),
                 ()))))
          statement = [ssql| select * from users |]
        printQuery statement

      it "select user.name from users" $ do
        let
          statement :: Statement DB () (Field "name" Text, ())
          statement = [ssql| select users.name from users |]
        printQuery statement

      it "select name from users" $ do
        let
          statement :: Statement DB () (Field "name" Text, ())
          statement = [ssql| select name from users |]
        printQuery statement

      it "select name, id from users" $ do
        let
          statement
            :: Statement DB ()
                 (Field "name" Text,
                 (Field "id" Text,
                 ()))
          statement = [ssql| select name, id from users |]
        printQuery statement

      it "select id, name from users" $ do
        let
          statement
            :: Statement DB ()
                 (Field "id" Text,
                 (Field "name" Text,
                 ()))
          statement = [ssql| select id, name from users |]
        printQuery statement

      it "select users.id, employee_id from users" $ do
        let
          statement
            :: Statement DB ()
                 (Field "id" Text,
                 (Field "employee_id" UUID,
                 ()))
          statement = [ssql| select users.id, employee_id from users |]
        printQuery statement

      it "select users.* from users" $ do
        let
          statement
            :: Statement DB ()
                 (Field "id" Text,
                 (Field "name" Text,
                 (Field "employee_id" UUID,
                 (Field "bio" (Maybe Text),
                 ()))))
          statement = [ssql| select users.* from users |]
        printQuery statement

      it "select users.id, employee_id as emp_id from users" $ do
        let
          statement
            :: Statement DB ()
                 (Field "id" Text,
                 (Field "emp_id" UUID,
                 ()))
          statement = [ssql| select users.id, employee_id as emp_id from users |]
        printQuery statement

      it "select users.id as user_id, employee_id from users" $ do
        let
          statement
            :: Statement DB ()
                 (Field "user_id" Text,
                 (Field "employee_id" UUID,
                 ()))
          statement = [ssql| select users.id as user_id, employee_id from users |]
        printQuery statement


printQuery :: RenderSQL a => a -> IO ()
printQuery = putStrLn . T.unpack . TE.decodeUtf8 . renderSQL


