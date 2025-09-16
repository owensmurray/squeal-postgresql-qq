{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- {-# OPTIONS_GHC -ddump-splices #-}

module Main (main) where

import Data.Aeson (Value)
import Data.Int (Int32, Int64)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Time (Day, UTCTime)
import Data.UUID (UUID)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Prelude
  ( Applicative(pure), Eq((==)), Maybe(Just, Nothing), MonadFail(fail)
  , Semigroup((<>)), Show(show), ($), (.), Bool, IO, putStrLn
  )
import Squeal.PostgreSQL
  ( NullType(NotNull, Null), Optionality(Def, NoDef)
  , PGType(PGint4, PGjson, PGjsonb, PGtext, PGuuid), RenderSQL(renderSQL)
  , SchemumType(Table), TableConstraint(ForeignKey, PrimaryKey), (:::), (:=>)
  , Json, Jsonb, Only, Statement
  )
import Squeal.QuasiQuotes (Field, ssql)
import Test.Hspec (describe, hspec, it)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Generics.SOP as SOP




{- FOURMOLU_DISABLE -}
{- Copied mostly from the squeal documentation: -}
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
   ,   "email" ::: 'NoDef :=> 'Null 'PGtext
   ]
type EmailsConstraints =
  '[ "pk_emails"  ::: 'PrimaryKey '["id"]
   , "fk_user_id" ::: 'ForeignKey '["user_id"] "public" "users" '["id"]
   ]
type Schema =
  '[      "users" ::: 'Table (UsersConstraints :=> UsersColumns)
   ,     "emails" ::: 'Table (EmailsConstraints :=> EmailsColumns)
   , "jsonb_test" ::: 'Table ('[] :=> '["data" ::: 'NoDef :=> 'NotNull 'PGjsonb])
   ,  "json_test" ::: 'Table ('[] :=> '["data" ::: 'NoDef :=> 'NotNull 'PGjson])
   , "users_copy" ::: 'Table (UsersCopyConstraints :=> UsersCopyColumns)
   ]
type UsersCopyColumns =
  '[ "id"   ::: 'NoDef :=> 'NotNull 'PGtext
   , "name" ::: 'NoDef :=> 'NotNull 'PGtext
   , "bio"  ::: 'NoDef :=> 'Null    'PGtext
   ]
type UsersCopyConstraints = '[ "pk_users_copy" ::: 'PrimaryKey '["id"] ]
type DB =
  '[ "public" ::: Schema
   ,  "other" ::: Schema
   ]
{- FOURMOLU_ENABLE -}


data User = User
  { id :: Text
  , name :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)


main :: IO ()
main =
  hspec $ do
    describe "queries" $ do
      it "select * from users" $ do
        let
          statement
            :: Statement
                 DB
                 ()
                 ( Field "id" Text
                 , ( Field "name" Text
                   , ( Field "employee_id" UUID
                     , ( Field "bio" (Maybe Text)
                       , ()
                       )
                     )
                   )
                 )
          statement = [ssql| select * from users |]
          squealRendering :: Text
          squealRendering = "SELECT * FROM \"users\" AS \"users\""
        checkStatement squealRendering statement

      it "select * from public.users" $ do
        let
          statement
            :: Statement
                 DB
                 ()
                 ( Field "id" Text
                 , ( Field "name" Text
                   , ( Field "employee_id" UUID
                     , ( Field "bio" (Maybe Text)
                       , ()
                       )
                     )
                   )
                 )
          statement = [ssql| select * from public.users |]
          squealRendering :: Text
          squealRendering = "SELECT * FROM \"users\" AS \"users\""
        checkStatement squealRendering statement

      it "SELECT * FROM \"users\" AS \"users\"" $ do
        let
          statement
            :: Statement
                 DB
                 ()
                 ( Field "id" Text
                 , ( Field "name" Text
                   , ( Field "employee_id" UUID
                     , ( Field "bio" (Maybe Text)
                       , ()
                       )
                     )
                   )
                 )
          statement = [ssql| SELECT * FROM "users" AS "users" |]
          squealRendering :: Text
          squealRendering = "SELECT * FROM \"users\" AS \"users\""
        checkStatement squealRendering statement

      it "select * from users where name = 'bob'" $ do
        let
          statement
            :: Statement
                 DB
                 ()
                 ( Field "id" Text
                 , ( Field "name" Text
                   , ( Field "employee_id" UUID
                     , ( Field "bio" (Maybe Text)
                       , ()
                       )
                     )
                   )
                 )
          statement = [ssql| select * from users where name = 'bob' |]
          squealRendering :: Text
          squealRendering = "SELECT * FROM \"users\" AS \"users\" WHERE (\"name\" = (E'bob' :: text))"
        checkStatement squealRendering statement

      it "select * from users where id = $1" $ do
        let
          statement
            :: Statement
                 DB
                 (Only Text)
                 ( Field "id" Text
                 , ( Field "name" Text
                   , ( Field "employee_id" UUID
                     , ( Field "bio" (Maybe Text)
                       , ()
                       )
                     )
                   )
                 )
          statement = [ssql| select * from users where id = $1 |]
          squealRendering :: Text
          squealRendering = "SELECT * FROM \"users\" AS \"users\" WHERE (\"id\" = ($1 :: text))"
        checkStatement squealRendering statement

      it "select users.name from users" $ do
        let
          statement :: Statement DB () (Field "name" Text, ())
          statement = [ssql| select users.name from users |]
          squealRendering :: Text
          squealRendering = "SELECT \"users\".\"name\" AS \"name\" FROM \"users\" AS \"users\""
        checkStatement squealRendering statement

      it "select name from users" $ do
        let
          statement :: Statement DB () (Field "name" Text, ())
          statement = [ssql| select name from users |]
          squealRendering :: Text
          squealRendering = "SELECT \"name\" AS \"name\" FROM \"users\" AS \"users\""
        checkStatement squealRendering statement

      it "select count(*) from users group by ()" $ do
        let
          statement :: Statement DB () (Field "_col1" Int64, ())
          statement = [ssql| select count(*) from users group by () |]
          squealRendering :: Text
          squealRendering = "SELECT count(*) AS \"_col1\" FROM \"users\" AS \"users\""
        checkStatement squealRendering statement

      it "select name, id from users" $ do
        let
          statement
            :: Statement
                 DB
                 ()
                 ( Field "name" Text
                 , ( Field "id" Text
                   , ()
                   )
                 )
          statement = [ssql| select name, id from users |]
          squealRendering :: Text
          squealRendering = "SELECT \"name\" AS \"name\", \"id\" AS \"id\" FROM \"users\" AS \"users\""
        checkStatement squealRendering statement

      it "select id, name from users" $ do
        let
          statement
            :: Statement
                 DB
                 ()
                 ( Field "id" Text
                 , ( Field "name" Text
                   , ()
                   )
                 )
          statement = [ssql| select id, name from users |]
          squealRendering :: Text
          squealRendering = "SELECT \"id\" AS \"id\", \"name\" AS \"name\" FROM \"users\" AS \"users\""
        checkStatement squealRendering statement

      it "select users.id, employee_id from users" $ do
        let
          statement
            :: Statement
                 DB
                 ()
                 ( Field "id" Text
                 , ( Field "employee_id" UUID
                   , ()
                   )
                 )
          statement = [ssql| select users.id, employee_id from users |]
          squealRendering :: Text
          squealRendering =
            "SELECT \"users\".\"id\" AS \"id\", \"employee_id\" AS \"employee_id\" FROM \"users\" AS \"users\""
        checkStatement squealRendering statement

      it "select users.* from users" $ do
        let
          statement
            :: Statement
                 DB
                 ()
                 ( Field "id" Text
                 , ( Field "name" Text
                   , ( Field "employee_id" UUID
                     , ( Field "bio" (Maybe Text)
                       , ()
                       )
                     )
                   )
                 )
          statement = [ssql| select users.* from users |]
          squealRendering :: Text
          squealRendering = "SELECT \"users\".* FROM \"users\" AS \"users\""
        checkStatement squealRendering statement

      it "select users.* from other.users" $ do
        let
          statement
            :: Statement
                 DB
                 ()
                 ( Field "id" Text
                 , ( Field "name" Text
                   , ( Field "employee_id" UUID
                     , ( Field "bio" (Maybe Text)
                       , ()
                       )
                     )
                   )
                 )
          statement = [ssql| select users.* from other.users |]
          squealRendering :: Text
          squealRendering = "SELECT \"users\".* FROM \"other\".\"users\" AS \"users\""
        checkStatement squealRendering statement

      it "select * from users limit 3" $ do
        let
          statement
            :: Statement
                 DB
                 ()
                 ( Field "id" Text
                 , ( Field "name" Text
                   , ( Field "employee_id" UUID
                     , ( Field "bio" (Maybe Text)
                       , ()
                       )
                     )
                   )
                 )
          statement = [ssql| select * from users limit 3 |]
          squealRendering :: Text
          squealRendering = "SELECT * FROM \"users\" AS \"users\" LIMIT 3"
        checkStatement squealRendering statement

      it "select * from users limit inline(lim)" $ do
        let
          mkStatement
            :: Word64
            -> Statement
                 DB
                 ()
                 ( Field "id" Text
                 , ( Field "name" Text
                   , ( Field "employee_id" UUID
                     , ( Field "bio" (Maybe Text)
                       , ()
                       )
                     )
                   )
                 )
          mkStatement lim =
            [ssql| select * from users limit inline(lim) |]

          squealRendering1 :: Text
          squealRendering1 = "SELECT * FROM \"users\" AS \"users\" LIMIT 10"

          squealRendering2 :: Text
          squealRendering2 = "SELECT * FROM \"users\" AS \"users\" LIMIT 20"

        checkStatement squealRendering1 (mkStatement 10)
        checkStatement squealRendering2 (mkStatement 20)

      it "select * from users offset inline(off)" $ do
        let
          mkStatement
            :: Word64
            -> Statement
                 DB
                 ()
                 ( Field "id" Text
                 , ( Field "name" Text
                   , ( Field "employee_id" UUID
                     , ( Field "bio" (Maybe Text)
                       , ()
                       )
                     )
                   )
                 )
          mkStatement off =
            [ssql| select * from users offset inline(off) |]

          squealRendering1 :: Text
          squealRendering1 = "SELECT * FROM \"users\" AS \"users\" OFFSET 5"

          squealRendering2 :: Text
          squealRendering2 = "SELECT * FROM \"users\" AS \"users\" OFFSET 15"

        checkStatement squealRendering1 (mkStatement 5)
        checkStatement squealRendering2 (mkStatement 15)

      it "select * from users offset 1" $ do
        let
          statement
            :: Statement
                 DB
                 ()
                 ( Field "id" Text
                 , ( Field "name" Text
                   , ( Field "employee_id" UUID
                     , ( Field "bio" (Maybe Text)
                       , ()
                       )
                     )
                   )
                 )
          statement = [ssql| select * from users offset 1 |]
          squealRendering :: Text
          squealRendering = "SELECT * FROM \"users\" AS \"users\" OFFSET 1"
        checkStatement squealRendering statement

      it "select users.id, employee_id as emp_id from users" $ do
        let
          statement
            :: Statement
                 DB
                 ()
                 ( Field "id" Text
                 , ( Field "emp_id" UUID
                   , ()
                   )
                 )
          statement = [ssql| select users.id, employee_id as emp_id from users |]
          squealRendering :: Text
          squealRendering =
            "SELECT \"users\".\"id\" AS \"id\", \"employee_id\" AS \"emp_id\" FROM \"users\" AS \"users\""
        checkStatement squealRendering statement

      it "select users.id as user_id, employee_id from users" $ do
        let
          statement
            :: Statement
                 DB
                 ()
                 ( Field "user_id" Text
                 , ( Field "employee_id" UUID
                   , ()
                   )
                 )
          statement = [ssql| select users.id as user_id, employee_id from users |]
          squealRendering :: Text
          squealRendering =
            "SELECT \"users\".\"id\" AS \"user_id\", \"employee_id\" AS \"employee_id\" FROM \"users\" AS \"users\""
        checkStatement squealRendering statement

      it
        "select users.id from users left outer join emails on emails.user_id = users.id"
        $ do
          let
            statement
              :: Statement
                   DB
                   ()
                   ( Field "id" Text
                   , ()
                   )
            statement =
              [ssql|
                select users.id
                from users
                left outer join emails
                on emails.user_id = users.id
              |]
            squealRendering :: Text
            squealRendering =
              "SELECT \"users\".\"id\" AS \"id\" FROM \"users\" AS \"users\" LEFT OUTER JOIN \"emails\" AS \"emails\" ON (\"emails\".\"user_id\" = \"users\".\"id\")"
          checkStatement squealRendering statement

      it
        "select users.id, users.name, emails.email from users left outer join emails on emails.user_id = users.id where emails.email = inline(\"targetEmail\")"
        $ do
          let
            targetEmail :: Text
            targetEmail = "foo@bar.com"

            statement
              :: Statement
                   DB
                   ()
                   ( Field "id" Text
                   , ( Field "name" Text
                     , ( Field "email" (Maybe Text)
                       , ()
                       )
                     )
                   )
            statement =
              [ssql|
                select users.id, users.name, emails.email
                from users
                left outer join emails
                on emails.user_id = users.id
                where emails.email = inline("targetEmail")
              |]

            squealRendering :: Text
            squealRendering =
              "SELECT \"users\".\"id\" AS \"id\", \"users\".\"name\" AS \"name\", \"emails\".\"email\" AS \"email\" FROM \"users\" AS \"users\" LEFT OUTER JOIN \"emails\" AS \"emails\" ON (\"emails\".\"user_id\" = \"users\".\"id\") WHERE (\"emails\".\"email\" = (E'foo@bar.com' :: text))"

          checkStatement squealRendering statement

      it "select 'text_val'" $ do
        let
          statement
            :: Statement
                 DB
                 ()
                 ( Field "_col1" Text
                 , ()
                 )
          statement = [ssql| select 'text_val' |]
          squealRendering :: Text
          squealRendering = "SELECT * FROM (VALUES ((E'text_val' :: text))) AS t (\"_col1\")"
        checkStatement squealRendering statement

      it "select 1" $ do
        let
          statement
            :: Statement
                 DB
                 ()
                 ( Field "_col1" Int64
                 , ()
                 )
          statement = [ssql| select 1 |]
          squealRendering :: Text
          squealRendering = "SELECT * FROM (VALUES (1)) AS t (\"_col1\")"
        checkStatement squealRendering statement

      it "select 1 AS num, 'text_val' AS txt" $ do
        let
          statement
            :: Statement
                 DB
                 ()
                 ( Field "num" Int64
                 , ( Field "txt" Text
                   , ()
                   )
                 )
          statement = [ssql| select 1 AS num, 'text_val' AS txt |]
          squealRendering :: Text
          squealRendering = "SELECT * FROM (VALUES (1, (E'text_val' :: text))) AS t (\"num\", \"txt\")"
        checkStatement squealRendering statement

      describe "group by" $ do
        it "select name from users group by name" $ do
          let
            statement :: Statement DB () (Field "name" Text, ())
            statement = [ssql| select name from users group by name |]
            squealRendering :: Text
            squealRendering = "SELECT \"name\" AS \"name\" FROM \"users\" AS \"users\" GROUP BY \"name\""
          checkStatement squealRendering statement

        it "select employee_id, count(id) from users group by employee_id" $ do
          let
            statement
              :: Statement
                   DB
                   ()
                   ( Field "employee_id" UUID
                   , ( Field "_col2" Int64 -- Assuming count returns Int64
                     , ()
                     )
                   )
            statement = [ssql| select employee_id, count(id) from users group by employee_id |]
            squealRendering :: Text
            squealRendering =
              "SELECT \"employee_id\" AS \"employee_id\", count(ALL \"id\") AS \"_col2\" FROM \"users\" AS \"users\" GROUP BY \"employee_id\""
          checkStatement squealRendering statement

        it "select employee_id, name, count(id) from users group by employee_id, name" $ do
          let
            statement
              :: Statement
                   DB
                   ()
                   ( Field "employee_id" UUID
                   , ( Field "name" Text
                     , ( Field "_col3" Int64
                       , ()
                       )
                     )
                   )
            statement =
              [ssql| select employee_id, name, count(id) from users group by employee_id, name |]
            squealRendering :: Text
            squealRendering =
              "SELECT \"employee_id\" AS \"employee_id\", \"name\" AS \"name\", count(ALL \"id\") AS \"_col3\" FROM \"users\" AS \"users\" GROUP BY \"employee_id\", \"name\""
          checkStatement squealRendering statement

      describe "common table expressions" $ do
        it "with users_cte as (select * from users) select * from users_cte" $ do
          let
            statement
              :: Statement
                   DB
                   ()
                   ( Field "id" Text
                   , ( Field "name" Text
                     , ( Field "employee_id" UUID
                       , ( Field "bio" (Maybe Text)
                         , ()
                         )
                       )
                     )
                   )
            statement =
              [ssql|
                with users_cte as (select * from users)
                select * from users_cte
              |]
            squealRendering :: Text
            squealRendering =
              "WITH \"users_cte\" AS (SELECT * FROM \"users\" AS \"users\") SELECT * FROM \"users_cte\" AS \"users_cte\""
          checkStatement squealRendering statement

        it
          "with recursive t as ( select 1 as n union all select (n + 1) as n from t where n < 100) select n from t"
          $ do
            let
              statement
                :: Statement
                     DB
                     ()
                     (Field "n" Int64, ())
              statement =
                [ssql|
                with recursive t as (
                  select 1 as n
                  union all
                  select (n + 1) as n from t where n < 100
                )
                select n from t
              |]
              squealRendering :: Text
              squealRendering =
                "WITH RECURSIVE \"t\" AS ((SELECT * FROM (VALUES (1)) AS t (\"n\")) UNION ALL (SELECT (\"n\" + 1) AS \"n\" FROM \"t\" AS \"t\" WHERE (\"n\" < 100))) SELECT \"n\" AS \"n\" FROM \"t\" AS \"t\""
            checkStatement squealRendering statement

        it
          "with recursive users_cte as ( select id, name from users union all select id, name from users_cte) select * from users_cte"
          $ do
            let
              statement
                :: Statement
                     DB
                     ()
                     (Field "id" Text, (Field "name" Text, ()))
              statement =
                [ssql|
                with recursive users_cte as (
                  select id, name from users
                  union all
                  select id, name from users_cte
                )
                select * from users_cte
              |]
              squealRendering :: Text
              squealRendering =
                "WITH RECURSIVE \"users_cte\" AS ((SELECT \"id\" AS \"id\", \"name\" AS \"name\" FROM \"users\" AS \"users\") UNION ALL (SELECT \"id\" AS \"id\", \"name\" AS \"name\" FROM \"users_cte\" AS \"users_cte\")) SELECT * FROM \"users_cte\" AS \"users_cte\""
            checkStatement squealRendering statement

        it
          "with users_cte as (select * from users), emails_cte as (select * from emails) select users_cte.*, emails_cte.email from users_cte join emails_cte on users_cte.id = emails_cte.user_id"
          $ do
            let
              statement
                :: Statement
                     DB
                     ()
                     ( Field "id" Text
                     , ( Field "name" Text
                       , ( Field "employee_id" UUID
                         , ( Field "bio" (Maybe Text)
                           , ( Field "email" (Maybe Text)
                             , ()
                             )
                           )
                         )
                       )
                     )
              statement =
                [ssql|
                  with
                    users_cte as (select * from users),
                    emails_cte as (select * from emails)
                  select users_cte.*, emails_cte.email
                  from users_cte
                  join emails_cte on users_cte.id = emails_cte.user_id
                |]
              squealRendering :: Text
              squealRendering =
                "WITH \"users_cte\" AS (SELECT * FROM \"users\" AS \"users\"), \"emails_cte\" AS (SELECT * FROM \"emails\" AS \"emails\") SELECT \"users_cte\".*, \"emails_cte\".\"email\" AS \"email\" FROM \"users_cte\" AS \"users_cte\" INNER JOIN \"emails_cte\" AS \"emails_cte\" ON (\"users_cte\".\"id\" = \"emails_cte\".\"user_id\")"
            checkStatement squealRendering statement

      describe "set operations" $ do
        it "select name from users union select name from users_copy" $ do
          let
            statement :: Statement DB () (Field "name" Text, ())
            statement = [ssql| select name from users union select name from users_copy |]
            squealRendering :: Text
            squealRendering =
              "(SELECT \"name\" AS \"name\" FROM \"users\" AS \"users\") UNION (SELECT \"name\" AS \"name\" FROM \"users_copy\" AS \"users_copy\")"
          checkStatement squealRendering statement

        it "select name from users union all select name from users_copy" $ do
          let
            statement :: Statement DB () (Field "name" Text, ())
            statement = [ssql| select name from users union all select name from users_copy |]
            squealRendering :: Text
            squealRendering =
              "(SELECT \"name\" AS \"name\" FROM \"users\" AS \"users\") UNION ALL (SELECT \"name\" AS \"name\" FROM \"users_copy\" AS \"users_copy\")"
          checkStatement squealRendering statement

        it "select name from users intersect select name from users_copy" $ do
          let
            statement :: Statement DB () (Field "name" Text, ())
            statement = [ssql| select name from users intersect select name from users_copy |]
            squealRendering :: Text
            squealRendering =
              "(SELECT \"name\" AS \"name\" FROM \"users\" AS \"users\") INTERSECT (SELECT \"name\" AS \"name\" FROM \"users_copy\" AS \"users_copy\")"
          checkStatement squealRendering statement

        it "select name from users intersect all select name from users_copy" $ do
          let
            statement :: Statement DB () (Field "name" Text, ())
            statement = [ssql| select name from users intersect all select name from users_copy |]
            squealRendering :: Text
            squealRendering =
              "(SELECT \"name\" AS \"name\" FROM \"users\" AS \"users\") INTERSECT ALL (SELECT \"name\" AS \"name\" FROM \"users_copy\" AS \"users_copy\")"
          checkStatement squealRendering statement

        it "select name from users except select name from users_copy" $ do
          let
            statement :: Statement DB () (Field "name" Text, ())
            statement = [ssql| select name from users except select name from users_copy |]
            squealRendering :: Text
            squealRendering =
              "(SELECT \"name\" AS \"name\" FROM \"users\" AS \"users\") EXCEPT (SELECT \"name\" AS \"name\" FROM \"users_copy\" AS \"users_copy\")"
          checkStatement squealRendering statement

        it "select name from users except all select name from users_copy" $ do
          let
            statement :: Statement DB () (Field "name" Text, ())
            statement = [ssql| select name from users except all select name from users_copy |]
            squealRendering :: Text
            squealRendering =
              "(SELECT \"name\" AS \"name\" FROM \"users\" AS \"users\") EXCEPT ALL (SELECT \"name\" AS \"name\" FROM \"users_copy\" AS \"users_copy\")"
          checkStatement squealRendering statement

        it "(select name from users) union (select name from users_copy)" $ do
          let
            statement :: Statement DB () (Field "name" Text, ())
            statement = [ssql| (select name from users) union (select name from users_copy) |]
            squealRendering :: Text
            squealRendering =
              "(SELECT \"name\" AS \"name\" FROM \"users\" AS \"users\") UNION (SELECT \"name\" AS \"name\" FROM \"users_copy\" AS \"users_copy\")"
          checkStatement squealRendering statement

    describe "inserts" $ do
      it "insert into emails (id, user_id, email) values (1, 'user-1', 'foo@bar')" $ do
        let
          statement
            :: Statement DB () ()
          statement =
            [ssql|
              insert into
                emails (id, user_id, email)
                values (1, 'user-1', 'foo@bar')
            |]
          squealRendering :: Text
          squealRendering =
            "INSERT INTO \"emails\" AS \"emails\" (\"id\", \"user_id\", \"email\") VALUES (1, (E'user-1' :: text), (E'foo@bar' :: text))"
        checkStatement squealRendering statement

      it "insert into emails (id, user_id, email) values (1, 'user-1', $1)" $ do
        let
          statement
            :: Statement
                 DB
                 (Only (Maybe Text))
                 ()
          statement =
            [ssql|
              insert into
                emails (id, user_id, email)
                values (1, 'user-1', $1)
            |]
          squealRendering :: Text
          squealRendering =
            "INSERT INTO \"emails\" AS \"emails\" (\"id\", \"user_id\", \"email\") VALUES (1, (E'user-1' :: text), ($1 :: text))"
        checkStatement squealRendering statement

      it "insert into emails (id, user_id, email) values (1, $2, $1)" $ do
        let
          statement
            :: Statement
                 DB
                 (Maybe Text, Text)
                 ()
          statement =
            {-
              Note the parameters are backwards (i.e. $2 comes before $1),
              to test that you can do this kind of thing out of order.
            -}
            [ssql|
              insert into
                emails (id, user_id, email)
                values (1, $2, $1)
            |]
          squealRendering :: Text
          squealRendering =
            "INSERT INTO \"emails\" AS \"emails\" (\"id\", \"user_id\", \"email\") VALUES (1, ($2 :: text), ($1 :: text))"
        checkStatement squealRendering statement

      it "insert into users_copy (id, name, bio) values ($1, $2, $3)" $ do
        let
          statement
            :: Statement
                 DB
                 (Text, Text, Maybe Text)
                 ()
          statement =
            [ssql|
              insert into
                users_copy (id, name, bio)
                values ($1, $2, $3)
            |]
          squealRendering :: Text
          squealRendering =
            "INSERT INTO \"users_copy\" AS \"users_copy\" (\"id\", \"name\", \"bio\") VALUES (($1 :: text), ($2 :: text), ($3 :: text))"
        checkStatement squealRendering statement

      it
        "insert into emails (id, user_id, email) values (inline(i), inline(uid), inline_param(e))"
        $ do
          let
            mkStatement :: Int32 -> Text -> Maybe Text -> Statement DB () ()
            mkStatement i uid e =
              [ssql|
                insert into
                  emails (id, user_id, email)
                  values (inline(i), inline(uid), inline_param(e))
              |]

          checkStatement
            "INSERT INTO \"emails\" AS \"emails\" (\"id\", \"user_id\", \"email\") VALUES ((1 :: int4), (E'user-1' :: text), NULL)"
            (mkStatement 1 "user-1" Nothing)
          checkStatement
            "INSERT INTO \"emails\" AS \"emails\" (\"id\", \"user_id\", \"email\") VALUES ((1 :: int4), (E'user-1' :: text), (E'foo@bar.com' :: text))"
            (mkStatement 1 "user-1" (Just "foo@bar.com"))

      describe "default keyword" $ do
        it "insert into emails (id, user_id, email) values (default, 'foo', 'bar')" $ do
          let
            statement
              :: Statement
                   DB
                   (Maybe Text, Text)
                   ()
            statement =
              [ssql|
                insert into
                  emails (id, user_id, email)
                  values (default, 'foo', 'bar')
              |]
            squealRendering :: Text
            squealRendering =
              "INSERT INTO \"emails\" AS \"emails\" (\"id\", \"user_id\", \"email\") VALUES (DEFAULT, (E'foo' :: text), (E'bar' :: text))"
          checkStatement squealRendering statement

        it "insert into emails (id, user_id, email) values (deFault, 'foo', 'bar')" $ do
          let
            statement
              :: Statement
                   DB
                   (Maybe Text, Text)
                   ()
            statement =
              [ssql|
                insert into
                  emails (id, user_id, email)
                  values (deFault, 'foo', 'bar')
              |]
            squealRendering :: Text
            squealRendering =
              "INSERT INTO \"emails\" AS \"emails\" (\"id\", \"user_id\", \"email\") VALUES (DEFAULT, (E'foo' :: text), (E'bar' :: text))"
          checkStatement squealRendering statement

        it "insert into emails (id, user_id, email) values (DEFAULT, 'foo', 'bar')" $ do
          let
            statement
              :: Statement
                   DB
                   (Maybe Text, Text)
                   ()
            statement =
              [ssql|
                insert into
                  emails (id, user_id, email)
                  values (DEFAULT, 'foo', 'bar')
              |]
            squealRendering :: Text
            squealRendering =
              "INSERT INTO \"emails\" AS \"emails\" (\"id\", \"user_id\", \"email\") VALUES (DEFAULT, (E'foo' :: text), (E'bar' :: text))"
          checkStatement squealRendering statement

      describe "null keyword" $ do
        it "insert into emails (id, user_id, email) values (DEFAULT, 'foo', null)" $ do
          let
            statement
              :: Statement
                   DB
                   (Maybe Text, Text)
                   ()
            statement =
              [ssql|
                insert into
                  emails (id, user_id, email)
                  values (DEFAULT, 'foo', null)
              |]
            squealRendering :: Text
            squealRendering =
              "INSERT INTO \"emails\" AS \"emails\" (\"id\", \"user_id\", \"email\") VALUES (DEFAULT, (E'foo' :: text), NULL)"
          checkStatement squealRendering statement

        it "insert into emails (id, user_id, email) values (DEFAULT, 'foo', NULL)" $ do
          let
            statement
              :: Statement
                   DB
                   (Maybe Text, Text)
                   ()
            statement =
              [ssql|
                insert into
                  emails (id, user_id, email)
                  values (DEFAULT, 'foo', NULL)
              |]
            squealRendering :: Text
            squealRendering =
              "INSERT INTO \"emails\" AS \"emails\" (\"id\", \"user_id\", \"email\") VALUES (DEFAULT, (E'foo' :: text), NULL)"
          checkStatement squealRendering statement

        it "insert into emails (id, user_id, email) values (DEFAULT, 'foo', NuLL)" $ do
          let
            statement
              :: Statement
                   DB
                   (Maybe Text, Text)
                   ()
            statement =
              [ssql|
                insert into
                  emails (id, user_id, email)
                  values (DEFAULT, 'foo', NuLL)
              |]
            squealRendering :: Text
            squealRendering =
              "INSERT INTO \"emails\" AS \"emails\" (\"id\", \"user_id\", \"email\") VALUES (DEFAULT, (E'foo' :: text), NULL)"
          checkStatement squealRendering statement

      describe "insert ... select ..." $ do
        it "insert into emails select id, user_id, email from emails where id = 1" $ do
          let
            statement :: Statement DB () ()
            statement =
              [ssql|
                insert into emails
                select id, user_id, email from emails where id = 1
              |]
            squealRendering :: Text
            squealRendering =
              "INSERT INTO \"emails\" AS \"emails\" SELECT \"id\" AS \"id\", \"user_id\" AS \"user_id\", \"email\" AS \"email\" FROM \"emails\" AS \"emails\" WHERE (\"id\" = 1)"
          checkStatement squealRendering statement

        it "insert into emails select id, user_id, email from emails where id = $1" $ do
          let
            statement :: Statement DB (Only Int32) ()
            statement =
              [ssql|
                insert into emails
                select id, user_id, email from emails where id = $1
              |]
            squealRendering :: Text
            squealRendering =
              "INSERT INTO \"emails\" AS \"emails\" SELECT \"id\" AS \"id\", \"user_id\" AS \"user_id\", \"email\" AS \"email\" FROM \"emails\" AS \"emails\" WHERE (\"id\" = ($1 :: int4))"
          checkStatement squealRendering statement

        it
          "insert into users_copy select id, name, bio from users where users.id = 'uid1'"
          $ do
            let
              statement :: Statement DB () ()
              statement =
                [ssql|
                  insert into users_copy
                  select id, name, bio from users where users.id = 'uid1'
                |]
              squealRendering :: Text
              squealRendering =
                "INSERT INTO \"users_copy\" AS \"users_copy\" SELECT \"id\" AS \"id\", \"name\" AS \"name\", \"bio\" AS \"bio\" FROM \"users\" AS \"users\" WHERE (\"users\".\"id\" = (E'uid1' :: text))"
            checkStatement squealRendering statement

      describe "returning clause" $ do
        it
          "insert into emails (id, user_id, email) values (1, 'user-1', 'foo@bar') returning id"
          $ do
            let
              statement
                :: Statement DB () (Field "id" Int32, ())
              statement =
                [ssql|
                insert into
                  emails (id, user_id, email)
                  values (1, 'user-1', 'foo@bar')
                returning id
              |]
              squealRendering :: Text
              squealRendering =
                "INSERT INTO \"emails\" AS \"emails\" (\"id\", \"user_id\", \"email\") VALUES (1, (E'user-1' :: text), (E'foo@bar' :: text)) RETURNING \"id\" AS \"id\""
            checkStatement squealRendering statement

        it
          "insert into emails (id, user_id, email) values (1, 'user-1', 'foo@bar') returning *"
          $ do
            let
              statement
                :: Statement
                     DB
                     ()
                     ( Field "id" Int32
                     , ( Field "user_id" Text
                       , ( Field "email" (Maybe Text)
                         , ()
                         )
                       )
                     )
              statement =
                [ssql|
                  insert into
                  emails (id, user_id, email)
                  values (1, 'user-1', 'foo@bar')
                returning *
              |]
              squealRendering :: Text
              squealRendering =
                "INSERT INTO \"emails\" AS \"emails\" (\"id\", \"user_id\", \"email\") VALUES (1, (E'user-1' :: text), (E'foo@bar' :: text)) RETURNING *"
            checkStatement squealRendering statement

      describe "with common table expressions" $ do
        it
          "with new_user (id, name, bio) as (values ('id_new', 'new_name', 'new_bio')) insert into users_copy select * from new_user"
          $ do
            let
              statement :: Statement DB () ()
              statement =
                [ssql|
                  with new_user (id, name, bio) as (values ('id_new', 'new_name', 'new_bio'))
                  insert into users_copy
                  select * from new_user
                |]
              squealRendering :: Text
              squealRendering =
                "WITH \"new_user\" AS (SELECT * FROM (VALUES ((E'id_new' :: text), (E'new_name' :: text), (E'new_bio' :: text))) AS t (\"id\", \"name\", \"bio\")) INSERT INTO \"users_copy\" AS \"users_copy\" SELECT * FROM \"new_user\" AS \"new_user\""
            checkStatement squealRendering statement

      describe "on conflict" $ do
        it
          "insert into users_copy (id, name, bio) values ('id1', 'name1', null) on conflict on constraint pk_users_copy do nothing"
          $ do
            let
              statement :: Statement DB () ()
              statement =
                [ssql|
                insert into users_copy (id, name, bio)
                values ('id1', 'name1', null)
                on conflict on constraint pk_users_copy do nothing
              |]
              squealRendering :: Text
              squealRendering =
                "INSERT INTO \"users_copy\" AS \"users_copy\" (\"id\", \"name\", \"bio\") VALUES ((E'id1' :: text), (E'name1' :: text), NULL) ON CONFLICT ON CONSTRAINT \"pk_users_copy\" DO NOTHING"
            checkStatement squealRendering statement

        it
          "insert into users_copy (id, name, bio) values ('id1', 'name1', 'bio1') on conflict on constraint pk_users_copy do update set name = 'new_name'"
          $ do
            let
              statement :: Statement DB () ()
              statement =
                [ssql|
                insert into users_copy (id, name, bio)
                values ('id1', 'name1', 'bio1')
                on conflict on constraint pk_users_copy
                do update set name = 'new_name'
              |]
              squealRendering :: Text
              squealRendering =
                "INSERT INTO \"users_copy\" AS \"users_copy\" (\"id\", \"name\", \"bio\") VALUES ((E'id1' :: text), (E'name1' :: text), (E'bio1' :: text)) ON CONFLICT ON CONSTRAINT \"pk_users_copy\" DO UPDATE SET \"name\" = (E'new_name' :: text)"
            checkStatement squealRendering statement

        it
          "insert into users_copy (id, name, bio) values ('id1', 'name1', null) on conflict on constraint pk_users_copy do update set name = 'new_name' where users_copy.name = 'old_name'"
          $ do
            let
              statement :: Statement DB () ()
              statement =
                [ssql|
                insert into users_copy (id, name, bio)
                values ('id1', 'name1', null)
                on conflict on constraint pk_users_copy
                do update set name = 'new_name'
                where users_copy.name = 'old_name'
              |]
              squealRendering :: Text
              squealRendering =
                "INSERT INTO \"users_copy\" AS \"users_copy\" (\"id\", \"name\", \"bio\") VALUES ((E'id1' :: text), (E'name1' :: text), NULL) ON CONFLICT ON CONSTRAINT \"pk_users_copy\" DO UPDATE SET \"name\" = (E'new_name' :: text) WHERE (\"users_copy\".\"name\" = (E'old_name' :: text))"
            checkStatement squealRendering statement

        it
          "insert into users_copy (id, name, bio) values ('id1', 'name1', null) on conflict on constraint pk_users_copy do nothing returning id"
          $ do
            let
              statement :: Statement DB () (Field "id" Text, ())
              statement =
                [ssql|
                insert into users_copy (id, name, bio)
                values ('id1', 'name1', null)
                on conflict on constraint pk_users_copy do nothing
                returning id
              |]
              squealRendering :: Text
              squealRendering =
                "INSERT INTO \"users_copy\" AS \"users_copy\" (\"id\", \"name\", \"bio\") VALUES ((E'id1' :: text), (E'name1' :: text), NULL) ON CONFLICT ON CONSTRAINT \"pk_users_copy\" DO NOTHING RETURNING \"id\" AS \"id\""
            checkStatement squealRendering statement

        it
          "insert into users_copy (id, name, bio) values ('id1', 'name1', 'bio1') on conflict on constraint pk_users_copy do update set name = 'new_name' returning *"
          $ do
            let
              statement
                :: Statement
                     DB
                     ()
                     ( Field "id" Text
                     , ( Field "name" Text
                       , ( Field "bio" (Maybe Text)
                         , ()
                         )
                       )
                     )
              statement =
                [ssql|
                insert into users_copy (id, name, bio)
                values ('id1', 'name1', 'bio1')
                on conflict on constraint pk_users_copy
                do update set name = 'new_name'
                returning *
              |]
              squealRendering :: Text
              squealRendering =
                "INSERT INTO \"users_copy\" AS \"users_copy\" (\"id\", \"name\", \"bio\") VALUES ((E'id1' :: text), (E'name1' :: text), (E'bio1' :: text)) ON CONFLICT ON CONSTRAINT \"pk_users_copy\" DO UPDATE SET \"name\" = (E'new_name' :: text) RETURNING *"
            checkStatement squealRendering statement

    describe "deletes" $ do
      it "delete from users where true" $ do
        let
          statement :: Statement DB () ()
          statement = [ssql| delete from users where true |]
          squealRendering :: Text
          squealRendering = "DELETE FROM \"users\" AS \"users\" WHERE TRUE"

        checkStatement squealRendering statement

      it "delete from emails where id = 1" $ do
        let
          statement :: Statement DB () ()
          statement = [ssql| delete from emails where id = 1 |]
          squealRendering :: Text
          squealRendering =
            "DELETE FROM \"emails\" AS \"emails\" WHERE (\"id\" = 1)"
        checkStatement squealRendering statement

      it "delete from emails where id = $1" $ do
        let
          statement :: Statement DB (Only Int32) ()
          statement = [ssql| delete from emails where id = $1 |]
          squealRendering :: Text
          squealRendering = "DELETE FROM \"emails\" AS \"emails\" WHERE (\"id\" = ($1 :: int4))"
        checkStatement squealRendering statement

      it "delete from emails where email = inline(e)" $ do
        let
          statement :: Statement DB () ()
          statement = [ssql| delete from emails where email = inline(e) |]
          e :: Text
          e = "foo"
          squealRendering :: Text
          squealRendering =
            "DELETE FROM \"emails\" AS \"emails\" WHERE (\"email\" = (E'foo' :: text))"
        checkStatement squealRendering statement

      it "delete from users where id = 'some-id' returning id" $ do
        let
          statement :: Statement DB () (Field "id" Text, ())
          statement = [ssql| delete from users where id = 'some-id' returning id |]
          squealRendering :: Text
          squealRendering =
            "DELETE FROM \"users\" AS \"users\" WHERE (\"id\" = (E'some-id' :: text)) RETURNING \"id\" AS \"id\""
        checkStatement squealRendering statement

      describe "with common table expressions" $ do
        it
          "with to_delete as (select id from users where name = 'Alice') delete from users where id in (select to_delete.id from to_delete)"
          $ do
            let
              statement :: Statement DB () ()
              statement =
                [ssql|
                  with to_delete as (select id from users where name = 'Alice')
                  delete from users
                  where id in (select to_delete.id from to_delete)
                |]
              squealRendering :: Text
              squealRendering =
                "WITH \"to_delete\" AS (SELECT \"id\" AS \"id\" FROM \"users\" AS \"users\" WHERE (\"name\" = (E'Alice' :: text))) DELETE FROM \"users\" AS \"users\" WHERE (\"id\" = ANY (SELECT \"to_delete\".\"id\" AS \"id\" FROM \"to_delete\" AS \"to_delete\"))"
            checkStatement squealRendering statement

        it
          "with to_delete as (select id from users where name = 'Alice') delete from users using to_delete where users.id = to_delete.id"
          $ do
            let
              statement :: Statement DB () ()
              statement =
                [ssql|
                  with to_delete as (select id from users where name = 'Alice')
                  delete from users
                  using to_delete
                  where users.id = to_delete.id
                |]
              squealRendering :: Text
              squealRendering =
                "WITH \"to_delete\" AS (SELECT \"id\" AS \"id\" FROM \"users\" AS \"users\" WHERE (\"name\" = (E'Alice' :: text))) DELETE FROM \"users\" AS \"users\" USING \"to_delete\" AS \"to_delete\" WHERE (\"users\".\"id\" = \"to_delete\".\"id\")"
            checkStatement squealRendering statement

    describe "updates" $ do
      it "update users set name = 'new name' where id = 'some-id'" $ do
        let
          statement :: Statement DB () ()
          statement = [ssql| update users set name = 'new name' where id = 'some-id' |]
          squealRendering :: Text
          squealRendering =
            "UPDATE \"users\" AS \"users\" SET \"name\" = (E'new name' :: text) WHERE (\"id\" = (E'some-id' :: text))"
        checkStatement squealRendering statement

      it "update users set name = 'new name', bio = 'new bio' where id = 'some-id'" $ do
        let
          statement :: Statement DB () ()
          statement =
            [ssql| update users set name = 'new name', bio = 'new bio' where id = 'some-id' |]
          squealRendering :: Text
          squealRendering =
            "UPDATE \"users\" AS \"users\" SET \"name\" = (E'new name' :: text), \"bio\" = (E'new bio' :: text) WHERE (\"id\" = (E'some-id' :: text))"
        checkStatement squealRendering statement

      it "update users set name = inline(n) where id = 'some-id'" $ do
        let
          n :: Text
          n = "new name"
          statement :: Statement DB () ()
          statement = [ssql| update users set name = inline(n) where id = 'some-id' |]
          squealRendering :: Text
          squealRendering =
            "UPDATE \"users\" AS \"users\" SET \"name\" = (E'new name' :: text) WHERE (\"id\" = (E'some-id' :: text))"
        checkStatement squealRendering statement

      it "update users set name = $1 where id = $2" $ do
        let
          statement :: Statement DB (Text, Text) ()
          statement = [ssql| update users set name = $1 where id = $2 |]
          squealRendering :: Text
          squealRendering =
            "UPDATE \"users\" AS \"users\" SET \"name\" = ($1 :: text) WHERE (\"id\" = ($2 :: text))"
        checkStatement squealRendering statement

      it "update users set name = 'new name' where id = 'some-id' returning id" $ do
        let
          statement :: Statement DB () (Field "id" Text, ())
          statement =
            [ssql| update users set name = 'new name' where id = 'some-id' returning id |]
          squealRendering :: Text
          squealRendering =
            "UPDATE \"users\" AS \"users\" SET \"name\" = (E'new name' :: text) WHERE (\"id\" = (E'some-id' :: text)) RETURNING \"id\" AS \"id\""
        checkStatement squealRendering statement

      describe "with common table expressions" $ do
        it
          "with to_update as (select id from users where name = 'Alice') update users set name = 'Alicia' from to_update where users.id = to_update.id"
          $ do
            let
              statement :: Statement DB () ()
              statement =
                [ssql|
                  with to_update as (select id from users where name = 'Alice')
                  update users
                  set name = 'Alicia'
                  from to_update
                  where users.id = to_update.id
                |]
              squealRendering :: Text
              squealRendering =
                "WITH \"to_update\" AS (SELECT \"id\" AS \"id\" FROM \"users\" AS \"users\" WHERE (\"name\" = (E'Alice' :: text))) UPDATE \"users\" AS \"users\" SET \"name\" = (E'Alicia' :: text) FROM \"to_update\" AS \"to_update\" WHERE (\"users\".\"id\" = \"to_update\".\"id\")"
            checkStatement squealRendering statement

    describe "scalar expressions" $ do
      -- Binary Operators
      it "select users.id != 'no-such-user' as neq from users" $ do
        let
          stmt :: Statement DB () (Field "neq" (Maybe Bool), ())
          stmt = [ssql| select users.id != 'no-such-user' as neq from users |]
          squealRendering :: Text
          squealRendering =
            "SELECT (\"users\".\"id\" <> (E'no-such-user' :: text)) AS "
              <> "\"neq\" FROM \"users\" AS \"users\""
        checkStatement squealRendering stmt

      it "select * from users where users.id <> 'no-such-user'" $ do
        let
          stmt
            :: Statement
                 DB
                 ()
                 ( Field "id" Text
                 , ( Field "name" Text
                   , ( Field "employee_id" UUID
                     , ( Field "bio" (Maybe Text)
                       , ()
                       )
                     )
                   )
                 )
          stmt = [ssql| select * from users where users.id <> 'no-such-user' |]
          squealRendering :: Text
          squealRendering =
            "SELECT * FROM \"users\" AS \"users\" WHERE (\"users\".\"id\" <> (E'no-such-user' :: text))"
        checkStatement squealRendering stmt

      it "select * from emails where emails.id > 0" $ do
        let
          stmt
            :: Statement
                 DB
                 ()
                 (Field "id" Int32, (Field "user_id" Text, (Field "email" (Maybe Text), ())))
          stmt = [ssql| select * from emails where emails.id > 0 |]
          squealRendering :: Text
          squealRendering = "SELECT * FROM \"emails\" AS \"emails\" WHERE (\"emails\".\"id\" > 0)"
        checkStatement squealRendering stmt

      it "select * from emails where emails.id >= 0" $ do
        let
          stmt
            :: Statement
                 DB
                 ()
                 (Field "id" Int32, (Field "user_id" Text, (Field "email" (Maybe Text), ())))
          stmt = [ssql| select * from emails where emails.id >= 0 |]
          squealRendering :: Text
          squealRendering = "SELECT * FROM \"emails\" AS \"emails\" WHERE (\"emails\".\"id\" >= 0)"
        checkStatement squealRendering stmt

      it "select * from emails where emails.id < 10" $ do
        let
          stmt
            :: Statement
                 DB
                 ()
                 (Field "id" Int32, (Field "user_id" Text, (Field "email" (Maybe Text), ())))
          stmt = [ssql| select * from emails where emails.id < 10 |]
          squealRendering :: Text
          squealRendering = "SELECT * FROM \"emails\" AS \"emails\" WHERE (\"emails\".\"id\" < 10)"
        checkStatement squealRendering stmt

      it "select * from emails where emails.id <= 10" $ do
        let
          stmt
            :: Statement
                 DB
                 ()
                 (Field "id" Int32, (Field "user_id" Text, (Field "email" (Maybe Text), ())))
          stmt = [ssql| select * from emails where emails.id <= 10 |]
          squealRendering :: Text
          squealRendering = "SELECT * FROM \"emails\" AS \"emails\" WHERE (\"emails\".\"id\" <= 10)"
        checkStatement squealRendering stmt

      it "select emails.id + 1 as plus_one from emails" $ do
        let
          stmt :: Statement DB () (Field "plus_one" Int32, ())
          stmt = [ssql| select emails.id + 1 as plus_one from emails |]
          squealRendering :: Text
          squealRendering =
            "SELECT (\"emails\".\"id\" + 1) AS \"plus_one\" FROM \"emails\" AS \"emails\""
        checkStatement squealRendering stmt

      it "select emails.id - 1 as minus_one from emails" $ do
        let
          stmt :: Statement DB () (Field "minus_one" Int32, ())
          stmt = [ssql| select emails.id - 1 as minus_one from emails |]
          squealRendering :: Text
          squealRendering =
            "SELECT (\"emails\".\"id\" - 1) AS \"minus_one\" FROM \"emails\" AS \"emails\""
        checkStatement squealRendering stmt

      it "select emails.id * 2 as times_two from emails" $ do
        let
          stmt :: Statement DB () (Field "times_two" Int32, ())
          stmt = [ssql| select emails.id * 2 as times_two from emails |]
          squealRendering :: Text
          squealRendering =
            "SELECT (\"emails\".\"id\" * 2) AS \"times_two\" FROM \"emails\" AS \"emails\""
        checkStatement squealRendering stmt

      it "select * from users where users.id = 'a' and users.name = 'b'" $ do
        let
          stmt
            :: Statement
                 DB
                 ()
                 ( Field "id" Text
                 , (Field "name" Text, (Field "employee_id" UUID, (Field "bio" (Maybe Text), ())))
                 )
          stmt = [ssql| select * from users where users.id = 'a' and users.name = 'b' |]
          squealRendering :: Text
          squealRendering =
            "SELECT * FROM \"users\" AS \"users\" WHERE ((\"users\".\"id\" = (E'a' :: text)) AND (\"users\".\"name\" = (E'b' :: text)))"
        checkStatement squealRendering stmt

      it "select * from users where users.id = 'a' or users.name = 'b'" $ do
        let
          stmt
            :: Statement
                 DB
                 ()
                 ( Field "id" Text
                 , (Field "name" Text, (Field "employee_id" UUID, (Field "bio" (Maybe Text), ())))
                 )
          stmt = [ssql| select * from users where users.id = 'a' or users.name = 'b' |]
          squealRendering :: Text
          squealRendering =
            "SELECT * FROM \"users\" AS \"users\" WHERE ((\"users\".\"id\" = (E'a' :: text)) OR (\"users\".\"name\" = (E'b' :: text)))"
        checkStatement squealRendering stmt

      it "select * from users where users.name like 'A%'" $ do
        let
          stmt
            :: Statement
                 DB
                 ()
                 ( Field "id" Text
                 , (Field "name" Text, (Field "employee_id" UUID, (Field "bio" (Maybe Text), ())))
                 )
          stmt = [ssql| select * from users where users.name like 'A%' |]
          squealRendering :: Text
          squealRendering =
            "SELECT * FROM \"users\" AS \"users\" WHERE (\"users\".\"name\" LIKE (E'A%' :: text))"
        checkStatement squealRendering stmt

      it "select * from users where users.name ilike 'a%'" $ do
        let
          stmt
            :: Statement
                 DB
                 ()
                 ( Field "id" Text
                 , (Field "name" Text, (Field "employee_id" UUID, (Field "bio" (Maybe Text), ())))
                 )
          stmt = [ssql| select * from users where users.name ilike 'a%' |]
          squealRendering :: Text
          squealRendering =
            "SELECT * FROM \"users\" AS \"users\" WHERE (\"users\".\"name\" ILIKE (E'a%' :: text))"
        checkStatement squealRendering stmt

      -- Prefix Operators
      it "select * from users where not (users.name = 'no-one')" $ do
        let
          stmt
            :: Statement
                 DB
                 ()
                 ( Field "id" Text
                 , (Field "name" Text, (Field "employee_id" UUID, (Field "bio" (Maybe Text), ())))
                 )
          stmt = [ssql| select * from users where not (users.name = 'no-one') |]
          squealRendering :: Text
          squealRendering =
            "SELECT * FROM \"users\" AS \"users\" WHERE (NOT (\"users\".\"name\" = (E'no-one' :: text)))"
        checkStatement squealRendering stmt

      it "select -emails.id as neg_id from emails" $ do
        let
          stmt :: Statement DB () (Field "neg_id" Int32, ())
          stmt = [ssql| select -emails.id as neg_id from emails |]
          squealRendering :: Text
          squealRendering =
            "SELECT ((0 :: int4) - \"emails\".\"id\") AS \"neg_id\" FROM \"emails\" AS \"emails\""
        checkStatement squealRendering stmt

      -- Postfix Operators
      it "select * from users where users.bio is null" $ do
        let
          stmt
            :: Statement
                 DB
                 ()
                 ( Field "id" Text
                 , (Field "name" Text, (Field "employee_id" UUID, (Field "bio" (Maybe Text), ())))
                 )
          stmt = [ssql| select * from users where users.bio is null |]
          squealRendering :: Text
          squealRendering = "SELECT * FROM \"users\" AS \"users\" WHERE \"users\".\"bio\" IS NULL"
        checkStatement squealRendering stmt

      it "select * from users where users.bio is not null" $ do
        let
          stmt
            :: Statement
                 DB
                 ()
                 ( Field "id" Text
                 , (Field "name" Text, (Field "employee_id" UUID, (Field "bio" (Maybe Text), ())))
                 )
          stmt = [ssql| select * from users where users.bio is not null |]
          squealRendering :: Text
          squealRendering = "SELECT * FROM \"users\" AS \"users\" WHERE \"users\".\"bio\" IS NOT NULL"
        checkStatement squealRendering stmt

      describe "function calls" $ do
        -- Function Calls
        it "select coalesce(users.bio, 'no bio') as bio from users" $ do
          let
            stmt :: Statement DB () (Field "bio" Text, ())
            stmt = [ssql| select coalesce(users.bio, 'no bio') as bio from users |]
            squealRendering :: Text
            squealRendering =
              "SELECT COALESCE(\"users\".\"bio\", (E'no bio' :: text)) AS \"bio\" FROM \"users\" AS \"users\""
          checkStatement squealRendering stmt

        it "select lower(users.name) as lower_name from users" $ do
          let
            stmt :: Statement DB () (Field "lower_name" Text, ())
            stmt = [ssql| select lower(users.name) as lower_name from users |]
            squealRendering :: Text
            squealRendering =
              "SELECT lower(\"users\".\"name\") AS \"lower_name\" FROM \"users\" AS \"users\""
          checkStatement squealRendering stmt

        it "select char_length(users.name) as name_len from users" $ do
          let
            stmt :: Statement DB () (Field "name_len" Int32, ())
            stmt = [ssql| select char_length(users.name) as name_len from users |]
            squealRendering :: Text
            squealRendering =
              "SELECT char_length(\"users\".\"name\") AS \"name_len\" FROM \"users\" AS \"users\""
          checkStatement squealRendering stmt

        it "select character_length(users.name) as name_len_alias from users" $ do
          let
            stmt :: Statement DB () (Field "name_len_alias" Int32, ())
            stmt = [ssql| select character_length(users.name) as name_len_alias from users |]
            squealRendering :: Text
            squealRendering =
              "SELECT char_length(\"users\".\"name\") AS \"name_len_alias\" FROM \"users\" AS \"users\""
          checkStatement squealRendering stmt

        it "select \"upper\"(users.name) as upper_name from users" $ do
          let
            stmt :: Statement DB () (Field "upper_name" Text, ())
            stmt = [ssql| select "upper"(users.name) as upper_name from users |]
            squealRendering :: Text
            squealRendering =
              "SELECT upper(\"users\".\"name\") AS \"upper_name\" FROM \"users\" AS \"users\""
          checkStatement squealRendering stmt

        it "select now() as current_time" $ do
          let
            stmt :: Statement DB () (Field "current_time" UTCTime, ())
            stmt = [ssql| select now() as current_time |]
            squealRendering :: Text
            squealRendering = "SELECT * FROM (VALUES (now())) AS t (\"current_time\")"
          checkStatement squealRendering stmt

        it "select current_date as today" $ do
          let
            stmt :: Statement DB () (Field "today" Day, ())
            stmt = [ssql| select current_date as today |]
            squealRendering :: Text
            squealRendering = "SELECT * FROM (VALUES (CURRENT_DATE)) AS t (\"today\")"
          checkStatement squealRendering stmt

        describe "aggregate functions" $ do
          it "select sum(id) as total_ids from emails" $ do
            let
              stmt :: Statement DB () (Field "total_ids" (Maybe Int64), ())
              stmt = [ssql| select sum(id) as total_ids from emails group by () |]
              squealRendering :: Text
              squealRendering =
                "SELECT sum(ALL \"id\") AS \"total_ids\" FROM \"emails\" AS \"emails\""
            checkStatement squealRendering stmt

          it "select sum(all id) as total_ids from emails" $ do
            let
              stmt :: Statement DB () (Field "total_ids" (Maybe Int64), ())
              stmt = [ssql| select sum(all id) as total_ids from emails group by () |]
              squealRendering :: Text
              squealRendering =
                "SELECT sum(ALL \"id\") AS \"total_ids\" FROM \"emails\" AS \"emails\""
            checkStatement squealRendering stmt

          it "select sum(distinct id) as total_ids from emails" $ do
            let
              stmt :: Statement DB () (Field "total_ids" (Maybe Int64), ())
              stmt = [ssql| select sum(distinct id) as total_ids from emails group by () |]
              squealRendering :: Text
              squealRendering =
                "SELECT sum(DISTINCT \"id\") AS \"total_ids\" FROM \"emails\" AS \"emails\""
            checkStatement squealRendering stmt

          it "select count(distinct id) as distinct_ids from emails" $ do
            let
              stmt :: Statement DB () (Field "distinct_ids" Int64, ())
              stmt = [ssql| select count(distinct id) as distinct_ids from emails group by () |]
              squealRendering :: Text
              squealRendering =
                "SELECT count(DISTINCT \"id\") AS \"distinct_ids\" FROM \"emails\" AS \"emails\""
            checkStatement squealRendering stmt

          it "select count(all id) as all_ids from emails" $ do
            let
              stmt :: Statement DB () (Field "all_ids" Int64, ())
              stmt = [ssql| select count(all id) as all_ids from emails group by () |]
              squealRendering :: Text
              squealRendering =
                "SELECT count(ALL \"id\") AS \"all_ids\" FROM \"emails\" AS \"emails\""
            checkStatement squealRendering stmt

          it "select avg(id) as avg_id from emails" $ do
            let
              stmt
                :: Statement
                     DB
                     ()
                     ( Field "avg_id" (Maybe Scientific)
                     , ()
                     )
              stmt = [ssql| select avg(id) as avg_id from emails group by () |]
              squealRendering :: Text
              squealRendering =
                "SELECT avg(ALL \"id\") AS \"avg_id\" FROM \"emails\" AS \"emails\""
            checkStatement squealRendering stmt

          it "select min(id) as min_id from emails" $ do
            let
              stmt :: Statement DB () (Field "min_id" (Maybe Int32), ())
              stmt = [ssql| select min(id) as min_id from emails group by () |]
              squealRendering :: Text
              squealRendering =
                "SELECT min(ALL \"id\") AS \"min_id\" FROM \"emails\" AS \"emails\""
            checkStatement squealRendering stmt

          it "select max(id) as max_id from emails" $ do
            let
              stmt :: Statement DB () (Field "max_id" (Maybe Int32), ())
              stmt = [ssql| select max(id) as max_id from emails group by () |]
              squealRendering :: Text
              squealRendering =
                "SELECT max(ALL \"id\") AS \"max_id\" FROM \"emails\" AS \"emails\""
            checkStatement squealRendering stmt

        it "haskell variables in expressions" $ do
          let
            mkStatement
              :: Text
              -> Statement
                   DB
                   ()
                   ( Field "id" Text
                   , ( Field "name" Text
                     , ( Field "employee_id" UUID
                       , ( Field "bio" (Maybe Text)
                         , ()
                         )
                       )
                     )
                   )
            mkStatement haskellVariable =
              [ssql| select * from users where name = inline("haskellVariable") |]

            squealRendering1 :: Text
            squealRendering1 = "SELECT * FROM \"users\" AS \"users\" WHERE (\"name\" = (E'Alice' :: text))"

            squealRendering2 :: Text
            squealRendering2 = "SELECT * FROM \"users\" AS \"users\" WHERE (\"name\" = (E'Bob' :: text))"

          checkStatement squealRendering1 (mkStatement "Alice")
          checkStatement squealRendering2 (mkStatement "Bob")

      -- PARENS (implicitly tested by complex expressions)
      it "select (emails.id + 1) * 2 as calc from emails" $ do
        let
          stmt :: Statement DB () (Field "calc" Int32, ())
          stmt = [ssql| select (emails.id + 1) * 2 as calc from emails |]
          squealRendering :: Text
          squealRendering =
            "SELECT ((\"emails\".\"id\" + 1) * 2) AS \"calc\" FROM \"emails\" AS \"emails\""
        checkStatement squealRendering stmt

      -- IN / NOT IN
      it "select * from users where users.name in ('Alice', 'Bob')" $ do
        let
          stmt
            :: Statement
                 DB
                 ()
                 ( Field "id" Text
                 , (Field "name" Text, (Field "employee_id" UUID, (Field "bio" (Maybe Text), ())))
                 )
          stmt = [ssql| select * from users where users.name in ('Alice', 'Bob') |]
          squealRendering :: Text
          squealRendering =
            "SELECT * FROM \"users\" AS \"users\" WHERE \"users\".\"name\" IN ((E'Alice' :: text), (E'Bob' :: text))"
        checkStatement squealRendering stmt

      it "select * from users where users.id in (select emails.user_id from emails)" $ do
        let
          stmt
            :: Statement
                 DB
                 ()
                 ( Field "id" Text
                 , (Field "name" Text, (Field "employee_id" UUID, (Field "bio" (Maybe Text), ())))
                 )
          stmt =
            [ssql| select * from users where users.id in (select emails.user_id from emails) |]
          squealRendering :: Text
          squealRendering =
            "SELECT * FROM \"users\" AS \"users\" WHERE (\"users\".\"id\" = ANY (SELECT \"emails\".\"user_id\" AS \"user_id\" FROM \"emails\" AS \"emails\"))"
        checkStatement squealRendering stmt

      it "select * from users where users.name not in ('Alice', 'Bob')" $ do
        let
          stmt
            :: Statement
                 DB
                 ()
                 ( Field "id" Text
                 , (Field "name" Text, (Field "employee_id" UUID, (Field "bio" (Maybe Text), ())))
                 )
          stmt = [ssql| select * from users where users.name not in ('Alice', 'Bob') |]
          squealRendering :: Text
          squealRendering =
            "SELECT * FROM \"users\" AS \"users\" WHERE \"users\".\"name\" NOT IN ((E'Alice' :: text), (E'Bob' :: text))"
        checkStatement squealRendering stmt

      -- BETWEEN / NOT BETWEEN
      it "select * from emails where emails.id between 0 and 10" $ do
        let
          stmt
            :: Statement
                 DB
                 ()
                 (Field "id" Int32, (Field "user_id" Text, (Field "email" (Maybe Text), ())))
          stmt = [ssql| select * from emails where emails.id between 0 and 10 |]
          squealRendering :: Text
          squealRendering =
            "SELECT * FROM \"emails\" AS \"emails\" WHERE \"emails\".\"id\" BETWEEN 0 AND 10"
        checkStatement squealRendering stmt

      it "select * from emails where emails.id not between 0 and 10" $ do
        let
          stmt
            :: Statement
                 DB
                 ()
                 (Field "id" Int32, (Field "user_id" Text, (Field "email" (Maybe Text), ())))
          stmt = [ssql| select * from emails where emails.id not between 0 and 10 |]
          squealRendering :: Text
          squealRendering =
            "SELECT * FROM \"emails\" AS \"emails\" WHERE \"emails\".\"id\" NOT BETWEEN 0 AND 10"
        checkStatement squealRendering stmt

      -- CAST
      it "select (e.id :: text) as casted_id from emails as e" $ do
        let
          stmt :: Statement DB () (Field "casted_id" Text, ())
          stmt = [ssql| select (e.id :: text) as casted_id from emails as e |]
          squealRendering :: Text
          squealRendering = "SELECT (\"e\".\"id\" :: text) AS \"casted_id\" FROM \"emails\" AS \"e\""
        checkStatement squealRendering stmt

      it "select * from users for update" $ do
        let
          statement
            :: Statement
                 DB
                 ()
                 ( Field "id" Text
                 , ( Field "name" Text
                   , ( Field "employee_id" UUID
                     , ( Field "bio" (Maybe Text)
                       , ()
                       )
                     )
                   )
                 )
          statement = [ssql| select * from users for update |]
          squealRendering :: Text
          squealRendering = "SELECT * FROM \"users\" AS \"users\" FOR UPDATE"
        checkStatement squealRendering statement

      it "select * from jsonb_test" $ do
        let
          statement
            :: Statement
                 DB
                 ()
                 ( Field "data" (Jsonb Value)
                 , ()
                 )
          statement = [ssql| select * from jsonb_test |]
          squealRendering :: Text
          squealRendering = "SELECT * FROM \"jsonb_test\" AS \"jsonb_test\""
        checkStatement squealRendering statement

      it "select * from json_test" $ do
        let
          statement
            :: Statement
                 DB
                 ()
                 ( Field "data" (Json Value)
                 , ()
                 )
          statement = [ssql| select * from json_test |]
          squealRendering :: Text
          squealRendering = "SELECT * FROM \"json_test\" AS \"json_test\""
        checkStatement squealRendering statement

      it "select distinct name from users" $ do
        let
          statement :: Statement DB () (Field "name" Text, ())
          statement = [ssql| select distinct name from users |]
          squealRendering :: Text
          squealRendering = "SELECT DISTINCT \"name\" AS \"name\" FROM \"users\" AS \"users\""
        checkStatement squealRendering statement

      it "select distinct * from users" $ do
        let
          statement
            :: Statement
                 DB
                 ()
                 ( Field "id" Text
                 , ( Field "name" Text
                   , ( Field "employee_id" UUID
                     , ( Field "bio" (Maybe Text)
                       , ()
                       )
                     )
                   )
                 )
          statement = [ssql| select distinct * from users |]
          squealRendering :: Text
          squealRendering = "SELECT DISTINCT * FROM \"users\" AS \"users\""
        checkStatement squealRendering statement

      it "select distinct on (employee_id) employee_id, name from users" $ do
        let
          statement
            :: Statement
                 DB
                 ()
                 ( Field "employee_id" UUID
                 , ( Field "name" Text
                   , ()
                   )
                 )
          statement = [ssql| select distinct on (employee_id) employee_id, name from users |]
          squealRendering :: Text
          squealRendering =
            "SELECT DISTINCT ON (\"employee_id\") \"employee_id\" AS \"employee_id\", \"name\" AS \"name\" FROM \"users\" AS \"users\" ORDER BY \"employee_id\" ASC"
        checkStatement squealRendering statement

      it "select distinct on (employee_id, name) employee_id, name, id from users" $ do
        let
          statement
            :: Statement
                 DB
                 ()
                 ( Field "employee_id" UUID
                 , ( Field "name" Text
                   , ( Field "id" Text
                     , ()
                     )
                   )
                 )
          statement =
            [ssql| select distinct on (employee_id, name) employee_id, name, id from users |]
          squealRendering :: Text
          squealRendering =
            "SELECT DISTINCT ON (\"employee_id\", \"name\") \"employee_id\" AS \"employee_id\", \"name\" AS \"name\", \"id\" AS \"id\" FROM \"users\" AS \"users\" ORDER BY \"employee_id\" ASC, \"name\" ASC"
        checkStatement squealRendering statement

      describe "order by" $ do
        it "select * from users order by name" $ do
          let
            statement
              :: Statement
                   DB
                   ()
                   ( Field "id" Text
                   , ( Field "name" Text
                     , ( Field "employee_id" UUID
                       , ( Field "bio" (Maybe Text)
                         , ()
                         )
                       )
                     )
                   )
            statement = [ssql| select * from users order by name |]
            squealRendering :: Text
            squealRendering = "SELECT * FROM \"users\" AS \"users\" ORDER BY \"name\" ASC"
          checkStatement squealRendering statement

        it "select * from users order by name asc" $ do
          let
            statement
              :: Statement
                   DB
                   ()
                   ( Field "id" Text
                   , ( Field "name" Text
                     , ( Field "employee_id" UUID
                       , ( Field "bio" (Maybe Text)
                         , ()
                         )
                       )
                     )
                   )
            statement = [ssql| select * from users order by name asc |]
            squealRendering :: Text
            squealRendering = "SELECT * FROM \"users\" AS \"users\" ORDER BY \"name\" ASC"
          checkStatement squealRendering statement

        it "select * from users order by name desc" $ do
          let
            statement
              :: Statement
                   DB
                   ()
                   ( Field "id" Text
                   , ( Field "name" Text
                     , ( Field "employee_id" UUID
                       , ( Field "bio" (Maybe Text)
                         , ()
                         )
                       )
                     )
                   )
            statement = [ssql| select * from users order by name desc |]
            squealRendering :: Text
            squealRendering = "SELECT * FROM \"users\" AS \"users\" ORDER BY \"name\" DESC"
          checkStatement squealRendering statement

      describe "having clause" $ do
        it
          "select employee_id, count(id) from users group by employee_id having count(id) > 1"
          $ do
            let
              statement
                :: Statement
                     DB
                     ()
                     ( Field "employee_id" UUID
                     , ( Field "_col2" Int64 -- Assuming count returns Int64
                       , ()
                       )
                     )
              statement =
                [ssql|
                  select employee_id, count(id)
                  from users
                  group by employee_id
                  having count(id) > 1
                |]
              squealRendering :: Text
              squealRendering =
                "SELECT \"employee_id\" AS \"employee_id\", count(ALL \"id\") AS \"_col2\" FROM \"users\" AS \"users\" GROUP BY \"employee_id\" HAVING (count(ALL \"id\") > 1)"
            checkStatement squealRendering statement

      describe "window functions" $ do
        it "select name, row_number() over () as rn from users" $ do
          let
            statement
              :: Statement
                   DB
                   ()
                   ( Field "name" Text
                   , ( Field "rn" Int64
                     , ()
                     )
                   )
            statement = [ssql| select name, row_number() over () as rn from users |]
            squealRendering :: Text
            squealRendering =
              "SELECT \"name\" AS \"name\", row_number() OVER () AS \"rn\" FROM \"users\" AS \"users\""
          checkStatement squealRendering statement

        it
          "select name, rank() over (partition by employee_id order by name) as r from users"
          $ do
            let
              statement
                :: Statement
                     DB
                     ()
                     ( Field "name" Text
                     , ( Field "r" Int64
                       , ()
                       )
                     )
              statement =
                [ssql| select name, rank() over (partition by employee_id order by name) as r from users |]
              squealRendering :: Text
              squealRendering =
                "SELECT \"name\" AS \"name\", rank() OVER (PARTITION BY \"employee_id\" ORDER BY \"name\" ASC) AS \"r\" FROM \"users\" AS \"users\""
            checkStatement squealRendering statement

        it "select email, sum(id) over (partition by user_id) as user_total from emails" $ do
          let
            statement
              :: Statement
                   DB
                   ()
                   ( Field "email" (Maybe Text)
                   , ( Field "user_total" (Maybe Int64)
                     , ()
                     )
                   )
            statement =
              [ssql| select email, sum(id) over (partition by user_id) as user_total from emails |]
            squealRendering :: Text
            squealRendering =
              "SELECT \"email\" AS \"email\", sum(\"id\") OVER (PARTITION BY \"user_id\") AS \"user_total\" FROM \"emails\" AS \"emails\""
          checkStatement squealRendering statement

        it "select email, avg(id) over (partition by user_id) as user_avg from emails" $ do
          let
            statement
              :: Statement
                   DB
                   ()
                   ( Field "email" (Maybe Text)
                   , ( Field "user_avg" (Maybe Scientific)
                     , ()
                     )
                   )
            statement =
              [ssql| select email, avg(id) over (partition by user_id) as user_avg from emails |]
            squealRendering :: Text
            squealRendering =
              "SELECT \"email\" AS \"email\", avg(\"id\") OVER (PARTITION BY \"user_id\") AS \"user_avg\" FROM \"emails\" AS \"emails\""
          checkStatement squealRendering statement

        it "select email, min(id) over (partition by user_id) as user_min from emails" $ do
          let
            statement
              :: Statement
                   DB
                   ()
                   ( Field "email" (Maybe Text)
                   , ( Field "user_min" (Maybe Int32)
                     , ()
                     )
                   )
            statement =
              [ssql| select email, min(id) over (partition by user_id) as user_min from emails |]
            squealRendering :: Text
            squealRendering =
              "SELECT \"email\" AS \"email\", min(\"id\") OVER (PARTITION BY \"user_id\") AS \"user_min\" FROM \"emails\" AS \"emails\""
          checkStatement squealRendering statement

        it "select email, max(id) over (partition by user_id) as user_max from emails" $ do
          let
            statement
              :: Statement
                   DB
                   ()
                   ( Field "email" (Maybe Text)
                   , ( Field "user_max" (Maybe Int32)
                     , ()
                     )
                   )
            statement =
              [ssql| select email, max(id) over (partition by user_id) as user_max from emails |]
            squealRendering :: Text
            squealRendering =
              "SELECT \"email\" AS \"email\", max(\"id\") OVER (PARTITION BY \"user_id\") AS \"user_max\" FROM \"emails\" AS \"emails\""
          checkStatement squealRendering statement

        it
          "select name, row_number() over (order by name), rank() over (order by name) from users"
          $ do
            let
              statement
                :: Statement
                     DB
                     ()
                     ( Field "name" Text
                     , ( Field "_window1" Int64
                       , ( Field "_window2" Int64
                         , ()
                         )
                       )
                     )
              statement =
                [ssql| select name, row_number() over (order by name), rank() over (order by name) from users |]
              squealRendering :: Text
              squealRendering =
                "SELECT \"name\" AS \"name\", row_number() OVER ( ORDER BY \"name\" ASC) AS \"_window1\", rank() OVER ( ORDER BY \"name\" ASC) AS \"_window2\" FROM \"users\" AS \"users\""
            checkStatement squealRendering statement

        it
          "select name, row_number() over (partition by employee_id order by name), rank() over (order by name) from users"
          $ do
            let
              statement
                :: Statement
                     DB
                     ()
                     ( Field "name" Text
                     , ( Field "_window1" Int64
                       , ( Field "_window2" Int64
                         , ()
                         )
                       )
                     )
              statement =
                [ssql| select name, row_number() over (partition by employee_id order by name), rank() over (order by name) from users |]
              squealRendering :: Text
              squealRendering =
                "SELECT \"name\" AS \"name\", row_number() OVER (PARTITION BY \"employee_id\" ORDER BY \"name\" ASC) AS \"_window1\", rank() OVER ( ORDER BY \"name\" ASC) AS \"_window2\" FROM \"users\" AS \"users\""
            checkStatement squealRendering statement


_printQuery :: (RenderSQL a) => a -> IO ()
_printQuery = putStrLn . T.unpack . TE.decodeUtf8 . renderSQL


checkStatement
  :: (RenderSQL sql)
  => Text
  -> sql
  -> IO ()
checkStatement expect statement =
  let
    rendered :: Text
    rendered = TE.decodeUtf8 (renderSQL statement)
  in
    if rendered == expect
      then
        pure ()
      else
        fail $
          "SQL statements do not match.\nExpected:\n"
            <> show expect
            <> "\nActual:\n"
            <> show rendered
            <> "\n"


