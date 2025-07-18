{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

{- |
  Description: quasiquoter understanding SQL and producing [squeal-postgresql](https://hackage.haskell.org/package/squeal-postgresql) expressions.
-}
module Squeal.QuasiQuotes (
  -- * The quasi-quoter
  ssql,
  Field (..),

  -- * Discussion about monomorphized 'Statements'

  --
  -- $discussion
) where

import Language.Haskell.TH.Quote
  ( QuasiQuoter(QuasiQuoter, quoteDec, quoteExp, quotePat, quoteType)
  )
import Language.Haskell.TH.Syntax (Exp(AppE, VarE), Q, runIO)
import Prelude
  ( Applicative(pure), Either(Left, Right), Maybe(Nothing), MonadFail(fail)
  , Semigroup((<>)), Show(show), ($), (.), String, error, print
  )
import Squeal.QuasiQuotes.Delete (toSquealDelete)
import Squeal.QuasiQuotes.Insert (toSquealInsert)
import Squeal.QuasiQuotes.Query (toSquealQuery)
import Squeal.QuasiQuotes.RowType
  ( Field(Field, unField), monoManipulation, monoQuery
  )
import Squeal.QuasiQuotes.Update (toSquealUpdate)
import qualified Data.Text as Text
import qualified PostgresqlSyntax.Ast as PGT_AST
import qualified PostgresqlSyntax.Parsing as PGT_Parse


{- |
  Splice in a squeal expression with the following type:

  > Statement db () <Canonical-Haskell-Row-Type>

  @\<Canonical-Haskell-Row-Type\>@ is going to be some concrete tuple of the
  form:

  > (Field name1 type1,
  > (Field name2 type2,
  > (Field name3 type3,
  > <continue nesting>,
  > ()))...)

  where the "name\<N\>" are phantom types of kind `Symbol`, which provide
  the name of the corresponding column, and types "type\<N\>" are whatever
  appropriate haskell type represents the postgres column type.

  See the [discussion](#discussion) section for why we monomorphize the
  squeal 'Statement' in this way.

  = Haskell values

  The way you get Haskell values into your sql statements is with special bulit-in sql functions:

  * @inline(\<ident\>)@: Corresponds to 'Squeal.PostgreSQL.Expression.Inline.inline' (value being inlined must not be null)
  * @inline_param(\<ident\>)@: Corresponds to 'Squeal.PostgreSQL.Expression.Inline.inlineParam' (value can be null)

  where @\<ident\>@ is a haskell identifier in scope, whose type has an
  'Squeal.PostgreSQL.Inline' instance.

  = Example

  For the examples, let's assume you have a database like this:

  > type UsersConstraints = '[ "pk_users" ::: 'PrimaryKey '["id"] ]
  > type UsersColumns =
  >   '[          "id" :::   'Def :=> 'NotNull 'PGtext
  >    ,        "name" ::: 'NoDef :=> 'NotNull 'PGtext
  >    , "employee_id" ::: 'NoDef :=> 'NotNull 'PGuuid
  >    ,         "bio" ::: 'NoDef :=> 'Null    'PGtext
  >    ]
  > type EmailsConstraints =
  >   '[ "pk_emails"  ::: 'PrimaryKey '["id"]
  >    , "fk_user_id" ::: 'ForeignKey '["user_id"] "public" "users" '["id"]
  >    ]
  > type EmailsColumns =
  >   '[      "id" :::   'Def :=> 'NotNull 'PGint4
  >    , "user_id" ::: 'NoDef :=> 'NotNull 'PGtext
  >    ,   "email" ::: 'NoDef :=> 'Null 'PGtext
  >    ]
  > type Schema =
  >   '[  "users" ::: 'Table (UsersConstraints :=> UsersColumns)
  >    , "emails" ::: 'Table (EmailsConstraints :=> EmailsColumns)
  >    ]


  == Insert Example

  > mkStatement :: Int32 -> Text -> Maybe Text -> Statement DB () ()
  > mkStatement emailId uid email =
  >   [ssql|
  >     insert into
  >       emails (id, user_id, email)
  >       values (inline("emailId"), inline(uid), inline_param(email))
  >   |]

  Notice the quotes around @"emailId"@. This is because postgres SQL
  parsing mandates the unquoted idents be converted to lower case
  (as a way of being "case insensitive"), which would result in the
  quasiquoter injecting the lower case @emailid@ variable, which is not
  in scope. The solution is to double quote the SQL ident so that its
  casing is preserved.

  == Select Example

  > mkStatement
  >   :: Text
  >   -> Statement
  >        DB
  >        ()
  >        ( Field "id" Text
  >        , ( Field "name" Text
  >          , ( Field "email" (Maybe Text)
  >            , ()
  >            )
  >          )
  >        )
  > mkStatement targetEmail =
  >   [ssql|
  >     select users.id, users.name, emails.email
  >     from users
  >     left outer join emails
  >     on emails.user_id = users.id
  >     where emails.email = inline("targetEmail")
  >   |]

  These examples are more or less taken from the
  test suite. I strongly recommend reading [the test
  code](https://github.com/owensmurray/squeal-postgresql-qq/blob/master/test/test.hs)
  to see what is currently supported.
-}
ssql :: QuasiQuoter
ssql =
  QuasiQuoter
    { quoteExp =
        toSqueal . PGT_Parse.run PGT_Parse.preparableStmt . Text.strip . Text.pack
    , quotePat = error "pattern quotes not supported"
    , quoteType = error "type quotes not supported"
    , quoteDec = error "declaration quotes not supported"
    }


toSqueal :: Either String PGT_AST.PreparableStmt -> Q Exp
toSqueal = \case
  Left err -> fail err
  Right statement -> do
    runIO (print statement)
    toSquealStatement statement


toSquealStatement :: PGT_AST.PreparableStmt -> Q Exp
toSquealStatement = \case
  PGT_AST.SelectPreparableStmt theQuery -> do
    queryExp <- toSquealQuery [] Nothing theQuery
    pure $ VarE 'monoQuery `AppE` queryExp
  PGT_AST.InsertPreparableStmt stmt -> do
    manipExp <- toSquealInsert stmt
    pure $ VarE 'monoManipulation `AppE` manipExp
  PGT_AST.UpdatePreparableStmt stmt -> do
    manipExp <- toSquealUpdate stmt
    pure $ VarE 'monoManipulation `AppE` manipExp
  PGT_AST.DeletePreparableStmt stmt -> do
    manipExp <- toSquealDelete stmt
    pure $ VarE 'monoManipulation `AppE` manipExp
  unsupported ->
    error $ "Unsupported statement: " <> show unsupported


{- $discussion

  #discussion#

  The reason we monomorphize the SQL statement using the nested tuple
  structure (see 'ssql') is that Squeal by default allows for generic
  based polymorphic input row types and output row types. This is
  problematic for a number of reasons. It severely hinders type inference,
  and it produces very bad error messages when the types are misaligned.

  If you were to manually craft Squeal expressions, you would have the
  opportunity to add helpful type annotations for convenient and critical
  sub-expressions of the overall top-level Squeal expression. But because
  this quasi-quoter generates the Squeal expression for you, you have
  no opportunity to do something similar. The only place you can place
  a type annotation is at the entire top level expression generated by
  the quasi-quoter.

  Therefore, the trade-offs between going with the polymorphic approach
  and a monomorphic approach don't have the same costs/benefits when
  using the quasi-quoter as they do when manually crafting Squeal.

  To solve this problem (or rather to choose a different set of
  trade-offs), the quasi-quoter forces all input and output rows to be
  monomorphized into a "canonical" Haskell type which has this nested
  tuple of fields structure. Under the hood we do this via an injective
  type family, so that type inference can go both ways.

  If you have a quasi-quoted query in hand, you can use a type hole to
  ask GHC what its inferred concrete type is.  Conversely, if you know
  what the expected row type is, but your quasi-quoted SQL statement is
  failing to align with your expected types, you can put your expected
  row type in a type signature and get a much better error message about
  how and why the SQL statement is failing to type check.
-}


