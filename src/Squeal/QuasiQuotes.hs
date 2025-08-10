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
import Squeal.QuasiQuotes.MonoRow
  ( Field(Field, unField), monoManipulation, monoQuery
  )
import Squeal.QuasiQuotes.Query (toSquealQuery)
import Squeal.QuasiQuotes.Update (toSquealUpdate)
import qualified Data.Text as Text
import qualified PostgresqlSyntax.Ast as PGT_AST
import qualified PostgresqlSyntax.Parsing as PGT_Parse


{- |
  Splice in a squeal expression with the following type:

  > Statement db params <Concrete-Haskell-Row-Type>

  = Input parameters

  The input parameters @params@ is a polymorphic type that gets encoded into
  SQL statement parameters. It is a direct pass-through to Squeal using Squeal's
  `Squeal.PostgreSQL.genericParams`. The upshot is that we don't monomorphize
  this type the same way we do for the statement's resulting row type. See the
  [Polymorphic Params Discussion](#polymorphic-params) for the reason why not.

  = Resulting Row Type

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

  = Inline Haskell Values

  If you don't want to use statement parameters, you can still get Haskell
  values into your statements is with special bulit-in sql functions:

  * @inline(\<ident\>)@: Corresponds to
    'Squeal.PostgreSQL.Expression.Inline.inline' (value being inlined must
    not be null)

  * @inline_param(\<ident\>)@: Corresponds to
    'Squeal.PostgreSQL.Expression.Inline.inlineParam' (value can be null)

  where @\<ident\>@ is a haskell identifier in scope, whose type has an
  'Squeal.PostgreSQL.Inline' instance.

  = Examples

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

  #monomorphized-output-rows#

  == Monomorphized Output Rows

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

  #polymorphic-params#

  == Polymorphic Input Parameters

  Type inferencing of statement parameters is not supported, unlike the
  type inferencing of statement output row types. That is to say you
  can write this (i.e. a type hole in the /output/ slot) and expect to
  get a concrete type in your GHC error message.

  > statement :: Statement DB (Only Text) _ 
  > statement [ssql| select name from users where id = $1 |]

  But GHC will not give you anything useful if you type this instead (i.e. a
  type hole in the /input/ slot):

  > statement :: Statement DB _ (Field "name" Text, ())
  > statement [ssql| select name from users where id = $1 |]

  The TLDR is that it can't easily be made to work without some pretty
  extensive machinery to cope with SQL's type system and even then it
  would come with some unpalatable trade-offs.

  The basic problem is that SQL comparisons and other operators support
  null polymorphic values as their parameters. So for instance, an SQL
  statement can compare a non-null column with the literal value @null@,
  and that is valid SQL.  So if you compare the non-null column with a
  parameter instead, what should the type of the parameter be?


  E.g.: 

  > select * from users where id = null -- compare with `null`.
  > select * from users where id = $1   -- compare with a param.
  > select * from users where id = id   -- compare with non-nullable column.

  Should the parameter be nullable or not nullable? It can be
  either! Therefore the corresponding Haskell type can be either! How do we choose?

  We could say, by fiat, that input params always nullable (and make the
  user type a bunch of 'Just's everywhere), but then consider this insert
  statement where the parameter is being used to specify the value of
  a non-null column.

  > insert into users (id) values ($1)

  This instance of the parameter can't be nullable.  It must be
  non-nullable.

  So sometimes statement parameters can be null polymorphic and sometimes
  they can't. Sometimes a Haskell type of /either/ @Something@ /or/
  @(Maybe Something)@ will work, and sometimes it won't.

  I think the decision requires semantic knowledge of a non-trivial
  subset of SQL. A design goal of this library is specifically to offload
  semantic knowledge of SQL to Squeal, not to re-implement it. So unless
  I can think of (or someone contributes) a really clever trick, I think
  input params are going to stay polymorphic, with all the inscrutable
  Squeal and @Generics.SOP@ type errors you get when your types don't
  quite align.
-}


