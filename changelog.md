### 0.1.4.0

* New features
  * Support for SQL window functions (`OVER` clause).
    * `select name, row_number() over () as rn from users` [✔]
    * `select name, rank() over (partition by employee_id order by name) as r from users` [✔]
    * `select email, sum(id) over (partition by user_id) as user_total from emails` [✔]
    * `select email, avg(id) over (partition by user_id) as user_avg from emails` [✔]
    * `select email, min(id) over (partition by user_id) as user_min from emails` [✔]
    * `select email, max(id) over (partition by user_id) as user_max from emails` [✔]
  * Support for SQL set operations (`UNION`, `INTERSECT`, `EXCEPT`).
    * `select name from users union select name from users_copy` [✔]
    * `select name from users union all select name from users_copy` [✔]
    * `select name from users intersect select name from users_copy` [✔]
    * `select name from users intersect all select name from users_copy` [✔]
    * `select name from users except select name from users_copy` [✔]
    * `select name from users except all select name from users_copy` [✔]
  * Support for `WITH RECURSIVE` clauses.
    * `with recursive t as ( select 1 as n union all select (n + 1) as n from t where n < 100) select n from t` [✔]
    * `with recursive users_cte as ( select id, name from users union all select id, name from users_cte) select * from users_cte` [✔]
  * Support for aggregate functions (`sum`, `avg`, `min`, `max`).
    * `select sum(id) as total_ids from emails` [✔]
    * `select sum(all id) as total_ids from emails` [✔]
    * `select sum(distinct id) as total_ids from emails` [✔]
    * `select count(distinct id) as distinct_ids from emails` [✔]
    * `select count(all id) as all_ids from emails` [✔]
    * `select avg(id) as avg_id from emails` [✔]
    * `select min(id) as min_id from emails` [✔]
    * `select max(id) as max_id from emails` [✔]
  * Support for `PGnumeric` type, mapping to `Scientific`.

### 0.1.3.0

* New features
  * Support `ON CONFLICT ON CONSTRAINT` in `INSERT` statements.
    * `insert into users_copy (id, name, bio) values ('id1', 'name1', null) on conflict on constraint pk_users_copy do nothing` [✔]
    * `insert into users_copy (id, name, bio) values ('id1', 'name1', 'bio1') on conflict on constraint pk_users_copy do update set name = 'new_name'` [✔]
    * `insert into users_copy (id, name, bio) values ('id1', 'name1', null) on conflict on constraint pk_users_copy do update set name = 'new_name' where users_copy.name = 'old_name'` [✔]
    * `insert into users_copy (id, name, bio) values ('id1', 'name1', null) on conflict on constraint pk_users_copy do nothing returning id` [✔]
    * `insert into users_copy (id, name, bio) values ('id1', 'name1', 'bio1') on conflict on constraint pk_users_copy do update set name = 'new_name' returning *` [✔]

### 0.1.2.1

* No-op improvements
  * Minor documentation edits.

### 0.1.2.0

* New features
  * Officially support statement parameters

    They where technically working prior to this version, but now that I realize
    they can't be made monomorphic I have decided to support them officially in
    their polymorphic form. See the Haddocks for more information.

    I am electing to escalate this to a minor version bump when I think it
    could technically be a patch version (from the standpoint of "whether it
    compiles") to reflect the "officially supported" nature of the feature.

* No-Op improvements
  * Expand and improve the documentation
  * Some internal refactors


### 0.1.1.1

No feature or behavior changes. Only documentation.

### 0.1.1.0

* Support common table expressions (CTEs).
  * `with users_cte as (select * from users) select * from users_cte` [✔]
  * `with users_cte as (select * from users), emails_cte as (select * from emails) select users_cte.*, emails_cte.email from users_cte join emails_cte on users_cte.id = emails_cte.user_id` [✔]
  * `with new_user (id, name, bio) as (values ('id_new', 'new_name', 'new_bio')) insert into users_copy select * from new_user` [✔]
  * `with to_delete as (select id from users where name = 'Alice') delete from users where id in (select to_delete.id from to_delete)` [✔]
  * `with to_delete as (select id from users where name = 'Alice') delete from users using to_delete where users.id = to_delete.id` [✔]
  * `with to_update as (select id from users where name = 'Alice') update users set name = 'Alicia' from to_update where users.id = to_update.id` [✔]

* Support `IN` subqueries.
  * `select * from users where users.id in (select emails.user_id from emails)` [✔]

### 0.1.0.0

Initial release, supports the following features (taken from the test suite output):

* queries
  * `select * from users` [✔]
  * `select * from public.users` [✔]
  * `SELECT * FROM "users" AS "users"` [✔]
  * `select * from users where name = 'bob'` [✔]
  * `select users.name from users` [✔]
  * `select name from users` [✔]
  * `select count(*) from users group by ()` [✔]
  * `select name, id from users` [✔]
  * `select id, name from users` [✔]
  * `select users.id, employee_id from users` [✔]
  * `select users.* from users` [✔]
  * `select users.* from other.users` [✔]
  * `select * from users limit 3` [✔]
  * `select * from users limit inline(lim)` [✔]
  * `select * from users offset inline(off)` [✔]
  * `select * from users offset 1` [✔]
  * `select users.id, employee_id as emp_id from users` [✔]
  * `select users.id as user_id, employee_id from users` [✔]
  * `select users.id from users left outer join emails on emails.user_id = users.id` [✔]
  * `select users.id, users.name, emails.email from users left outer join emails on emails.user_id = users.id where emails.email = inline("targetEmail")` [✔]
  * `select 'text_val'` [✔]
  * `select 1` [✔]
  * `select 1 AS num, 'text_val' AS txt` [✔]
  * group by
    * `select name from users group by name` [✔]
    * `select employee_id, count(id) from users group by employee_id` [✔]
    * `select employee_id, name, count(id) from users group by employee_id, name` [✔]
* inserts
  * `insert into emails (id, user_id, email) values (1, 'user-1', 'foo@bar')` [✔]
  * `insert into emails (id, user_id, email) values (1, 'user-1', $1)` [✔]
  * `insert into emails (id, user_id, email) values (1, $2, $1)` [✔]
  * `insert into emails (id, user_id, email) values (inline(i), inline(uid), inline_param(e))` [✔]
  * default keyword
    * `insert into emails (id, user_id, email) values (default, 'foo', 'bar')` [✔]
    * `insert into emails (id, user_id, email) values (deFault, 'foo', 'bar')` [✔]
    * `insert into emails (id, user_id, email) values (DEFAULT, 'foo', 'bar')` [✔]
  * null keyword
    * `insert into emails (id, user_id, email) values (DEFAULT, 'foo', null)` [✔]
    * `insert into emails (id, user_id, email) values (DEFAULT, 'foo', NULL)` [✔]
    * `insert into emails (id, user_id, email) values (DEFAULT, 'foo', NuLL)` [✔]
  * insert ... select ...
    * `insert into emails select id, user_id, email from emails where id = 1` [✔]
    * `insert into emails select id, user_id, email from emails where id = $1` [✔]
    * `insert into users_copy select id, name, bio from users where users.id = 'uid1'` [✔]
  * returning clause
    * `insert into emails (id, user_id, email) values (1, 'user-1', 'foo@bar') returning id` [✔]
    * `insert into emails (id, user_id, email) values (1, 'user-1', 'foo@bar') returning *` [✔]
* deletes
  * `delete from users where true` [✔]
  * `delete from emails where id = 1` [✔]
  * `delete from emails where email = inline(e)` [✔]
  * `delete from users where id = 'some-id' returning id` [✔]
* updates
  * `update users set name = 'new name' where id = 'some-id'` [✔]
  * `update users set name = 'new name', bio = 'new bio' where id = 'some-id'` [✔]
  * `update users set name = inline(n) where id = 'some-id'` [✔]
  * `update users set name = 'new name' where id = 'some-id' returning id` [✔]
* scalar expressions
  * `select users.id != 'no-such-user' as neq from users` [✔]
  * `select * from users where users.id <> 'no-such-user'` [✔]
  * `select * from emails where emails.id > 0` [✔]
  * `select * from emails where emails.id >= 0` [✔]
  * `select * from emails where emails.id < 10` [✔]
  * `select * from emails where emails.id <= 10` [✔]
  * `select emails.id + 1 as plus_one from emails` [✔]
  * `select emails.id - 1 as minus_one from emails` [✔]
  * `select emails.id * 2 as times_two from emails` [✔]
  * `select * from users where users.id = 'a' and users.name = 'b'` [✔]
  * `select * from users where users.id = 'a' or users.name = 'b'` [✔]
  * `select * from users where users.name like 'A%'` [✔]
  * `select * from users where users.name ilike 'a%'` [✔]
  * `select * from users where not (users.name = 'no-one')` [✔]
  * `select -emails.id as neg_id from emails` [✔]
  * `select * from users where users.bio is null` [✔]
  * `select * from users where users.bio is not null` [✔]
  * function calls
    * `select coalesce(users.bio, 'no bio') as bio from users` [✔]
    * `select lower(users.name) as lower_name from users` [✔]
    * `select char_length(users.name) as name_len from users` [✔]
    * `select character_length(users.name) as name_len_alias from users` [✔]
    * `select "upper"(users.name) as upper_name from users` [✔]
    * `select now() as current_time` [✔]
    * `select current_date as today` [✔]
    * haskell variables in expressions
      * `select * from users where name = inline("haskellVariable")` [✔]
  * `select (emails.id + 1) * 2 as calc from emails` [✔]
  * `select * from users where users.name in ('Alice', 'Bob')` [✔]
  * `select * from users where users.name not in ('Alice', 'Bob')` [✔]
  * `select * from emails where emails.id between 0 and 10` [✔]
  * `select * from emails where emails.id not between 0 and 10` [✔]
  * `select (e.id :: text) as casted_id from emails as e` [✔]
  * `select * from users for update` [✔]
  * `select * from jsonb_test` [✔]
  * `select * from json_test` [✔]
  * `select distinct name from users` [✔]
  * `select distinct * from users` [✔]
  * `select distinct on (employee_id) employee_id, name from users` [✔]
  * `select distinct on (employee_id, name) employee_id, name, id from users` [✔]
  * order by
    * `select * from users order by name` [✔]
    * `select * from users order by name asc` [✔]
    * `select * from users order by name desc` [✔]
  * having clause
    * `select employee_id, count(id) from users group by employee_id having count(id) > 1` [✔]
