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
