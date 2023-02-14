with recursive fk_tree as (
  -- All tables not referencing anything else
  select t.oid as reloid,
         t.relname as table_name,
         s.nspname as schema_name,
         null::text COLLATE "C" as referenced_table_name,
         null::text COLLATE "C" as referenced_schema_name,
         1 as level
  from pg_class t
    join pg_namespace s on s.oid = t.relnamespace
  where relkind = 'r'
    and not exists (select *
                    from pg_constraint
                    where contype = 'f'
                      and conrelid = t.oid)
    and s.nspname = 'SCHEMANAME' -- limit to one schema
  union all
  select ref.oid,
         ref.relname,
         rs.nspname,
         p.table_name,
         p.schema_name,
         p.level + 1
  from pg_class ref
    join pg_namespace rs on rs.oid = ref.relnamespace
    join pg_constraint c on c.contype = 'f' and c.conrelid = ref.oid
    join fk_tree p on p.reloid = c.confrelid
  where ref.oid != p.reloid  -- do not enter to tables referencing theirselves.
), all_tables as (
  -- this picks the highest level for each table
  select schema_name, table_name,
         level,
         row_number() over (partition by schema_name, table_name order by level desc) as last_table_row
  from fk_tree
)
select schema_name, table_name, level
from all_tables at
where last_table_row = 1
order by level;

