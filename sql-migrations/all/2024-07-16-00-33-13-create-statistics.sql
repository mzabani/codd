CREATE STATISTICS IF NOT EXISTS test_stat_expr
    ( dependencies,mcv) ON employee_id, lower(employee_name)
    FROM employee ;
CREATE STATISTICS IF NOT EXISTS test_stat
    ( dependencies,ndistinct,mcv) ON employee_id, employee_name
    FROM employee ;

CREATE STATISTICS IF NOT EXISTS test_stat_with_expr
    ( dependencies,ndistinct,mcv) ON id, (lower(popular_name)), popular_name, (id*42)
    FROM animals;

