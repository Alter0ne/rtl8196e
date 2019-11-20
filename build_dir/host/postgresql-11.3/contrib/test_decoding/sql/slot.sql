-- predictability
SET synchronous_commit = on;

SELECT 'init' FROM pg_create_logical_replication_slot('regression_slot_p', 'test_decoding');
SELECT 'init' FROM pg_create_logical_replication_slot('regression_slot_t', 'test_decoding', true);

SELECT pg_drop_replication_slot('regression_slot_p');
SELECT 'init' FROM pg_create_logical_replication_slot('regression_slot_p', 'test_decoding', false);

SELECT 'init' FROM pg_create_logical_replication_slot('regression_slot_t2', 'test_decoding', true);

SELECT pg_create_logical_replication_slot('foo', 'nonexistent');

-- here we want to start a new session and wait till old one is gone
select pg_backend_pid() as oldpid \gset
\c -
SET synchronous_commit = on;

do 'declare c int = 0;
begin
  while (select count(*) from pg_replication_slots where active_pid = '
    :'oldpid'
  ') > 0 loop c := c + 1; perform pg_sleep(0.01); end loop;
  raise log ''slot test looped % times'', c;
end';

-- should fail because the temporary slots were dropped automatically
SELECT pg_drop_replication_slot('regression_slot_t');
SELECT pg_drop_replication_slot('regression_slot_t2');

-- permanent slot has survived
SELECT pg_drop_replication_slot('regression_slot_p');

-- test switching between slots in a session
SELECT 'init' FROM pg_create_logical_replication_slot('regression_slot1', 'test_decoding', true);

CREATE TABLE replication_example(id SERIAL PRIMARY KEY, somedata int, text varchar(120));
BEGIN;
INSERT INTO replication_example(somedata, text) VALUES (1, 1);
INSERT INTO replication_example(somedata, text) VALUES (1, 2);
COMMIT;

SELECT 'init' FROM pg_create_logical_replication_slot('regression_slot2', 'test_decoding', true);

INSERT INTO replication_example(somedata, text) VALUES (1, 3);

SELECT data FROM pg_logical_slot_get_changes('regression_slot1', NULL, NULL, 'include-xids', '0', 'skip-empty-xacts', '1');
SELECT data FROM pg_logical_slot_get_changes('regression_slot2', NULL, NULL, 'include-xids', '0', 'skip-empty-xacts', '1');

INSERT INTO replication_example(somedata, text) VALUES (1, 4);
INSERT INTO replication_example(somedata, text) VALUES (1, 5);

SELECT pg_current_wal_lsn() AS wal_lsn \gset

INSERT INTO replication_example(somedata, text) VALUES (1, 6);

SELECT end_lsn FROM pg_replication_slot_advance('regression_slot1', :'wal_lsn') \gset
SELECT slot_name FROM pg_replication_slot_advance('regression_slot2', pg_current_wal_lsn());

SELECT :'wal_lsn' = :'end_lsn';

SELECT data FROM pg_logical_slot_get_changes('regression_slot1', NULL, NULL, 'include-xids', '0', 'skip-empty-xacts', '1');
SELECT data FROM pg_logical_slot_get_changes('regression_slot2', NULL, NULL, 'include-xids', '0', 'skip-empty-xacts', '1');

DROP TABLE replication_example;

-- error
SELECT 'init' FROM pg_create_logical_replication_slot('regression_slot1', 'test_decoding', true);

-- both should error as they should be dropped on error
SELECT pg_drop_replication_slot('regression_slot1');
SELECT pg_drop_replication_slot('regression_slot2');

-- slot advance with physical slot, error with non-reserved slot
SELECT slot_name FROM pg_create_physical_replication_slot('regression_slot3');
SELECT pg_replication_slot_advance('regression_slot3', '0/0'); -- invalid LSN
SELECT pg_replication_slot_advance('regression_slot3', '0/1'); -- error
SELECT pg_drop_replication_slot('regression_slot3');
