INSERT INTO version (table_name, table_version) values ('acc','5');
CREATE TABLE acc (
    id INTEGER PRIMARY KEY NOT NULL,
    method VARCHAR(16) DEFAULT '' NOT NULL,
    from_tag VARCHAR(64) DEFAULT '' NOT NULL,
    to_tag VARCHAR(64) DEFAULT '' NOT NULL,
    callid VARCHAR(255) DEFAULT '' NOT NULL,
    sip_code VARCHAR(3) DEFAULT '' NOT NULL,
    sip_reason VARCHAR(128) DEFAULT '' NOT NULL,
    time TIMESTAMP WITHOUT TIME ZONE NOT NULL
);

CREATE INDEX acc_callid_idx ON acc (callid);

INSERT INTO version (table_name, table_version) values ('acc_cdrs','1');
CREATE TABLE acc_cdrs (
    id INTEGER PRIMARY KEY NOT NULL,
    start_time VARCHAR(32) DEFAULT '' NOT NULL,
    end_time VARCHAR(32) DEFAULT '' NOT NULL,
    duration VARCHAR(32) DEFAULT '' NOT NULL
);

CREATE INDEX acc_cdrs_start_time_idx ON acc_cdrs (start_time);

INSERT INTO version (table_name, table_version) values ('missed_calls','4');
CREATE TABLE missed_calls (
    id INTEGER PRIMARY KEY NOT NULL,
    method VARCHAR(16) DEFAULT '' NOT NULL,
    from_tag VARCHAR(64) DEFAULT '' NOT NULL,
    to_tag VARCHAR(64) DEFAULT '' NOT NULL,
    callid VARCHAR(255) DEFAULT '' NOT NULL,
    sip_code VARCHAR(3) DEFAULT '' NOT NULL,
    sip_reason VARCHAR(128) DEFAULT '' NOT NULL,
    time TIMESTAMP WITHOUT TIME ZONE NOT NULL
);

CREATE INDEX missed_calls_callid_idx ON missed_calls (callid);

