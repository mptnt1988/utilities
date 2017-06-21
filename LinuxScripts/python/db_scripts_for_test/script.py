#!/usr/bin/env python
import ConfigParser
import sys
import os.path
import getopt
from collections import OrderedDict
from sqlalchemy import *
from sqlalchemy_utils import database_exists, create_database, drop_database


def check_config(config):
    # Check section 'database'
    if not config.has_section('database'):
        print "Section [database] must exist."
        sys.exit(1)
    # Check options of section 'database'
    expectedOptions = ['dbuser', 'dbpwd', 'dbname', 'dbtables']
    for opt in expectedOptions:
        if not config.has_option('database', opt):
            print "Option \'" + opt + "\' must exist."
            sys.exit(1)
    # Check tables specified in 'database.dbtables'
    for tbl in config.get('database', 'dbtables').split('\n')[-1].split(','):
        mandatoryOpts = ['tbname', 'tbcolumn']
        if not config.has_section(tbl):
            print "Table '" + tbl + "' declared without being defined."
            sys.exit(1)
        elif not (config.options(tbl) and
                  all(opt in config.options(tbl) for opt in mandatoryOpts)):
            print "Table '" + tbl + "' declared but has not all mandatory options" + \
                "('tbname', 'tbcolumn')."
            sys.exit(1)
        elif not all(config.get(tbl, opt) for opt in mandatoryOpts):
            print "Table '" + tbl + "' must have value for all mandatory options" + \
                "('tbname', 'tbcolumn')."
            sys.exit(1)


def get_config(config):
    loaded_config = {}
    # 'database' field includes:
    # 'dbtype', 'dbhost', 'dpport', 'dbuser', 'dbpwd', 'dbname', 'dbtables'
    dbinfo = {'dbtype': None,
              'dbhost': None,
              'dbport': None}
    for opt in config.options('database'):
        dbinfo[opt] = config.get('database', opt)
    dbinfo['dbtype'] = dbinfo['dbtype'] or 'mysql'
    dbinfo['dbhost'] = dbinfo['dbhost'] or 'localhost'
    dbinfo['dbport'] = dbinfo['dbport'] or '3306'
    loaded_config['database'] = dbinfo
    # 'tables' field includes:
    # tables in which each one has 'tbname' and 'tbcolumns'
    tables = []
    for tbl in config.get('database', 'dbtables').split('\n')[-1].split(','):
        tblElem = {}
        tblElem['tbname'] = config.get(tbl, 'tbname')
        tblElem['tbcolumns'] = config.get(tbl, 'tbcolumn').split('\n')
        tables.append(tblElem)
    loaded_config['tables'] = tables
    # 'sql' field includes:
    # SQL queries to be executed
    sqls = []
    if config.has_section('sqls') and config.options('sqls'):
        for q in config.get('sqls', 'sql').split('\n'):
            qInfo = q.split(',')
            qElem = {}
            qElem['table'] = qInfo[0]
            qElem['data'] = {}
            for col in qInfo[1:]:
                splittedCol = col.split('=')
                qElem['data'][splittedCol[0]] = splittedCol[1]
            sqls.append(qElem)
    loaded_config['sqls'] = sqls
    return loaded_config


def recreate_schema(dbInfo, tablesInfo):
    engineUrl = dbInfo['dbtype'] + "://" + dbInfo['dbuser'] + ":" + dbInfo['dbpwd'] + \
                "@" + dbInfo['dbhost'] + ":" + dbInfo['dbport'] + "/" + dbInfo['dbname']
    engine = create_engine(engineUrl, echo=False)
    if database_exists(engine.url):
        drop_database(engine.url)
    create_database(engine.url)
    metadata = MetaData()
    for tbl in tablesInfo:
        Table(tbl['tbname'], metadata,
              *map(lambda x: eval("Column(" + x + ")"), tbl['tbcolumns']))
    metadata.create_all(engine)
    return engine


def load_config(filename):
    if not os.path.isfile(filename):
        print("No such file")
        sys.exit(1)

    class MultiOrderedDict(OrderedDict):
        def __setitem__(self, key, value):
            if isinstance(value, list) and key in self:
                self[key].extend(value)
            else:
                OrderedDict.__setitem__(self, key, value)

    config = ConfigParser.RawConfigParser(dict_type=MultiOrderedDict)
    config.read(filename)
    return config


def insert_data(engine, sqls):
    with engine.connect() as con:
        for sql in sqls:
            con.execute("INSERT INTO " + sql['table'] +
                        " (" + ','.join(sql['data'].keys()) +
                        ") VALUES (" + ','.join(sql['data'].values()) + ")")


def drop_schema(filename):
    config = load_config(filename)
    check_config(config)
    fullConfig = get_config(config)
    dbInfo = fullConfig['database']
    engineUrl = dbInfo['dbtype'] + "://" + dbInfo['dbuser'] + ":" + dbInfo['dbpwd'] + \
        "@" + dbInfo['dbhost'] + ":" + dbInfo['dbport'] + "/" + dbInfo['dbname']
    engine = create_engine(engineUrl, echo=False)
    if database_exists(engine.url):
        drop_database(engine.url)


def print_color(text):
    print '\033[31m' + text + '\033[0m'


def print_help(scriptFile):
    print_color("***USAGE***")
    print "\t-h/--help"
    print "\t-d/--drop"


if __name__ == "__main__":
    try:
        opts, args = getopt.getopt(sys.argv[1:], "hd:", ["help", "drop="])
    except getopt.GetoptError, err:
        print "Error while parsing arguments..."
        sys.exit(2)
    for opt, arg in opts:
        if opt in ("-h", "--help"):
            print_help()
            sys.exit()
        if opt in ("-d"):
            drop_schema(arg)
            sys.exit()
    config = load_config(args[0])
    check_config(config)
    fullConfig = get_config(config)
    engine = recreate_schema(fullConfig['database'], fullConfig['tables'])
    if 'sqls' in fullConfig:
        insert_data(engine, fullConfig['sqls'])
