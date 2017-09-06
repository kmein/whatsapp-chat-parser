#!/usr/bin/env python3
from collections import namedtuple
import sqlite3
import time

TABLE_NAME = "messages"

Message = namedtuple("Message", ["date_time", "user", "message"])

def read_db(path):
    with sqlite3.connect(path) as conn:
        c = conn.cursor()
        for (date_time, user, message) in c.execute("SELECT date_time, user, message FROM {table} ORDER BY date_time".format(table=TABLE_NAME)):
            yield Message(time.strptime(date_time, "%Y-%m-%d %H:%M:%S"), user, message)
    # not committing any changes here, so no need to 'commmit'
