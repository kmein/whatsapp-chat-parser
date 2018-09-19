#!/usr/bin/env python3
import argparse
from collections import namedtuple
from fpdf import FPDF
from re import compile
from sqlite3 import connect
from sys import argv
from time import strptime, strftime

TABLE_NAME = "messages"

Message = namedtuple("Message", ["date_time", "user", "message"])

def hex2rgb(hexcode):
    return tuple(int(hexcode[i:i+2], 16) for i in (0, 2 ,4))

def read_db(path, query=""):
    with connect(path) as conn:
        c = conn.cursor()
        for (date_time, user, message) in c.execute("SELECT date_time, user, message FROM {table} {query} ORDER BY date_time".format(table=TABLE_NAME, query=query)):
            yield Message(strptime(date_time, "%Y-%m-%d %H:%M:%S"), user, message)
    # not committing any changes here, so no need to 'commmit'

def kill_emoji(text):
    emoji_pattern = compile("["
        "\U0001F600-\U0001F64F"  # emoticons
        "\U0001F300-\U0001F5FF"  # symbols & pictographs
        "\U0001F680-\U0001F6FF"  # transport & map symbols
        "\U0001F1E0-\U0001F1FF"  # flags (iOS)
        "]+")
    return emoji_pattern.sub(r'[?]', text)

def print_message(fpdf, args, message):
    timestamp = strftime("%Y-%m-%d %H:%M",message.date_time)
    body = kill_emoji(message.message).encode("latin-1", "ignore").decode("latin-1")
    if args.perspective == message.user: # outgoing
        fpdf.set_x(100)
        fpdf.set_font_size(8)
        fpdf.cell(85, 3, txt=message.user+" · "+timestamp,ln=1, align="R")
        fpdf.set_x(100)
        fpdf.set_font_size(10)
        fpdf.set_fill_color(*hex2rgb(args.outgoing))
        fpdf.set_text_color(*hex2rgb(args.fgcolour))
        fpdf.multi_cell(85, 5, txt=body, align="L", fill=True)
    else:
        fpdf.set_font_size(8)
        fpdf.cell(85, 3, txt=timestamp+" · "+message.user,ln=1, align="L")
        fpdf.set_font_size(10)
        fpdf.set_fill_color(*hex2rgb(args.incoming))
        fpdf.set_text_color(*hex2rgb(args.fgcolour))
        fpdf.multi_cell(85, 5, txt=body, align="L", fill=True)
    fpdf.set_y(fpdf.get_y() + 1)
    fpdf.set_text_color(0, 0, 0)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Typeset chats contained in a database")
    parser.add_argument("db", metavar="DB-PATH", type=str, help="an sqlite3 database")
    parser.add_argument("pdf", metavar="OUT-PATH", type=str, help="an output file")
    parser.add_argument("--perspective", metavar="USERNAME", type=str, help="your WhatsApp username")
    parser.add_argument("--query", metavar="SQL", nargs="?", type=str, help="an SQL query to filter chat messages")
    parser.add_argument("--incoming", metavar="HEXCODE", nargs="?", type=str, default="ffffff", help="colour of incoming messages")
    parser.add_argument("--outgoing", metavar="HEXCODE", nargs="?", type=str, default="dcf8c6", help="colour of outgoing messages")
    parser.add_argument("--fgcolour", metavar="HEXCODE", nargs="?", type=str, default="000000", help="message text colour")
    args = parser.parse_args()
    print(args)

    fpdf = FPDF()
    fpdf.add_font("OpenSansEmoji", fname="OpenSansEmoji.ttf", uni=True)
    fpdf.set_auto_page_break(True, margin=1)
    fpdf.add_page()
    fpdf.set_font("OpenSansEmoji", size=10)
    for msg in read_db(args.db, args.query):
        print_message(fpdf, args, msg)
    fpdf.output(args.pdf)
