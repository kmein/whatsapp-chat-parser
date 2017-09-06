#!/usr/bin/env python3
from collections import namedtuple
import sqlite3
import string
import time

TABLE_NAME = "messages"

Message = namedtuple("Message", ["date_time", "user", "message"])

def latex_escape(text):
    return text.replace("\\", "\\textbackslash{}").replace("#", "\\#").replace("$", "\\$"
            ).replace("%", "\\%").replace("^", "\\^{}").replace("&", "\\&"
                    ).replace("{", "\\{").replace("}", "\\}").replace("~", "\\~{}"
                            ).replace("_", "\\_{}")

def latex_message(current_user, msg):
    timestamp = time.strftime("%Y-%m-%d %H:%M", msg.date_time)
    msg_body = latex_escape(msg.message)
    if msg.user == current_user:
        return "\\outgoing{{{title}}}{{{body}}}".format(
                title=msg.user + " · " + timestamp,
                body=msg_body)
    else:
        return "\\incoming{{{title}}}{{{body}}}".format(
                title=timestamp + " · " + msg.user,
                body=msg_body)

def read_db(path):
    with sqlite3.connect(path) as conn:
        c = conn.cursor()
        for (date_time, user, message) in c.execute("SELECT date_time, user, message FROM {table} ORDER BY date_time".format(table=TABLE_NAME)):
            yield Message(time.strptime(date_time, "%Y-%m-%d %H:%M:%S"), user, message)
    # not committing any changes here, so no need to 'commmit'

latex = string.Template(r"""
\documentclass{scrartcl}
\usepackage{fontspec}
\usepackage[many]{tcolorbox}

\definecolor{incoming}{HTML}{$incoming_colour}
\definecolor{outgoing}{HTML}{$outgoing_colour}

\setmainfont{$mainfont}

\newtcbox{incoming}[1]{
    enhanced,
    interior code={},
    frame code={
        \coordinate (aux1) at ([shift={(-5pt,0pt)}]frame.south west);
        \filldraw[incoming,draw=black]
            (aux1) to
            ([shift={(0pt,\BoxP)}]frame.south west) --
            ([shift={(\BoxP,0pt)}]frame.south west) arc(270:230:\BoxP)
            to (aux1);
        \filldraw[incoming,draw=black]
            ([shift={(0pt,\BoxP)}]frame.south west) --
            ([shift={(0pt,-\BoxP)}]frame.north west) to[out=90,in=180]
            ([shift={(\BoxP,0pt)}]frame.north west) --
            ([shift={(-\BoxP,0pt)}]frame.north east) to[out=0,in=90]
            ([shift={(0pt,-\BoxP)}]frame.north east) --
            ([shift={(0pt,\BoxP)}]frame.south east) to[out=-90,in=0]
            ([shift={(-\BoxP,0pt)}]frame.south east) --
            ([shift={(\BoxP,0pt)}]frame.south west) arc(270:230:\BoxP)
            ([shift={(0pt,\BoxP)}]frame.south west);
    },
    width=10cm, bottom=0pt, left=2pt, right=2pt,
    coltitle=black,
    capture=minipage,
    attach boxed title to top left, title=#1,
    boxed title style={size=small,frame empty,interior empty}
}

\def\BoxP{8pt}

\newtcbox{outgoing}[1]{
    enhanced,
    interior code={},
    frame code={
        \coordinate (aux1) at ([shift={(5pt,0pt)}]frame.south east);
        \filldraw[outgoing,draw=black]
            (aux1) to
            ([shift={(0pt,\BoxP)}]frame.south east) --
            ([shift={(-\BoxP,0pt)}]frame.south east) arc(270:310:\BoxP)
            to (aux1);
        \filldraw[outgoing,draw=black]
            ([shift={(0pt,\BoxP)}]frame.south east) --
            ([shift={(0pt,-\BoxP)}]frame.north east) to[out=90,in=0]
            ([shift={(-\BoxP,0pt)}]frame.north east) --
            ([shift={(\BoxP,0pt)}]frame.north west) to[out=180,in=90]
            ([shift={(0pt,-\BoxP)}]frame.north west) --
            ([shift={(0pt,\BoxP)}]frame.south west) to[out=-90,in=180]
            ([shift={(\BoxP,0pt)}]frame.south west) --
            ([shift={(-\BoxP,0pt)}]frame.south east) arc(270:310:\BoxP)
            ([shift={(0pt,\BoxP)}]frame.south east);
    },
    width=10cm, bottom=0pt, left=2pt, right=2pt,
    coltitle=black,
    capture=minipage,
    attach boxed title to top right, title=#1
    boxed title style={size=small,frame empty,interior empty},
    enlarge left by=2cm,
}
\begin{document}$messages\end{document}""")

print(latex.substitute(
    messages="\n".join(latex_message("Leonie Kuhne", m) for m in read_db("../leonie.db")),
    mainfont="OpenSansEmoji",
    incoming_colour="FFEDAA",
    outgoing_colour="4C688B"))
