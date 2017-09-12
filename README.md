# WhatsApp Chat Parser

*the most creatively named tool for parsing WhatsApp chats*

The tool consists (at least right now) of two components
* `whatsapp2db` which is written in Haskell and converts a WhatsApp chat text
  file into an SQLite3 database. You can obtain the text file by WhatsApp-emailing the
  chat you want to convert and
* `db2pdf` which is written in Python and reads out the SQLite3 database to
  generate a PDF using [FPDF](https://github.com/reingart/pyfpdf).

## Usage

To compile the Haskell project, you ideally have to have
[Stack](haskellstack.org) installed and run `stack install` in the whatsapp2db
directory. You run it via

```bash
whatsapp-to-db [TEXT FILE] [DB FILE]
```

then, you can convert the resulting DB file into PDF by running (in the db2pdf
directory)

```bash
python3 db2pdf.py [DB FILE] [PDF FILE]
```

to which you can optionally add an SQL query (via the `--query` parameter) like

```sql
WHERE strftime('%Y', date_time) = '2015'
```

and a WhatsApp username which generates the PDF from that user's perspective
(via the `--perspective` parameter).

## Limitations
* the tool doesn't support Emoji yet (it uses a font which does, but FPDF
  doesn't)
