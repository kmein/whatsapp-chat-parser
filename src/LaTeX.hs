{-# LANGUAGE OverloadedStrings #-}
module LaTeX where

import Chat

import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import Text.LaTeX.Base
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Syntax

escape :: Text.Text -> Text.Text
escape =
    Text.replace "#" "\\#" .
    Text.replace "_" "\\_{}" .
    Text.replace "\\" "\\textbackslash{}"

latexMessage :: Direction -> Message -> Text.Text
latexMessage Incoming msg = "\\incoming{" <> Text.pack (show $ dateTime msg) <> " · " <> user msg <> "}{" <> escape (message msg) <> "}\n"
latexMessage Outgoing msg = "\\outgoing{" <> user msg <> " · " <> Text.pack (show $ dateTime msg) <> "}{" <> escape (message msg) <> "}\n"

latexChat :: Maybe (Color, Color) -> [(Direction, Message)] -> Text.Text
latexChat _ msgs  = preamble <> foldMap (uncurry latexMessage) msgs <> "\\end{document}"
  where
    preamble =
        "\\documentclass{scrartcl}\
\\\usepackage[T1]{fontenc} \\usepackage[utf8]{inputenc} \\usepackage[many]{tcolorbox} \\usepackage{fontspec}\
\\\definecolor{incoming}{HTML}{FFEDAA} \\definecolor{outgoing}{HTML}{4C688B}\
\\\setmainfont{OpenSansEmoji}\
\\\newtcbox{incoming}[1]{ enhanced, interior code={},\
\  frame code={\
\    \\coordinate (aux1) at ([shift={(-5pt,0pt)}]frame.south west);\
\    \\filldraw[incoming,draw=black]\
\      (aux1) to\
\      ([shift={(0pt,\\BoxP)}]frame.south west) --\
\      ([shift={(\\BoxP,0pt)}]frame.south west) arc(270:230:\\BoxP)\
\      to (aux1);\
\    \\filldraw[incoming,draw=black]\
\      ([shift={(0pt,\\BoxP)}]frame.south west) --\
\      ([shift={(0pt,-\\BoxP)}]frame.north west) to[out=90,in=180]\
\      ([shift={(\\BoxP,0pt)}]frame.north west) --\
\      ([shift={(-\\BoxP,0pt)}]frame.north east) to[out=0,in=90]\
\      ([shift={(0pt,-\\BoxP)}]frame.north east) --\
\      ([shift={(0pt,\\BoxP)}]frame.south east) to[out=-90,in=0]\
\      ([shift={(-\\BoxP,0pt)}]frame.south east) --\
\      ([shift={(\\BoxP,0pt)}]frame.south west) arc(270:230:\\BoxP)\
\      ([shift={(0pt,\\BoxP)}]frame.south west); },\
\  width=10cm, bottom=0pt, capture=minipage,\
\  attach boxed title to top left, coltitle=black,\
\  boxed title style={size=small,frame empty,interior empty},\
\  title=#1, left=2pt, right=2pt }\
\\\def\\BoxP{8pt}\
\\\newtcbox{outgoing}[1]{ enhanced, interior code={},\
\  frame code={\
\    \\coordinate (aux1) at ([shift={(5pt,0pt)}]frame.south east);\
\    \\filldraw[outgoing,draw=black]\
\      (aux1) to\
\      ([shift={(0pt,\\BoxP)}]frame.south east) -- \
\      ([shift={(-\\BoxP,0pt)}]frame.south east) arc(270:310:\\BoxP)\
\      to (aux1);\
\    \\filldraw[outgoing,draw=black]\
\      ([shift={(0pt,\\BoxP)}]frame.south east) --\
\      ([shift={(0pt,-\\BoxP)}]frame.north east) to[out=90,in=0]\
\      ([shift={(-\\BoxP,0pt)}]frame.north east) --\
\      ([shift={(\\BoxP,0pt)}]frame.north west) to[out=180,in=90]\
\      ([shift={(0pt,-\\BoxP)}]frame.north west) --\
\      ([shift={(0pt,\\BoxP)}]frame.south west) to[out=-90,in=180]\
\      ([shift={(\\BoxP,0pt)}]frame.south west) --\
\      ([shift={(-\\BoxP,0pt)}]frame.south east) arc(270:310:\\BoxP)\
\      ([shift={(0pt,\\BoxP)}]frame.south east); },\
\  width=10cm, bottom=0pt, capture=minipage, enlarge left by=2cm,\
\  attach boxed title to top right, coltitle=black,\
\  boxed title style={size=small,frame empty,interior empty},\
\  title=#1, left=2pt, right=2pt }\
\\\begin{document}"

-- latexMessage :: Direction -> Message -> LaTeX
-- latexMessage Incoming msg =
--     comm2
--         "incoming"
--         (texy $ Text.pack (show $ dateTime msg) <> " · " <> user msg)
--         (texy $ message msg)
-- latexMessage Outgoing msg =
--     comm2
--         "outgoing"
--         (texy $ user msg <> " · " <> Text.pack (show $ dateTime msg))
--         (texy $ message msg)
--
-- latexChat :: Maybe (Color, Color) -> [(Direction, Message)] -> LaTeX
-- latexChat cols msgs =
--     let (inColour, outColour) = fromMaybe ("FFEDAA", "4C688B") cols
--     in documentclass [] "scrartcl" <> usepackage ["many"] "tcolorbox" <>
--        comm3 definecolor inc html (texy inColour) <>
--        comm3 definecolor outg html (texy outColour) <>
--        TeXComm newtcbox [FixArg inc, OptArg (texy one), FixArg incoming] <>
--        TeXComm newtcbox [FixArg outg, OptArg (texy one), FixArg outgoing] <>
--        document (foldMap (uncurry latexMessage) msgs)
--   where
--     inc = "incoming" :: LaTeX
--     outg = "outgoing" :: LaTeX
--     definecolor = "definecolor" :: String
--     newtcbox = "newtcbox" :: String
--     html = "HTML" :: LaTeX
--     one = "1" :: Text.Text
--     properties =
--         "enhanced,interior code={},width=10cm,bottom=0pt,capture=minipage,fontupper=\\sffamily,coltitle=black,fonttitle=\\sffamily,boxed title style={size=small,frame empty,interior empty},title=#1,left=2pt,right=2pt"
--     outgoing =
--         properties <>
--         ",attach boxed title to top right,frame code={\
--         \\\coordinate (aux1) at ([shift={(5pt,0pt)}]frame.south east);\
--         \\\filldraw[outgoing,draw=black]\
--             \(aux1) to\
--             \([shift={(0pt,8pt)}]frame.south east) --\
--             \([shift={(-8pt,0pt)}]frame.south east) arc(270:310:8pt)\
--             \to (aux1);\
--         \\\filldraw[outgoing,draw=black]\
--             \([shift={(0pt,8pt)}]frame.south east) --\
--             \([shift={(0pt,-8pt)}]frame.north east) to[out=90,in=0]\
--             \([shift={(-8pt,0pt)}]frame.north east) --\
--             \([shift={(8pt,0pt)}]frame.north west) to[out=180,in=90]\
--             \([shift={(0pt,-8pt)}]frame.north west) --\
--             \([shift={(0pt,8pt)}]frame.south west) to[out=-90,in=180]\
--             \([shift={(8pt,0pt)}]frame.south west) --\
--             \([shift={(-8pt,0pt)}]frame.south east) arc(270:310:8pt)\
--             \([shift={(0pt,8pt)}]frame.south east);}" :: LaTeX
--     incoming =
--         properties <>
--         ",attach boxed title to top left,frame code={\
--             \\\coordinate (aux1) at ([shift={(-5pt,0pt)}]frame.south west);\
--             \\\filldraw[incoming,draw=black]\
--             \(aux1) to\
--             \([shift={(0pt,8pt)}]frame.south west) --\
--             \([shift={(8pt,0pt)}]frame.south west) arc(270:230:8pt)\
--             \to (aux1);\
--             \\\filldraw[incoming,draw=black]\
--             \([shift={(0pt,8pt)}]frame.south west) --\
--             \([shift={(0pt,-8pt)}]frame.north west) to[out=90,in=180]\
--             \([shift={(8pt,0pt)}]frame.north west) --\
--             \([shift={(-8pt,0pt)}]frame.north east) to[out=0,in=90]\
--             \([shift={(0pt,-8pt)}]frame.north east) --\
--             \([shift={(0pt,8pt)}]frame.south east) to[out=-90,in=0]\
--             \([shift={(-8pt,0pt)}]frame.south east) --\
--             \([shift={(8pt,0pt)}]frame.south west) arc(270:230:8pt)\
--             \([shift={(0pt,8pt)}]frame.south west);}" :: LaTeX
--
--
