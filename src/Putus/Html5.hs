module Putus.Html5 where

import Putus.Core
import Putus.Dom
import Putus.JSFFI


-- |=Elements

div_ :: Component m => m () -> m ()
div_ = wrapRawElement js_document_createElement_div

h1_ :: Component m => m () -> m ()
h1_ = wrapRawElement js_document_createElement_h1

h2_ :: Component m => m () -> m ()
h2_ = wrapRawElement js_document_createElement_h2

h3_ :: Component m => m () -> m ()
h3_ = wrapRawElement js_document_createElement_h3

h4_ :: Component m => m () -> m ()
h4_ = wrapRawElement js_document_createElement_h4

h5_ :: Component m => m () -> m ()
h5_ = wrapRawElement js_document_createElement_h5

p_ :: Component m => m () -> m ()
p_ = wrapRawElement js_document_createElement_p

small_ :: Component m => m () -> m ()
small_ = wrapRawElement js_document_createElement_small

span_ :: Component m => m () -> m ()
span_ = wrapRawElement js_document_createElement_span

button_ :: Component m => m () -> m ()
button_ = wrapRawElement js_document_createElement_button

input_ :: Component m => m () -> m ()
input_ = wrapRawElement js_document_createElement_input

header_ :: Component m => m () -> m ()
header_ = wrapRawElement js_document_createElement_header

main_ :: Component m => m () -> m ()
main_ = wrapRawElement js_document_createElement_main

section_ :: Component m => m () -> m ()
section_ = wrapRawElement js_document_createElement_section

progress_ :: Component m => m () -> m ()
progress_ = wrapRawElement js_document_createElement_progress

ul_ :: Component m => m () -> m ()
ul_ = wrapRawElement js_document_createElement_ul

li_ :: Component m => m () -> m ()
li_ = wrapRawElement js_document_createElement_li

hgroup_ :: Component m => m () -> m ()
hgroup_ = wrapRawElement js_document_createElement_hgroup

table_ :: Component m => m () -> m ()
table_ = wrapRawElement js_document_createElement_table

tbody_ :: Component m => m () -> m ()
tbody_ = wrapRawElement js_document_createElement_tbody

tr_ :: Component m => m () -> m ()
tr_ = wrapRawElement js_document_createElement_tr

td_ :: Component m => m () -> m ()
td_ = wrapRawElement js_document_createElement_td

a_ :: Component m => m () -> m ()
a_ = wrapRawElement js_document_createElement_a

-- | =Attributes

class_ :: Component m => String -> m ()
class_ = setAttribute "class"

id_ :: Component m => String -> m ()
id_ = setAttribute "id"

type_ :: Component m => String -> m ()
type_ = setAttribute "type"

role_ :: Component m => String -> m ()
role_ = setAttribute "role"
