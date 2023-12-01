(ql:quickload :cl-ppcre)
(cl-ppcre:split "a" "abababa" :omit-unmatched-p t)
