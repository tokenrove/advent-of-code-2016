#!/usr/bin/env ijconsole
NB. -*- j -*-
ab =. <: (+./ ARGV = <'--initial') { 400000 40
in =: in #~ 2>'.^'i. in =: stdin ''
next_row =. 3 : 'r [ n =: n + +/r [ r =: 3 ({.={:);._3 (|.1,|.1,y)'
(next_row^:ab r) [ n =: +/r [ r =: '.'=in
echo n
exit''
