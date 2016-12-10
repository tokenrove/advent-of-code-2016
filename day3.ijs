#!/usr/bin/env ijconsole
NB. -*- j -*-
ab=.(+./ ARGV = <'--initial')
input =: }. ".;._2 LF,stdin ''
f =. 3 : '0 < <./ +`-/"1 (i.!3) A. y'
g =. 3 : ',.&.|: (|:"2 i.(9%~*/$y),3 3) { ,y'
echo +/ f"1 (g`]@.ab) input
exit''
