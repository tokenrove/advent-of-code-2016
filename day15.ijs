f =. 3 : '*/ 0 = (A+1+y+i.$A) |~ B [ A =. init [ B =. bases'
1 i.~ f"0 i.*/bases

1 i.~ f"0 i.*/bases [ init =. init,0 [ bases =. bases,11

NB. alternately, with CRT from http://code.jsoftware.com/wiki/Essays/Chinese_Remainder_Theorem
g0    =: , ,. =@i.@2:
it    =: {: ,: {. - {: * <.@%&{./
gcd   =: (}.@{.) @ (it^:(*@{.@{:)^:_) @ g0

assert=: 3 : 'assert. y'
ab    =: |.@(gcd/ * [ % +./)@(,&{.)
cr1   =: [: |/\ *.&{. , ,&{: +/ .* ab
chkc  =: [: assert ,&{: -: ,&{. | {:@cr1
cr    =: cr1 [ chkc
(9!:11) 9
cr/bases,.(bases|bases-init+1+i.$init)
