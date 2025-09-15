{-# LANGUAGE MultilineStrings #-}
module Quest_16_input where
import Parser
import Control.Applicative (some, (<|>))
import Data.List (nub, transpose)
import Data.Char
import Data.Either

symbol0 = read <$> natural

symbol1 :: Parser Char Integer Char
symbol1 = satisfy (\x -> x /= '\n')

sep0 = char ','
sep1 = char ' '
sep2 = char '\n'

type Turns = [Int]
type Wheel = [[Char]]

turns :: Parser Char Integer Turns
turns = sepBy symbol0 sep0

wheel :: Parser Char Integer Wheel
wheel = sepBy (exact 3 symbol1) sep1

wheel2 :: Parser Char Integer Wheel
wheel2 = sepBy (symbol1 <* symbol1 <:> symbol1 <:> pure []) sep1

final :: Parser Char Integer (Turns, [Wheel])
final = (\(t, w) -> (t, map (filter $ all (/= ' ')) $ transpose w)) <$> (turns <* sep2 <* sep2 <~> sepBy wheel sep2)

final2 :: Parser Char Integer (Turns, [Wheel])
final2 = (\(t, w) -> (t, map (filter $ all (/= ' ')) $ transpose w)) <$> (turns <* sep2 <* sep2 <~> sepBy wheel2 sep2)


run part = parse (final >>= (return . part))
run2 part = parse (final2 >>= (return . part))

-- ================== INPUTS ====================
example1 = """
1,2,3

^_^ -.- ^,-
>.- ^_^ >.<
-_- -.- >.<
    -.^ ^_^
    >.>    
"""

example2 = """
1,2,3

^_^ -.- ^,-
>.- ^_^ >.<
-_- -.- ^.^
    -.^ >.<
    >.>    
"""

input1 = """
10,3,11,20

-,> -,> -,> -,>
-,- >_< >.* -:^
<_- >.* -,> ^.<
>.* <_- -:^ -:^
*.- <_- -,> >.*
-,> ^,* >.* *.-
-:^ -,- -,> ^,*
>.* -:^ ^,* -,-
^.< -:^ ^,* <.>
^,* >_< >_< -,>
    *.- ^,*    
    >_< <_-    
    >.* *.-    
        <_-    
        <.>    
        -,-    
        *.-    
        *.-    
        -,-    
        ^,*    
        -,>    
        >.*    
        -:^    
        -,>    
"""

input2 = """
47,71,43,61,79,67,73,59,83,53

>.^ Q,T ),[ &:% $,I %.- 0_~ #,\\ o;X X_]
~;@ Q.= Q_X |.S P_{ Y,+ $;X &,@ I,\\ *:+
<:< };$ >:] |.< ~.Q |;$ U;0 `.~ S;I o,[
|:* ):( #." U:< $,= T;{ X.> }_o ].= '_'
P;0 \\.o @;( (_< [_Y P,/ [:# Y:- x;Q ).'
{.@ |:\\ &:- #;/ );* G,) }.T x;U T_* [_+
#:' |,] X,< /.0 P:& @_< },0 %.+ (.G P.)
";< &;+ %;\\ ):% #_% I:+ ]:I >:T x.< x:#
].+ O;} ]:Q );[ ";\\ @," ^,' ],= '.+ ~,@
%.- @_U T,/ '_% &.) G,Y }.< S_) (:Y ^.>
(,| P,P -:* P,< ]:# )_& -.+ o;< (_I U.X
^:~ -_" [,< 0,@ >;- *_x ~_' ):- I;T /_X
O.$ T_' $:I };X 0;] *_P X,I o,S $.~ S,Y
\\;= S_' Q_o X,# ".` {.] &./ ]_G U.) ~_'
&,Y Q,) `;` ],] `.$ $,| >.) o_- ",I `:\\
0_Y }.O 0;> I.{ |;) @,\\ ^:% *;( ';* ~_(
I_- U;# P;^ +.o )_' U,- ";S /,$ *_x ',}
T,I ~.) =;\\ %_- \\;& `:> >,I %,~ %,& /,#
I:o &_\\ /;% O;} [:P I,U ]_@ ';$ X,O T;G
<:" P;~ 0:- 0;o P./ T_) x:~ (:/ ^,\\ o_Q
    Q_+ G;= \\;` },X U,0 ]_= '.'     0:P
    #,% ".& [:@ ]:< ":X x:I >.U     Q;~
    `:{ #,# }_U <;" 0.$ I.' (;}     -:+
    T,G >.) ";( \\:S %;' }.I o_\\     =:^
    x.) |,T }.> &,I *_+ =;U $_*     {,I
    @:0 G;Y x,G         =;$ +:/     ';^
    `,o >;^ =_`         Y;' @,|     ^:`
    ).- /;I Q,{         o.x I_"     '_Q
    >,+ `_- Q.\\         $:| <,0     #;[
    {_^ =_' "_/         \\,\\ S:=     |,[
    (:%     &:@             |_'     ];o
    X:0     T.U             X.[     T.%
    0:+     0_+             `.P     ),*
    S:<     (_]             X_{     ^.&
    0.\\     ).%             ]:S     O,*
    #,&     0.+                        
    -,o     ~:<                        
    P.[     -,U                        
    ":x     P;X                        
            >_(                        
"""

input3 = """
47,53,61,59,43

o:- -.$ -:> $.* -:-
>.< -:> >.* $:$ -:=
$:= o:$ -:< -.^ ^.<
-:* *:< >.* $:< >.>
$.= -.o $:* *.* o:^
^:o =.> =.o <:> o:<
-:= >:* <:* *.o *:^
=:= =:< =.^ ^.> >:-
>.- ^.= o.^ ^:= o.>
<:- ^.> -.> =.< >:*
^.* ^.$ <.- o.$ =:=
^:= $.o <.* $.- =.<
=.= >:$ $:$ <:* >:<
^.< o.* $.$ -:> -:>
>:* <.< *:< ^.= ^.*
^:^ ^:< *:^ =:o >.<
*.> >.$ ^:= >:< <:=
o.= ^.$ *:- >.$ o:<
>:> *:$ =:< o.* o:*
=:^ >:o o:^ *.^ -.=
o.^ ^:- $:^ o.* *:>
*.o $:< -.o ^:= ^.$
o.^ ^:= *.< -.= >.$
>:= $.$ =.^ >:^ <.$
-:> -.> <.> o:= =.^
^:< =:* <.- $.> *:>
^.^ o:* o:$ -:< ^.o
o.* ^:- -:* <:^ *.$
<.< *.o $:= ^.= >.^
=:o ^.o <:* ^.^ $.^
-.* ^:$ *:^     ^.=
>.> <:o ^.o     -.^
$:$ <.- -.^     -.-
^.= >.- ^.>     o:$
=:< <.* -.=     $.*
<.o $:o o.=        
>:= =.^ $.o        
=.$ >.> <.-        
*.$ -.^ ^.-        
=:$ <.- *:o        
^.= =.$            
^.* -.o            
$.* ^.o            
o.- >.$            
$:> $.-            
$:*                
^:o                
$:*                
=:=                
^:*                
"""
