{-# LANGUAGE MultilineStrings #-}
module Quest_2_input where
import Parser
import Control.Applicative (some, (<|>))
import Data.List (nub, transpose, intersperse)
import Data.Char
import Data.Either

type Index = Int
type Symbol = (Int, Char)
type Commands = Either AddCommand SwapCommand
type AddCommand = (Index, (Symbol, Symbol))
type SwapCommand = Index

sep0 = char '\n'
sep1 = char ' '
equals = char '='

nat :: Parser Char Int Int
nat  = read <$> natural

letter :: Parser Char Int Char
letter = satisfy (\x -> True)

index = string "id" *> equals *> nat
symbol str = string str *> equals *> ((char '[' *> nat <* char ',') <~> (letter <* char ']'))

addCommand :: Parser Char Int AddCommand
addCommand = (string "ADD" *> sep1 *> (index <* sep1) <~> (symbol "left" <* sep1 <~> symbol "right"))

swapCommand = string "SWAP" <* sep1 *> nat

command :: Parser Char Int Commands
command = addCommand <||> swapCommand

final :: Parser Char Int [Commands]
final = sepBy command sep0 <* eof

run part = parse (final >>= (return . part))

exportCmd (Left (id, ((k1, v1), (k2, v2)))) = concat $ intersperse " " ["0", show id, show k1, [v1], show k2, [v2]]
exportCmd (Right id) = concat $ intersperse " " ["1", show id]

export out = writeFile out . concat . (\(x, _, _) -> map (\y -> (exportCmd y) ++ "\n") x) . fromRight ([], 0, []) . parse final

-- ================== INPUTS ====================
example1 = """
ADD id=1 left=[10,A] right=[30,H]
ADD id=2 left=[15,D] right=[25,I]
ADD id=3 left=[12,F] right=[31,J]
ADD id=4 left=[5,B] right=[27,L]
ADD id=5 left=[3,C] right=[28,M]
ADD id=6 left=[20,G] right=[32,K]
ADD id=7 left=[4,E] right=[21,N]
"""

example2 = """
ADD id=1 left=[160,E] right=[175,S]
ADD id=2 left=[140,W] right=[224,D]
ADD id=3 left=[122,U] right=[203,F]
ADD id=4 left=[204,N] right=[114,G]
ADD id=5 left=[136,V] right=[256,H]
ADD id=6 left=[147,G] right=[192,O]
ADD id=7 left=[232,I] right=[154,K]
ADD id=8 left=[118,E] right=[125,Y]
ADD id=9 left=[102,A] right=[210,D]
ADD id=10 left=[183,Q] right=[254,E]
ADD id=11 left=[146,E] right=[148,C]
ADD id=12 left=[173,Y] right=[299,S]
ADD id=13 left=[190,B] right=[277,B]
ADD id=14 left=[124,T] right=[142,N]
ADD id=15 left=[153,R] right=[133,M]
ADD id=16 left=[252,D] right=[276,M]
ADD id=17 left=[258,I] right=[245,P]
ADD id=18 left=[117,O] right=[283,!]
ADD id=19 left=[212,O] right=[127,R]
ADD id=20 left=[278,A] right=[169,C]
"""

example3 = """
ADD id=1 left=[10,A] right=[30,H]
ADD id=2 left=[15,D] right=[25,I]
ADD id=3 left=[12,F] right=[31,J]
ADD id=4 left=[5,B] right=[27,L]
ADD id=5 left=[3,C] right=[28,M]
SWAP 1
SWAP 5
ADD id=6 left=[20,G] right=[32,K]
ADD id=7 left=[4,E] right=[21,N]
"""

example4 = """
ADD id=1 left=[10,A] right=[30,H]
ADD id=2 left=[15,D] right=[25,I]
ADD id=3 left=[12,F] right=[31,J]
ADD id=4 left=[5,B] right=[27,L]
ADD id=5 left=[3,C] right=[28,M]
SWAP 1
SWAP 5
ADD id=6 left=[20,G] right=[32,K]
ADD id=7 left=[4,E] right=[21,N]
SWAP 2
"""

example5 = """
ADD id=1 left=[10,A] right=[30,H]
ADD id=2 left=[15,D] right=[25,I]
ADD id=3 left=[12,F] right=[31,J]
ADD id=4 left=[5,B] right=[27,L]
ADD id=5 left=[3,C] right=[28,M]
SWAP 1
SWAP 5
ADD id=6 left=[20,G] right=[32,K]
ADD id=7 left=[4,E] right=[21,N]
SWAP 2
SWAP 5
"""

input1 = """
ADD id=1 left=[208,B] right=[200,M]
ADD id=2 left=[216,Y] right=[295,P]
ADD id=3 left=[250,T] right=[296,Y]
ADD id=4 left=[204,Z] right=[251,X]
ADD id=5 left=[244,M] right=[252,F]
ADD id=6 left=[269,N] right=[104,T]
ADD id=7 left=[247,K] right=[150,P]
ADD id=8 left=[253,!] right=[239,F]
ADD id=9 left=[151,X] right=[173,L]
ADD id=10 left=[183,P] right=[261,T]
ADD id=11 left=[273,P] right=[245,X]
ADD id=12 left=[182,U] right=[180,Y]
ADD id=13 left=[132,G] right=[157,F]
ADD id=14 left=[205,H] right=[286,Y]
ADD id=15 left=[139,Q] right=[221,S]
ADD id=16 left=[299,Y] right=[131,M]
ADD id=17 left=[177,Z] right=[246,M]
ADD id=18 left=[224,C] right=[129,N]
ADD id=19 left=[191,A] right=[193,W]
ADD id=20 left=[207,F] right=[140,X]
"""

input2 = """
ADD id=1 left=[384,G] right=[451,L]
ADD id=2 left=[443,F] right=[467,Y]
ADD id=3 left=[472,S] right=[468,W]
ADD id=4 left=[478,W] right=[447,L]
ADD id=5 left=[526,M] right=[358,H]
SWAP 1
ADD id=6 left=[349,Z] right=[442,J]
ADD id=7 left=[413,Y] right=[352,N]
ADD id=8 left=[365,J] right=[476,G]
ADD id=9 left=[449,R] right=[376,V]
ADD id=10 left=[465,Z] right=[466,L]
SWAP 4
ADD id=11 left=[555,P] right=[486,Z]
ADD id=12 left=[855,T] right=[379,X]
ADD id=13 left=[562,N] right=[496,R]
ADD id=14 left=[739,H] right=[632,M]
ADD id=15 left=[612,V] right=[554,L]
SWAP 8
ADD id=16 left=[665,S] right=[457,L]
ADD id=17 left=[640,U] right=[581,J]
ADD id=18 left=[335,Z] right=[548,V]
ADD id=19 left=[503,N] right=[656,B]
ADD id=20 left=[622,Z] right=[461,V]
SWAP 16
SWAP 17
ADD id=21 left=[505,L] right=[643,W]
ADD id=22 left=[328,J] right=[338,F]
ADD id=23 left=[373,B] right=[329,F]
ADD id=24 left=[563,S] right=[670,B]
ADD id=25 left=[677,A] right=[364,B]
SWAP 15
SWAP 13
ADD id=26 left=[506,G] right=[731,J]
ADD id=27 left=[491,V] right=[490,N]
ADD id=28 left=[597,R] right=[620,W]
ADD id=29 left=[458,X] right=[463,H]
ADD id=30 left=[647,Z] right=[605,T]
SWAP 8
SWAP 3
SWAP 17
ADD id=31 left=[570,P] right=[730,G]
ADD id=32 left=[529,V] right=[354,X]
ADD id=33 left=[374,F] right=[184,X]
ADD id=34 left=[115,P] right=[593,R]
ADD id=35 left=[716,M] right=[598,Y]
SWAP 32
SWAP 9
SWAP 8
ADD id=36 left=[584,P] right=[735,S]
ADD id=37 left=[559,T] right=[147,N]
ADD id=38 left=[596,Z] right=[214,Y]
ADD id=39 left=[218,W] right=[609,F]
ADD id=40 left=[615,S] right=[333,V]
SWAP 3
SWAP 22
SWAP 34
SWAP 38
ADD id=41 left=[144,W] right=[538,N]
ADD id=42 left=[475,T] right=[673,Y]
ADD id=43 left=[227,J] right=[614,S]
ADD id=44 left=[771,V] right=[703,M]
ADD id=45 left=[646,W] right=[368,R]
SWAP 27
SWAP 33
SWAP 18
SWAP 30
ADD id=46 left=[293,N] right=[708,F]
ADD id=47 left=[363,N] right=[493,T]
ADD id=48 left=[148,J] right=[100,W]
ADD id=49 left=[903,Z] right=[679,J]
ADD id=50 left=[282,L] right=[477,X]
SWAP 26
SWAP 18
SWAP 31
SWAP 2
SWAP 41
ADD id=51 left=[832,S] right=[811,G]
ADD id=52 left=[793,Y] right=[787,C]
ADD id=53 left=[809,V] right=[806,L]
ADD id=54 left=[131,G] right=[637,B]
ADD id=55 left=[355,T] right=[834,S]
SWAP 54
SWAP 53
SWAP 43
SWAP 27
SWAP 20
ADD id=56 left=[808,B] right=[839,S]
ADD id=57 left=[153,G] right=[540,P]
ADD id=58 left=[801,L] right=[802,J]
ADD id=59 left=[720,L] right=[819,G]
ADD id=60 left=[925,N] right=[450,Z]
SWAP 53
SWAP 18
SWAP 56
SWAP 4
SWAP 12
SWAP 48
ADD id=61 left=[308,J] right=[905,W]
ADD id=62 left=[306,W] right=[970,J]
ADD id=63 left=[729,V] right=[275,Y]
ADD id=64 left=[919,X] right=[918,T]
ADD id=65 left=[723,R] right=[370,P]
SWAP 52
SWAP 15
SWAP 13
SWAP 20
SWAP 10
SWAP 64
ADD id=66 left=[233,B] right=[822,L]
ADD id=67 left=[471,F] right=[813,X]
ADD id=68 left=[945,G] right=[636,Y]
ADD id=69 left=[712,M] right=[139,N]
ADD id=70 left=[281,S] right=[339,Q]
SWAP 21
SWAP 35
SWAP 2
SWAP 3
SWAP 56
SWAP 31
SWAP 11
ADD id=71 left=[671,K] right=[112,V]
ADD id=72 left=[953,M] right=[271,T]
ADD id=73 left=[666,H] right=[669,P]
ADD id=74 left=[347,L] right=[759,Z]
ADD id=75 left=[143,R] right=[375,X]
SWAP 15
SWAP 63
SWAP 60
SWAP 54
SWAP 11
SWAP 6
SWAP 7
ADD id=76 left=[519,R] right=[240,W]
ADD id=77 left=[247,L] right=[794,B]
ADD id=78 left=[910,F] right=[545,T]
ADD id=79 left=[667,M] right=[760,T]
ADD id=80 left=[278,Y] right=[631,W]
SWAP 57
SWAP 47
SWAP 7
SWAP 59
SWAP 67
SWAP 64
SWAP 44
SWAP 25
ADD id=81 left=[295,X] right=[332,Z]
ADD id=82 left=[914,Z] right=[344,S]
ADD id=83 left=[947,R] right=[590,S]
ADD id=84 left=[909,S] right=[595,X]
ADD id=85 left=[516,P] right=[587,Z]
SWAP 48
SWAP 23
SWAP 35
SWAP 27
SWAP 59
SWAP 1
SWAP 20
SWAP 6
ADD id=86 left=[715,W] right=[589,Z]
ADD id=87 left=[371,W] right=[346,T]
ADD id=88 left=[550,B] right=[690,P]
ADD id=89 left=[709,Z] right=[814,!]
ADD id=90 left=[140,J] right=[244,Y]
SWAP 22
SWAP 89
SWAP 52
SWAP 55
SWAP 58
SWAP 3
SWAP 85
SWAP 40
SWAP 25
ADD id=91 left=[943,M] right=[688,T]
ADD id=92 left=[561,J] right=[722,W]
ADD id=93 left=[133,V] right=[950,W]
ADD id=94 left=[800,Z] right=[795,P]
ADD id=95 left=[724,P] right=[573,V]
SWAP 24
SWAP 16
SWAP 68
SWAP 83
SWAP 75
SWAP 62
SWAP 52
SWAP 21
SWAP 8
ADD id=96 left=[113,V] right=[336,L]
ADD id=97 left=[361,R] right=[741,Z]
ADD id=98 left=[838,H] right=[156,S]
ADD id=99 left=[980,M] right=[784,S]
ADD id=100 left=[681,T] right=[272,S]
SWAP 16
SWAP 50
SWAP 70
SWAP 92
SWAP 48
SWAP 77
SWAP 54
SWAP 53
SWAP 37
SWAP 94
"""

input3 = """
ADD id=1 left=[161,H] right=[285,B]
ADD id=2 left=[805,B] right=[220,Y]
ADD id=3 left=[215,B] right=[266,N]
ADD id=4 left=[254,P] right=[250,M]
ADD id=5 left=[241,V] right=[225,G]
SWAP 3
ADD id=6 left=[251,L] right=[222,W]
ADD id=7 left=[268,N] right=[231,M]
ADD id=8 left=[262,G] right=[536,T]
ADD id=9 left=[244,R] right=[358,X]
ADD id=10 left=[434,T] right=[543,P]
SWAP 8
ADD id=11 left=[228,Y] right=[259,N]
ADD id=12 left=[243,N] right=[464,G]
ADD id=13 left=[269,K] right=[485,F]
ADD id=14 left=[240,C] right=[393,Y]
ADD id=15 left=[413,P] right=[263,R]
SWAP 14
ADD id=16 left=[537,Y] right=[519,W]
ADD id=17 left=[291,J] right=[554,F]
ADD id=18 left=[221,T] right=[151,B]
ADD id=19 left=[150,G] right=[129,Z]
ADD id=20 left=[684,V] right=[235,L]
SWAP 13
SWAP 16
ADD id=21 left=[619,Z] right=[277,V]
ADD id=22 left=[741,V] right=[589,W]
ADD id=23 left=[131,G] right=[412,S]
ADD id=24 left=[674,N] right=[716,S]
ADD id=25 left=[264,P] right=[310,Q]
SWAP 17
SWAP 10
ADD id=26 left=[726,L] right=[550,F]
ADD id=27 left=[576,S] right=[312,Z]
ADD id=28 left=[771,M] right=[593,Y]
ADD id=29 left=[587,G] right=[685,Y]
ADD id=30 left=[775,T] right=[670,R]
SWAP 1
SWAP 22
SWAP 6
ADD id=31 left=[559,M] right=[885,Y]
ADD id=32 left=[833,H] right=[583,T]
ADD id=33 left=[134,S] right=[472,Z]
ADD id=34 left=[386,T] right=[122,Z]
ADD id=35 left=[610,Z] right=[103,M]
SWAP 31
SWAP 34
SWAP 19
ADD id=36 left=[555,B] right=[439,F]
ADD id=37 left=[306,T] right=[112,X]
ADD id=38 left=[722,Z] right=[721,G]
ADD id=39 left=[869,M] right=[578,R]
ADD id=40 left=[866,Z] right=[733,X]
SWAP 25
SWAP 10
SWAP 32
SWAP 6
ADD id=41 left=[153,T] right=[513,L]
ADD id=42 left=[253,G] right=[425,R]
ADD id=43 left=[551,M] right=[429,Y]
ADD id=44 left=[917,F] right=[581,N]
ADD id=45 left=[432,U] right=[704,H]
SWAP 29
SWAP 41
SWAP 38
SWAP 26
ADD id=46 left=[421,M] right=[887,H]
ADD id=47 left=[397,L] right=[474,V]
ADD id=48 left=[556,H] right=[689,B]
ADD id=49 left=[760,S] right=[824,Y]
ADD id=50 left=[611,G] right=[703,J]
SWAP 42
SWAP 16
SWAP 34
SWAP 39
SWAP 27
ADD id=51 left=[831,R] right=[588,J]
ADD id=52 left=[734,S] right=[598,M]
ADD id=53 left=[288,R] right=[544,X]
ADD id=54 left=[428,W] right=[496,M]
ADD id=55 left=[318,B] right=[324,N]
SWAP 13
SWAP 49
SWAP 21
SWAP 51
SWAP 7
ADD id=56 left=[836,T] right=[102,B]
ADD id=57 left=[465,S] right=[517,J]
ADD id=58 left=[114,L] right=[283,G]
ADD id=59 left=[140,Y] right=[612,R]
ADD id=60 left=[676,X] right=[532,B]
SWAP 37
SWAP 44
SWAP 33
SWAP 25
SWAP 29
SWAP 54
ADD id=61 left=[829,G] right=[790,W]
ADD id=62 left=[423,V] right=[857,N]
ADD id=63 left=[815,B] right=[714,X]
ADD id=64 left=[223,L] right=[416,S]
ADD id=65 left=[878,P] right=[392,G]
SWAP 36
SWAP 3
SWAP 5
SWAP 46
SWAP 4
SWAP 45
ADD id=66 left=[328,V] right=[309,Y]
ADD id=67 left=[892,L] right=[699,P]
ADD id=68 left=[732,R] right=[752,T]
ADD id=69 left=[107,H] right=[592,R]
ADD id=70 left=[415,P] right=[132,G]
SWAP 47
SWAP 38
SWAP 25
SWAP 22
SWAP 30
SWAP 24
SWAP 37
ADD id=71 left=[144,T] right=[507,S]
ADD id=72 left=[889,F] right=[871,N]
ADD id=73 left=[602,B] right=[813,F]
ADD id=74 left=[772,X] right=[120,T]
ADD id=75 left=[590,W] right=[594,Z]
SWAP 66
SWAP 49
SWAP 63
SWAP 33
SWAP 67
SWAP 62
SWAP 50
ADD id=76 left=[303,L] right=[400,T]
ADD id=77 left=[666,V] right=[127,L]
ADD id=78 left=[314,W] right=[522,R]
ADD id=79 left=[354,X] right=[441,L]
ADD id=80 left=[746,F] right=[826,B]
SWAP 68
SWAP 72
SWAP 47
SWAP 67
SWAP 29
SWAP 70
SWAP 15
SWAP 24
ADD id=81 left=[512,F] right=[895,J]
ADD id=82 left=[651,L] right=[455,J]
ADD id=83 left=[329,J] right=[146,G]
ADD id=84 left=[345,H] right=[655,V]
ADD id=85 left=[898,H] right=[521,W]
SWAP 55
SWAP 26
SWAP 12
SWAP 5
SWAP 74
SWAP 54
SWAP 66
SWAP 70
ADD id=86 left=[447,W] right=[798,S]
ADD id=87 left=[436,W] right=[273,N]
ADD id=88 left=[873,B] right=[724,W]
ADD id=89 left=[409,F] right=[320,W]
ADD id=90 left=[528,V] right=[295,H]
SWAP 22
SWAP 5
SWAP 25
SWAP 57
SWAP 42
SWAP 11
SWAP 44
SWAP 32
SWAP 66
ADD id=91 left=[804,X] right=[884,Y]
ADD id=92 left=[743,H] right=[816,N]
ADD id=93 left=[274,M] right=[653,Z]
ADD id=94 left=[276,X] right=[448,T]
ADD id=95 left=[745,X] right=[778,B]
SWAP 32
SWAP 86
SWAP 6
SWAP 1
SWAP 62
SWAP 64
SWAP 10
SWAP 23
SWAP 20
ADD id=96 left=[709,W] right=[832,P]
ADD id=97 left=[361,X] right=[147,T]
ADD id=98 left=[856,S] right=[820,P]
ADD id=99 left=[323,N] right=[121,Y]
ADD id=100 left=[145,L] right=[141,W]
SWAP 43
SWAP 63
SWAP 66
SWAP 44
SWAP 62
SWAP 57
SWAP 17
SWAP 94
SWAP 29
SWAP 84
ADD id=101 left=[493,W] right=[753,Y]
ADD id=102 left=[671,W] right=[737,M]
ADD id=103 left=[565,F] right=[591,R]
ADD id=104 left=[130,H] right=[339,S]
ADD id=105 left=[687,Y] right=[181,B]
SWAP 98
SWAP 84
SWAP 10
SWAP 94
SWAP 103
SWAP 11
SWAP 96
SWAP 42
SWAP 72
SWAP 44
ADD id=106 left=[638,Z] right=[912,T]
ADD id=107 left=[901,X] right=[226,V]
ADD id=108 left=[846,W] right=[840,F]
ADD id=109 left=[982,W] right=[340,Y]
ADD id=110 left=[239,W] right=[197,S]
SWAP 91
SWAP 89
SWAP 100
SWAP 57
SWAP 1
SWAP 76
SWAP 58
SWAP 49
SWAP 45
SWAP 38
SWAP 79
ADD id=111 left=[909,G] right=[928,N]
ADD id=112 left=[927,Y] right=[603,L]
ADD id=113 left=[937,P] right=[255,H]
ADD id=114 left=[916,N] right=[375,F]
ADD id=115 left=[365,X] right=[623,T]
SWAP 24
SWAP 3
SWAP 97
SWAP 100
SWAP 61
SWAP 60
SWAP 108
SWAP 35
SWAP 72
SWAP 66
SWAP 67
ADD id=116 left=[648,G] right=[337,P]
ADD id=117 left=[971,X] right=[381,V]
ADD id=118 left=[530,Y] right=[948,W]
ADD id=119 left=[348,R] right=[756,M]
ADD id=120 left=[182,G] right=[769,X]
SWAP 111
SWAP 63
SWAP 34
SWAP 9
SWAP 55
SWAP 96
SWAP 100
SWAP 91
SWAP 24
SWAP 70
SWAP 43
SWAP 31
ADD id=121 left=[955,J] right=[506,M]
ADD id=122 left=[749,G] right=[990,B]
ADD id=123 left=[480,R] right=[478,G]
ADD id=124 left=[165,N] right=[597,Y]
ADD id=125 left=[290,S] right=[902,Z]
SWAP 19
SWAP 103
SWAP 62
SWAP 30
SWAP 47
SWAP 122
SWAP 75
SWAP 105
SWAP 4
SWAP 88
SWAP 27
SWAP 1
ADD id=126 left=[748,T] right=[725,R]
ADD id=127 left=[484,S] right=[459,R]
ADD id=128 left=[326,P] right=[998,S]
ADD id=129 left=[710,N] right=[794,Y]
ADD id=130 left=[445,A] right=[205,S]
SWAP 72
SWAP 9
SWAP 26
SWAP 62
SWAP 89
SWAP 39
SWAP 118
SWAP 46
SWAP 15
SWAP 52
SWAP 76
SWAP 75
SWAP 59
ADD id=131 left=[882,H] right=[941,Z]
ADD id=132 left=[768,V] right=[701,H]
ADD id=133 left=[964,X] right=[505,S]
ADD id=134 left=[353,W] right=[932,Y]
ADD id=135 left=[762,Z] right=[606,Y]
SWAP 69
SWAP 57
SWAP 104
SWAP 68
SWAP 80
SWAP 62
SWAP 54
SWAP 19
SWAP 111
SWAP 83
SWAP 44
SWAP 127
SWAP 97
ADD id=136 left=[643,N] right=[368,R]
ADD id=137 left=[503,S] right=[692,W]
ADD id=138 left=[979,N] right=[609,M]
ADD id=139 left=[914,V] right=[433,S]
ADD id=140 left=[289,W] right=[913,R]
SWAP 41
SWAP 129
SWAP 95
SWAP 121
SWAP 122
SWAP 111
SWAP 70
SWAP 131
SWAP 107
SWAP 16
SWAP 45
SWAP 103
SWAP 91
SWAP 56
ADD id=141 left=[463,N] right=[170,X]
ADD id=142 left=[136,W] right=[870,F]
ADD id=143 left=[229,Y] right=[529,Z]
ADD id=144 left=[256,P] right=[333,F]
ADD id=145 left=[190,Z] right=[278,V]
SWAP 127
SWAP 60
SWAP 76
SWAP 129
SWAP 104
SWAP 133
SWAP 142
SWAP 2
SWAP 131
SWAP 98
SWAP 7
SWAP 61
SWAP 92
SWAP 103
ADD id=146 left=[542,T] right=[675,R]
ADD id=147 left=[738,F] right=[376,Z]
ADD id=148 left=[586,M] right=[515,H]
ADD id=149 left=[647,G] right=[642,!]
ADD id=150 left=[535,G] right=[533,F]
SWAP 131
SWAP 65
SWAP 125
SWAP 50
SWAP 110
SWAP 122
SWAP 57
SWAP 129
SWAP 85
SWAP 8
SWAP 24
SWAP 90
SWAP 31
SWAP 45
SWAP 95
ADD id=151 left=[377,B] right=[370,M]
ADD id=152 left=[886,L] right=[575,B]
ADD id=153 left=[943,G] right=[184,R]
ADD id=154 left=[214,N] right=[953,H]
ADD id=155 left=[950,L] right=[681,W]
SWAP 111
SWAP 73
SWAP 4
SWAP 117
SWAP 63
SWAP 48
SWAP 104
SWAP 124
SWAP 103
SWAP 12
SWAP 65
SWAP 95
SWAP 34
SWAP 80
SWAP 147
ADD id=156 left=[751,S] right=[686,J]
ADD id=157 left=[549,R] right=[194,S]
ADD id=158 left=[109,G] right=[213,N]
ADD id=159 left=[780,N] right=[495,S]
ADD id=160 left=[545,F] right=[954,H]
SWAP 87
SWAP 10
SWAP 155
SWAP 105
SWAP 49
SWAP 47
SWAP 122
SWAP 19
SWAP 135
SWAP 68
SWAP 120
SWAP 127
SWAP 73
SWAP 50
SWAP 63
SWAP 99
ADD id=161 left=[200,N] right=[644,B]
ADD id=162 left=[387,P] right=[940,X]
ADD id=163 left=[627,R] right=[196,S]
ADD id=164 left=[848,L] right=[961,J]
ADD id=165 left=[350,N] right=[534,T]
SWAP 110
SWAP 101
SWAP 105
SWAP 104
SWAP 92
SWAP 3
SWAP 77
SWAP 117
SWAP 96
SWAP 144
SWAP 31
SWAP 108
SWAP 55
SWAP 164
SWAP 6
SWAP 43
ADD id=166 left=[117,X] right=[978,M]
ADD id=167 left=[922,Y] right=[994,F]
ADD id=168 left=[839,B] right=[763,V]
ADD id=169 left=[572,L] right=[705,G]
ADD id=170 left=[789,S] right=[427,V]
SWAP 109
SWAP 76
SWAP 116
SWAP 88
SWAP 84
SWAP 40
SWAP 41
SWAP 13
SWAP 71
SWAP 105
SWAP 2
SWAP 127
SWAP 128
SWAP 96
SWAP 22
SWAP 130
SWAP 151
ADD id=171 left=[209,J] right=[172,G]
ADD id=172 left=[395,G] right=[616,Y]
ADD id=173 left=[837,Z] right=[284,F]
ADD id=174 left=[767,M] right=[944,J]
ADD id=175 left=[731,H] right=[189,S]
SWAP 92
SWAP 51
SWAP 105
SWAP 114
SWAP 75
SWAP 64
SWAP 31
SWAP 46
SWAP 123
SWAP 62
SWAP 151
SWAP 30
SWAP 131
SWAP 45
SWAP 141
SWAP 139
SWAP 171
ADD id=176 left=[367,G] right=[615,Z]
ADD id=177 left=[384,Z] right=[124,L]
ADD id=178 left=[851,Z] right=[106,H]
ADD id=179 left=[852,F] right=[321,B]
ADD id=180 left=[877,T] right=[808,L]
SWAP 1
SWAP 158
SWAP 23
SWAP 78
SWAP 59
SWAP 25
SWAP 4
SWAP 113
SWAP 153
SWAP 21
SWAP 12
SWAP 30
SWAP 172
SWAP 168
SWAP 101
SWAP 108
SWAP 106
SWAP 93
ADD id=181 left=[442,Z] right=[557,T]
ADD id=182 left=[497,F] right=[520,M]
ADD id=183 left=[398,F] right=[374,P]
ADD id=184 left=[449,V] right=[216,Z]
ADD id=185 left=[157,B] right=[341,J]
SWAP 107
SWAP 82
SWAP 123
SWAP 152
SWAP 21
SWAP 140
SWAP 124
SWAP 158
SWAP 92
SWAP 164
SWAP 23
SWAP 74
SWAP 56
SWAP 177
SWAP 93
SWAP 171
SWAP 167
SWAP 169
ADD id=186 left=[708,B] right=[174,H]
ADD id=187 left=[304,Y] right=[621,V]
ADD id=188 left=[541,S] right=[171,X]
ADD id=189 left=[747,R] right=[247,W]
ADD id=190 left=[803,Z] right=[645,S]
SWAP 113
SWAP 77
SWAP 185
SWAP 139
SWAP 27
SWAP 163
SWAP 184
SWAP 105
SWAP 5
SWAP 160
SWAP 179
SWAP 19
SWAP 30
SWAP 166
SWAP 39
SWAP 89
SWAP 21
SWAP 95
SWAP 104
ADD id=191 left=[946,Y] right=[149,L]
ADD id=192 left=[115,J] right=[801,G]
ADD id=193 left=[128,J] right=[695,M]
ADD id=194 left=[951,G] right=[108,S]
ADD id=195 left=[691,R] right=[845,S]
SWAP 166
SWAP 132
SWAP 128
SWAP 98
SWAP 30
SWAP 145
SWAP 17
SWAP 105
SWAP 25
SWAP 150
SWAP 87
SWAP 192
SWAP 18
SWAP 157
SWAP 7
SWAP 96
SWAP 103
SWAP 114
SWAP 42
ADD id=196 left=[494,X] right=[683,B]
ADD id=197 left=[265,F] right=[479,B]
ADD id=198 left=[729,J] right=[302,B]
ADD id=199 left=[525,T] right=[279,B]
ADD id=200 left=[770,J] right=[702,W]
SWAP 86
SWAP 7
SWAP 171
SWAP 70
SWAP 145
SWAP 100
SWAP 185
SWAP 45
SWAP 60
SWAP 22
SWAP 190
SWAP 82
SWAP 53
SWAP 143
SWAP 34
SWAP 156
SWAP 48
SWAP 175
SWAP 65
SWAP 87
"""
