{-# LANGUAGE MultilineStrings #-}
module Quest_19_input where
import Parser
import Control.Applicative (some, (<|>))
import Data.List (nub)
import Data.Char
import Data.Either

symbol :: Parser Char Int Char
symbol = satisfy (\x -> x /= '\n')
eol = char '\n'

type Line = String
type Rotation = Char

line :: Parser Char Int Line
line = some symbol

rotation :: Parser Char Int Rotation
rotation = oneOf ['L', 'R']

final :: Parser Char Int ([Rotation], [Line])
final = some rotation <* eol <* eol <~> sepBy line eol <* eof

run part = parse (final >>= (return . part))

-- ================== INPUTS ====================
example1 = """
LR

>-IN-
-----
W---<
"""

example2 = """
RRLL

A.VI..>...T
.CC...<...O
.....EIB.R.
.DHB...YF..
.....F..G..
D.H........
"""

input1 = """
LLRRLR

>+89++8+51++3+3+5<
++++++++++++++++++
8+++26+4++78+9++2+
"""

input2 = """
LRLRRRRLLRLLLLRRLRLRRRRRLLRLRRRLRLLRLRLRLRRLLLLLRRRLLLRRRRLRRLLLLLLLRRRRLLLRRRRLRRLRRLLRRLLRRLLLLRLL

..+...........+.........L....M.M...LM.M.M.MD..M.O.D.
..+.....C.W..7...N...M.M.MW0..MKMNMKXMC:MMM....O;...
........07..XKX1..UW....+....MMMXMMM..MOK.N;M.......
..3..+...0.W+.......:.K..MLMM.MMMMMM.MM..MMMKM......
.............+0.K+...M.N.M.MMDKX.MMMWM..MMKMMNM.....
.....+...M....4.....DLMMMMXMM0.MMMMKMMMMMMMMMMWM.NK.
.........+0...X+..O....MW.;X....MKCMWDMMM.MM.MMMM.MM
....X+KWL..DWW..00...W+MMMMMMM.MK;X..MM..MMMM0M.M..;
XCOX.M.M.5.ML...MD.K..KMXMOMMO..N...XKWLW.N..MMMMW..
....;.O.O......0.O.;M..MMMMMMM.MNO..MMMMMM..MKMMM...
...........C..K...O.....MM.MMM...W0.0MXO.N..XMMMW...
...........+X.......M..MMMMMKM.MMM.M.MMW.M.NM.MMM...
.......D..1...0;OM...M..M...MMM0MMMXMM.MMMO.MM.M.;..
...........+KWNXMM..M..MMMNNCMMXMMMMMMMMOMWMM.MDM...
.........;..!MK...W..M..OM.MMMMN.MMMK.MLK.MMMM.M.O..
.........C.++:QC....M.MMM:M:.MM.MMMMM.M.M0MMM.MMWO..
....D...M.5.D+KX....OM...M.MMMMMMMMW..K...KMMM;W..C.
....+M+.+K..5K+L.+.M.<MWM.MMMMM.;O.0OWW.CMMMMMMM.WX.
..>8N291..9..+.A+2..X.....M.MMMMM....;...0.0MNMMMMK.
..+++X++++;N..2+.+.W...M..MMMM.......;M:.OM:CM.MMX..
....N....0.+...X+..;....MMM.MMM...........WMM...M.O.
"""

input3 = """
LLLLLRLLRLRLLRRRRRLLLRLLRLLLRRLRLRLRLRRLRRRRLRRRRLLLLLRLLLRLRRRLRRLLRRLLLRLLLRLRLLRLRRRRLRLLRRRRLRRRRRRLLRLRLRLLLLLLRLRRLLRLRRRLL

..E.DE.............S.R.....RE.B.....C.OS...........................O.......S.YE.OE.D............V.O.....SEO....++..Y.........O.....S.DRDEYW..;OD...DE.RD.K.O..W.........L...................O...K..R....
.R.E.Y....D.........O.....R.VS.Y.Y..O.YR............................O.......V.SDSEEEDC.......DRYE...SOVSDESS:E1D.+0+O..KK.....X.E...B.EY.YSE...OEB.O.Y.C....W..........Y..................R.O....OOE....
.V..OS....C.O....C..CD....OVY.YRSECE..EO...........................Y......E..B....DDC..D.YY...CRO.OCEYYEYRCBX...V2.++K.D.....Y..Y....EEEXBEOKO.SCEW.B0D..Y....R.........:................D.N.N..........
.......D...DY.............REEDDYYDD.B.Y....................Y......E.S.........ESVE..EE.....KDCEYCN.EED.ODBY...+1.Y.SBR..V......BE..RY.VSSEOOCY.EROYBB.N...:...............................XD.....OV.....
....EE.....CY.....D.O......Y..YEE.YY...............Y..............E..CY.........CBDOEC...YDOYRVEVD.BDY..YDVD....O+YB0YCS.....SEOY..O.RO.DYRBDBEWRVEDCOC.......K....................C...OEBE...Y.....D.C.
.Y.R...YV.E...............OCS.R.C..DV.E.........Y:D........O.......EB..........EVDYY.CB.E.DBYCCRYCYDYSCK.R..O.R..DBEYB..O.OD..BX.SY.E.DVREOD0.EVC..OOS........W.........................W....XV.E.D..R..
.E......B..D......E.R...V....B..ER...........D..C..S.....B..B..............X.N.DEBOOB...Y.YOECOYEE.CDE..E..D..Y..OEOC.BDCCC..Y.EB:...SB.RYCOEYEVYED;DOY.;..RVE.X...................D.Y..VV.....S..D.EE.S
.........R.......O..Y...EE....YRD..E..O..C....E...RO......C..........E.........NBYCYOWODYOODOYOYEO.BL..VR.....E.RBOVOB..S.W...DDO.Y.;YCB0VYK...ECD.E.B.V...NDDL......................Y.EV.E...VEE..D.W..
...........D..........Y..RE.RO...E.X.......E...VV..C................R........D.YODYVY.EEO.BWYDBDOO....EY.B.E.R..YDOEDCD.....CY..SE..WOODXECBOLED.OYSE;E.OD..Y.OD......V.....D......E..BBEC...YCDKED.WNW.
.........B.O........D...DC.DD.RBS.....C..D.E.SO.CO...................D........RY.OESSV.E.;VD..BD....R...VE...EV..YXO.O........E.SS.YED.EODV.WCDO..YYRYE....K..EDRY.......O..D.O.K...DSSS..YD0.EVOO..Y...
...................;N.YYCEREYBE..YO....CB.Y.EE.B...E.......E........D....E..N..OE..SE.S...KD.BOEO..ES...R..E.X.SYYBYEY........B..DV.VOBECECOEEOBOV.Y.SD.E....SO...V.DYR..O.O.D..EK..E.EVDC.R.0EK;XO.....
...........E........OXNVVEYRD...DYE.E.O.Y..VOO..VY..........E.....Y..KWOV.....DV.OO..VEW...E.D.OD......D....VVY.BDOC.CD........YODE.OOYY.OBEVSVEE.D..RYBE.......BEC.C...OR..YVE...L..SSDER.OO..O.;:....N
...........Y..VR..VRVSBCR...CDB..S.E..D.CBEYBEBY.Y.........S......RV..V.EE....RWD...DB..KEOY.W.EE....Y....SC..R.DYY..VY..OK...Y.EYBYDOOSD.S.KEWE.YONDCN.OORERYEB..X...KSD..E...DVN...EYRYEY....O........
.D.........RKW.....EBO.OV...E.CO..OED...SR.SDSDY.OD......K....D.Y..EOEE.B.........V..DBO.0.O..D....O........Y..SD...W.E...O.VDEDODD.YECY.D.0BVXEXBWX.R.E..RYR.EOX.C...BV.O..EEYS..DE..EYCY.BO..E.Y..D...
.E.........EO.EBE..E.0VYED.OO........YY...VDDYCES........V.....ED.OE.DY.........EBR...D.EW..D..RCYY...B.....O...BB......SE.EBDESOYBXDDVE.D.BE.BD.DDERE.DBO.Y.BEV..R...DVO...V...DV..OOER.O..RD.DDD.E....
.V...S....OY...YD.DEVBNDD..:..E...DEDCE.CS..EVEO:DO..X......;O.EO..SE........EV...Y..E.D.ORV.E...V.E.Y.......C.C.E.O.....EE..ROCOOR.ROY:;L..DSCE.SSE.EV.KOYOBEOCE......YE.D.DV.....O0C..CDOV...B.Y.YEVO.
...........E.DEY.OEYV.ECCOYV...E..D...DD..SDVO.E..EY..O...K..Y....DEDD...E...E..Y..E....ES.D.B.EXO.B..BE.......YC.D.....R.EOC.OCBDE.EBREEODY:EKY.OEYSCYEDBODDEVRY....DO.XV...YNEE..;DWV..YY.B..YVYCOR...
C....E....CD.C.YSVO...OOY....;.....S.LEOBB..E.SVEE...E.K........D...YC...........OCY.O..ERB.O.....K.YO....Y.....OS........VDE.DOYYD.ORBYDDV.CYVDSROODBCYCECOYRSDRS.B..CE.EYSE.E..D..Y.EKRO.OEVYERC.Y.C..
S..D.WR......EE.OEKYR..VO.COC.....WVR.DE.RO.VSOCB.R...K....OE..RB...C........W.CD..ED...E.E.DODO;..C..VE.Y.....O..S.S...OSEOCYE.SEBYE..KVD.S...;..OOODXVDWEOEEYEO..D.CCB..DEVR..E.....V..EYSRCVBS.OO.D..
O.O.O.......EOE.VLEDV.EDB.KCDX.D...K.RYOE...C.E.OE.E.N....YVE......B.............O..D....RVSB..D.D.XY..R.........S..Y....DSEOES.EOV.WNER.BBKEC..YEDVYDBYYBEDDVE.EDOD.C.YY..V.NY.........ES.B..YBDB.SS.O.
...DLV..RC...E.EEEEROY.YYEOE.......R..YD.O..EBY.....K..D..OKD....O....OE....XX...D..XE....E.OYCDD....YRVYO..Y.......Y.RODSSYEROY.VVY;.E..BC...OO.EEEEDSYOOEYDVO..DED...D..D..COOE....WEOED...DRBOOO..DCO
E..........V..B..KENVOYE..V.R......E.OOYB.RYEYL.BO...:.....O..E...V...............OCO...ESRE.CYCOS..L.VE.DCO.....E.DS.DVCCDEYBED..NBSN.D.Y...E..V.EVYEVOE..OD.BO..R.RO..D....XX...EOD..VVECEXEBV.RBSDB..
...V;...RKY.D..SE.K.OOYVS.DBY.E....ESV.S..KYEV..OO.R......DBE...Y.B.....E...L.......BOYYVKVREESRR.SE..SRED..Y.RY..SDBE.EYECDVEYY..DD.E..EOK...DYYEE..BCDEO.BOBE..D...EEY.....E.EODEYOVRDEYEYRB..CKOS.E..
E..CX....0KESCE..Y.DODRE.D..VV..XYEEVYSOO..YD.Y..DYY.Y...R...BC..D.......W.....D.....Y.YWOEE.VEESY.....S..DOD.VW.C.D.OERCEES.OED.EYBSVB0.CW..EN.00C0BBOYD.EO..YD..ROEY..D.DY0YEO.O.EVWSRBDVDSBEX.OOC.B.D
..R.E..K.B.SEEEO..VXDYRRDRE.......DVE..CR.LEEY...YEX...R..CVOC..YEO............O..V.V..YYV.BVC.EC.DSKYBSBE........EEE.BEEXCOVRRV.NWL.EE..D...DEK.ERRSOVBDODODCB....R..EDY.O..S.Y..DBCYO.SODCDVDE.D.EESSO
......O0.0YYOVVXE..Y..VR....EY.E..DE.RB.S.EOE.RBYB..NE....C.L.EV.E.....Y...X.....V.....K0.RROCOR.S.OCRYE.WV...SVER..Y.ED.YYS..EW..E........E..EDRRDOVOEOYOEOE.BD...YV...W...S.DYD.D..YDR.SEYSE..X;YCOE.E
.......E...DBOOC.....EESR.EX...E.YEE..XD.O...BDY.YNEVK.EOW.S.E.C.E..R.L..O.....C.VVE.Y..ER..OD.D.YE.YOYE.Y........E..EDEVY;.EDSO.B.......O...DE.REERDVDYEDEY..SY...B.O......DC0.E.EYDYOYRYYDEYXKDDV.RYYC
.C....R..OE..CE....Y...WX...EO.RVBWCYYRCDODEOKD.OV...YC...ROY.VEOCE..BVOW.....K..RVCD..REK..O.DV..BE.OBYBV....E...V...YDRYORYDNLY..........DV.ERREDYDDEC..SYOEBV..EY...E.RX...D..EY.YDCOROEXOYCEYOS.YV.B
.K.;........YYRD..OR...D..Y..YDDBOB.KBY.XBESC.VC.......XK....SKEOV.YE.E.OVDEK.CSOVD........Y.C..DOY.EYL..YR........E.DOOEO;EVDX..VEN.....E..DOEOEYEE;BOO..SBDD.O.....E.S......D...DDVEE.VVBO.YEYEER.SB.E
..........V...DE......O....BED.VOVEEYDYCCOSODV..O.V.0...VLOE..NDV.EC.YO.OY.XO.E..DR.O..O.Y....YC..RW..K..YV..YE......ER.BODOSO;EE...D..C..ECR:O..ECORCYSCDDV.ESES.E..;S.C......O.SOCOSO..RDBY...EBEOEE.E
X.........O...O.....D....YO..EBCXDECOYR.YOEBD.DEER.B.....DV:EEY.CEEEVBW.DO...R..C.ER.OC..BCY....Y.WBWRBOX.R...V.O.....EO..XRECOY.CVB....EYV.EKB.EEYEYSCY....YDV..........V.K.....CO.E.BD..Y.ODEYDECEECEE
.....EV...........W........V.ORSOOEYR0.EDOCOC..ECER..Y....WV..EDYEBVWC..OK.VY...SD.SW;NOREB....OY..EDWYB....BO.Y.....E.KRRS.DCDEBC..DX...DOESEOEYVE.ODOODDES.B.E.:..........Y.....OY..E..R.YERDDEYDVEWY.
.....X.....C....BS...V.....Y...CYDSE.EYRYCO.YB..E.DV.SO..KDY.EB..VEYC.X.D..E.R...Y...YCO.YR.+KC..DRB.E.E....WOR........OR.YYEBCY...EE..SSO.DOEDVX..REYEDYORE.OY.........EVXR.....C..E.O..OEBYYCBE.EXO...
........;...K.....;.E.......DVEE;ODE..DCSDRWE..V..YYY....L.SVOE..O:EOBE..EWV..CY.S.YOE..BKDE..KDESYOD0S.YW..B.VY.DO..0YY.Y..EWXDVC..B....V.BCOBEDYS.COBYYCCBO..E.....XE..E.OV......C..V...OESOCCD.S.OCS.
....;...0.B.X.....C..VE.......OD.SOBYE.BOVY.VK...EDR.RODYCE.YOCOSWDREEXDDD..OY.CY.C...R..OB..VY.DBOYOE.Y...;..EOCD.....DERCRVD.O.OR..W.R...CYESEEOR.YYS.EYDSOOO........VV..Y......+...D.OYBODEORYO.SSYOD
...................YV....N.....YK..EO0Y...YV.....YW.DWVSVD.YERSV.ECDD.DC.EODSE.OEE.D...O..EVYR+BX+DC;.E.O...SRBE.CDRESOROSRSVCYBSO..BK..YN..YYVYEDCR.RDOBD.DKEOO......E.D....E.....O.VO.RBBEODEOO...RDRE
.K.O....0..V......E....D......O..WDC.VE.DY..CEYSE.EOBDO.E.EV0OYES.EOEOXC.O...EW.V..S...DB..V.YD...SV.E..R....RY.YEYKE..DDBEYDYEW.YOO.E0BEY.YER.VOOSCYE....KYYV..................DE+.YODY.DECDEOBSCW.CD..
..V......B.........E....OC.....L...DVEOO.D.EO.EOSCODB.DYEEOEROSSOYR.KS.DD.C.;EVE...O.D..CYND0E+..16.....S.OERYDEDCE...YYEEOECEECOOY..Y...Y..EODRD.EOOYV.D..CN.D.....D.......YO..E..Y.B.VEVBSOEVEO...D...
...;...CL.WY..0.D..ED..........O..OD....KKCERD.DDOC...E.XEDB.BED.....SEKEC.Y..VD...ECO.DD.OW..+O++E+.Y.ED.VESRXBB.BO..EVEOOVSDOSSNDK.;...VE....E.EEEEO.ED.W.OEW........BO..R..D.....VCD.SEREVBSYO....C.E
..K.....YN..R...X.SN.NY............O.....O.C.DRER.VYO..OEOO.YYRDKO..NRROO.O.O..DD.BL.D..XCD2..+...OD..DLOE.CDOSY..DCX0CEEBYREELDREC....O;.VS...EEBVVCBEODDYY.0.....YS..Y....E.....CY..V.SEVBDOBCDOD.....
.......CVYB..OR.E.EEE.......O..D.K.Y.K...V.R.;DOOYO....CD..DSRDOK.E.REXRO..YC.EEKRYS..V..YCD..EC..O..R.D.EBORYBDK..B.YBCEOSRYV.EY.ODES.VR..RE;Y..EC.RKVROEX.R.VE..R..Y.....Y........Y..DOOCBDECCD...WOK.
........ECBWL..RB.SSOB......O.O..D..E......EE.CEYDECDD..OD..E.D..E.E;YVN.DY.DE0DE..Y.D+......S..E.....Y...EOOOEBE..VYYEORSEV.DEO...DSE....CB.YOV..E...0EC.RDE.D....O.....YE.........O...BSYEODDESEBEB0RO
....E..DS.EDWDEDE.Y..........Y.E.O............YCCBDB:...DO+COECK+VYYEE.B.O.WEYCEEE.CY.R....O.+E.....CC..S.EEDEEYBO.YLO.RCWSERVB.OE..D.:ES.OOD....RC.EEVYRRO.O.....E........Y.........B.SDSCOYYESCE.ODY..
...E.....YSOD..S..EV........CLD..D..6....C..Y..R.RO.CY...>EYYVV5RD4VODEY.SV.O..R...;VB+EDOB+EX........C..EVE..EYR..C:DDDSD.CROREK..C...C.EYDRY.EOOV.EEEECBY.....ES.........B...E.......OD.CEDOVEVDWY..Y0
....V..EC.R..ES....SRC.O;..R.OY..CD.....R....XEOY.B..ER.C..SEESVSRRODECVBK.S.OKD.X.R.RO1O.9KE+........Y...EY..Y.SEOD.CCDODEVV.Y.....C.:RY.Y.YB..EOBEOVCYEE..YD....S.E.......Y.....E.....OE..BYSDOD.SVLRS
.X...0Y.;.B.SY.E.K...KXD.R..O;C...DDK...RC.YB.....VE....R.ERDSREKYYVOOR..S;Y..OEN..X.BSYYO0DBW.........D.Y.Y.VD.OYOD...ODEEOBE..B....S.EEEY.Y..O.ECYBVNRRBY.......E...E.......OD.S.....B..YER.EOYBVERWED
CE..E..OS.OEREE...B...YD....DDB:EE.D.W.OK...RW:S..E..R..C.CRERESYDC.DES.E..OOY..EY...DOO..EV0OY..R.......DE.EDEEOYOD:CY.EBS...W....CO.RV..YD....OOY.SORBERV........D.....B.....D.........V..EYRODB.WNEEE
YS.S.EY.SDYRV..E...0.EE.WWK.EKD.....YC.D.YB..R..W.C...VDOOVECDKKESCSODDE.EDO..Y.BEX...COEVCYE.......B.....DY..C0OYL.WOEEYEEE:E.YE..Y.R..EE....DY..ODRVOBCEDY.O....:..EVBR.....C..E.........EO..ESWEN:O.R
EEESEE.OEOSYOOC.B......RE0E..O.V.....D......D..E.XE....RYDERWYYBOBEBSDCDVN.DE.YXO.E:..NY.KD0EEOEVE.Y.....O..SDR.OSESEBO.WVSEY.SC.O..E.YO..E.:E..O.YEROYYBOV:....C.XB..V.VV.........E.....EEYB..E.V.R..O.
DOOE..ERVEDRYOE.DX......O....EXK..C.R<.O..E.......O.DODC.EE.YCCEERCEYYECEOBNEY.WN..CD.OB.+0.VYS.EV..R......E..SDD.EEEYYYEWE.LEBE.D...OE:R.E.....O..DOEYBBEBYYBDD...DYSROE..E..........ER..VSYSV.EDDNC.D.
DOSOES.OEEDOBVYR.........V...DE.VD...9.0KE.....C.EDXOE.YYBESVRYOCBEYYSDEBYEECVR.ED.RBO...O..EY.XW+YE.........0S.BEREBO..YC..E.SOO0O....O.R.......BOVOYCEOODDDYDOY.OY.OE.RBE.......DR..V..D.EEY....KBW...
OOCDES.VEO.BON...X........E.;..OS.RKEO0D..C.VE..SDBE.XEYX.YRVBEOREOEEEEVYDDBVO..L...OOX..+KD0.6D.8...K.....VDEV.VE.YOVEO....EVOLY....ROOOEVE.C..D..YCYDSOEE.R.DORDYDRYECO..SDOD..E..Y.....ERV.R.....EN..
CDBBEES.SYEYR.D..C.....E.R.N....EEO+2W...OY.C..V.E.C...CEVEYEOCVRYEYYSSYOYEYYD..O....BO.RV......O..O..O.OD..ROSEB.ESWV....EKRNSR.Y..LOYVCR....E.YE..YVOOBO.C..ECYDDYEEO..VVVEBE...V.....RB..YE....CS..VE
D.YVVESEV..B..RE....N...K.:....DO:Y..O.O..E......BY.DRSKE.BEYBDVDYOSRKOYDODCDEE..BY.BDODB6..K..O.O..VYO.B..D.YYC0..RBE..E....DE.D.R.WDBSORDEDEE..ROOD.CEC.VCEYOOKORRCOEYE..E.S..YY..W.....V.DR..LK..ES.E
.DDC.D.YEV.OWLYE.V...YYDON......B.D.C..YE.........E.E..VDW:LE.BYYBERE..V.YOSEV.ES..SSY...R.........D..;.....YEE.SOD.XVEYEXOOB.XS.EY.YBOVSCCD.OE.EY.DOE..O.L.XS0EYDERY.O.OE..SEVBY.N.......R...O..OWKD.;.
....CODRD.RE..YO....R0...Y....YCYD+CKD....NW.......YVBYY..WSYEOO.CYYES.RVDEOSDEO.YDKB..X....L....B..R.....BY..ERYEYEDRB.DB.YXO.E.SE.EDBVREYC.DD..RD....BO.RDEBROOODRDYDVD......COY.........EXVSWCD...C..
.YO..OBESDEOYEC........VVK.B.O.Y+DEES..CCO...........YDONEO...D.DYEVB.EE.YYBBYYDCY.;E......X....V...........Y..VBDROC.ED.N.ED.....S...EYVCED...DSORS.VCOD.EONDEOC:SRXKYOOD..VERO.O.B.Y.......ER.BE..O...
.V..OC.SYEEOO.RB....DE..E.R....K...EEOC.D..........YDXS..:V...OODDCYYYVRRE.BYDEO....E.....N0................B...Y.VBSEW.EREB.;...D..EO.DVY....C...EV..SYOEDVCBRYOCYCYEEDOR..R....YB.....O..R..EE...BD..K
.EO..CYRREE.X.OV.....WY..WEE..E...VEVY..D..............K..BY.EE.DY.EDEVRBEYDX..YO..........EC..ED...O........Y;NC.YO.ED..E.EE...Y....LDVCCO..SCE..S..O.DEBBE.OSOBD.D.EOBYE0.DD..:.C.BO.Y....RE..RE..CE..
....DY.D..SRC.EDK...E.....K.W..KO.RSDEO..................WR.YSC...EEDOVSO..BD.DO.E..........K.....O............YB.DO..DOSSOE..V.....BS..DER..Y..YE....YCY.DDYBRDEEVOYREV.D...C.EV..NEY.....Y..LE..VBR.Y.
E.BE.D.D.ER.VOB......C...EYC.Y..EO.BEX...E................R...0.EO.Y..EY.EY.CYRRO...B.VB.....R.YYD..............R0...SO.DC.V.D....E.EOD.BEB...BY....DV..VOY;OBODDCVE.DOR.E.OD.COC...V.DY..O......E...E.E
Y..DOCESSEES...E.DEEW..W.E....O.N;.BK.C..............B..Y...L.CD...Y.VORY.DBYDD..SY.............YDO.YB.........E..RE0.CE...E....RY..O..BCO...Y........D.DEYDCYBDO.VD.SS...V.BR..Y..N...DDE.....EY.V..E..
R.D.ODVCOD..VY..K..C....E.Y..YCSS..Y..CO....O...E......ER.......YDYOYRYB.YDODRDDO.BD..ONN....YO.O.D..OE.............SOSYO...OYO.E.R..OS.BLODE...Y.....E..OEDCOOEBYODEC.....V..Y.D...EBSO..C.YB..YBLEY.R.
....DC.DCOEE.D...X...E...LRE.YCES..EVDN..C...RO......OE.O.....EOVOOSBE..DORYOOODB.BC...R..........RV...............DX.RRY.WD..E..;..O........O.V.......DYEDEOSDOVYYB.VK...R..E..B....D..EDY.E..R.EV0.REC
.....DDYCOEE.DE...........R..EDXE.D...O...ESCS..O.....CYX.;..B..OB.BOVES.OEDWYDERS................B..K.....R.....CYCE.BEED..O.....OE..YEYVR..EV.DE.......OD.CYOEEXVRDY..D......EO.YYBRDB..E......BY.EC..
.....YDDODDVE.............VVEWBCERDY..E....V...OB....Y.NEL.....B.BSEBDES.VDD.CCCDBVY..V...........................S.VYON..R...D.....E.DKB.....DO....E.....Y...OV.O.E.YYD.....EODE.RVOD.OR..........E.O..
......D.COYE.VE......D:..:..+..WO.....D....ORC..E......OC.C.....EYYEESVVE.OVDSY.BRRRE..RE.........E........O.......BC.YED.V.VK......E..DOC...C..YBYDD..O.SX.ERDDCCEVDD...VER..VNCEYESE.X..........R..E..
.....EEOYS.D..EE....KNW.KEX..O.OEB....S....S.SY..........Y........Y.VEEEVDV..S.EYOOR.;...................B..V............OB...C......DV..E.Y.VD..E.DC.N...E.SEEYOOOO.BDD.WDE.V.SCD.YCBV............RDDO.
...S....V..EV...W..X..VY............VYCD..DO............E...V....EYRVE.SVRED.VSVEDECY....V........E.......B..........OBYOO.DCV......E..WEDEY..EEYEODYY.C+B..YOEERDEEVE..VYE..EON:....K0...........R..O..
...EE...DEYS..D.D....BYEW.E0.B.....Y.V.E.....Y...........C.....S....C.OYDRE.S..E..DR............E..E................Y.O...DV;.W.....ED..OEED.DS.DE...E.+..C.EVBV0BEY.OSY.XVESO..:OVY.:.C........E...SBO.
....DD.OY...D..............0K........BD.NNW...E.........O......EE...RDOEYVOESB..YBYVE....E.......V...................Y.D.....O....O..ED.YC.C.ROD....ND..BO.BOYCCS.YR.W.......OV.Y.R....:Y.......ES..OS.V
...CD.EOOCO...............R...........D......................E..RE.RD.Y.DEB....E.......E..E.........................C..E..V...E.....RY.BYROODCEEV...LO..K.RB.YEEDOOBYY..EV0...R..YOV..........E..SS.O...
.DOOYYDSYV..O..................S.......Y.E.............N.......BB.YBRRYBEREV.VB.B.......R.................................EO....D.CVEYEYBBDOESD..B.D.LO..O..XCD.DBD.ODY..V.....ECE.E..:.........RE.EE..E
DVREEEOOB...Y................0+.........................Y.C..YEEDODDSEO.C.....YO.RY....................................E..R..L...0RNRRSEBOB.DDD.O..OYWEKR.....ECRLE.EOV.WKY.....O.D...........O..SY....V
.EEEBE.ED.C.O.................0........C...............D.CO...YDOYBEYY.OY....D.EO......................................C..SY.Y..DBDRYOO.DOO.E.BY.O.XXC.D..W....EYCECLXCE..B.:R...:.S...........NDYBY.0E.
DYEYRV.YDO...C................................................EOERYSDEO.C......CYCE....................................O.B..Y.B..Y.ODYO.OYKOVRE.O..W.E..E.X.......SDV....O..E.X....DE.................C.
VDC.R.CV.DOE.................................................EO.O.ESDCVE........Y......................................YC.SR.....D.E.C.CO.WWDRO........NYE.....E.O.O......K.R......EON.........O.....W..
"""
