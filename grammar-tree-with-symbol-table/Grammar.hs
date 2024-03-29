{-# OPTIONS_GHC -w #-}
module Grammar
( parse
) where

import Tokens
import GrammarTree
import qualified Data.Map as Map
-- import Data.Map((!?))

-- Declarations = Map.Map String Declaration
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.11

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 (Program)
	| HappyAbsSyn5 (Map.Map String Declaration)
	| HappyAbsSyn6 (Declarations -> [Command])
	| HappyAbsSyn7 (Declarations -> Command)
	| HappyAbsSyn8 (Declarations -> Expression)
	| HappyAbsSyn9 (Declarations -> Condition)
	| HappyAbsSyn10 (Declarations -> Value)
	| HappyAbsSyn11 (Declarations -> Identifier)

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83,
 action_84,
 action_85,
 action_86,
 action_87,
 action_88,
 action_89,
 action_90,
 action_91,
 action_92,
 action_93,
 action_94,
 action_95,
 action_96,
 action_97,
 action_98,
 action_99 :: () => Int -> ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34 :: () => ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,235) ([24576,0,0,32768,0,0,0,1,0,0,0,0,0,8720,389,0,35392,1556,0,0,0,0,16384,0,0,0,0,8192,24576,0,0,32768,1,0,0,18564,97,0,16,0,0,64,0,0,384,0,0,4096,0,1024,0,0,8192,8192,0,0,0,21025,24,0,4,0,0,0,0,256,0,0,0,0,0,0,0,0,16384,0,2048,0,16384,7304,6,0,1024,0,0,0,61440,3,1024,0,0,96,0,0,384,0,0,0,0,0,0,0,0,0,0,0,4,0,992,0,0,0,32,0,0,128,34880,1556,0,384,0,0,1536,0,0,6144,0,0,24576,0,0,32768,1,0,0,6,0,0,8720,389,0,96,0,0,384,0,0,0,0,0,0,0,0,0,0,32768,0,21033,24,0,0,0,16,8,0,0,0,0,0,128,0,0,0,3072,0,0,0,0,16385,5768,6,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,34106,1,0,0,0,0,0,0,0,6,0,0,24,0,0,96,0,0,384,0,0,1536,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8448,6226,0,0,0,0,0,0,0,0,0,0,32768,1,0,0,6,0,0,0,0,64,0,0,512,0,0,0,512,0,0,0,64,0,0,256,0,0,21281,24,0,0,0,0,8720,389,0,34880,1556,0,0,0,2048,0,0,0,4096,50466,1,16384,5256,7,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parse","program","declarations","commands","command","expression","condition","value","identifier","num","pidentifier","VAR","BEGIN","END","ASSIGN","IF","THEN","ELSE","ENDIF","WHILE","DO","ENDWHILE","REPEAT","UNTIL","FOR","FROM","TO","DOWNTO","ENDFOR","READ","WRITE","PLUS","MINUS","TIMES","DIV","MOD","EQ","NEQ","LE","GE","LEQ","GEQ","','","'['","':'","']'","';'","%eof"]
        bit_start = st * 50
        bit_end = (st + 1) * 50
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..49]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (14) = happyShift action_2
action_0 (15) = happyShift action_4
action_0 (4) = happyGoto action_3
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (14) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (13) = happyShift action_16
action_2 (5) = happyGoto action_15
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (50) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (13) = happyShift action_8
action_4 (18) = happyShift action_9
action_4 (22) = happyShift action_10
action_4 (25) = happyShift action_11
action_4 (27) = happyShift action_12
action_4 (32) = happyShift action_13
action_4 (33) = happyShift action_14
action_4 (6) = happyGoto action_5
action_4 (7) = happyGoto action_6
action_4 (11) = happyGoto action_7
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (13) = happyShift action_8
action_5 (16) = happyShift action_32
action_5 (18) = happyShift action_9
action_5 (22) = happyShift action_10
action_5 (25) = happyShift action_11
action_5 (27) = happyShift action_12
action_5 (32) = happyShift action_13
action_5 (33) = happyShift action_14
action_5 (7) = happyGoto action_31
action_5 (11) = happyGoto action_7
action_5 _ = happyFail (happyExpListPerState 5)

action_6 _ = happyReduce_8

action_7 (17) = happyShift action_30
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (46) = happyShift action_29
action_8 _ = happyReduce_32

action_9 (12) = happyShift action_22
action_9 (13) = happyShift action_8
action_9 (9) = happyGoto action_28
action_9 (10) = happyGoto action_27
action_9 (11) = happyGoto action_21
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (12) = happyShift action_22
action_10 (13) = happyShift action_8
action_10 (9) = happyGoto action_26
action_10 (10) = happyGoto action_27
action_10 (11) = happyGoto action_21
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (13) = happyShift action_8
action_11 (18) = happyShift action_9
action_11 (22) = happyShift action_10
action_11 (25) = happyShift action_11
action_11 (27) = happyShift action_12
action_11 (32) = happyShift action_13
action_11 (33) = happyShift action_14
action_11 (6) = happyGoto action_25
action_11 (7) = happyGoto action_6
action_11 (11) = happyGoto action_7
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (13) = happyShift action_24
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (13) = happyShift action_8
action_13 (11) = happyGoto action_23
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (12) = happyShift action_22
action_14 (13) = happyShift action_8
action_14 (10) = happyGoto action_20
action_14 (11) = happyGoto action_21
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (15) = happyShift action_18
action_15 (45) = happyShift action_19
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (46) = happyShift action_17
action_16 _ = happyReduce_5

action_17 (12) = happyShift action_51
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (13) = happyShift action_8
action_18 (18) = happyShift action_9
action_18 (22) = happyShift action_10
action_18 (25) = happyShift action_11
action_18 (27) = happyShift action_12
action_18 (32) = happyShift action_13
action_18 (33) = happyShift action_14
action_18 (6) = happyGoto action_50
action_18 (7) = happyGoto action_6
action_18 (11) = happyGoto action_7
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (13) = happyShift action_49
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (49) = happyShift action_48
action_20 _ = happyFail (happyExpListPerState 20)

action_21 _ = happyReduce_31

action_22 _ = happyReduce_30

action_23 (49) = happyShift action_47
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (28) = happyShift action_46
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (13) = happyShift action_8
action_25 (18) = happyShift action_9
action_25 (22) = happyShift action_10
action_25 (25) = happyShift action_11
action_25 (26) = happyShift action_45
action_25 (27) = happyShift action_12
action_25 (32) = happyShift action_13
action_25 (33) = happyShift action_14
action_25 (7) = happyGoto action_31
action_25 (11) = happyGoto action_7
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (23) = happyShift action_44
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (39) = happyShift action_38
action_27 (40) = happyShift action_39
action_27 (41) = happyShift action_40
action_27 (42) = happyShift action_41
action_27 (43) = happyShift action_42
action_27 (44) = happyShift action_43
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (19) = happyShift action_37
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (12) = happyShift action_35
action_29 (13) = happyShift action_36
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (12) = happyShift action_22
action_30 (13) = happyShift action_8
action_30 (8) = happyGoto action_33
action_30 (10) = happyGoto action_34
action_30 (11) = happyGoto action_21
action_30 _ = happyFail (happyExpListPerState 30)

action_31 _ = happyReduce_7

action_32 _ = happyReduce_2

action_33 (49) = happyShift action_72
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (34) = happyShift action_67
action_34 (35) = happyShift action_68
action_34 (36) = happyShift action_69
action_34 (37) = happyShift action_70
action_34 (38) = happyShift action_71
action_34 _ = happyReduce_18

action_35 (48) = happyShift action_66
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (48) = happyShift action_65
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (13) = happyShift action_8
action_37 (18) = happyShift action_9
action_37 (22) = happyShift action_10
action_37 (25) = happyShift action_11
action_37 (27) = happyShift action_12
action_37 (32) = happyShift action_13
action_37 (33) = happyShift action_14
action_37 (6) = happyGoto action_64
action_37 (7) = happyGoto action_6
action_37 (11) = happyGoto action_7
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (12) = happyShift action_22
action_38 (13) = happyShift action_8
action_38 (10) = happyGoto action_63
action_38 (11) = happyGoto action_21
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (12) = happyShift action_22
action_39 (13) = happyShift action_8
action_39 (10) = happyGoto action_62
action_39 (11) = happyGoto action_21
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (12) = happyShift action_22
action_40 (13) = happyShift action_8
action_40 (10) = happyGoto action_61
action_40 (11) = happyGoto action_21
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (12) = happyShift action_22
action_41 (13) = happyShift action_8
action_41 (10) = happyGoto action_60
action_41 (11) = happyGoto action_21
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (12) = happyShift action_22
action_42 (13) = happyShift action_8
action_42 (10) = happyGoto action_59
action_42 (11) = happyGoto action_21
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (12) = happyShift action_22
action_43 (13) = happyShift action_8
action_43 (10) = happyGoto action_58
action_43 (11) = happyGoto action_21
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (13) = happyShift action_8
action_44 (18) = happyShift action_9
action_44 (22) = happyShift action_10
action_44 (25) = happyShift action_11
action_44 (27) = happyShift action_12
action_44 (32) = happyShift action_13
action_44 (33) = happyShift action_14
action_44 (6) = happyGoto action_57
action_44 (7) = happyGoto action_6
action_44 (11) = happyGoto action_7
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (12) = happyShift action_22
action_45 (13) = happyShift action_8
action_45 (9) = happyGoto action_56
action_45 (10) = happyGoto action_27
action_45 (11) = happyGoto action_21
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (12) = happyShift action_22
action_46 (13) = happyShift action_8
action_46 (10) = happyGoto action_55
action_46 (11) = happyGoto action_21
action_46 _ = happyFail (happyExpListPerState 46)

action_47 _ = happyReduce_16

action_48 _ = happyReduce_17

action_49 (46) = happyShift action_54
action_49 _ = happyReduce_3

action_50 (13) = happyShift action_8
action_50 (16) = happyShift action_53
action_50 (18) = happyShift action_9
action_50 (22) = happyShift action_10
action_50 (25) = happyShift action_11
action_50 (27) = happyShift action_12
action_50 (32) = happyShift action_13
action_50 (33) = happyShift action_14
action_50 (7) = happyGoto action_31
action_50 (11) = happyGoto action_7
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (47) = happyShift action_52
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (12) = happyShift action_85
action_52 _ = happyFail (happyExpListPerState 52)

action_53 _ = happyReduce_1

action_54 (12) = happyShift action_84
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (29) = happyShift action_82
action_55 (30) = happyShift action_83
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (49) = happyShift action_81
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (13) = happyShift action_8
action_57 (18) = happyShift action_9
action_57 (22) = happyShift action_10
action_57 (24) = happyShift action_80
action_57 (25) = happyShift action_11
action_57 (27) = happyShift action_12
action_57 (32) = happyShift action_13
action_57 (33) = happyShift action_14
action_57 (7) = happyGoto action_31
action_57 (11) = happyGoto action_7
action_57 _ = happyFail (happyExpListPerState 57)

action_58 _ = happyReduce_29

action_59 _ = happyReduce_28

action_60 _ = happyReduce_27

action_61 _ = happyReduce_26

action_62 _ = happyReduce_25

action_63 _ = happyReduce_24

action_64 (13) = happyShift action_8
action_64 (18) = happyShift action_9
action_64 (20) = happyShift action_78
action_64 (21) = happyShift action_79
action_64 (22) = happyShift action_10
action_64 (25) = happyShift action_11
action_64 (27) = happyShift action_12
action_64 (32) = happyShift action_13
action_64 (33) = happyShift action_14
action_64 (7) = happyGoto action_31
action_64 (11) = happyGoto action_7
action_64 _ = happyFail (happyExpListPerState 64)

action_65 _ = happyReduce_33

action_66 _ = happyReduce_34

action_67 (12) = happyShift action_22
action_67 (13) = happyShift action_8
action_67 (10) = happyGoto action_77
action_67 (11) = happyGoto action_21
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (12) = happyShift action_22
action_68 (13) = happyShift action_8
action_68 (10) = happyGoto action_76
action_68 (11) = happyGoto action_21
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (12) = happyShift action_22
action_69 (13) = happyShift action_8
action_69 (10) = happyGoto action_75
action_69 (11) = happyGoto action_21
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (12) = happyShift action_22
action_70 (13) = happyShift action_8
action_70 (10) = happyGoto action_74
action_70 (11) = happyGoto action_21
action_70 _ = happyFail (happyExpListPerState 70)

action_71 (12) = happyShift action_22
action_71 (13) = happyShift action_8
action_71 (10) = happyGoto action_73
action_71 (11) = happyGoto action_21
action_71 _ = happyFail (happyExpListPerState 71)

action_72 _ = happyReduce_9

action_73 _ = happyReduce_23

action_74 _ = happyReduce_22

action_75 _ = happyReduce_21

action_76 _ = happyReduce_20

action_77 _ = happyReduce_19

action_78 (13) = happyShift action_8
action_78 (18) = happyShift action_9
action_78 (22) = happyShift action_10
action_78 (25) = happyShift action_11
action_78 (27) = happyShift action_12
action_78 (32) = happyShift action_13
action_78 (33) = happyShift action_14
action_78 (6) = happyGoto action_90
action_78 (7) = happyGoto action_6
action_78 (11) = happyGoto action_7
action_78 _ = happyFail (happyExpListPerState 78)

action_79 _ = happyReduce_11

action_80 _ = happyReduce_12

action_81 _ = happyReduce_13

action_82 (12) = happyShift action_22
action_82 (13) = happyShift action_8
action_82 (10) = happyGoto action_89
action_82 (11) = happyGoto action_21
action_82 _ = happyFail (happyExpListPerState 82)

action_83 (12) = happyShift action_22
action_83 (13) = happyShift action_8
action_83 (10) = happyGoto action_88
action_83 (11) = happyGoto action_21
action_83 _ = happyFail (happyExpListPerState 83)

action_84 (47) = happyShift action_87
action_84 _ = happyFail (happyExpListPerState 84)

action_85 (48) = happyShift action_86
action_85 _ = happyFail (happyExpListPerState 85)

action_86 _ = happyReduce_6

action_87 (12) = happyShift action_94
action_87 _ = happyFail (happyExpListPerState 87)

action_88 (23) = happyShift action_93
action_88 _ = happyFail (happyExpListPerState 88)

action_89 (23) = happyShift action_92
action_89 _ = happyFail (happyExpListPerState 89)

action_90 (13) = happyShift action_8
action_90 (18) = happyShift action_9
action_90 (21) = happyShift action_91
action_90 (22) = happyShift action_10
action_90 (25) = happyShift action_11
action_90 (27) = happyShift action_12
action_90 (32) = happyShift action_13
action_90 (33) = happyShift action_14
action_90 (7) = happyGoto action_31
action_90 (11) = happyGoto action_7
action_90 _ = happyFail (happyExpListPerState 90)

action_91 _ = happyReduce_10

action_92 (13) = happyShift action_8
action_92 (18) = happyShift action_9
action_92 (22) = happyShift action_10
action_92 (25) = happyShift action_11
action_92 (27) = happyShift action_12
action_92 (32) = happyShift action_13
action_92 (33) = happyShift action_14
action_92 (6) = happyGoto action_97
action_92 (7) = happyGoto action_6
action_92 (11) = happyGoto action_7
action_92 _ = happyFail (happyExpListPerState 92)

action_93 (13) = happyShift action_8
action_93 (18) = happyShift action_9
action_93 (22) = happyShift action_10
action_93 (25) = happyShift action_11
action_93 (27) = happyShift action_12
action_93 (32) = happyShift action_13
action_93 (33) = happyShift action_14
action_93 (6) = happyGoto action_96
action_93 (7) = happyGoto action_6
action_93 (11) = happyGoto action_7
action_93 _ = happyFail (happyExpListPerState 93)

action_94 (48) = happyShift action_95
action_94 _ = happyFail (happyExpListPerState 94)

action_95 _ = happyReduce_4

action_96 (13) = happyShift action_8
action_96 (18) = happyShift action_9
action_96 (22) = happyShift action_10
action_96 (25) = happyShift action_11
action_96 (27) = happyShift action_12
action_96 (31) = happyShift action_99
action_96 (32) = happyShift action_13
action_96 (33) = happyShift action_14
action_96 (7) = happyGoto action_31
action_96 (11) = happyGoto action_7
action_96 _ = happyFail (happyExpListPerState 96)

action_97 (13) = happyShift action_8
action_97 (18) = happyShift action_9
action_97 (22) = happyShift action_10
action_97 (25) = happyShift action_11
action_97 (27) = happyShift action_12
action_97 (31) = happyShift action_98
action_97 (32) = happyShift action_13
action_97 (33) = happyShift action_14
action_97 (7) = happyGoto action_31
action_97 (11) = happyGoto action_7
action_97 _ = happyFail (happyExpListPerState 97)

action_98 _ = happyReduce_14

action_99 _ = happyReduce_15

happyReduce_1 = happyReduce 5 4 happyReduction_1
happyReduction_1 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (let vars = happy_var_2 in Program vars (happy_var_4 vars)
	) `HappyStk` happyRest

happyReduce_2 = happySpecReduce_3  4 happyReduction_2
happyReduction_2 _
	(HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (let vars = Map.empty in Program vars (happy_var_2 vars)
	)
happyReduction_2 _ _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_3  5 happyReduction_3
happyReduction_3 (HappyTerminal (Pidentifier happy_var_3))
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (if Map.member happy_var_3 happy_var_1
                                                             then error ("redeclaration of a variable: " ++ happy_var_3)
                                                             else Map.insert happy_var_3 (Variable happy_var_3) happy_var_1
	)
happyReduction_3 _ _ _  = notHappyAtAll 

happyReduce_4 = happyReduce 8 5 happyReduction_4
happyReduction_4 (_ `HappyStk`
	(HappyTerminal (Num happy_var_7)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Num happy_var_5)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Pidentifier happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (if happy_var_5 > happy_var_7
                                                             then error ("arrays of 0 length are not allowed: " ++ happy_var_3 ++ "[" ++ show happy_var_5 ++ ":" ++ show happy_var_7 ++ "]")
                                                             else if Map.member happy_var_3 happy_var_1
                                                                    then error ("redeclaration of a variable: " ++ happy_var_3)
                                                                    else Map.insert happy_var_3 (Array happy_var_3 happy_var_5 happy_var_7) happy_var_1
	) `HappyStk` happyRest

happyReduce_5 = happySpecReduce_1  5 happyReduction_5
happyReduction_5 (HappyTerminal (Pidentifier happy_var_1))
	 =  HappyAbsSyn5
		 (Map.singleton happy_var_1 (Variable happy_var_1)
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happyReduce 6 5 happyReduction_6
happyReduction_6 (_ `HappyStk`
	(HappyTerminal (Num happy_var_5)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Num happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Pidentifier happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (if happy_var_3 > happy_var_5
                                                             then error ("arrays of 0 length are not allowed: " ++ happy_var_1 ++ "[" ++ show happy_var_3 ++ ":" ++ show happy_var_5 ++ "]")
                                                             else Map.singleton happy_var_1 (Array happy_var_1 happy_var_3 happy_var_5)
	) `HappyStk` happyRest

happyReduce_7 = happySpecReduce_2  6 happyReduction_7
happyReduction_7 (HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (\vars -> happy_var_1 vars ++ [happy_var_2 vars]
	)
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  6 happyReduction_8
happyReduction_8 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (\vars -> [happy_var_1 vars]
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happyReduce 4 7 happyReduction_9
happyReduction_9 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (\vars -> case (happy_var_1 vars) of
                                                                                   Var (Constant c) -> error ("cannot modify '" ++ c ++ "' by ASSIGN because it is an iterator")
                                                                                   assignId   -> Assign assignId (happy_var_3 vars)
	) `HappyStk` happyRest

happyReduce_10 = happyReduce 7 7 happyReduction_10
happyReduction_10 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (\vars -> IfElse (happy_var_2 vars) (happy_var_4 vars) (happy_var_6 vars)
	) `HappyStk` happyRest

happyReduce_11 = happyReduce 5 7 happyReduction_11
happyReduction_11 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (\vars -> If (happy_var_2 vars) (happy_var_4 vars)
	) `HappyStk` happyRest

happyReduce_12 = happyReduce 5 7 happyReduction_12
happyReduction_12 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (\vars -> While (happy_var_2 vars) (happy_var_4 vars)
	) `HappyStk` happyRest

happyReduce_13 = happyReduce 5 7 happyReduction_13
happyReduction_13 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (\vars -> Repeat (happy_var_2 vars) (happy_var_4 vars)
	) `HappyStk` happyRest

happyReduce_14 = happyReduce 9 7 happyReduction_14
happyReduction_14 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Pidentifier happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (\vars -> case Map.lookup happy_var_2 vars of
                                                                                  Nothing -> let localVars = Map.insert happy_var_2 (Constant happy_var_2) vars
                                                                                             in  ForTo happy_var_2 (happy_var_4 vars) (happy_var_6 vars) (happy_var_8 localVars)
                                                                                  Just _  -> error (happy_var_2 ++ " is already declared")
	) `HappyStk` happyRest

happyReduce_15 = happyReduce 9 7 happyReduction_15
happyReduction_15 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Pidentifier happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (\vars -> case Map.lookup happy_var_2 vars of
                                                                                  Nothing -> let localVars = Map.insert happy_var_2 (Constant happy_var_2) vars
                                                                                             in  ForDownTo happy_var_2 (happy_var_4 vars) (happy_var_6 vars) (happy_var_8 localVars)
                                                                                  Just _  -> error (happy_var_2 ++ " is already declared")
	) `HappyStk` happyRest

happyReduce_16 = happySpecReduce_3  7 happyReduction_16
happyReduction_16 _
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (\vars -> case happy_var_2 vars of
                                                                                   Var (Constant c) -> error ("cannot modify '" ++ c ++ "' by READ because it is an iterator")
                                                                                   readId     -> Read readId
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  7 happyReduction_17
happyReduction_17 _
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (\vars -> Write (happy_var_2 vars)
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  8 happyReduction_18
happyReduction_18 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn8
		 (\vars -> Single (happy_var_1 vars)
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  8 happyReduction_19
happyReduction_19 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn8
		 (\vars -> Plus (happy_var_1 vars) (happy_var_3 vars)
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  8 happyReduction_20
happyReduction_20 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn8
		 (\vars -> Minus (happy_var_1 vars) (happy_var_3 vars)
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  8 happyReduction_21
happyReduction_21 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn8
		 (\vars -> Times (happy_var_1 vars) (happy_var_3 vars)
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  8 happyReduction_22
happyReduction_22 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn8
		 (\vars -> Div (happy_var_1 vars) (happy_var_3 vars)
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  8 happyReduction_23
happyReduction_23 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn8
		 (\vars -> Mod (happy_var_1 vars) (happy_var_3 vars)
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  9 happyReduction_24
happyReduction_24 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (\vars -> Eq (happy_var_1 vars) (happy_var_3 vars)
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  9 happyReduction_25
happyReduction_25 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (\vars -> NEq (happy_var_1 vars) (happy_var_3 vars)
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  9 happyReduction_26
happyReduction_26 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (\vars -> Le (happy_var_1 vars) (happy_var_3 vars)
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  9 happyReduction_27
happyReduction_27 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (\vars -> Ge (happy_var_1 vars) (happy_var_3 vars)
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3  9 happyReduction_28
happyReduction_28 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (\vars -> LEq (happy_var_1 vars) (happy_var_3 vars)
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  9 happyReduction_29
happyReduction_29 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (\vars -> GEq (happy_var_1 vars) (happy_var_3 vars)
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  10 happyReduction_30
happyReduction_30 (HappyTerminal (Num happy_var_1))
	 =  HappyAbsSyn10
		 (\vars -> Number happy_var_1
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  10 happyReduction_31
happyReduction_31 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 (\vars -> Identifier (happy_var_1 vars)
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  11 happyReduction_32
happyReduction_32 (HappyTerminal (Pidentifier happy_var_1))
	 =  HappyAbsSyn11
		 (\vars -> case Map.lookup happy_var_1 vars of
                                                       Nothing -> error ("undeclared identifier: " ++ happy_var_1)
                                                       Just (Array _ _ _) -> error ("missing index for accessing an array: " ++ happy_var_1)
                                                       Just decl -> Var decl
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happyReduce 4 11 happyReduction_33
happyReduction_33 (_ `HappyStk`
	(HappyTerminal (Pidentifier happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Pidentifier happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (\vars -> case Map.lookup happy_var_1 vars of
                                                       Nothing -> error ("undeclared array identifier: " ++ happy_var_1)
                                                       Just arrDecl@(Array _ _ _) -> case Map.lookup happy_var_3 vars of
                                                                                         Nothing -> error ("undeclared identifier: " ++ happy_var_3)
                                                                                         Just (Array _ _ _) -> error ("identifier '" ++ happy_var_3 ++ "' may not be an array")
                                                                                         Just indDecl -> ArrVar arrDecl indDecl
                                                       Just _  -> error ("identifier '" ++ happy_var_1 ++ "' should be an array")
	) `HappyStk` happyRest

happyReduce_34 = happyReduce 4 11 happyReduction_34
happyReduction_34 (_ `HappyStk`
	(HappyTerminal (Num happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Pidentifier happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (\vars -> case Map.lookup happy_var_1 vars of
                                                       Nothing -> error ("undeclared array identifier: " ++ happy_var_1)
                                                       Just arrDecl@(Array _ _ _) -> ArrNum arrDecl happy_var_3
                                                       Just _  -> error ("identifier '" ++ happy_var_1 ++ "' should be an array")
	) `HappyStk` happyRest

happyNewToken action sts stk [] =
	action 50 50 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	Num happy_dollar_dollar -> cont 12;
	Pidentifier happy_dollar_dollar -> cont 13;
	VarK -> cont 14;
	BeginK -> cont 15;
	EndK -> cont 16;
	AssignK -> cont 17;
	IfK -> cont 18;
	ThenK -> cont 19;
	ElseK -> cont 20;
	EndIfK -> cont 21;
	WhileK -> cont 22;
	DoK -> cont 23;
	EndWhileK -> cont 24;
	RepeatK -> cont 25;
	UntilK -> cont 26;
	ForK -> cont 27;
	FromK -> cont 28;
	ToK -> cont 29;
	DownToK -> cont 30;
	EndForK -> cont 31;
	ReadK -> cont 32;
	WriteK -> cont 33;
	PlusK -> cont 34;
	MinusK -> cont 35;
	TimesK -> cont 36;
	DivK -> cont 37;
	ModK -> cont 38;
	EqK -> cont 39;
	NEqK -> cont 40;
	LeK -> cont 41;
	GeK -> cont 42;
	LEqK -> cont 43;
	GEqK -> cont 44;
	Comma -> cont 45;
	LBrac -> cont 46;
	Colon -> cont 47;
	RBrac -> cont 48;
	Semicolon -> cont 49;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 50 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [String]) -> HappyIdentity a
happyError' = HappyIdentity . (\(tokens, _) -> parseError tokens)
parse tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError _ = error "Parse error"

-- case Map.lookup (idName $1) vars of
--                                                                                  Nothing -> error ("ASSIGN to undeclared identifier: " ++ idName $1)
--                                                                                  Just _  ->
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}







# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4











































{-# LINE 7 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc/include/ghcversion.h" #-}















{-# LINE 7 "<command-line>" #-}
{-# LINE 1 "/tmp/ghc8336_0/ghc_2.h" #-}
































































































































































































{-# LINE 7 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 









{-# LINE 43 "templates/GenericTemplate.hs" #-}

data Happy_IntList = HappyCons Int Happy_IntList







{-# LINE 65 "templates/GenericTemplate.hs" #-}

{-# LINE 75 "templates/GenericTemplate.hs" #-}

{-# LINE 84 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 137 "templates/GenericTemplate.hs" #-}

{-# LINE 147 "templates/GenericTemplate.hs" #-}
indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 267 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 333 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
