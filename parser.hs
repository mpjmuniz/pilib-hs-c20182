{-# OPTIONS_GHC -w #-}
module Parser where
import Lexer
import Data.Typeable
import Data.Maybe
import Data.Dynamic
import qualified Data.Map.Strict as Map
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.9

data HappyAbsSyn t4 t5 t6 t7 t8 t9
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,127) ([19968,3074,39936,2052,0,0,0,967,0,6145,0,0,0,0,0,0,0,0,0,1156,0,2360,0,0,0,0,0,0,4096,0,128,0,1,19968,2,39936,2052,0,12288,0,999,0,6209,0,16,32768,144,0,295,0,590,0,1180,0,2312,0,4624,0,9248,0,18496,0,36992,0,8448,1,16896,2,49152,1,32768,3,0,7,0,14,0,0,0,32,0,64,0,30944,0,0,0,0,0,0,0,19968,0,0,0,0,0,0,0,0,512,0,0,16384,1552,0,32768,0,0,2,32768,1,0,48,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseCalc","Sttmnt","Expr","Aexpr","Bexpr","Cmd","Identifier","int","true","false","'=='","'+'","'-'","'*'","'/'","'('","')'","'>'","'<'","'>='","'<='","'&&'","'||'","':='","var","while","do","'{'","'}'","';'","%eof"]
        bit_start = st * 33
        bit_end = (st + 1) * 33
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..32]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (10) = happyShift action_6
action_0 (11) = happyShift action_7
action_0 (12) = happyShift action_8
action_0 (15) = happyShift action_9
action_0 (18) = happyShift action_10
action_0 (27) = happyShift action_11
action_0 (28) = happyShift action_15
action_0 (4) = happyGoto action_12
action_0 (5) = happyGoto action_2
action_0 (6) = happyGoto action_3
action_0 (7) = happyGoto action_4
action_0 (8) = happyGoto action_13
action_0 (9) = happyGoto action_14
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (10) = happyShift action_6
action_1 (11) = happyShift action_7
action_1 (12) = happyShift action_8
action_1 (15) = happyShift action_9
action_1 (18) = happyShift action_10
action_1 (27) = happyShift action_11
action_1 (5) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 (7) = happyGoto action_4
action_1 (9) = happyGoto action_5
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_1

action_3 (14) = happyShift action_26
action_3 (15) = happyShift action_27
action_3 (16) = happyShift action_28
action_3 (20) = happyShift action_29
action_3 (21) = happyShift action_30
action_3 (22) = happyShift action_31
action_3 (23) = happyShift action_32
action_3 _ = happyReduce_3

action_4 (13) = happyShift action_23
action_4 (24) = happyShift action_24
action_4 (25) = happyShift action_25
action_4 _ = happyReduce_4

action_5 _ = happyReduce_5

action_6 _ = happyReduce_11

action_7 _ = happyReduce_20

action_8 _ = happyReduce_21

action_9 (10) = happyShift action_6
action_9 (15) = happyShift action_9
action_9 (18) = happyShift action_22
action_9 (6) = happyGoto action_21
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (10) = happyShift action_6
action_10 (11) = happyShift action_7
action_10 (12) = happyShift action_8
action_10 (15) = happyShift action_9
action_10 (18) = happyShift action_10
action_10 (6) = happyGoto action_19
action_10 (7) = happyGoto action_20
action_10 _ = happyFail (happyExpListPerState 10)

action_11 _ = happyReduce_25

action_12 (33) = happyAccept
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (32) = happyShift action_18
action_13 _ = happyReduce_2

action_14 (26) = happyShift action_17
action_14 _ = happyReduce_5

action_15 (18) = happyShift action_16
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (10) = happyShift action_6
action_16 (11) = happyShift action_7
action_16 (12) = happyShift action_8
action_16 (15) = happyShift action_9
action_16 (18) = happyShift action_10
action_16 (6) = happyGoto action_40
action_16 (7) = happyGoto action_50
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (10) = happyShift action_6
action_17 (11) = happyShift action_7
action_17 (12) = happyShift action_8
action_17 (15) = happyShift action_9
action_17 (18) = happyShift action_10
action_17 (27) = happyShift action_11
action_17 (5) = happyGoto action_49
action_17 (6) = happyGoto action_3
action_17 (7) = happyGoto action_4
action_17 (9) = happyGoto action_5
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (27) = happyShift action_11
action_18 (28) = happyShift action_15
action_18 (8) = happyGoto action_47
action_18 (9) = happyGoto action_48
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (14) = happyShift action_26
action_19 (15) = happyShift action_27
action_19 (16) = happyShift action_28
action_19 (19) = happyShift action_46
action_19 (20) = happyShift action_29
action_19 (21) = happyShift action_30
action_19 (22) = happyShift action_31
action_19 (23) = happyShift action_32
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (13) = happyShift action_23
action_20 (19) = happyShift action_45
action_20 (24) = happyShift action_24
action_20 (25) = happyShift action_25
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (16) = happyShift action_28
action_21 _ = happyReduce_6

action_22 (10) = happyShift action_6
action_22 (15) = happyShift action_9
action_22 (18) = happyShift action_22
action_22 (6) = happyGoto action_44
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (10) = happyShift action_6
action_23 (11) = happyShift action_7
action_23 (12) = happyShift action_8
action_23 (15) = happyShift action_9
action_23 (18) = happyShift action_10
action_23 (6) = happyGoto action_40
action_23 (7) = happyGoto action_43
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (10) = happyShift action_6
action_24 (11) = happyShift action_7
action_24 (12) = happyShift action_8
action_24 (15) = happyShift action_9
action_24 (18) = happyShift action_10
action_24 (6) = happyGoto action_40
action_24 (7) = happyGoto action_42
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (10) = happyShift action_6
action_25 (11) = happyShift action_7
action_25 (12) = happyShift action_8
action_25 (15) = happyShift action_9
action_25 (18) = happyShift action_10
action_25 (6) = happyGoto action_40
action_25 (7) = happyGoto action_41
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (10) = happyShift action_6
action_26 (15) = happyShift action_9
action_26 (18) = happyShift action_22
action_26 (6) = happyGoto action_39
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (10) = happyShift action_6
action_27 (15) = happyShift action_9
action_27 (18) = happyShift action_22
action_27 (6) = happyGoto action_38
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (10) = happyShift action_6
action_28 (15) = happyShift action_9
action_28 (18) = happyShift action_22
action_28 (6) = happyGoto action_37
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (10) = happyShift action_6
action_29 (15) = happyShift action_9
action_29 (18) = happyShift action_22
action_29 (6) = happyGoto action_36
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (10) = happyShift action_6
action_30 (15) = happyShift action_9
action_30 (18) = happyShift action_22
action_30 (6) = happyGoto action_35
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (10) = happyShift action_6
action_31 (15) = happyShift action_9
action_31 (18) = happyShift action_22
action_31 (6) = happyGoto action_34
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (10) = happyShift action_6
action_32 (15) = happyShift action_9
action_32 (18) = happyShift action_22
action_32 (6) = happyGoto action_33
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (14) = happyShift action_26
action_33 (15) = happyShift action_27
action_33 (16) = happyShift action_28
action_33 _ = happyReduce_14

action_34 (14) = happyShift action_26
action_34 (15) = happyShift action_27
action_34 (16) = happyShift action_28
action_34 _ = happyReduce_15

action_35 (14) = happyShift action_26
action_35 (15) = happyShift action_27
action_35 (16) = happyShift action_28
action_35 _ = happyReduce_12

action_36 (14) = happyShift action_26
action_36 (15) = happyShift action_27
action_36 (16) = happyShift action_28
action_36 _ = happyReduce_13

action_37 _ = happyReduce_9

action_38 (16) = happyShift action_28
action_38 _ = happyReduce_8

action_39 (16) = happyShift action_28
action_39 _ = happyReduce_7

action_40 (14) = happyShift action_26
action_40 (15) = happyShift action_27
action_40 (16) = happyShift action_28
action_40 (20) = happyShift action_29
action_40 (21) = happyShift action_30
action_40 (22) = happyShift action_31
action_40 (23) = happyShift action_32
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (13) = happyShift action_23
action_41 (24) = happyShift action_24
action_41 (25) = happyShift action_25
action_41 _ = happyReduce_18

action_42 (13) = happyShift action_23
action_42 (24) = happyShift action_24
action_42 (25) = happyShift action_25
action_42 _ = happyReduce_17

action_43 (13) = happyShift action_23
action_43 (24) = happyShift action_24
action_43 (25) = happyShift action_25
action_43 _ = happyReduce_16

action_44 (14) = happyShift action_26
action_44 (15) = happyShift action_27
action_44 (16) = happyShift action_28
action_44 (19) = happyShift action_46
action_44 _ = happyFail (happyExpListPerState 44)

action_45 _ = happyReduce_19

action_46 _ = happyReduce_10

action_47 (32) = happyShift action_18
action_47 _ = happyReduce_24

action_48 (26) = happyShift action_17
action_48 _ = happyFail (happyExpListPerState 48)

action_49 _ = happyReduce_22

action_50 (13) = happyShift action_23
action_50 (19) = happyShift action_51
action_50 (24) = happyShift action_24
action_50 (25) = happyShift action_25
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (29) = happyShift action_52
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (30) = happyShift action_53
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (27) = happyShift action_11
action_53 (28) = happyShift action_15
action_53 (8) = happyGoto action_54
action_53 (9) = happyGoto action_48
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (31) = happyShift action_55
action_54 (32) = happyShift action_18
action_54 _ = happyFail (happyExpListPerState 54)

action_55 _ = happyReduce_23

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (E happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  4 happyReduction_2
happyReduction_2 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn4
		 (C happy_var_1
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  5 happyReduction_3
happyReduction_3 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (Ae happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  5 happyReduction_4
happyReduction_4 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn5
		 (Be happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  5 happyReduction_5
happyReduction_5 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn5
		 (Id happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_2  6 happyReduction_6
happyReduction_6 (HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (Mul (N(-1)) happy_var_2
	)
happyReduction_6 _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  6 happyReduction_7
happyReduction_7 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (Sum happy_var_1 happy_var_3
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  6 happyReduction_8
happyReduction_8 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (Sub happy_var_1 happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  6 happyReduction_9
happyReduction_9 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (Mul happy_var_1 happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  6 happyReduction_10
happyReduction_10 _
	(HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (happy_var_2
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  6 happyReduction_11
happyReduction_11 (HappyTerminal (TokenInt happy_var_1))
	 =  HappyAbsSyn6
		 (N happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  7 happyReduction_12
happyReduction_12 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn7
		 (Lt happy_var_1 happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  7 happyReduction_13
happyReduction_13 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn7
		 (Gt happy_var_1 happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  7 happyReduction_14
happyReduction_14 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn7
		 (Le happy_var_1 happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  7 happyReduction_15
happyReduction_15 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn7
		 (Ge happy_var_1 happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  7 happyReduction_16
happyReduction_16 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (Eq happy_var_1 happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  7 happyReduction_17
happyReduction_17 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (And happy_var_1 happy_var_3
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  7 happyReduction_18
happyReduction_18 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (Or happy_var_1 happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  7 happyReduction_19
happyReduction_19 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (happy_var_2
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  7 happyReduction_20
happyReduction_20 _
	 =  HappyAbsSyn7
		 (B True
	)

happyReduce_21 = happySpecReduce_1  7 happyReduction_21
happyReduction_21 _
	 =  HappyAbsSyn7
		 (B False
	)

happyReduce_22 = happySpecReduce_3  8 happyReduction_22
happyReduction_22 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (A happy_var_1 happy_var_3
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happyReduce 8 8 happyReduction_23
happyReduction_23 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (L happy_var_3 happy_var_7
	) `HappyStk` happyRest

happyReduce_24 = happySpecReduce_3  8 happyReduction_24
happyReduction_24 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (Cs happy_var_1 happy_var_3
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  9 happyReduction_25
happyReduction_25 (HappyTerminal (TokenVarId happy_var_1))
	 =  HappyAbsSyn9
		 (I happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 33 33 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenInt happy_dollar_dollar -> cont 10;
	TokenTrue -> cont 11;
	TokenFalse -> cont 12;
	TokenEq -> cont 13;
	TokenPlus -> cont 14;
	TokenMinus -> cont 15;
	TokenTimes -> cont 16;
	TokenDiv -> cont 17;
	TokenLParen -> cont 18;
	TokenRParen -> cont 19;
	TokenMaior -> cont 20;
	TokenMenor -> cont 21;
	TokenMaiorIgual -> cont 22;
	TokenMenorIgual -> cont 23;
	TokenAnd -> cont 24;
	TokenOr -> cont 25;
	TokenAssign -> cont 26;
	TokenVarId happy_dollar_dollar -> cont 27;
	TokenWhile -> cont 28;
	TokenDo -> cont 29;
	TokenLBrace -> cont 30;
	TokenRBrace -> cont 31;
	TokenSemiComma -> cont 32;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 33 tk tks = happyError' (tks, explist)
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
parseCalc tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError _ = error "Parse error"

data Control = S Statement | K Keyword deriving (Show, Eq)

data Statement = E Expression
               | C Command
               | D Declaration deriving (Show, Eq)

data Expression = Ae ArithmeticExpression 
                | Be BooleanExpression 
                | Id Identifier -- ser� que n�o � Id String ?
                | Rf Expression -- ser� que n�o � Rf Reference ?
                | Dr Identifier
                | Vr Identifier deriving (Show, Eq) 

data ArithmeticExpression = N   Int 
                          | Sum ArithmeticExpression ArithmeticExpression  
                          | Sub ArithmeticExpression ArithmeticExpression
                          | Mul ArithmeticExpression ArithmeticExpression deriving (Show, Eq)

data BooleanExpression = B   Bool 
                       | Not BooleanExpression 
                       | Eq  BooleanExpression    BooleanExpression 
                       | And BooleanExpression    BooleanExpression  
                       | Or  BooleanExpression    BooleanExpression  
                       | Gt  ArithmeticExpression ArithmeticExpression 
                       | Ge  ArithmeticExpression ArithmeticExpression
                       | Lt  ArithmeticExpression ArithmeticExpression
                       | Le  ArithmeticExpression ArithmeticExpression deriving (Show, Eq)

data Command = A  Identifier Expression
             | L  BooleanExpression Command
             | Cs Command Command 
             | Bl Declaration Command deriving (Show, Eq)

data Declaration = Bi Identifier Expression
                 | Ds Declaration Declaration deriving (Show, Eq)

data Identifier = I String deriving (Show, Eq, Ord)

data Keyword = KWSum | KWMul | KWSub | KWNot | KWAnd | KWEq | KWOr | KWLt | KWLe | KWGt | KWGe
             | KWAssign | KWLoop | KWRef
             | KWCns | KWDec | KWBlk | KWBind | KWDSeq deriving (Show, Eq)
 
data Location = Loc Int | Sto Storable deriving (Show, Eq, Ord) 
type Storable = Either Bool Int 

data Value = Vb  { bval :: Bool } 
           | Vi  { ival :: Int } 
           | Vlp { beval :: BooleanExpression, cmdval :: Command} 
           | Vid { idval :: Identifier } 
           | Vcm { cval :: Command } 
           | Vl  { lval :: Location } 
           | Vls { lvls :: [Int] } 
           | Lvls{ lvals :: [Location] } 
           | Bng { xval :: Expression, itval :: Identifier} 
           | Env { enval :: Map.Map Identifier Location} 
           | En  { lcval :: Location, idtval :: Identifier} deriving (Show, Eq)
{-# LINE 1 "templates\GenericTemplate.hs" #-}
{-# LINE 1 "templates\\\\GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "D:/GitHub/haskell-platform/build/ghc-bindist/local/lib/include/ghcversion.h" #-}















{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "F:/Users/randy/AppData/Local/Temp/ghc15608_0/ghc_2.h" #-}














































































































































































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "templates\\\\GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 









{-# LINE 43 "templates\\\\GenericTemplate.hs" #-}

data Happy_IntList = HappyCons Int Happy_IntList







{-# LINE 65 "templates\\\\GenericTemplate.hs" #-}

{-# LINE 75 "templates\\\\GenericTemplate.hs" #-}

{-# LINE 84 "templates\\\\GenericTemplate.hs" #-}

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

{-# LINE 137 "templates\\\\GenericTemplate.hs" #-}

{-# LINE 147 "templates\\\\GenericTemplate.hs" #-}
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

{-# LINE 267 "templates\\\\GenericTemplate.hs" #-}
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

{-# LINE 333 "templates\\\\GenericTemplate.hs" #-}
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
