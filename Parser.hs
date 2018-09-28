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

data HappyAbsSyn t4 t5 t6 t7 t8
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,118) ([9984,1537,8448,1,28672,0,0,0,8448,1,8448,1,0,0,28672,60,2048,192,0,0,0,256,0,0,0,0,9984,1,0,0,0,1,9984,1,28672,62,2048,194,9984,1537,0,1536,9984,1,9984,1,9984,1,8448,1,8448,1,8448,1,8448,1,8448,1,8448,1,8448,1,28672,2,16384,0,0,0,28672,0,28672,0,28672,0,28672,0,0,0,16384,0,16384,0,28672,60,0,0,0,0,0,0,0,0,0,256,0,0,0,0,2048,194,0,2048,0,4096,0,1536,0,24576,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseCalc","Expr","Aexpr","Bexpr","Cmd","Identifier","int","true","false","'=='","'+'","'-'","'*'","'/'","'('","')'","'>'","'<'","'>='","'<='","'&&'","'||'","':='","var","while","do","'{'","'}'","';'","%eof"]
        bit_start = st * 32
        bit_end = (st + 1) * 32
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..31]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (9) = happyShift action_3
action_0 (10) = happyShift action_11
action_0 (11) = happyShift action_12
action_0 (14) = happyShift action_4
action_0 (17) = happyShift action_13
action_0 (26) = happyShift action_14
action_0 (27) = happyShift action_15
action_0 (4) = happyGoto action_6
action_0 (5) = happyGoto action_7
action_0 (6) = happyGoto action_8
action_0 (7) = happyGoto action_9
action_0 (8) = happyGoto action_10
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (9) = happyShift action_3
action_1 (14) = happyShift action_4
action_1 (17) = happyShift action_5
action_1 (5) = happyGoto action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (13) = happyShift action_24
action_2 (14) = happyShift action_25
action_2 (15) = happyShift action_26
action_2 _ = happyFail (happyExpListPerState 2)

action_3 _ = happyReduce_10

action_4 (9) = happyShift action_3
action_4 (14) = happyShift action_4
action_4 (17) = happyShift action_5
action_4 (5) = happyGoto action_32
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (9) = happyShift action_3
action_5 (14) = happyShift action_4
action_5 (17) = happyShift action_5
action_5 (5) = happyGoto action_31
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (32) = happyAccept
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (13) = happyShift action_24
action_7 (14) = happyShift action_25
action_7 (15) = happyShift action_26
action_7 (19) = happyShift action_27
action_7 (20) = happyShift action_28
action_7 (21) = happyShift action_29
action_7 (22) = happyShift action_30
action_7 _ = happyReduce_1

action_8 (12) = happyShift action_21
action_8 (23) = happyShift action_22
action_8 (24) = happyShift action_23
action_8 _ = happyReduce_2

action_9 (31) = happyShift action_20
action_9 _ = happyReduce_3

action_10 (25) = happyShift action_19
action_10 _ = happyReduce_4

action_11 _ = happyReduce_19

action_12 _ = happyReduce_20

action_13 (9) = happyShift action_3
action_13 (10) = happyShift action_11
action_13 (11) = happyShift action_12
action_13 (14) = happyShift action_4
action_13 (17) = happyShift action_13
action_13 (5) = happyGoto action_17
action_13 (6) = happyGoto action_18
action_13 _ = happyFail (happyExpListPerState 13)

action_14 _ = happyReduce_24

action_15 (17) = happyShift action_16
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (9) = happyShift action_3
action_16 (10) = happyShift action_11
action_16 (11) = happyShift action_12
action_16 (14) = happyShift action_4
action_16 (17) = happyShift action_13
action_16 (5) = happyGoto action_41
action_16 (6) = happyGoto action_49
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (13) = happyShift action_24
action_17 (14) = happyShift action_25
action_17 (15) = happyShift action_26
action_17 (18) = happyShift action_33
action_17 (19) = happyShift action_27
action_17 (20) = happyShift action_28
action_17 (21) = happyShift action_29
action_17 (22) = happyShift action_30
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (12) = happyShift action_21
action_18 (18) = happyShift action_48
action_18 (23) = happyShift action_22
action_18 (24) = happyShift action_23
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (9) = happyShift action_3
action_19 (10) = happyShift action_11
action_19 (11) = happyShift action_12
action_19 (14) = happyShift action_4
action_19 (17) = happyShift action_13
action_19 (26) = happyShift action_14
action_19 (27) = happyShift action_15
action_19 (4) = happyGoto action_47
action_19 (5) = happyGoto action_7
action_19 (6) = happyGoto action_8
action_19 (7) = happyGoto action_9
action_19 (8) = happyGoto action_10
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (26) = happyShift action_14
action_20 (27) = happyShift action_15
action_20 (7) = happyGoto action_45
action_20 (8) = happyGoto action_46
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (9) = happyShift action_3
action_21 (10) = happyShift action_11
action_21 (11) = happyShift action_12
action_21 (14) = happyShift action_4
action_21 (17) = happyShift action_13
action_21 (5) = happyGoto action_41
action_21 (6) = happyGoto action_44
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (9) = happyShift action_3
action_22 (10) = happyShift action_11
action_22 (11) = happyShift action_12
action_22 (14) = happyShift action_4
action_22 (17) = happyShift action_13
action_22 (5) = happyGoto action_41
action_22 (6) = happyGoto action_43
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (9) = happyShift action_3
action_23 (10) = happyShift action_11
action_23 (11) = happyShift action_12
action_23 (14) = happyShift action_4
action_23 (17) = happyShift action_13
action_23 (5) = happyGoto action_41
action_23 (6) = happyGoto action_42
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (9) = happyShift action_3
action_24 (14) = happyShift action_4
action_24 (17) = happyShift action_5
action_24 (5) = happyGoto action_40
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (9) = happyShift action_3
action_25 (14) = happyShift action_4
action_25 (17) = happyShift action_5
action_25 (5) = happyGoto action_39
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (9) = happyShift action_3
action_26 (14) = happyShift action_4
action_26 (17) = happyShift action_5
action_26 (5) = happyGoto action_38
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (9) = happyShift action_3
action_27 (14) = happyShift action_4
action_27 (17) = happyShift action_5
action_27 (5) = happyGoto action_37
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (9) = happyShift action_3
action_28 (14) = happyShift action_4
action_28 (17) = happyShift action_5
action_28 (5) = happyGoto action_36
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (9) = happyShift action_3
action_29 (14) = happyShift action_4
action_29 (17) = happyShift action_5
action_29 (5) = happyGoto action_35
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (9) = happyShift action_3
action_30 (14) = happyShift action_4
action_30 (17) = happyShift action_5
action_30 (5) = happyGoto action_34
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (13) = happyShift action_24
action_31 (14) = happyShift action_25
action_31 (15) = happyShift action_26
action_31 (18) = happyShift action_33
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (15) = happyShift action_26
action_32 _ = happyReduce_5

action_33 _ = happyReduce_9

action_34 (13) = happyShift action_24
action_34 (14) = happyShift action_25
action_34 (15) = happyShift action_26
action_34 _ = happyReduce_13

action_35 (13) = happyShift action_24
action_35 (14) = happyShift action_25
action_35 (15) = happyShift action_26
action_35 _ = happyReduce_14

action_36 (13) = happyShift action_24
action_36 (14) = happyShift action_25
action_36 (15) = happyShift action_26
action_36 _ = happyReduce_11

action_37 (13) = happyShift action_24
action_37 (14) = happyShift action_25
action_37 (15) = happyShift action_26
action_37 _ = happyReduce_12

action_38 _ = happyReduce_8

action_39 (15) = happyShift action_26
action_39 _ = happyReduce_7

action_40 (15) = happyShift action_26
action_40 _ = happyReduce_6

action_41 (13) = happyShift action_24
action_41 (14) = happyShift action_25
action_41 (15) = happyShift action_26
action_41 (19) = happyShift action_27
action_41 (20) = happyShift action_28
action_41 (21) = happyShift action_29
action_41 (22) = happyShift action_30
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (12) = happyShift action_21
action_42 (23) = happyShift action_22
action_42 (24) = happyShift action_23
action_42 _ = happyReduce_17

action_43 (12) = happyShift action_21
action_43 (23) = happyShift action_22
action_43 (24) = happyShift action_23
action_43 _ = happyReduce_16

action_44 (12) = happyShift action_21
action_44 (23) = happyShift action_22
action_44 (24) = happyShift action_23
action_44 _ = happyReduce_15

action_45 (31) = happyShift action_20
action_45 _ = happyReduce_23

action_46 (25) = happyShift action_19
action_46 _ = happyFail (happyExpListPerState 46)

action_47 _ = happyReduce_21

action_48 _ = happyReduce_18

action_49 (12) = happyShift action_21
action_49 (18) = happyShift action_50
action_49 (23) = happyShift action_22
action_49 (24) = happyShift action_23
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (28) = happyShift action_51
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (29) = happyShift action_52
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (26) = happyShift action_14
action_52 (27) = happyShift action_15
action_52 (7) = happyGoto action_53
action_52 (8) = happyGoto action_46
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (30) = happyShift action_54
action_53 (31) = happyShift action_20
action_53 _ = happyFail (happyExpListPerState 53)

action_54 _ = happyReduce_22

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (Aexp happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  4 happyReduction_2
happyReduction_2 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn4
		 (Bexp happy_var_1
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  4 happyReduction_3
happyReduction_3 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn4
		 (Comm happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  4 happyReduction_4
happyReduction_4 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn4
		 (Idtf happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_2  5 happyReduction_5
happyReduction_5 (HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (Mul (Num(-1)) happy_var_2
	)
happyReduction_5 _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  5 happyReduction_6
happyReduction_6 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Sum happy_var_1 happy_var_3
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  5 happyReduction_7
happyReduction_7 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Sub happy_var_1 happy_var_3
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  5 happyReduction_8
happyReduction_8 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Mul happy_var_1 happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  5 happyReduction_9
happyReduction_9 _
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (happy_var_2
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  5 happyReduction_10
happyReduction_10 (HappyTerminal (TokenInt happy_var_1))
	 =  HappyAbsSyn5
		 (Num happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  6 happyReduction_11
happyReduction_11 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn6
		 (Lt happy_var_1 happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  6 happyReduction_12
happyReduction_12 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn6
		 (Gt happy_var_1 happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  6 happyReduction_13
happyReduction_13 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn6
		 (Le happy_var_1 happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  6 happyReduction_14
happyReduction_14 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn6
		 (Ge happy_var_1 happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  6 happyReduction_15
happyReduction_15 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (Eq happy_var_1 happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  6 happyReduction_16
happyReduction_16 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (And happy_var_1 happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  6 happyReduction_17
happyReduction_17 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (Or happy_var_1 happy_var_3
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  6 happyReduction_18
happyReduction_18 _
	(HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (happy_var_2
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  6 happyReduction_19
happyReduction_19 _
	 =  HappyAbsSyn6
		 (Boo True
	)

happyReduce_20 = happySpecReduce_1  6 happyReduction_20
happyReduction_20 _
	 =  HappyAbsSyn6
		 (Boo False
	)

happyReduce_21 = happySpecReduce_3  7 happyReduction_21
happyReduction_21 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (Assign happy_var_1 happy_var_3
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happyReduce 8 7 happyReduction_22
happyReduction_22 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (Loop happy_var_3 happy_var_7
	) `HappyStk` happyRest

happyReduce_23 = happySpecReduce_3  7 happyReduction_23
happyReduction_23 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (CSeq happy_var_1 happy_var_3
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  8 happyReduction_24
happyReduction_24 (HappyTerminal (TokenVarId happy_var_1))
	 =  HappyAbsSyn8
		 (Id happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 32 32 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenInt happy_dollar_dollar -> cont 9;
	TokenTrue -> cont 10;
	TokenFalse -> cont 11;
	TokenEq -> cont 12;
	TokenPlus -> cont 13;
	TokenMinus -> cont 14;
	TokenTimes -> cont 15;
	TokenDiv -> cont 16;
	TokenLParen -> cont 17;
	TokenRParen -> cont 18;
	TokenMaior -> cont 19;
	TokenMenor -> cont 20;
	TokenMaiorIgual -> cont 21;
	TokenMenorIgual -> cont 22;
	TokenAnd -> cont 23;
	TokenOr -> cont 24;
	TokenAssign -> cont 25;
	TokenVarId happy_dollar_dollar -> cont 26;
	TokenWhile -> cont 27;
	TokenDo -> cont 28;
	TokenLBrace -> cont 29;
	TokenRBrace -> cont 30;
	TokenSemiComma -> cont 31;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 32 tk tks = happyError' (tks, explist)
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



data Expr = Aexp Aexpr 
        | Bexp Bexpr 
        | Idtf Identifier 
        | Kw Keyword
        | Comm Cmd deriving (Show, Eq)
		 
data Aexpr = Num Int 
        | Sum Aexpr Aexpr 
        | Sub Aexpr Aexpr 
        | Mul Aexpr Aexpr deriving (Show, Eq)

data Bexpr = Boo Bool 
        | Eq Bexpr Bexpr 
        | Not Bexpr 
		| Gt Aexpr Aexpr 
		| Ge Aexpr Aexpr 
		| Lt Aexpr Aexpr 
		| Le Aexpr Aexpr
		| And Bexpr Bexpr 
		| Or Bexpr Bexpr deriving (Show, Eq)


data Statement = Exp Expr 
        | Command Cmd deriving Show

data Cmd = Assign Identifier Expr 
        | Loop Bexpr Cmd 
        | CSeq Cmd Cmd deriving (Show, Eq)

data Keyword = KWSum 
        | KWMul 
        | KWSub 
        | KWEq 
        | KWNot
        | KWOr 
        | KWAnd
        | KWLt 
        | KWLe
        | KWGt 
        | KWGe
        | KWAssign 
        | KWLoop deriving (Show, Eq)

data Identifier = Id String deriving (Show, Eq, Ord)

data Value = Bo { bval :: Bool } 
        | In { ival :: Int } 
        | Idt { idval :: Identifier } 
        | Lp {beval :: Bexpr, cmdval :: Cmd} 
        | Comd {cval :: Cmd } deriving (Show, Eq)
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
