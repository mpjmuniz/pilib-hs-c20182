module Calc where
import Parser
import Lexer
import Data.Dynamic
import Data.Maybe
import Data.Typeable
import System.IO
import qualified Data.Map.Strict as Map

{- |
 - Automata definition
 -}
 
type ValueStack = [Value]
type ControlStack = [Control]

data CmdPiAut = CmdPiAut { env :: Environment,
                           sto :: Store, 
                           val :: ValueStack,
                           cnt :: ControlStack,
                           locs :: [Location] 
                         } deriving (Show, Eq)

lookup' :: Ord k => k -> Map.Map k a -> a
lookup' k m = fromJust $ Map.lookup k m

-- Right -> int value, Left -> bool value
evalStorable :: Storable -> Value
evalStorable (Sint x) = Vi x
evalStorable (Sbool x) = Vb x

storeValue :: Value -> Storable
storeValue (Vi x) = Sint x
storeValue (Vb x) = Sbool x
storeValue _ = Sint 0

bindValue :: Value -> Bindable
bindValue (Vi x) = Sto $ Sint x
bindValue (Vb x) = Sto $ Sbool x

match :: [Identifier] -> [Value] -> Environment
match fl al = if length fl /= length al then Map.fromList []
                                        else match' fl al (Map.fromList []) 

match' :: [Identifier] -> [Value] -> Environment -> Environment
match' [] [] e = e
match' f a e   = Map.union (Map.fromList $ zip f (map (\v -> bindValue v) a)) e 

{-
 - Evaluation function. Given an automata, it recursively evaluates the expression in it's control stack
 -}
{-
evalExp :: DeclPiAut -> DeclPiAut
eval cpa@(DeclPiAut e s v c l)
-}
eval :: CmdPiAut -> CmdPiAut
eval cpa@(CmdPiAut {cnt = []}) = cpa
eval cpa@(CmdPiAut e s v c l)  = eval $ case (head c) of
                                      S (E (Ae (Sum aex1 aex2)))   -> cpa{cnt = S (E (Ae aex1)) : S (E (Ae aex2)) : K KWSum : tail c} 
                                      S (E (Ae (Sub aex1 aex2)))   -> cpa{cnt = S (E (Ae aex2)) : S (E (Ae aex1)) : K KWSub : tail c}
                                      S (E (Ae (Mul aex1 aex2)))   -> cpa{cnt = S (E (Ae aex1)) : S (E (Ae aex2)) : K KWMul : tail c}
                                      S (E (Be (Eq  exp1 exp2)))   -> cpa{cnt = S (E (Be exp1)) : S (E (Be exp2)) : K KWEq  : tail c} 
                                      S (E (Be (Or  exp1 exp2)))   -> cpa{cnt = S (E (Be exp1)) : S (E (Be exp2)) : K KWOr  : tail c} 
                                      S (E (Be (And exp1 exp2)))   -> cpa{cnt = S (E (Be exp1)) : S (E (Be exp2)) : K KWAnd : tail c} 
                                      S (E (Be (Lt  exp1 exp2)))   -> cpa{cnt = S (E (Ae exp1)) : S (E (Ae exp2)) : K KWLt  : tail c} 
                                      S (E (Be (Le  exp1 exp2)))   -> cpa{cnt = S (E (Ae exp1)) : S (E (Ae exp2)) : K KWLe  : tail c} 
                                      S (E (Be (Ge  exp1 exp2)))   -> cpa{cnt = S (E (Ae exp1)) : S (E (Ae exp2)) : K KWGe  : tail c} 
                                      S (E (Be (Gt  exp1 exp2)))   -> cpa{cnt = S (E (Ae exp1)) : S (E (Ae exp2)) : K KWGt  : tail c} 
                                      S (E (Be (Not ex)))          -> cpa{cnt = S (E (Be ex))   : K KWNot : tail c}
                                      S (E (Ae (N intval)))        -> cpa{cnt = tail c, val = Vi intval : v}
                                      S (E (Be (B booval)))        -> cpa{cnt = tail c, val = Vb booval : v}
                                      S (C (A idtf exp))           -> cpa{cnt = S (E exp) : K KWAssign : tail c, val = Vid idtf : v} 
                                      S (C (Cs cmd1 cmd2))         -> cpa{cnt = S (C cmd1) : S (C cmd2) : tail c}
                                      S (C (L bexp cmd))           -> cpa{cnt = S (E (Be bexp)) : K KWLoop : tail c, val = Vlp bexp cmd : v} 
                                      S (C (Call idt exps))        -> cpa{cnt = (reverse (map (\exp -> S (E exp)) exps)) ++ (K (KWCall idt (length exps)) : tail c)}
                                      S (E (Rf exp))               -> cpa{cnt = S (E exp) : K KWRef : tail c}
                                      S (E (Dr idt))               -> cpa{cnt = tail c, val = Vbi (lookup' idt e) : v}
                                      S (E (Vr idt))               -> cpa{cnt = tail c,
                                                                          val = evalStorable (lookup' (ival (evalStorable (lookup' (loc (lookup' idt e)) s))) s) : v}
                                      S (D (Ds dec1 dec2))         -> cpa{cnt = S (D dec1) : S (D dec2) : tail c}
                                      K KWNot                      -> cpa{cnt = tail c, val = Vb (not $ bval $ head v) : tail v}
                                      S (E (Ae (Id id)))           -> cpa{cnt = tail c, val = evalStorable (lookup' (loc (lookup' id e)) s) : v} 
                                      S (D (Bi id exp))            -> cpa{cnt = S (E exp) : K KWBind : tail c, val = Vid id : v}
                                      S (C (Bl decl cmd))          -> cpa{cnt = S (D decl) : K KWDec : S (C cmd) : K KWBlk : tail c, val = Lvls l : v, locs = []}
                                      Ab(Abs formals cmd)          -> cpa{cnt = tail c, val = Vclj (Clj formals cmd e) : v}  
                                      K KWRef   -> let bng = Lo ((Map.size s) + 1) in 
                                                             cpa{ cnt = tail c
                                                                , sto = Map.insert (loc bng) (storeValue (head v)) s
                                                                , val = Vbi bng : (tail v)
                                                                , locs = loc bng : l
                                                                }
                                      x -> let (va:vb:vs) = v in case x of
                                           K KWSum               -> cpa{cnt = tail c, val = Vi (ival va + ival vb)  : vs}
                                           K KWSub               -> cpa{cnt = tail c, val = Vi (ival va - ival vb)  : vs}
                                           K KWMul               -> cpa{cnt = tail c, val = Vi (ival va * ival vb)  : vs}
                                           K KWEq                -> cpa{cnt = tail c, val = Vb (bval va == bval vb) : vs}
                                           K KWLe                -> cpa{cnt = tail c, val = Vb (ival va >= ival vb) : vs}
                                           K KWLt                -> cpa{cnt = tail c, val = Vb (ival va > ival vb)  : vs}
                                           K KWGe                -> cpa{cnt = tail c, val = Vb (ival va <= ival vb) : vs}
                                           K KWGt                -> cpa{cnt = tail c, val = Vb (ival va < ival vb)  : vs}
                                           K KWOr                -> cpa{cnt = tail c, val = Vb (bval va || bval vb) : vs}
                                           K KWAnd               -> cpa{cnt = tail c, val = Vb (bval va && bval vb) : vs}
                                           K KWAssign            -> cpa{cnt = tail c, val = vs, sto = Map.insert (loc (lookup' (idval vb) e)) (storeValue va) s } 
                                           K KWLoop              -> cpa{cnt = if(bval $ va) then (S $ C $ cmdval vb) : S (C $ L (beval vb) (cmdval vb)) : tail c 
                                                                                            else tail c ,
                                                                        val = vs}
                                           K KWBind              -> cpa{cnt = tail c,
                                                                        val = case vs of
                                                                                [] -> (Env (Map.fromList [(idval vb, bival va)])) : []
                                                                                env:vls -> case env of
                                                                                    Env map -> (Env (Map.insert (idval vb) (bival va) (enval env))) : vls 
                                                                                    otherwise -> (Env (Map.fromList [(idval vb, bival va)])) : vs} 
                                           K KWDec               -> cpa{cnt = tail c,
                                                                        val = Env e : tail v,
                                                                        env = Map.union (enval va) e}
                                           K KWBlk               -> cpa{cnt = tail c,
                                                                        val = vs,
                                                                        env = enval va, 
                                                                        sto = Map.filterWithKey (\k _ -> not (k `elem` (lvals vb))) s, 
                                                                        locs = lvals vb}
                                           K (KWCall id n)       -> cpa{cnt = let b = blk (cl (lookup' id e)) in
                                                                             (S $ C b) : K KWBlk : tail c, 
                                                                        val = Env e : drop n v,
                                                                        env = let e1 = local (cl (lookup' id e))
                                                                                  f = formals (cl (lookup' id e)) in
                                                                             Map.union (match f (take n v)) e1 }
