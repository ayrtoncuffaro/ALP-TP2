module Simplytyped
  ( conversion
  ,    -- conversion a terminos localmente sin nombre
    eval
  ,          -- evaluador
    infer
  ,         -- inferidor de tipos
    quote          -- valores -> terminos
  )
where

import           Data.List
import           Data.Maybe
import           Prelude                 hiding ( (>>=) )
import           Text.PrettyPrint.HughesPJ      ( render )
import           PrettyPrinter
import           Common

-----------------------
-- conversion
-----------------------

-- conversion a términos localmente sin nombres
conversion :: LamTerm -> Term
conversion t = convAux t []

convAux :: LamTerm -> [String] -> Term
convAux (LVar s) e = case maybeBound s e of
  Nothing -> Free (Global s)
  Just i  -> Bound i

convAux (LAbs s t l) e = Lam t (convAux l (s : e))

convAux (LLet s t1 t2) e =  Let (convAux t1 e) (convAux t2 (s : e))

convAux (LApp t1 t2) e = convAux t1 e :@: convAux t2 e

convAux LZero e = Zero
convAux (LSuc n) e = Suc (convAux n e)
convAux (LRec t f n) e = Rec (convAux t e) (convAux f e) (convAux n e)

convAux LNil e = Nil
convAux (LCons x l) e = Cons (convAux x e) (convAux l e)
convAux (LRecL t f l) e = RecL (convAux t e) (convAux f e) (convAux l e)

maybeBound :: String -> [String] -> Maybe Int
maybeBound s e = aux e 0
  where
    aux [] i    = Nothing
    aux (n:e) i = if s == n then Just i else aux e (i+1)

----------------------------
--- evaluador de términos
----------------------------

-- substituye una variable por un término en otro término
sub :: Int -> Term -> Term -> Term
sub i t (Bound j) | i == j    = t
sub _ _ (Bound j) | otherwise = Bound j
sub _ _ (Free n   )           = Free n
sub i t (u   :@: v)           = sub i t u :@: sub i t v
sub i t (Lam t'  u)           = Lam t' (sub (i + 1) t u)

sub i t (Let t'  u) = Let (sub (i + 1) t t') (sub (i + 1) t u)

sub i t Zero        = Zero
sub i t (Suc n)     = Suc (sub i t n)
sub i t (Rec b f n) = Rec (sub i t b) (sub i t f) (sub i t n)

sub i t Nil          = Nil
sub i t (Cons x l)   = Cons (sub i t x) (sub i t l)
sub i t (RecL b f l) = RecL (sub i t b) (sub i t f) (sub i t l)

-- convierte un valor en el término equivalente
quote :: Value -> Term
quote (VLam t f)      = Lam t f

quote (VNum NZero)    = Zero
quote (VNum (NSuc n)) = Suc (quote (VNum n))

quote (VList VNil)       = Nil
quote (VList (VCons x l)) = Cons (quote (VNum x)) (quote (VList l)) 

-- evalúa un término en un entorno dado
eval :: NameEnv Value Type -> Term -> Value
eval nvs (Free n)          = findValue n nvs
eval nvs (Lam t f)         = VLam t f
eval nvs (t1 :@: t2)       = 
  let (VLam s u) = eval nvs t1
      v = eval nvs t2
  in eval nvs (sub 0 (quote v) u)

eval nvs (Let t1 t2) =
  let v = eval nvs t1
  in eval nvs (sub 0 (quote v) t2)

eval nvs Zero = VNum NZero

eval nvs (Suc n) =
  let VNum n' = eval nvs n
  in VNum (NSuc n')

eval nvs (Rec t f Zero) = eval nvs t

eval nvs (Rec t f (Suc n)) = eval nvs (f :@: (Rec t f n) :@: n)

eval nvs (Rec t f n) = eval nvs (Rec t f (quote (eval nvs n)))

eval nvs Nil = VList VNil

eval nvs (Cons x l) =
  let VNum x'  = eval nvs x
      VList l' = eval nvs l
  in VList (VCons x' l')

eval nvs (RecL t f Nil) = eval nvs t

eval nvs (RecL t f (Cons x l)) = eval nvs (f :@: x :@: (RecL t f l))

eval nvs (RecL t f l) = eval nvs (RecL t f (quote (eval nvs l)))


findValue :: Name -> NameEnv Value Type -> Value
findValue n ((m,(v,t)):xs) = if n == m then v else findValue n xs

----------------------
--- type checker
-----------------------

-- infiere el tipo de un término
infer :: NameEnv Value Type -> Term -> Either String Type
infer = infer' []

-- definiciones auxiliares
ret :: Type -> Either String Type
ret = Right

err :: String -> Either String Type
err = Left

(>>=)
  :: Either String Type -> (Type -> Either String Type) -> Either String Type
(>>=) v f = either Left f v
-- fcs. de error

matchError :: Type -> Type -> Either String Type
matchError t1 t2 =
  err
    $  "se esperaba "
    ++ render (printType t1)
    ++ ", pero "
    ++ render (printType t2)
    ++ " fue inferido."

notfunError :: Type -> Either String Type
notfunError t1 = err $ render (printType t1) ++ " no puede ser aplicado."

notfoundError :: Name -> Either String Type
notfoundError n = err $ show n ++ " no está definida."

-- infiere el tipo de un término a partir de un entorno local de variables y un entorno global
infer' :: Context -> NameEnv Value Type -> Term -> Either String Type
infer' c _ (Bound i) = ret (c !! i)
infer' _ e (Free  n) = case lookup n e of
  Nothing     -> notfoundError n
  Just (_, t) -> ret t
infer' c e (t :@: u) = infer' c e t >>= \tt -> infer' c e u >>= \tu ->
  case tt of
    FunT t1 t2 -> if (tu == t1) then ret t2 else matchError t1 tu
    _          -> notfunError tt
infer' c e (Lam t u) = infer' (t : c) e u >>= \tu -> ret $ FunT t tu

infer' c e (Let t u) = infer' c e t >>= \tt -> infer' (tt : c) e u

infer' c e Zero = ret NatT
infer' c e (Suc n) = infer' c e n >>= \tt -> if tt == NatT then ret NatT else matchError NatT tt
infer' c e (Rec t f n) = do
  tt <- infer' c e t
  tf <- infer' c e f
  tn <- infer' c e n
  let te = FunT tt (FunT NatT tt)
  if tf /= te then matchError te tf 
              else if tn /= NatT then matchError NatT tn
                                 else ret tt

infer' c e Nil = ret ListT
infer' c e (Cons x l) = do
  tx <- infer' c e x
  tl <- infer' c e l
  if tx /= NatT then matchError NatT tx
                else if tl /= ListT then matchError ListT tl
                                    else ret ListT

infer' c e (RecL t f l) = do
  tt <- infer' c e t
  tf <- infer' c e f
  tl <- infer' c e l
  let te = FunT NatT (FunT tt tt)
  if tf /= te then matchError te tf 
              else if tl /= ListT then matchError ListT tl
                                 else ret tt