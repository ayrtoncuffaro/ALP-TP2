module PrettyPrinter
  ( printTerm  ,     -- pretty printer para terminos
    printType        -- pretty printer para tipos
  )
where

import  Common
import  Text.PrettyPrint.HughesPJ
import  Prelude hiding ((<>))

-- lista de posibles nombres para variables
vars :: [String]
vars =
  [ c : n
  | n <- "" : map show [(1 :: Integer) ..]
  , c <- ['x', 'y', 'z'] ++ ['a' .. 'w']
  ]

parensIf :: Bool -> Doc -> Doc
parensIf True  = parens
parensIf False = id

-- pretty-printer de términos

pp :: Int -> [String] -> Term -> Doc
pp ii vs (Bound k         ) = text (vs !! (ii - k - 1))
pp _  _  (Free  (Global s)) = text s

pp ii vs (i :@: c         ) = sep
  [ parensIf (isLam i) (pp ii vs i)
  , nest 1 (parensIf (isLam c || isApp c) (pp ii vs c))
  ]
pp ii vs (Lam t c) =
  text "\\"
    <> text (vs !! ii)
    <> text ":"
    <> printType t
    <> text ". "
    <> pp (ii + 1) vs c

pp ii vs (Let v t) =
  text "let "
    <> text (vs !! ii)
    <> text " = "
    <> pp ii vs v
    <> text " in "
    <> pp (ii + 1) vs t

pp ii vs Zero = text "0"
pp ii vs (Suc n) =
  text "suc "
    <> parensIf (not (isAtom n)) (pp ii vs n)
pp ii vs (Rec t f n) =
  text "R "
    <> sep
      [ (parensIf (not (isAtom t)) (pp ii vs t))
      , nest 1 (parensIf (not (isAtom f)) (pp ii vs f))
      , nest 1 (parensIf (not (isAtom n)) (pp ii vs n))
      ]

pp ii vs Nil = text "nil"
pp ii vs (Cons x l) =
  text "cons "
    <> sep
      [ (parensIf (not (isAtom x)) (pp ii vs x))
      , nest 1 (parensIf (not (isAtom l)) (pp ii vs l))
      ]
pp ii vs (RecL t f l) =
  text "RL "
    <> sep
      [ (parensIf (not (isAtom t)) (pp ii vs t))
      , nest 1 (parensIf (not (isAtom f)) (pp ii vs f))
      , nest 1 (parensIf (not (isAtom l)) (pp ii vs l))
      ]

isLam :: Term -> Bool
isLam (Lam _ _) = True
isLam _         = False

isApp :: Term -> Bool
isApp (_ :@: _) = True
isApp _         = False

isAtom :: Term -> Bool
isAtom (Bound _) = True
isAtom (Free _)  = True
isAtom Zero      = True
isAtom Nil       = True
isAtom _         = False

-- pretty-printer de tipos
printType :: Type -> Doc
printType EmptyT = text "E"
printType NatT   = text "Nat"
printType ListT  = text "List Nat"
printType (FunT t1 t2) =
  sep [parensIf (isFun t1) (printType t1), text "->", printType t2]


isFun :: Type -> Bool
isFun (FunT _ _) = True
isFun _          = False

fv :: Term -> [String]
fv (Bound _         ) = []
fv (Free  (Global n)) = [n]
fv (t   :@: u       ) = fv t ++ fv u
fv (Lam _   u       ) = fv u
fv (Let t   u       ) = fv t ++ fv u
fv Zero               = []
fv (Suc n)            = fv n
fv (Rec t f n)        = fv t ++ fv f ++ fv n
fv Nil                = []
fv (Cons x l)         = fv x ++ fv l
fv (RecL t f l)       = fv t ++ fv f ++ fv l

---
printTerm :: Term -> Doc
printTerm t = pp 0 (filter (\v -> not $ elem v (fv t)) vars) t

