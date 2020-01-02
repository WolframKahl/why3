{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module Language.Why3.AST where
import Data.Data (Data, Typeable)
import Data.Text (Text)
import GHC.Generics (Generic)
import Control.DeepSeq(NFData(..))

type Name = Text

data Module
        = Module Name [Decl]
        deriving (Eq,Ord,Read,Show,Generic,Typeable,Data)

data Decl
        = Goal          Name Expr
        | Use           (Maybe ImpExp) Name (Maybe Name)
        | Axiom         Name Expr
        | Lemma         Name Expr
        | Type          Name [Text] [(Name,[Text])]
        | TypeDef       Name [Text] [(Name,[Text])] TypeDef
        | Predicate     Name [Text] [Type]
        | PredicateDef  Name [Text] [(Maybe Name,Type)] Expr
        | Function      Name [Text] [Type] Type
        | FunctionDef   Name [Text] [(Maybe Name, Type)] Type Expr
        | DLet          Name [Text] [ParamLet] Type [Spec] Expr
        deriving (Eq,Ord,Read,Show,Generic,Typeable,Data)

data TypeDef
        = TyRecord [ (Name, Type) ]
        | Ty Type
        | TyCase [TyCaseAlt]
        deriving (Eq,Ord,Read,Show,Generic,Typeable,Data)

data TyCaseAlt
        = TyCaseAlt Name [Text] [(Maybe Name, Type)]
        deriving (Eq,Ord,Read,Show,Generic,Typeable,Data)

data ImpExp
        = Import | Export
        deriving (Eq,Ord,Read,Show,Generic,Typeable,Data)

data Literal
        = Integer Integer
        | Real Text
        | Bool Bool
        deriving (Eq,Ord,Read,Show,Generic,Typeable,Data)

data Spec
        = Requires      Expr
        | Ensures       Expr
        deriving (Eq,Ord,Read,Show,Generic,Typeable,Data)

data ParamLet
        = PParamLet
        { paramGhost    :: Bool
        , paramName     :: Name
        , paramType     :: Type }
        deriving (Eq,Ord,Read,Show,Generic,Typeable,Data)

data Expr
        = Lit           Literal
        | Var           Name
        | App           Name [Expr]
        | Let           Bool Pattern Expr Expr
        | If            Expr Expr Expr
        | Match         [Expr] [(Pattern,Expr)]
        | Conn          Conn Expr Expr
        | Not           Expr
        | Quant         Quant [(Name,Type)] [ [Expr] ] Expr
        | Field         Name Expr
        | Record        [(Name, Expr)]
        | RecordUpdate  Expr [(Name, Expr)]
        | Cast          Expr Type
        | Labeled       Text Expr
        | Assert        Expr Expr
        | For           Name Expr Expr [Expr] Expr
        deriving (Eq,Ord,Show,Read,Generic,Typeable,Data)

data Quant
        = Forall | Exists
        deriving (Eq,Ord,Show,Read,Generic,Typeable,Data)

data Conn
        = And | AsymAnd | Or | AsymOr | Implies | Iff
        deriving (Eq,Ord,Show,Read,Generic,Typeable,Data)

data Pattern
        = PWild
        | PVar Name
        | PCon Name [Pattern]
        deriving (Eq,Ord,Show,Read,Generic,Typeable,Data)

data Type
        = TyCon Name [Type]
        | TyVar Name
        | Tuple  [Type]
        | TySnap Type
        deriving (Eq,Ord,Show,Read,Generic,Typeable,Data)


instance NFData Type where
  rnf ty =
    case ty of
      TyCon x xs -> rnf x `seq` rnf xs
      TyVar x    -> rnf x
      Tuple ts   -> rnf ts
      TySnap t   -> rnf t
