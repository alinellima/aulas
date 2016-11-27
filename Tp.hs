module Tp where


import Control.Applicative

{-

Exercício 8.4 Aproveitando o exercício anterior, faça uma instância de Applicative Functor para
o tipo Coisa definido no exercício 8.1. Para definir um Applicative Functor é necessário definir
seu elemento neutro pure(TresCoisas) e seu operador<*>. A regra para <*> deve ser a distribuição
das funções dos campos para o campo do argumento de forma ordenada. 

Por exemplo: 
			DuasCoisas (+4) (+5) <*> DuasCoisas 2 1 = ghci> DuasCoisas 6 6 (8.3)
			
Cada value constructor deve se combinar com o mesmo, caso contrário o valor retornado é sempre 
Nada. 

-}

data Coisa a = Nada | UmaCoisa a | DuasCoisas a a | TresCoisas a a a deriving Show
               
instance Functor Coisa where
    fmap f Nada = Nada
    fmap f (UmaCoisa x) = UmaCoisa (f x)
    fmap f (DuasCoisas x y) = DuasCoisas (f x) (f y)
    
instance Applicative Coisa where
    pure a = TresCoisas a a a
    (UmaCoisa a) <*> (UmaCoisa b) = UmaCoisa (a b)
    (DuasCoisas a b) <*> (DuasCoisas c d) = DuasCoisas (a c) (b d)
    (TresCoisas a b c) <*> (TresCoisas d e f) = TresCoisas (a d) (b e) (c f)
    _ <*> _ = Nada  
    
{-

pure (+3) <*> TresCoisas 5 10 15

pure (*2) <*> TresCoisas 2 4 8

UmaCoisa (*5) <*> UmaCoisa 30

DuasCoisas (+10) (+5) <*> DuasCoisas 5 10

TresCoisas (+3) (*2) (+1) <*> TresCoisas 2 4 6

4 tabelas 
apresentar o site
15 commits
3 git livre 
uma semana depois da p2



-}

4 tabelas 

{-

Exercício 8.9 Escreva a função do exercício 8.6 em termos dos operadores Applicative.

-}


mult234 :: Double -> Coisa Double
mult234 x = TresCoisas (x*2) (x*3) (x*4)

mult234Applicative :: Double -> Coisa Double
mult234Applicative x = TresCoisas (*2) (*3) (*4) <*> pure x 

{-

mult234 5

mult234Applicative 5 

mult234Applicative 2


-}
