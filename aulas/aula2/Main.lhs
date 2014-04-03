% Funções de Ordem Superior
% Programação Funcional em Haskell
% Prof. Rodrigo Ribeiro

Funções de Ordem Superior --- (I)
=================================

- Configurações iniciais...

> import Prelude hiding (map, foldr, 
>                        foldl, filter,
>                        scanr, scanl,
>                        max)
                        

Funções de Ordem Superior --- (II)
==================================

- Aplicação parcial e funções curried.

> multBy3 :: Int -> Int
> multBy3 = (3 *)

- Note que a função "*" está faltando um parâmetro.
    - Exatamente o parâmetro que é esperado por multBy3.

Funções de Ordem Superior --- (III)
===================================

- Outro exemplo: 
   - Uma função para testar se um caractere é uma letra.

> isLetter :: Char -> Bool
> isLetter = (`elem` (['a'..'z'] ++ ['A'..'Z']))


Funções de Ordem Superior --- (IV)
==================================

- Funções de ordem superior

> applyTwice :: (a -> a) -> a -> a
> applyTwice f x = f (f x)

- Exemplos

~~~~~~~~~~~~~~~~~~~~~~{.haskell}
applyTwice (* 2) 2 = 8
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
