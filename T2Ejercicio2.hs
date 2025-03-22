
--LEÓN HERNÁNDEZ IRAN PASCUAL 21200903

import Control.Monad (forM_)  --- itirerar
import Data.List (intercalate)  -- concatema cadenas

-- Lista original
miLista :: [Int]
miLista = [1..10]

-- concatenación usando mónadas
concatenada :: [Int]
concatenada = miLista ++ [11,12]

-- head y last usando Maybe para manejar errores
primerElemento :: Maybe Int
primerElemento = safeHead miLista

ultimoElemento :: Maybe Int
ultimoElemento = safeLast miLista

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just (last xs)

-- filtrar pares usando listas 
pares :: [Int]
pares = [x | x <- miLista, even x]

-- cuadrados usando mapM y efectos
cuadrados :: IO [Int]
cuadrados = mapM (\x -> return (x^2)) miLista

-- Función para mostrar todo usando mónadas
mainListas :: IO ()
mainListas = do
  putStrLn $ "Concatenación: " ++ show concatenada
  putStrLn $ "Primer elemento: " ++ show primerElemento
  putStrLn $ "Último elemento: " ++ show ultimoElemento
  putStrLn $ "Pares: " ++ intercalate ", " (map show pares)
  cs <- cuadrados
  putStrLn $ "Cuadrados: " ++ intercalate ", " (map show cs)
