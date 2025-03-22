
-- LEÓN HERNÁNDEZ IRAN PASCUAL 21200903


import Data.List (nub, intercalate)  -- eliminar duplicados y concatenar strings

-- representar el grafo como una lista de tuplas (nodo, vecinos)
type Grafo = [(Int, [Int])]

-- Grafo de ejemplo
grafo :: Grafo
grafo = [
    (1, [2,3]),
    (2, [1,4,5]),
    (3, [1,5]),
    (4, [2,5]),
    (5, [2,3,4])
  ]

-- obtener los vecinos de un nodo
vecinos :: Grafo -> Int -> [Int]
vecinos g n = case lookup n g of
  Just vs -> vs
  Nothing -> []

-- DFS para detectar ciclos y almacenar la ruta actual
dfsDetectarCiclos :: Grafo -> Int -> Int -> [Int] -> [[Int]]
dfsDetectarCiclos g nodo padre visitados
  | nodo `elem` visitados = [reverse (nodo : takeWhile (/= nodo) visitados)]  -- Ciclo detectado
  | otherwise =
      let nuevosVisitados = nodo : visitados
          resultados = concatMap (\v -> if v /= padre then dfsDetectarCiclos g v nodo nuevosVisitados else []) (vecinos g nodo)
      in resultados

-- encontrar todos los ciclos en el grafo
encontrarCiclos :: Grafo -> [[Int]]
encontrarCiclos g = nub [ciclo | nodo <- nodos g, ciclo <- dfsDetectarCiclos g nodo (-1) []]
  where
    nodos g = nub (map fst g ++ concatMap snd g)

-- contar el número de ciclos
contarCiclos :: Grafo -> Int
contarCiclos g = length (encontrarCiclos g)

-- mostrar los ciclos de manera legible
mostrarCiclos :: [[Int]] -> IO ()
mostrarCiclos ciclos = do
  putStrLn "Ciclos encontrados:"
  mapM_ (\ciclo -> putStrLn $ " - " ++ intercalate "-" (map show ciclo)) ciclos

-- Resultado
main :: IO ()
main = do
  let ciclos = encontrarCiclos grafo
  putStrLn ("¿El grafo tiene ciclos? " ++ show (not (null ciclos)))  
  putStrLn  ("Número de ciclos: " ++ show (contarCiclos grafo))  
  mostrarCiclos ciclos  
