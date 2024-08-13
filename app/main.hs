import Control.Monad (mapM_)
import System.Console.ANSI (clearScreen)
import System.Random (randomRIO)
import Data.List (minimumBy)
import Data.Ord (comparing)

-- Definição dos tipos
data Map
  = Road (Int, Int)     -- Estrada normal
  | EmptyRoad (Int, Int) -- Estrada vazia (sem pontuação)
  | Wall (Int, Int)     -- Parede
  deriving (Show, Eq)

data Entity = Entity
  { entityName :: String
  , maxScore   :: Int
  , score      :: Int
  , position   :: (Int, Int)
  } deriving (Show)

-- Função para criar uma matriz 24x21 de Map
criarMatriz :: [[Map]]
criarMatriz = [[elemento (r, c) | c <- [0..20]] | r <- [0..23]]
  where
    elemento (r, c)
      | r == 0 || r == 23 || c == 0 || c == 20 = Wall (r, c)  -- Bordas
      | r `mod` 2 == 0 && c `mod` 2 == 0 = Wall (r, c)  -- Padrão de paredes
      | otherwise = Road (r, c)  -- Caminhos internos

-- Função para converter um Map em uma letra/símbolo
mapToSymbol :: Map -> String
mapToSymbol (Road _) = "."
mapToSymbol (EmptyRoad _) = " "  -- Estrada vazia representada como espaço
mapToSymbol (Wall _) = "\ESC[34m🞓\ESC[0m"

-- Função para imprimir a matriz, considerando as posições das entidades
printMatrizComEntidades :: [[Map]] -> [Entity] -> IO ()
printMatrizComEntidades matriz entidades = mapM_ (putStrLn . unwords . map (mostrarComEntidades entidades)) coordenadas
  where
    coordenadas = [[(r, c) | c <- [0..20]] | r <- [0..23]]
    mostrarComEntidades ents (r, c) =
      case filter (\(Entity _ _ _ (er, ec)) -> (er, ec) == (r, c)) ents of
        [] -> mapToSymbol (matriz !! r !! c)
        (Entity nome _ _ _ : _)
          | nome == "Pacman" -> "\ESC[33m⚉\ESC[0m"  -- Representa o Pacman
          | otherwise -> "\ESC[31m⬤\ESC[0m"  -- Representa os fantasmas

-- Função para criar uma entidade com nome, posição e maxScore zerado
criarEntidade :: String -> (Int, Int) -> Entity
criarEntidade nome pos = Entity { entityName = nome, maxScore = 0, score = 0, position = pos }

-- Função para atualizar a matriz e a pontuação do Pacman
atualizarMapa :: [[Map]] -> (Int, Int) -> ([[Map]], Int)
atualizarMapa matriz pos =
  let (r, c) = pos
      roadOrWall = matriz !! r !! c
      (novaRoad, pontos) = case roadOrWall of
        Road _ -> (EmptyRoad (r, c), 1)  -- Converte Road para EmptyRoad e dá 1 ponto
        Wall _ -> (roadOrWall, 0)        -- Não altera Wall
        EmptyRoad _ -> (roadOrWall, 0)   -- Já é EmptyRoad, não dá ponto
      matrizAtualizada = take r matriz ++ [take c (matriz !! r) ++ [novaRoad] ++ drop (c + 1) (matriz !! r)] ++ drop (r + 1) matriz
  in (matrizAtualizada, pontos)

-- Função para capturar e verificar o caractere do usuário e atualizar a posição
moverPacman :: [[Map]] -> Entity -> Char -> ([[Map]], Entity)
moverPacman matriz pacman movimento =
  let (r, c) = position pacman
      novaPos = case movimento of
        'w' -> (r - 1, c)  -- Mover para cima
        's' -> (r + 1, c)  -- Mover para baixo
        'a' -> (r, c - 1)  -- Mover para a esquerda
        'd' -> (r, c + 1)  -- Mover para a direita
        _   -> (r, c)      -- Movimento inválido
      (novaMatriz, pontos, novaPosValida) =
        if isMovimentoValido matriz novaPos
        then let (m, p) = atualizarMapa matriz novaPos in (m, p, novaPos)
        else (matriz, 0, (r, c)) -- Se movimento não for válido, mantém a posição e matriz atuais
      pacmanAtualizado = pacman { position = novaPosValida, score = score pacman + pontos }
  in (novaMatriz, pacmanAtualizado)

-- Função para verificar se a nova posição é válida (não é uma parede)
isMovimentoValido :: [[Map]] -> (Int, Int) -> Bool
isMovimentoValido matriz (r, c) =
  case matriz !! r !! c of
    Road _ -> True
    EmptyRoad _ -> True  -- Permite mover sobre EmptyRoad
    Wall _ -> False

-- Função para gerar um movimento aleatório válido
randomMove :: [[Map]] -> (Int, Int) -> IO (Int, Int)
randomMove matriz (r, c) = do
  let movimentos = [(r-1, c), (r+1, c), (r, c-1), (r, c+1)]
  let movimentosValidos = filter (isMovimentoValido matriz) movimentos
  if null movimentosValidos
    then return (r, c)  -- Se não houver movimentos válidos, permanece na posição atual
    else do
      idx <- randomRIO (0, length movimentosValidos - 1)
      return (movimentosValidos !! idx)

-- Função para calcular o movimento mais próximo do Pacman
moverFantasmaEmDirecao :: [[Map]] -> Entity -> Entity -> IO Entity
moverFantasmaEmDirecao matriz fantasma pacman = do
  chance <- randomRIO (1 :: Int, 100 :: Int)  -- Especifica o tipo como Int
  if chance <= 40  
    then do
      let (fx, fy) = position fantasma
          (px, py) = position pacman
          movimentos = [(fx - 1, fy), (fx + 1, fy), (fx, fy - 1), (fx, fy + 1)]
          movimentosValidos = filter (isMovimentoValido matriz) movimentos
          melhorMovimento = minimumBy (comparing (\(mx, my) -> abs (mx - px) + abs (my - py))) movimentosValidos
      return $ fantasma { position = melhorMovimento }
    else do
      novaPos <- randomMove matriz (position fantasma)
      return $ fantasma { position = novaPos }
-- Função para mover os fantasmas em direção ao Pacman
moverFantasmas :: [[Map]] -> [Entity] -> Entity -> IO [Entity]
moverFantasmas matriz fantasmas pacman = mapM (\fantasma -> moverFantasmaEmDirecao matriz fantasma pacman) fantasmas

-- Função para verificar se o Pacman encontrou algum fantasma
verificarColisao :: Entity -> [Entity] -> Bool
verificarColisao pacman fantasmas =
  any (\f -> position pacman == position f) fantasmas

-- Função para verificar se ainda há Road no mapa
haRoadsRestantes :: [[Map]] -> Bool
haRoadsRestantes = any (any (\map -> case map of Road _ -> True; _ -> False))  -- Verifica se há algum Road restante

-- Função principal de jogo
main :: IO ()
main = do
  let matriz = criarMatriz
  let pacman = criarEntidade "Pacman" (11, 7)  -- Ajuste a posição inicial se necessário
  let azul = criarEntidade "Azul" (7, 7)      -- Ajuste as posições dos fantasmas
  let vermelho = criarEntidade "Vermelho" (6, 6)
  let rosa = criarEntidade "Rosa" (6, 7)
  let laranja = criarEntidade "Laranja" (6, 8)

  let entidades = [azul, vermelho, rosa, laranja]

  -- Loop principal do jogo
  gameLoop matriz pacman entidades

-- Loop principal do jogo
gameLoop :: [[Map]] -> Entity -> [Entity] -> IO ()
gameLoop matriz pacman fantasmas = do
  clearScreen -- Limpa o terminal
  let todasEntidades = pacman : fantasmas
  printMatrizComEntidades matriz todasEntidades
  putStrLn $ "Pontuação: " ++ show (score pacman)  -- Exibe a pontuação
  putStrLn "Digite um movimento (w, s, a, d):"
  movimento <- getChar
  let (novaMatriz, novoPacman) = moverPacman matriz pacman movimento
  fantasmasMovidos <- moverFantasmas novaMatriz fantasmas novoPacman
  if verificarColisao novoPacman fantasmasMovidos
    then putStrLn $ "Game Over! Pacman encontrou um fantasma. Pontuação final: " ++ show (score novoPacman)
    else if not (haRoadsRestantes novaMatriz)
         then putStrLn $ "Parabéns! Todas as estradas foram limpas. Pontuação final: " ++ show (score novoPacman)
         else gameLoop novaMatriz novoPacman fantasmasMovidos
