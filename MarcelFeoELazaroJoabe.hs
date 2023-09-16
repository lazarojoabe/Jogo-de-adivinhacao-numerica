import System.Random
import System.IO

atualizaArquivo::Int -> IO()
atualizaArquivo qtdTentativas = do
                recorde <- readFile "highscore.txt"
                let recordeInt = read recorde
                if qtdTentativas < recordeInt
                    then do
                        writeFile "highscore.txt" (show qtdTentativas)
                        putStrLn "Voce bateu o recorde!"
                    else 
                        putStrLn "Boa tentativa, mas voce nao bateu o recorde"
                
jogarNovamente :: IO ()
jogarNovamente = do
    putStrLn "Você quer jogar novamente? (s ou n)"
    op <- getLine

    if op == "s"
        then do
            putStrLn "<< JOGO NOVO >>"
            jogar
        else
            putStrLn "Até a próxima :)"

verificaNum :: Int -> Int -> IO ()
verificaNum numAleatorio qtdTentativas = do
    putStr "Qual número você acha é? "
    num <- readLn :: IO Int

    if num == numAleatorio
        then do
            atualizaArquivo qtdTentativas
            putStrLn $ "Você acertou!\nNúmero correto: " ++ show num ++ "\nTotal de tentativas: " ++ show qtdTentativas
            jogarNovamente
        else do
            if num < numAleatorio
                then putStrLn $ "Você errou. O número é maior."
                else putStrLn $ "Você errou. O número é menor."
            verificaNum numAleatorio (qtdTentativas + 1)

jogar :: IO ()
jogar = do
    numAleatorio <- randomRIO (1, 100)
    putStrLn "Digite um número entre 1 e 100:"
    verificaNum numAleatorio 1

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn "<< JOGO DA ADIVINHAÇÃO >>"
    jogar
