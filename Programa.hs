main = do
    funcionMenu


funcionMenu = do
    putStrLn("======BIENVENIDO AL MENU=========")
    putStrLn("1.-palindromo")
    putStrLn("2.-condicional[numero del 1 al 10]")
    putStrLn("3.-Factorial de un numero")
    putStrLn("4.-caculadora")
    putStrLn("5.-Desaparecer Numeros")
    putStrLn("6.-Salir")

    opcion <- getLine
  
    case opcion of
        "1" -> palindromo 
        "2" -> condicional 1
        "3" ->funcionFactorial
        "4" ->menuCal
        "5" -> funcionLista        
        "6" -> putStrLn("saliendo")
        _  -> print("Opcion invalida")

{-********************PALINDROMO************************************-}
palindromo = do
            putStrLn("ingrese su cadena")
            n <- getLine
            funcionPalin(n)
           
funcionPalin n = do
            if n == reverse n
            then do
                putStrLn("Es un palindromo")
            else do
                putStrLn("no lo es")
            funcionMenu

{-************************NUMEROS DEL 1 AL 10****************************************-}
condicional n = do
                if n <= 10
                    then do
                    print n
                    condicional (n+1) 
                else
                    putStrLn("lista terminada")   

                funcionMenu       
{-*******************************FACTORIAL****************************************************+-}    

funcionFactorial = do
                    putStrLn("Ingrese un numero")    
                    n  <- getLine
                    operacionFactorial(1) (read n) (1)

operacionFactorial a b c = do
                        if a <= b
                            then do
                                print a
                                operacionFactorial(a+1)(b)(c*a)
                        else
                            
                            putStrLn("El factorial del numero es:"++show(c))
                        putStrLn("Fin")
                        funcionMenu
                
 {-*************************CALCULADORA*********************************-}      
menuCal = do
            putStrLn("======MENU CALCULADORA====")
            putStrLn("1.- Suma")
            putStrLn("2.- Resta")
            putStrLn("3.- Multiplicacion")
            putStrLn("4.- Division")
            putStrLn("que opcion quiere realizar")
            o <- getLine
    
            case o of
                    "1" -> funcionSuma
                    "2" -> funcionResta
                    "3" -> funcionMultiplicacion
                    "4" -> funcionDivision
      

funcionSuma  = do
        putStrLn("numero 1")
        a <- getLine
        putStrLn("numero 2")
        b <- getLine
        putStrLn("El resultado es: "++show(suma(read a) (read b)))
        funcionMenu
      
suma a b =do
          a+b




funcionResta = do
        putStrLn("numero 1")
        c <- getLine
        putStrLn("numero 2")
        d <- getLine
        putStrLn("El resultado es: "++show(resta(read c) (read d)))
        funcionMenu

resta c d =do
          c-d


   

funcionMultiplicacion = do
        putStrLn("numero 1")
        e <- getLine
        putStrLn("numero 2")
        g <- getLine
        putStrLn("El resultado es: "++show(multiplicacion(read e) (read g)))
        funcionMenu

multiplicacion e g =do
          e*g




funcionDivision = do
        putStrLn("==ESTAS EN LA OPCION DIVISION==")
        putStrLn("numero 1")
        h <- getLine
        putStrLn("numero 2")
        i <- getLine
        putStrLn("El resultado es: "++show(division(read h) (read i)))
        funcionMenu


division h i = do
         h/i

{-****************************************DESAPARECER NUMEROS***********************************************-}
funcionLista = do
            putStrLn("ingrese los numeros del 0 al 10")
            lista <- getLine
            funcionNume (lista)
            funcionMenu

funcionNume lista = do
                if null lista
                    then do
                    putStrLn("terminar")      
                else do
                    print(lista)
                    let lista2 = init lista
                    funcionNume(lista2)

