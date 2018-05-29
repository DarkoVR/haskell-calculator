module Main (main) where
import Graphics.UI.Gtk
import Resources
import Sine
import Cosine
import Tangent
import Cotangent
import Secant
import Cosecant
import Logarithms
import Exponentials
import Roots

base::Double
base = 5.5
powerby::Integer
powerby = 2
polinomicFun :: [(Double,Integer)]
polinomicFun = [(1,2),(0,1),(-1,0)]

main :: IO ()
main = do
  initGUI
  window  <- windowNew
  set window [windowTitle := "Calculadora", containerBorderWidth := 20,
              windowDefaultWidth := 400, windowDefaultHeight := 100]
  table   <- tableNew 8 8 True
  containerAdd window table

  display <- entryNew
  set display [ entryXalign := 1 ,
                entryText := "[0,3.1416,6.2832,1]"]
  tableAttachDefaults table display 0 8 0 1
  button1 <- buttonNewWithLabel "Seno"
  onClicked button1 (calculateSine display window)
  tableAttachDefaults table button1 0 4 1 2
  button2 <- buttonNewWithLabel "Coseno"
  onClicked button2 (calculateCosine display window)
  tableAttachDefaults table button2 0 4 2 3
  button3 <- buttonNewWithLabel "Tangente"
  onClicked button3 (calculateTangent display window)
  tableAttachDefaults table button3 0 4 3 4
  button4 <- buttonNewWithLabel "Cotangente"
  onClicked button4 (calculateCotangent display window)
  tableAttachDefaults table button4 0 4 4 5
  button5 <- buttonNewWithLabel "Secante"
  onClicked button5 (calculateSecant display window)
  tableAttachDefaults table button5 0 4 5 6
  button6 <- buttonNewWithLabel "Cosecante"
  onClicked button6 (calculateCosecant display window)
  tableAttachDefaults table button6 0 4 6 7
  button7 <- buttonNewWithLabel "Logaritmo Natural"
  onClicked button7 (calculateLogaritm display window)
  tableAttachDefaults table button7 4 8 1 2
  button8 <- buttonNewWithLabel ("Potencia a la " ++ (show powerby))
  onClicked button8 (calculatePower display window)
  tableAttachDefaults table button8 4 8 2 3
  button9 <- buttonNewWithLabel "Factorial ([Int])"
  onClicked button9 (calculateFactorial display window)
  tableAttachDefaults table button9 4 8 3 4
  button10 <- buttonNewWithLabel ("Exponencial base " ++ (show base))
  onClicked button10 (calculateExponential display window)
  tableAttachDefaults table button10 4 8 4 5
  button11 <- buttonNewWithLabel ("Logaritmo base " ++ (show base))
  onClicked button11 (calculateLogaritmBase display window)
  tableAttachDefaults table button11 4 8 5 6
  button11 <- buttonNewWithLabel ("Polinomio x²-1 [(Double, Integer)]")
  onClicked button11 (calculateRoot display window)
  tableAttachDefaults table button11 4 8 6 7
  buttonZ <- buttonNewWithLabel "Salir"
  onClicked buttonZ mainQuit
  tableAttachDefaults table buttonZ 0 8 7 8

  onDestroy window mainQuit
  widgetShowAll window
  mainGUI

calculateSine :: Entry -> Window -> IO ()
calculateSine display window = do
  txt <- entryGetText display
  -- putStrLn txt
  let input = read txt :: [Double]
  let function = (sine input)
  let output | txt == txt = "Seno ( " ++ txt  ++ " ) " ++  "  =>  " ++ (show function)
  md <- messageDialogNew (Just window) [] MessageInfo ButtonsOk output
  set md [ windowTitle := "Función Seno" ]
  dialogRun md
  widgetDestroy md
  return ()

calculateCosine :: Entry -> Window -> IO ()
calculateCosine display window = do
  txt <- entryGetText display
  -- putStrLn txt
  let input = read txt :: [Double]
  let function = (cosine input)
  let output | txt == txt = "Coseno ( " ++ txt  ++ " ) " ++  "  =>  " ++ (show function)
  md <- messageDialogNew (Just window) [] MessageInfo ButtonsOk output
  set md [ windowTitle := "Función Coseno" ]
  dialogRun md
  widgetDestroy md
  return ()

calculateTangent :: Entry -> Window -> IO ()
calculateTangent display window = do
  txt <- entryGetText display
  -- putStrLn txt
  let input = read txt :: [Double]
  let function = (tangent input)
  let output | txt == txt = "Tangente ( " ++ txt  ++ " ) " ++  "  =>  " ++ (show function)
  md <- messageDialogNew (Just window) [] MessageInfo ButtonsOk output
  set md [ windowTitle := "Función Tangente" ]
  dialogRun md
  widgetDestroy md
  return ()

calculateCotangent :: Entry -> Window -> IO ()
calculateCotangent display window = do
  txt <- entryGetText display
  -- putStrLn txt
  let input = read txt :: [Double]
  let function = (cotangent input)
  let output | txt == txt = "Cotangente ( " ++ txt  ++ " ) " ++  "  =>  " ++ (show function)
  md <- messageDialogNew (Just window) [] MessageInfo ButtonsOk output
  set md [ windowTitle := "Función Cotangente" ]
  dialogRun md
  widgetDestroy md
  return ()

calculateSecant :: Entry -> Window -> IO ()
calculateSecant display window = do
  txt <- entryGetText display
  -- putStrLn txt
  let input = read txt :: [Double]
  let function = (secant input)
  let output | txt == txt = "Secante ( " ++ txt  ++ " ) " ++  "  =>  " ++ (show function)
  md <- messageDialogNew (Just window) [] MessageInfo ButtonsOk output
  set md [ windowTitle := "Función Secante" ]
  dialogRun md
  widgetDestroy md
  return ()

calculateCosecant :: Entry -> Window -> IO ()
calculateCosecant display window = do
  txt <- entryGetText display
  -- putStrLn txt
  let input = read txt :: [Double]
  let function = (cosecant input)
  let output | txt == txt = "Cosecante ( " ++ txt  ++ " ) " ++  "  =>  " ++ (show function)
  md <- messageDialogNew (Just window) [] MessageInfo ButtonsOk output
  set md [ windowTitle := "Función Cosecante" ]
  dialogRun md
  widgetDestroy md
  return ()

calculateLogaritm :: Entry -> Window -> IO ()
calculateLogaritm display window = do
  txt <- entryGetText display
  -- putStrLn txt
  let input = read txt :: [Double]
  let function = (logNatural input)
  let output | txt == txt = "Logaritmo ( " ++ txt  ++ " ) " ++  "  =>  " ++ (show function)
  md <- messageDialogNew (Just window) [] MessageInfo ButtonsOk output
  set md [ windowTitle := "Función Logaritmo" ]
  dialogRun md
  widgetDestroy md
  return ()

calculatePower :: Entry -> Window -> IO ()
calculatePower display window = do
  txt <- entryGetText display
  -- putStrLn txt
  let input = read txt :: [Double]
  let function = (polinomial powerby input)
  let output | txt == txt = "Potencia a la "++ (show powerby) ++ " base ( " ++ txt  ++ " ) " ++  "  =>  " ++ (show function)
  md <- messageDialogNew (Just window) [] MessageInfo ButtonsOk output
  set md [ windowTitle := "Función Potencia" ]
  dialogRun md
  widgetDestroy md
  return ()

calculateFactorial :: Entry -> Window -> IO ()
calculateFactorial display window = do
  txt <- entryGetText display
  -- putStrLn txt
  let input = read txt :: [Integer]
  let function = (factorialList input)
  let output | txt == txt = "Factorial ( " ++ txt  ++ " ) " ++  "  =>  " ++ (show function)
  md <- messageDialogNew (Just window) [] MessageInfo ButtonsOk output
  set md [ windowTitle := "Función Factorial" ]
  dialogRun md
  widgetDestroy md
  return ()

calculateExponential :: Entry -> Window -> IO ()
calculateExponential display window = do
  txt <- entryGetText display
  -- putStrLn txt
  let input = read txt :: [Double]
  let function = (exponential base input)
  let output | txt == txt = "Exponencial "++ (show base) ++ " ( " ++ txt  ++ " ) " ++  "  =>  " ++ (show function)
  md <- messageDialogNew (Just window) [] MessageInfo ButtonsOk output
  set md [ windowTitle := "Función Exponencial" ]
  dialogRun md
  widgetDestroy md
  return ()

calculateLogaritmBase :: Entry -> Window -> IO ()
calculateLogaritmBase display window = do
  txt <- entryGetText display
  -- putStrLn txt
  let input = read txt :: [Double]
  let function = (anyLog base input)
  let output | txt == txt = "Logaritmo "++ (show base) ++ " ( " ++ txt  ++ " ) " ++  "  =>  " ++ (show function)
  md <- messageDialogNew (Just window) [] MessageInfo ButtonsOk output
  set md [ windowTitle := "Función Logaritmo base ..." ]
  dialogRun md
  widgetDestroy md
  return ()

calculateRoot :: Entry -> Window -> IO ()
calculateRoot display window = do
  txt <- entryGetText display
  -- putStrLn txt
  let input = read txt :: [(Double, Integer)]
  let function = (raiz input)
  let output | txt == txt = "Polinomio ( " ++ txt  ++ " ) " ++  "  =>  " ++ (show function)
  md <- messageDialogNew (Just window) [] MessageInfo ButtonsOk output
  set md [ windowTitle := "Función Polinomial base ..." ]
  dialogRun md
  widgetDestroy md
  return ()

sendMessage :: Entry -> Window -> IO ()
sendMessage display window = do
  txt <- entryGetText display
  putStrLn txt
  md <- messageDialogNew (Just window) [] MessageInfo ButtonsOk "Función aún no agregada!"
  set md [ windowTitle := "Función perdida!" ]
  dialogRun md
  widgetDestroy md
  return ()
