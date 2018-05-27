module Main (main) where

import Graphics.UI.Gtk

main :: IO ()
main = do
  initGUI
  window  <- windowNew
  set window [windowTitle := "Calculadora", containerBorderWidth := 20,
              windowDefaultWidth := 150, windowDefaultHeight := 100]
  table   <- tableNew 8 4 True
  containerAdd window table

  display <- entryNew
  set display [ entryXalign := 1 ]
  tableAttachDefaults table display 0 4 0 1
  button1 <- buttonNewWithLabel "Seno"
  onClicked button1 (sendMessage display window)
  tableAttachDefaults table button1 0 4 1 2
  button2 <- buttonNewWithLabel "Coseno"
  onClicked button2 (sendMessage display window)
  tableAttachDefaults table button2 0 4 2 3
  button3 <- buttonNewWithLabel "Tangente"
  onClicked button3 (sendMessage display window)
  tableAttachDefaults table button3 0 4 3 4
  button4 <- buttonNewWithLabel "Cotangente"
  onClicked button4 (sendMessage display window)
  tableAttachDefaults table button4 0 4 4 5
  button5 <- buttonNewWithLabel "Secante"
  onClicked button5 (sendMessage display window)
  tableAttachDefaults table button5 0 4 5 6
  button6 <- buttonNewWithLabel "Cosecante"
  onClicked button6 (sendMessage display window)
  tableAttachDefaults table button6 0 4 6 7
  button7 <- buttonNewWithLabel "Salir"
  onClicked button7 mainQuit
  tableAttachDefaults table button7 0 4 7 8

  onDestroy window mainQuit
  widgetShowAll window
  mainGUI

sendMessage :: Entry -> Window -> IO ()
sendMessage display window = do
  txt <- entryGetText display
  putStrLn txt
  md <- messageDialogNew (Just window) [] MessageInfo ButtonsOk txt
  set md [ windowTitle := "Funcion" ]
  dialogRun md
  widgetDestroy md
  return ()
