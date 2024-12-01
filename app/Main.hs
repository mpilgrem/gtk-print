{-# LANGUAGE ImplicitParams      #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

module Main
  ( main
  ) where

import           Control.Monad ( void )
import           Data.GI.Base ( AttrOp (..), new, on )
import qualified GI.Gtk as Gtk

main :: IO ()
main = do
  app <- new Gtk.Application
    [ #applicationId := "com.pilgrem.gtk-print"
    , On #activate (activate ?self)
    ]
  void $ app.run Nothing

activate :: Gtk.Application -> IO ()
activate app = do
  window <- new Gtk.ApplicationWindow
    [ #application := app
    , #title := "GTK4 printer example"
    , #resizable := False
    ]

  pageSetupButton <- new Gtk.Button
    [ #label := "Page setup" ]

  let onClickPageSetup :: IO ()
      onClickPageSetup = do
        printSettings <- Gtk.printSettingsNew
        void $ Gtk.printRunPageSetupDialog
          (Just window)
          (Nothing :: Maybe Gtk.PageSetup)
          printSettings

  void $ on pageSetupButton #clicked onClickPageSetup

  #setChild window (Just pageSetupButton)

  window.show
