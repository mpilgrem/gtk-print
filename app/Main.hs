{-# LANGUAGE ImplicitParams      #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

module Main
  ( main
  ) where

import           Control.Monad ( void )
import           Data.GI.Base ( AttrOp (..), new, on )
import           Data.IORef ( IORef, newIORef, readIORef, writeIORef )
import qualified GI.Gtk as Gtk

data AppState = AppState
  { appPageSetup :: Gtk.PageSetup
  , appPrintSettings :: Gtk.PrintSettings
  }

main :: IO ()
main = do
  pageSetupInit <- Gtk.pageSetupNew
  printSettingsInit <- Gtk.printSettingsNew
  let appState = AppState
        { appPageSetup = pageSetupInit
        , appPrintSettings = printSettingsInit
        }
  appStateRef <- newIORef appState
  app <- new Gtk.Application
    [ #applicationId := "com.pilgrem.gtk-print"
    , On #activate (activate ?self appStateRef)
    ]
  void $ app.run Nothing

activate :: Gtk.Application -> IORef AppState -> IO ()
activate app appStateRef = do
  window <- new Gtk.ApplicationWindow
    [ #application := app
    , #title := "GTK4 printer example"
    , #resizable := False
    ]

  pageSetupButton <- new Gtk.Button
    [ #label := "Page setup" ]

  let onClickPageSetup :: IO ()
      onClickPageSetup = do
        appState <- readIORef appStateRef
        -- On Windows 11, this fails with mingw-w64-x86_64-gtk4 4.16.5-1, with:
        --
        --   GLib-CRITICAL **: <time stamp>: g_utf8_to_utf16: assertion 'str != NULL' failed
        --
        -- Updating to mingw-w64-x86_64-gtk4 4.16.7-1, it reports the same
        -- message to the standard error channel but does not fail.
        pageSetup <- Gtk.printRunPageSetupDialog
          (Just window)
          (Just $ appPageSetup appState)
          (appPrintSettings appState)
        writeIORef appStateRef $ appState
          { appPageSetup = pageSetup }

  void $ on pageSetupButton #clicked onClickPageSetup

  #setChild window (Just pageSetupButton)

  window.show
