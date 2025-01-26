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
import           Data.Text ( Text )
import qualified GI.Cairo.Render as Cairo
import qualified GI.Cairo.Render.Connector as Cairo
import qualified GI.Gtk as Gtk
import qualified GI.Pango as Pango
import qualified GI.PangoCairo as Pango

data AppState = AppState
  { appPageSetup :: Gtk.PageSetup
  , appPrintSettings :: Gtk.PrintSettings
  }

exampleRender :: Cairo.Render ()
exampleRender = do
  Cairo.moveTo 72 72
  Cairo.setFontSize 10
  Cairo.showText ("Hello, Printer!" :: Text)
  Cairo.moveTo 72 144
  Cairo.toRender $ \context -> do
    layout <- Pango.createLayout context
    #setText layout "Hello, Printer!" (-1)
    fontDescription <- Pango.fontDescriptionFromString "Cambria 14"
    #setFontDescription layout (Just fontDescription)
    Pango.showLayout context layout

activate :: Gtk.Application -> IORef AppState -> IO ()
activate app appStateRef = do
  window <- new Gtk.ApplicationWindow
    [ #application := app
    , #title := "GTK4 printer example"
    , #resizable := False
    ]

  box <- new Gtk.Box
    [ #orientation := Gtk.OrientationHorizontal
    , #spacing := 5
    , #marginTop := 5
    , #marginBottom := 5
    , #marginStart := 5
    , #marginEnd := 5
    , #halign := Gtk.AlignCenter
    ]

  pageSetupButton <- new Gtk.Button
    [ #label := "Page setup" ]

  printButton <- new Gtk.Button
    [ #label := "Print" ]

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

  let onClickPrint :: IO ()
      onClickPrint = do
        appState <- readIORef appStateRef
        printOperation <- new Gtk.PrintOperation
          [ #nPages := 1
          , #unit := Gtk.UnitPoints
          , #defaultPageSetup := appPageSetup appState
          , #printSettings := appPrintSettings appState
          , On #drawPage onDrawPage
          ]
        void $
          #run printOperation Gtk.PrintOperationActionPrintDialog (Just window)
        mPrintSettings <- #getPrintSettings printOperation
        case mPrintSettings of
          Nothing -> pure ()
          Just printSettings ->
            writeIORef appStateRef $ appState
              { appPrintSettings = printSettings }

  void $ on pageSetupButton #clicked onClickPageSetup
  void $ on printButton #clicked onClickPrint

  #append box pageSetupButton
  #append box printButton
  #setChild window (Just box)

  window.show

onDrawPage :: Gtk.PrintOperationDrawPageCallback
onDrawPage printContext _pageNr = do
  context <- #getCairoContext printContext
  Cairo.renderWithContext exampleRender context

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
