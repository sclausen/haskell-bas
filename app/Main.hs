{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import qualified Control.Concurrent.MVar    as M
import           Control.Monad.Trans        (liftIO)
import           Data.Monoid                ((<>))
import qualified Data.Text                  as T
import           Lens.Micro                 ((&), (.~), (^.))
import           Lens.Micro.TH

import           Brick
import qualified Brick.AttrMap              as A
import           Brick.Focus                (focusGetCurrent, focusRingCursor)
import           Brick.Forms                (Form, allFieldsValid,
                                             checkboxField, editPasswordField,
                                             editShowableField, editTextField,
                                             focusedFormInputAttr, formFocus,
                                             formState, handleFormEvent,
                                             invalidFields,
                                             invalidFormInputAttr, newForm,
                                             radioField, renderForm,
                                             setFieldValid, (@@=))
import qualified Brick.Widgets.Border       as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center       as C
import qualified Brick.Widgets.Core         as C
import qualified Brick.Widgets.Edit         as E
import qualified Brick.Widgets.List         as L
import           Data.Storage.Stock
import           Data.Storage.Storage
import           Data.Storage.User
import qualified Data.Vector                as Vec
import qualified Graphics.Vty               as V
import           Text.Printf                (printf)

type ID = Int

data Name = NameField
          | PriceField
          | IsAdminField
          | HandedField
          | MenuList
          | PasswordField
          | LeftHandField
          | RightHandField
          | RadioOption ID
          | AmbiField
          | DesscriptionField
          deriving (Eq, Ord, Show)

data MenuItem = MenuPurchases
              | MenuStocks
              | MenuAdminStocks
              | MenuAdminPurchases
              | MenuAdminUsers
              deriving (Eq, Ord, Show)

data StockInfo = StockInfo
  { _availableStocks :: [Stock]
  , _selectedStock   :: Int
  }
  deriving (Show)

type StockForm = Form StockInfo () Name

data AppState = AppState
  { _selectedMenu :: MenuItem
  , _storage      :: Storage
  , _user         :: User
  , _stocks       :: [Stock]
  , _buyStockForm :: StockForm
  , _menuItems    :: L.List Name MenuItem
  }
  -- deriving (Show)

instance Show AppState where
  show e =
    concat [ "AppState { _selectedMenu = " <> show (_selectedMenu e)
            , " , _storage = " <> show (_storage e)
            , " , _user = " <> show (_user e)
            , " , _stocks = " <> show (_stocks e)
            , " , _buyStockForm = " <> show (formState $ _buyStockForm e)
            , " , _menuItems = " <> show (_menuItems e)
            , "}"
            ]

concat <$> mapM makeLenses [''StockInfo, ''AppState, ''User, ''Stock]

mkForm :: StockInfo -> Form StockInfo () Name
mkForm stockInfo =
    let label s w = padBottom (Pad 1) $
                    (vLimit 1 $ hLimit 15 $ str s <+> fill ' ') <+> w
    in newForm [ label "Stock to buy" @@=
                   radioField selectedStock (makeRadioTuples (stockInfo ^. availableStocks))
               ] stockInfo

makeRadioTuples :: [Stock] -> [(Int, Name, T.Text)]
makeRadioTuples xs = (\(i,s)-> (i, RadioOption i, _stockLabel s)) <$> zip ([0..] :: [Int]) xs

theMap :: A.AttrMap
theMap = attrMap V.defAttr
  [ (E.editAttr,           V.white `on` V.brightBlack)
  , (E.editFocusedAttr,    V.black `on` V.white)
  , (invalidFormInputAttr, V.white `on` V.red)
  , (focusedFormInputAttr, V.black `on` V.white)
  , ("redText",            fg V.red)
  , ("greenText",          fg V.green)
  , ("normalText",         fg V.white)
  , (customAttr,           fg V.brightBlack)
  ]

app :: App AppState () Name
app =
    App { appDraw = drawUI
        , appHandleEvent = handleEvent
        , appChooseCursor = neverShowCursor
        , appStartEvent = return
        , appAttrMap = const theMap
        }

handleEvent :: AppState -> BrickEvent Name () -> EventM Name (Next AppState)
-- handleEvent appState (AppEvent Tick)                       = handleTick appState
-- handleEvent appState (VtyEvent (V.EvKey (V.KChar 'r') [])) = restart appState
handleEvent st (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt st
handleEvent st (VtyEvent (V.EvKey V.KEsc []))        = halt st
-- handleEvent st _                                     = continue st
-- handleEvent st e = M.continue =<< L.handleListEvent e (st ^. menuItems)
handleEvent st e = continue =<< L.handleListEvent e (st ^. menuItems)
-- handleEvent st e = do

--   f' <- handleFormEvent e (st ^. buyStockForm)
--   continue $ st & buyStockForm .~ f'
-- handleEvent st e = do
--   st' <- handleFormEvent e (st ^. buyStockForm)
--   continue st'

data LoadUserInfoEvent = LoadUserInfoEvent

myEvent :: AppState -> BrickEvent n LoadUserInfoEvent -> EventM n (Next AppState)
myEvent s (AppEvent LoadUserInfoEvent) = do
  newUser <- liftIO $ M.readMVar (_currentUser (s^.storage))
  continue $ s & user .~ newUser

drawUI :: AppState -> [Widget Name]
drawUI appState = [
          C.center $ C.joinBorders $ vLimit 30 $ hLimit 80 $
          B.borderWithLabel (padRight Max $ str " Beverage Accounting System " <+> B.hBorder) $
          vBox [
            hBox [ padLeft Max $ drawUserInfo appState ]
          , B.hBorder
          , hBox [
              vBox [ hLimit 15 $ L.renderList listDrawElement True (appState ^. menuItems)]
              -- L.renderList listDrawElement True (appState ^. menuItems)
            , B.vBorder
            , vBox [ padBottom Max $ drawBuyStock appState ]
                 ]
              ]
          ]

drawUserInfo :: AppState -> Widget Name
drawUserInfo appState = userStr <+> str " - " <+> debts
  where
    userStr = withDefAttr (if currentUser ^. userIsAdmin then "greenText" else "normalText") (str $ currentUser ^. userName)
    debts = withDefAttr "redText" (str $ formatCurrency $ currentUser ^. userDebts)
    currentUser = appState ^. user

drawBuyStock :: AppState -> Widget Name
drawBuyStock appState = renderForm (appState ^. buyStockForm)

main :: IO ()
main = do
    libStorage <- newStorage
    user <- M.readMVar $ _currentUser libStorage
    stocks <- _fetchStocks libStorage

    let buildVty = do
          v <- V.mkVty =<< V.standardIOConfig
          V.setMode (V.outputIface v) V.Mouse True
          return v

        stockInfo = StockInfo { _selectedStock = 0
                              , _availableStocks = stocks
                              }
        menuItems = L.list MenuList (Vec.fromList [MenuStocks, MenuPurchases]) 1

        appState = AppState {
          _selectedMenu = MenuStocks
        , _storage = libStorage
        , _user = user
        , _stocks = stocks
        , _menuItems = menuItems
        , _buyStockForm = mkForm stockInfo
        }

    -- f' <- customMain buildVty Nothing app f
    f' <- customMain buildVty Nothing app appState

    putStrLn "The starting app state was:"
    print appState

    putStrLn "The final app state was:"
    print f'

    -- putStrLn "The final form state was:"
    -- print $ formState f'

    -- if allFieldsValid f'
    --    then putStrLn "The final form inputs were valid."
    --    else putStrLn $ "The final form had invalid inputs: " <> show (invalidFields f')

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

listDrawElement :: (Show a) => Bool -> a -> Widget Name
listDrawElement sel a =
    let selStr s = if sel
                   then withAttr customAttr (str s)
                   else str s
    in selStr (show a)

formatCurrency :: Int -> String
formatCurrency a = printf "%.2f€" (fromIntegral a / 100 :: Float)
