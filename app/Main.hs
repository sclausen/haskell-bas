{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import           Data.Monoid                ((<>))
import qualified Data.Text                  as T
import           Lens.Micro                 ((^.))
import           Lens.Micro.TH

import           Brick
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
import qualified Graphics.Vty               as V

type ID = Int

data Name = NameField
          | PriceField
          | IsAdminField
          | HandedField
          | PasswordField
          | LeftHandField
          | RightHandField
          | RadioOption ID
          | AmbiField
          | DesscriptionField
          deriving (Eq, Ord, Show)

data StockInfo =
    StockInfo { _name       :: T.Text
             , _price       :: Int
             , _description :: T.Text
             , _isAdmin     :: Bool
             , _stock       :: Int
             , _password    :: T.Text
             }
             deriving (Show)

makeLenses ''StockInfo

-- This form is covered in the Brick User Guide; see the "Input Forms"
-- section.
mkForm :: StockInfo -> Form StockInfo e Name
mkForm =
    let label s w = padBottom (Pad 1) $
                    (vLimit 1 $ hLimit 15 $ str s <+> fill ' ') w
    in newForm [ label "Search" @@=
                   editTextField name NameField (Just 1)
               , label "Description" @@=
                 B.border @@=
                   editTextField description DesscriptionField (Just 3)
               , label "Price" @@=
                   editShowableField price PriceField
               , label "Stock to buy" @@=
                   radioField stock [  (1, RadioOption 1, "Club Mate")
                                     , (2, RadioOption 2, "Kong")
                                     , (3, RadioOption 3, "Flora Power")
                                     ]
               , label "" @@=
                   checkboxField isAdmin IsAdminField "Is Admin?"
               ]

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (E.editAttr, V.white `on` V.black)
  , (E.editFocusedAttr, V.black `on` V.white)
  , (invalidFormInputAttr, V.white `on` V.red)
  , (focusedFormInputAttr, V.black `on` V.white)
  , ("redText", fg V.red)
  , ("greenText", fg V.green)
  ]

draw :: Form StockInfo e Name -> [Widget Name]
draw f = [
          C.center $ C.joinBorders $ vLimit 30 $ hLimit 80 $
          B.borderWithLabel (padRight Max $ str " Beverage Accounting System " <+> B.hBorder) $
          vBox [
            hBox [ padLeft Max $ (withDefAttr "greenText" $ str "sclausen") <+> str " - " <+> (withDefAttr "redText" $ str "10.30€") ]
            --  <+> withDefAttr "redText" $ str "€ 10.30"
          , B.hBorder
          , hBox [
              vBox [ hLimit 15 $ padRight Max $ str "Stocks\nDebts", hLimit 15 B.hBorder, hLimit 15 $ padBottom Max $ str "Admin\n Users\n Stocks" ]
            , B.vBorder
            , vBox [ padBottom Max form ]
                 ]
              ]
          ]
    where
        form = renderForm f
        help = padTop (Pad 1) $ B.borderWithLabel (str "Help") body
        body = str $ "- Name is free-form text\n" <>
                     "- Age must be an integer (try entering an\n" <>
                     "  invalid age!)\n" <>
                     "- Handedness selects from a list of options\n" <>
                     "- The last option is a checkbox\n" <>
                     "- Enter/Esc quit, mouse interacts with fields"

app :: App (Form StockInfo e Name) e Name
app =
    App { appDraw = draw
        , appHandleEvent = \s ev ->
            case ev of
                VtyEvent (V.EvResize {})     -> continue s
                VtyEvent (V.EvKey V.KEsc [])   -> halt s
                -- Enter quits only when we aren't in the multi-line editor.
                VtyEvent (V.EvKey V.KEnter [])
                    | focusGetCurrent (formFocus s) /= Just DesscriptionField -> halt s
                _ -> do
                    s' <- handleFormEvent ev s

                    -- Example of external validation:
                    -- Require price field to be zero or positive.
                    continue $ setFieldValid ((formState s')^.price >= 0) PriceField s'

        , appChooseCursor = focusRingCursor formFocus
        , appStartEvent = return
        , appAttrMap = const theMap
        }

main :: IO ()
main = do
    let buildVty = do
          v <- V.mkVty =<< V.standardIOConfig
          V.setMode (V.outputIface v) V.Mouse True
          return v

        initialStockInfo = StockInfo { _name = ""
                                   , _description = ""
                                   , _price = 0
                                   , _stock = 1
                                   , _isAdmin = False
                                   , _password = ""
                                   }
        f = setFieldValid False PriceField $
            mkForm initialStockInfo

    f' <- customMain buildVty Nothing app f

    putStrLn "The starting form state was:"
    print initialStockInfo

    putStrLn "The final form state was:"
    print $ formState f'

    if allFieldsValid f'
       then putStrLn "The final form inputs were valid."
       else putStrLn $ "The final form had invalid inputs: " <> show (invalidFields f')

doubleHorizontal :: BS.BorderStyle
doubleHorizontal = BS.BorderStyle
    { BS.bsCornerTL = '╒'
    , BS.bsCornerTR = '╕'
    , BS.bsCornerBR = '╛'
    , BS.bsCornerBL = '╘'
    , BS.bsIntersectL = '╞'
    , BS.bsIntersectR = '╡'
    , BS.bsIntersectT = '╤'
    , BS.bsIntersectB = '╧'
    , BS.bsIntersectFull = '╪'
    , BS.bsHorizontal = '═'
    , BS.bsVertical = '│'
    }
