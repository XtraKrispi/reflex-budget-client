{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE JavaScriptFFI      #-}
module Main where

import Data.Time.Calendar
import Data.Time.Format
import Data.Time
import Reflex
import Reflex.Dom
import Data.Aeson
import GHC.Generics
import Data.Text
import Data.Monoid
import Formattable.NumFormat
import Data.Map
import Control.Monad
import Control.Monad.IO.Class

import qualified GHCJS.Types    as T
import qualified GHCJS.Foreign  as F
import qualified Data.JSString  as J
-- FFI
foreign import javascript unsafe "confirm($1)" confirmJS :: T.JSString -> IO (Bool)

data BudgetItem = BudgetItem {
     budgetItemId :: Int
    ,description :: Text
    ,amount :: Double
    ,dueDate :: Day
}
    deriving (Generic, Show)

instance FromJSON BudgetItem

data UpcomingItemAction = 
      MarkItemPaid BudgetItem
    | RemoveItem BudgetItem
    deriving (Show)

div' :: (MonadWidget t m) => m a -> m a
div' = el "div"

formatDate :: Day -> Text
formatDate date = pack $ formatTime defaultTimeLocale "%B %e %Y" date

formatCurrency :: Real a => a -> Text
formatCurrency = formatNum (NumFormat 1 "$" "" "," "." Fixed (Just (2, Decimals)) NegMinusSign)

confirmEvent :: MonadWidget t m => (a -> String) -> Event t a -> m (Event t (Maybe a))
confirmEvent str e = performEvent (confirm str <$> e)
    where confirm strFn a = do
                                result <- liftIO $ confirmJS $ J.pack $ strFn a 
                                return $ if result then Just a else Nothing

main :: IO ()
main = mainWidget $ do
    loadEvt <- getPostBuild
    let getUpcomingItemsUrl = const "http://localhost:3000/upcomingItems?paid=false&deleted=false" <$> loadEvt
    itemsEvt <- getUpcomingItems getUpcomingItemsUrl
    itemsDyn <- holdDyn [] itemsEvt
    divClass "container" $ do
        divClass "col-sm-4" $ do
            evt <- upcomingItemsWidget itemsDyn
            return ()
        divClass "col-sm-4" blank
        divClass "col-sm-4" blank

performUpcomingItemAction :: (MonadWidget t m) => UpcomingItemAction -> m ()
performUpcomingItemAction (MarkItemPaid b) = do
    let markPaidUrl = "http://localhost:3000/upcomingItems" <> (pack . show $ budgetItemId b)
    return ()
performUpcomingItemAction (RemoveItem b) = do
    result <- liftIO $ confirmJS "Are you sure?"
    return ()

renderLoading :: (MonadWidget t m ) => Bool -> m ()
renderLoading True = div' $ text "Loading..."
renderLoading False = return ()

getUpcomingItems :: (MonadWidget t m) => Event t Text -> m (Event t [BudgetItem])
getUpcomingItems urlEvt = do
    respEvt <- getAndDecode urlEvt
    return $ ffor respEvt $
                    \result ->
                        case result of
                            Just bis -> bis
                            Nothing -> []

markUpcomingItemPaid :: (MonadWidget t m) => Text -> Event t BudgetItem -> m (Event t (Either String ()))
markUpcomingItemPaid baseUrl budgetItemEvt =
    do 
        return never
upcomingItemsWidget :: (MonadWidget t m) => Dynamic t [BudgetItem] -> m (Event t UpcomingItemAction)
upcomingItemsWidget itemsDyn = do
    el "h3" $ text "Upcoming Items"
    loadingDyn <- toggle True (updated itemsDyn)
    loadingIndicatorDyn <- (return . fmap renderLoading) loadingDyn
    dyn loadingIndicatorDyn
    upcomingItemEvt <- upcomingItemsListWidget itemsDyn

    test <- holdDyn "" $ fmap (pack . show) upcomingItemEvt
    dynText test
    return upcomingItemEvt


upcomingItemsListWidget :: (MonadWidget t m) => Dynamic t [BudgetItem] -> m (Event t UpcomingItemAction)
upcomingItemsListWidget itemsDyn = do
    itemsRenderedDyn <- simpleList itemsDyn upcomingItemWidget
    return $ switchPromptlyDyn $ fmap leftmost itemsRenderedDyn

upcomingItemWidget :: (MonadWidget t m) => Dynamic t BudgetItem -> m (Event t UpcomingItemAction)
upcomingItemWidget budgetItemDyn = do
    budgetItem@(BudgetItem id description amount dueDate) <- sample . current $ budgetItemDyn
    divClass "panel panel-default budget-item" $ do
        divClass "panel-body" $ do
            divClass "row" $ do
                divClass "col-sm-6" $ do
                    divClass "description" $ text description
                divClass "col-sm-6" $ do
                    divClass "amount" $ text . formatCurrency $ amount
            divClass "row" $ do
                divClass "col-sm-8" $ do
                    divClass "due-date" $ text . formatDate $ dueDate
                divClass "col-sm-4" $ do
                    divClass "budget-item-actions" $ do
                        (markPaidEl, _) <- elClass' "a" "mark-paid" $ do
                            elClass "i" "glyphicon glyphicon-ok" blank
                        (removeItemEl, _) <- elClass' "a" "remove-item" $ do
                            elClass "i" "glyphicon glyphicon-trash" blank
                        let markPaidEvt = MarkItemPaid budgetItem <$ domEvent Click markPaidEl
                        let removeItemEvt = RemoveItem budgetItem <$ domEvent Click removeItemEl
                        return $ leftmost [markPaidEvt, removeItemEvt]