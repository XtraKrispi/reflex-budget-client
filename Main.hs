{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE JavaScriptFFI      #-}
{-# LANGUAGE RecursiveDo #-}
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
import Data.List
import Data.Maybe
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
    deriving (Generic, Show, Eq)

instance FromJSON BudgetItem where 
    parseJSON = withObject "BudgetItem" $ \v -> BudgetItem
        <$> v .: "id"
        <*> v .: "description"
        <*> v .: "amount"
        <*> v .: "dueDate"

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
    divClass "container" $ do
        divClass "col-sm-4" $ do
            rec
                let getUpcomingItemsUrl = const "http://localhost:3000/upcomingItems?paid=false&deleted=false" <$> loadEvt
                itemsEvt <- getUpcomingItems getUpcomingItemsUrl
                itemsDyn <- holdDyn [] $ leftmost [itemsEvt]
                upcomingItemActionEvt <- upcomingItemsWidget itemsEvt itemsDyn
                let onlyMarkedItemsEvt = ffilter ffilterFunc upcomingItemActionEvt
                let removedItemsEvt = ffilter (not . ffilterFunc) upcomingItemActionEvt
                removedItemsConfirmationEvt <- ffilter isJust <$> confirmEvent (const "Are you sure?") removedItemsEvt
                let removedAndConfirmedItemsEvt = fromJust <$> removedItemsConfirmationEvt
                budgetItemToRemoveEvt <- performRequestForUpcomingItemAction $ leftmost [removedAndConfirmedItemsEvt, onlyMarkedItemsEvt]
                let itemsToRemoveEvt = filterItemsToRemove <$> attachPromptlyDyn itemsDyn budgetItemToRemoveEvt
                blahDyn <- holdDyn [] $ traceEvent "Remove" itemsToRemoveEvt
                display blahDyn
                return ()
            return ()
        divClass "col-sm-4" blank
        divClass "col-sm-4" blank  

ffilterFunc :: UpcomingItemAction -> Bool
ffilterFunc (MarkItemPaid _) = True
ffilterFunc (RemoveItem _) = False    

filterItemsToRemove :: ([BudgetItem], Maybe BudgetItem) -> [BudgetItem]
filterItemsToRemove (bs, Nothing) = bs 
filterItemsToRemove (bs, Just b) = Data.List.filter (/= b) bs

performRequestForUpcomingItemAction ::(MonadWidget t m) => Event t UpcomingItemAction -> m (Event t (Maybe BudgetItem))
performRequestForUpcomingItemAction upcomingItemActionEvt = do
    response <- performRequestAsync $ getRequest <$> upcomingItemActionEvt
    return $ decodeXhrResponse <$> response 
    where
        getRequest :: UpcomingItemAction -> XhrRequest Text 
        getRequest upcomingItemAction =
            case upcomingItemAction of
                MarkItemPaid b ->
                    let url = "http://localhost:3000/upcomingItems/" <> (pack . show $ budgetItemId b)
                    in  xhrRequest "PATCH" url $ def & xhrRequestConfig_sendData .~ "{paid: true}"
                RemoveItem b ->
                    let url = "http://localhost:3000/upcomingItems/" <> (pack . show $ budgetItemId b)
                    in  xhrRequest "PATCH" url $ def & xhrRequestConfig_sendData .~ "{deleted: true}"

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
                            Nothing  -> []
        
upcomingItemsWidget :: (MonadWidget t m) => Event t [BudgetItem] -> Dynamic t [BudgetItem] -> m (Event t UpcomingItemAction)
upcomingItemsWidget itemsEvt itemsDyn = do
    el "h3" $ text "Upcoming Items"
    loadingIndicatorDyn <- toggle True itemsEvt >>= (return . fmap renderLoading)
    dyn loadingIndicatorDyn
    upcomingItemEvt <- upcomingItemsListWidget itemsDyn
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