import qualified Control.Monad
import qualified Data.Char

data Item = Item
    { name :: String
    , weight :: Float
    , price :: Float
    }

data State = State
    { items :: [Item]
    }

instance Show Item where
    show (Item name weight price) = "Name: " ++ name ++ " Weight: " ++ show weight ++ " Price: " ++ show price

maxItemPrice :: Float
maxItemPrice = 1000000

main = do
    let items = [] :: [Item]
    let state = State
            {items = items}
    putStrLn "Welcome to your home."
    homeMenu state

homeMenu :: State -> IO State
homeMenu state = do
    let choiceStrs = 
            [ "-----HOME-----"
            , "1. Help"
            , "2. View items in your home"
            , "3. View items by name"
            , "4. Sort items in your home by price (from high to low)"
            , "5. Go to the store"
            , "6. Exit"
            ]
    let choiceCount = 6

    mapM_ putStrLn choiceStrs
    choice <- getChoice 1 choiceCount

    let action = case choice of
             1 -> helpMenu state
             2 -> viewItems state
             3 -> viewItemsByName state
             4 -> sortItems state
             5 -> store state
             6 -> return state
             _ -> invalid state
    newState <- action
    
    if choice == choiceCount
        then return newState
        else homeMenu newState

getChoice :: Int -> Int -> IO Int
getChoice min max = do
    choice <- getLine
    let intChoice = read choice :: Int
    if intChoice >= min && intChoice <= max
        then return intChoice
        else do
            putStrLn "Invalid choice."
            getChoice min max

invalid :: State -> IO State
invalid state = do
    putStrLn "Invalid choice."
    return state

helpMenu :: State -> IO State
helpMenu state = do
    mapM_ putStrLn
        [ "Welcome to your home management system. You may manage and obtain items."
        , "In order to select a choice, enter its corresponding number."
        ]
    return state

viewItems :: State -> IO State
viewItems state@(State items) = do
    mapM_ print items
    return state

viewItemsByName :: State -> IO State
viewItemsByName state@(State items) = do
    putStrLn "Enter the name of the item you wish to view."
    name <- getLine
    
    let matchingItems = foldr
            (\item@(Item itemName _ _) acc ->
                if itemName == name
                    then item : acc
                    else acc
            ) [] items
    mapM_ print matchingItems
    return state

sortItems :: State -> IO State
sortItems state@(State items) = do
    let newItems = reverse $ selectionSort items
    return State
        { items = newItems
        }

selectionSort :: [Item] -> [Item]
selectionSort [] = []
selectionSort list = minItem : selectionSort (removeAtIndex list minIndex 0)
    where
        minInfo = minimumPrice (zip list [0..length list - 1])
            (Item { name = ""
                , weight = 0
                , price = maxItemPrice + 1
                }, -1)
        minItem = fst minInfo
        minIndex = snd minInfo

minimumPrice :: [(Item, Int)] -> (Item, Int) -> (Item, Int)
minimumPrice [] minItemInfo = minItemInfo
minimumPrice (itemInfo@(Item {price = price}, _):itemInfos) minItemInfo@(Item {price = minPrice}, _) =
    if price < minPrice
        then minimumPrice itemInfos itemInfo
        else minimumPrice itemInfos minItemInfo

removeAtIndex :: [a] -> Int -> Int -> [a]
removeAtIndex [] _ _ = []
removeAtIndex (element:list) removeIndex index =
    if index == removeIndex
        then list
        else element : removeAtIndex list removeIndex (index + 1)

store :: State -> IO State
store state = do
    let choiceStrs =
            [ "-----STORE-----"
            , "1. Buy Item"
            , "2. Exit"
            ]
    let choiceCount = 2

    mapM_ putStrLn choiceStrs

    choice <- getChoice 1 choiceCount
    let action = case choice of
             1 -> buyItem state
             2 -> return state
             _ -> invalid state
    newState <- action
    
    if choice == choiceCount
        then return newState
        else store newState

buyItem :: State -> IO State
buyItem state@(State items) = do
    name <- getItemName

    putStrLn "What is the weight of the item you would like to buy in kilograms?"
    strWeight <- getLine
    let weight = read strWeight :: Float

    price <- getItemPrice

    return State
        { items = Item
            { name = name
            , weight = weight
            , price = price
            } : items
        }
        
getItemName :: IO String
getItemName = do
    putStrLn "What is the name of the item you would like to buy?"
    name <- getLine

    let action = case validItemName name of
            Nothing -> return True
            Just message -> do
                putStrLn message
                return False
    result <- action

    if result
        then return name
        else getItemName

getItemPrice :: IO Float
getItemPrice = do
    putStrLn "What is the price of the item you would like to buy in dollars?"
    strPrice <- getLine
    let price = read strPrice :: Float
    
    let action = case validItemPrice price of
            Nothing -> return True
            Just message -> do
                putStrLn message
                return False
    result <- action
    if result
        then return price
        else getItemPrice
    
validItemName :: String -> Maybe String
validItemName name
    | length name > 10 =
            Just "Name is too long."
    | not $ foldl (\acc x -> acc && Data.Char.isAlpha x) True name =
            Just "Name must not contain numbers or symbols."
    | otherwise = Nothing

validItemPrice :: Float -> Maybe String
validItemPrice price
    | price > maxItemPrice =
            Just "Price is too large."
    | otherwise = Nothing
