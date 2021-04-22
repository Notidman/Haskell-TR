module Main where

import System.IO

data GlobalState = GlobalState { varG :: Int, varT :: Int, varGD :: Int}

main = do
--начало
    putStrLn "Добро пожаловать в Temp & Work!"
    putStrLn "Введите h - для помощи!"
    putStrLn "Введите e - для выхода!"

    loopPower GlobalState { varG = 0, varT = 0, varGD = 0}

    
--меню
loopPower :: GlobalState -> IO ()
loopPower state@GlobalState { varG = g, varT = t, varGD = gd} = do
    putStr "> "
    x <- getLine

    case x of
     "h" -> do
       helpCommand state
     "t" -> do
       print t 
       loopPower state
     "w" -> do
       workCommand state
     "s" -> do
       sleepCommand state
     "m" -> do
       marketplaceCommand state
     "g" -> do
        print g
        loopPower state
     "e" -> do
        quitCommand state
     "goldup" -> do
        loopPower state {varG = g + 10000 }
     _ -> do
        putStrLn "Неизвестная команда."
        quitCommand state

helpCommand state = do
    putStrLn "Список команд: " 
    putStrLn "h - Помощь."
    putStrLn "t - Температура."
    putStrLn "w - Работать в шахте."
    putStrLn "s - Отдых."
    putStrLn "m - Маркеты."
    putStrLn "g - количество золота"
    loopPower state

succ' x = x + 2 

workCommand state@GlobalState { varT = t, varG = g, varGD = gd} = do
  if t == 99 then do
    putStrLn "Вы слишком устали"
    loopPower state { varT = t}
    else do
     putStrLn "Работа..."
     loopPower state { varT = succ t, varG = succ' g + gd }

sleepCommand state@GlobalState { varT = t }  = do
    if t == 0 then do
        putStrLn "Вы отдохнули :3"
        loopPower state
    else do
        putStrLn "Отдых..."
        loopPower state { varT = pred t }
    

marketplaceCommand state = do
    putStrLn "Выберете магазин: / 'E' - для выхода"
    putStrLn "Магазин Bit & Coin. - BC"
    putStrLn "Магазин End & Close. - EC"
    marketplaceCommand' state

marketplaceCommand' state = do
    putStr "> "
    x <- getLine 
    case x of
        "BC" -> do
            marketCommandBC state
        "EC" -> do
            marketCommandEC state
        "E" -> do
            loopPower state
        _ -> do
            putStrLn "Такого магазина нет!"
            putStrLn "Хотите выйти? 'Y'/'N'"
            y <- getLine 
            case y of 
             "Y" -> do
                loopPower state
             "N" -> do
                marketplaceCommand state

marketCommandEC state@GlobalState { varG = g} = do
    putStrLn "Магазин End & Close. - EC"
    putStrLn "В магазине сейчас только один товар:"
    putStrLn "ЛЕГЕНДАРНЫЙ"
    putStrLn "УЛЬТРА"
    putStrLn "МЕГА"
    putStrLn "УБЕР"
    putStrLn "МЕЧ"
    putStrLn "..."
    quitCommandSword state


marketCommandBC state@GlobalState { varG = g, varGD = gd } = do
    putStrLn "Магазин Bit & Coin."
    putStrLn "Выберите Улучшение! / Введите 'E' для выхода"
    putStrLn "Введите 'H' - чтобы узнать какие есть улучшения."
    marketCommandBC' state

marketCommandBC' state@GlobalState { varG = g, varGD = gd } = do
    putStr "> "
    x <- getLine 
    case x of
        "H" -> do
            putStrLn "Перчатки дают +1 G, к получаемому золоту : Стоимость - 100 G"
            putStrLn "Для покупки введите 'P'"
            putStrLn "Лопата даёт +2 G, к получаемому золоту : Стоимость - 999 G"
            putStrLn "Для покупки введите 'L'"
            putStrLn "Бур даёт +3 G, к получаемому золоту : Стоимость - 2500 G"
            putStrLn "Для покупки введите 'B'"
            putStrLn "Лазерный бур даёт +4 G, к получаемому золоту : Стоимость - 3500 G"
            putStrLn "Для покупки введите 'LB'"
            marketCommandBC' state
        "P" -> do
            if g < 100 then do  
                putStrLn "Недостаточно средств"
                marketCommandBC' state
            else do
                marketCommandBC' state { varG = g - 100, varGD = gd + 1 }
                putStrLn "Покупка прошла успешно !"
            marketCommandBC' state
        "L" -> do
            if g < 999 then do
                putStrLn "Недостаточно средств"
                marketCommandBC' state
            else do
                marketCommandBC' state { varG = g - 999, varGD = gd + 2 }
                putStrLn "Покупка прошла успешно !"
            marketCommandBC' state
        "B" -> do
            if g < 2500 then do
                putStrLn "Недостаточно средств"
                marketCommandBC' state 
            else do
                marketCommandBC' state { varG = g - 2500, varGD = gd + 3 }
                putStrLn "Покупка прошла успешно !"
            marketCommandBC' state
        "LB" -> do
            if g < 3500 then do
                putStrLn "Недостаточно средств"
                marketCommandBC' state
            else do
                marketCommandBC' state { varG = g - 3500, varGD = gd + 4 }
                putStrLn "Покупка прошла успешно !"
            marketCommandBC' state
        "E" -> do
            marketplaceCommand state
        _ -> do
            quitCommandM state

quitCommand state = do
        putStrLn "Хотите выйти? 'Y'/'N'"
        putStr "> "
        y <- getLine 
        case y of
            "Y" -> do
             putStrLn ""
            "N" -> do
             loopPower state
            _ -> do
             putStrLn "Неизвестная команда."
             quitCommand state
            
quitCommandM state = do
        putStrLn "Хотите выйти? 'Y'/'N'"
        putStr "> "
        y <- getLine 
        case y of
            "Y" -> do
             loopPower state
            "N" -> do
             marketCommandBC' state
            _ -> do
             putStrLn "Неизвестная команда."
             quitCommandM state

quitCommandSword state@GlobalState { varG = g} = do
    putStrLn "Хотите купить? 'Y'/'N'"
    putStr "> "
    x <- getLine
    case x of
            "Y" -> do
             putStrLn "Стоимость меча 10 000 G"
             putStrLn "Хотите купить? 'Y'/'N'"
             y <- getLine
             case y of
                 "Y" -> do
                     if g < 9999 then do 
                        putStrLn "Недостаточно средств"
                        marketplaceCommand state
                     else
                         winSub state
                 "N" -> do
                    marketplaceCommand state
                 _ -> do
                    putStrLn "Неизвестная команда."
                    quitCommandSword state
                
            "N" -> do
             loopPower state
            _ -> do
             putStrLn "Неизвестная команда."
             quitCommand state

winSub state = do
    putStrLn "Вы прошли игру!"
    putStrLn "УрААААААААААААААААААААААААААААААААААААААААААААААААААААААААААААААААААААААААААААААААААААААААААААААААААААААААААААААААААААААААААААААААААААААААААААААААААААААААААААААААААААААААААААААААААААААААААА"
    putStrLn "Над игрой работали:"
    putStrLn "Boris sis)"
    putStrLn "Нажмите любую клавишу чтобы выйти)"
    _ <- getLine 
    putStrLn ""
