import Control.Concurrent
import Control.Concurrent.MVar
import System.Random
import Data.List

--Here we defined the User Data type
data User = User {username :: String} deriving (Show,Eq) 

instance Ord User where
    compare (User u1) (User u2) = compare u1 u2

--Defineed the Message data type, sender, receiver and body
data Message = Message {sender :: User, receiver :: User, body :: String}


--Random username function is created
randomUser :: [User] -> IO User
randomUser x = do  

    userList <- newStdGen
    let(n,_) = randomR (0, (length x)-1) userList
    return (x !! n )


--Random message function is created
randomMessage :: [User] -> User -> IO ()
randomMessage users sender = do

    body <- getStdRandom (randomR ('a', 'z'))  --random messages will be selected from a-z
    putStrLn ( username sender ++ " Sent this message: " ++ [body])   --printed the username and message


simulateUser :: [User] -> User -> MVar [(User, Int)] -> IO ()
simulateUser users user totalMessage = do

    random <- (getStdRandom (randomR (1, 1000)))
    threadDelay random
    randomMessage users user  --randommessage
    modifyMVar totalMessage (\map -> return (raiseCount user map, ())) --raisecount function is called to add the count and MVar is modified
    number <- readMVar totalMessage    
    totalCount <- return $ sum $ map snd number  
    if totalCount < 100 then simulateUser users user totalMessage else  return () --loop until the message count reaches 100


raiseCount :: User -> [(User, Int)] -> [(User, Int)]
raiseCount user map =
    let (Just index) = findIndex (\(u, _) -> u == user) map  --raisecount function is defined for the increment 
    in take index map ++ [(user, (snd $ map !! index) + 1)] ++ drop (index + 1) map  --added the count


--Main thread function
main :: IO ()
main = do

    let users = [User ("User " ++ show i) | i <- [1 .. 10]]  --user number is defined here
    totalMessage <- newMVar [(user, 0) | user <- users]
    mapM_ (\user -> forkIO (simulateUser users user totalMessage)) users --giving the total number of messages
    threadDelay =<<randomRIO(1000000,5000000)
    messages <- readMVar totalMessage

    mapM_ (\(user,i) -> putStrLn $ (username user ++ " received " ++ show i ++ " messages." )) messages  --printed all 10 users with number of messages 