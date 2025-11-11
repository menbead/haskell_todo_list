import Data.List
import Data.List.Split
import Data.Char
import Data.Ord (Down(..))
import System.IO
import System.Console.ANSI

{-
	a program which reads + write from a file, which is a todo list
-}

{-
====== UNUSED ======
-- module enum
data UniModules = COM2004 | COM2008 | COM2108 | COM2109
====== ====== ======
-}


-- contains: name, uniModule, dueDate, importance
data Task = Task { name :: String,
				   uniModule :: String,
				   dueDate :: String, 
				   importance :: Importance 
				 } deriving (Show)

-- importance enum
data Importance = Low | Medium | High deriving (Show, Eq, Ord)

stringToImportance :: String -> Importance
stringToImportance s 
	| s == "High" = High
	| s == "Medium" = Medium
	| otherwise = Low

sortByImportance :: [Task] -> [Task]
sortByImportance = sortOn (Down . importance)

{-
		READING FILE
-}

readTaskList :: IO [Task] 
readTaskList = do
	taskListFile <- openFile "taskList.txt" ReadMode
	taskList <- readFileLines taskListFile
	hClose taskListFile
	return(taskList)


-- reads the file and turns each entry into a list of Tasks
readFileLines :: Handle -> IO [Task]
readFileLines taskListFile = do
	-- first:line by line reading and adding to a list
	-- i lied actually first we should check if end of file
	eOfFile <- hIsEOF taskListFile

	-- if end of file, we can return an empty list
	if eOfFile then return [] 
		else do 
			-- get the current line
			line <- hGetLine taskListFile
			-- turn it into a task
			let task = turnLineIntoTask line

			-- then recursively continue through the file
			restOfLines <- readFileLines taskListFile

			-- construct the list of tasks
			let taskList = sortByImportance $ task : restOfLines
			return (taskList)

-- reads a line and turns it into a Task
turnLineIntoTask :: String -> Task
turnLineIntoTask line = 
	-- split the line into a list of tasks
	let listOfWords = splitOn "," line

	-- then construct the Task
	in Task {name = listOfWords !! 0, 
			 uniModule = listOfWords !! 1, 
			 dueDate = listOfWords !! 2, 
		 	 importance = stringToImportance $ listOfWords !! 3 
		 	}


{- 
		SAVING FILE
		-- writes the task list neatly
-}

-- write tasklist back to file
saveAndClose :: [Task] -> IO ()
saveAndClose taskList = do
	let stringList = unpackToString taskList

	taskListFile <- openFile "taskList.txt" WriteMode

	recursiveAddToFile stringList taskListFile
	hClose taskListFile

saveAndContinue :: [Task] -> IO [Task] 
saveAndContinue taskList = do
	saveAndClose taskList
	newTaskList <- readTaskList
	return(newTaskList)


unpackToString :: [Task] -> [String]
unpackToString taskList = map unpackList taskList

unpackList :: Task -> String
unpackList task = 
	name task ++ "," ++ uniModule task ++ "," ++ dueDate task ++ "," ++ show (importance task)

recursiveAddToFile :: [String] -> Handle -> IO ()
recursiveAddToFile [] _ = return ()
recursiveAddToFile (x:xs) hdl = do
	hPutStrLn hdl x
	recursiveAddToFile xs hdl

{-
	PRINTING OPTIONS
-}

-- decides task format
taskFormat :: Task -> String
taskFormat task = 
	name task ++ 
	"\n    Module     : " ++ uniModule task ++ 
	"\n    Due Date   : " ++ dueDate task ++ 
	"\n    Importance : " ++ show (importance task)

-- writes the task list neatly
printTaskList :: [Task] -> IO ()
printTaskList taskList = mapM_ (putStrLn . taskFormat) taskList

-- print options
printHeader :: [Task] -> IO ()
printHeader taskList = do
	putStrLn("===================================")
	putStrLn("     welcome to the todo list!     ")
	putStrLn("===================================")
	putStrLn("               TASKS               ")
	putStrLn("             <=======>             ")
	printTaskList taskList
	putStrLn("===================================")
	putStrLn("1 :: Add a task\n2 :: Finish a Task\n3 :: Quit")
	putStrLn("\nwhich option would you like to choose?: ")





{-
		the main loop
-}

-- inputting the option, deciding what to do
mainLoop :: [Task] -> IO ()
mainLoop taskList = do
    clearConsole
    -- starting off, write the header
    printHeader taskList

    -- get option
    option <- getLine
    let opt = if null option then '0' else head option

    -- and then do the option
    case opt of
        '1' -> do
            newList <- appendTask taskList
            updatedList <- saveAndContinue newList
            mainLoop updatedList
        '2' -> do
            newList <- finishTask taskList
            updatedList <- saveAndContinue newList
            mainLoop updatedList
        _   -> saveAndClose taskList
	 	

clearConsole :: IO ()
clearConsole = do
	clearScreen
	setCursorPosition 0 0

{-
		ADD A TASK
-}

-- create new task
makeNewTask :: IO Task
makeNewTask = do
	putStrLn("what is the name of the task?: ")
	name <- getLine
	putStrLn("what module is it for?: ")
	uniModule <- getLine
	putStrLn("when is it due?: ")
	dueDate <- getLine
	putStrLn("and how important is it to get done?")
	putStrLn("please type either \"High\", \"Medium\", or \"Low\"")
	inputImp <- getLine
	let importance = stringToImportance . decideImportance . head $ map toLower inputImp


	return (Task {name = name, 
			 uniModule = uniModule, 
			 dueDate = dueDate, 
		 	 importance = importance 
		 	})

decideImportance :: Char -> String
decideImportance s 
			| s == 'h' = "High" 
			| s == 'm' = "Medium"
			| otherwise = "Low"

-- save task to list
appendTask :: [Task] -> IO [Task]
appendTask taskList = do
	task <- makeNewTask
	let newTaskList = sortByImportance $ task : taskList

	return newTaskList




{-
		FINISH A TASK
-}
-- "finish" aka delete a task
finishTask :: [Task] -> IO [Task]
finishTask taskList = do
	putStrLn("what is the name of the task you have finished?: ")
	findName <- getLine

	let newTaskList = sortByImportance $ filter (\x -> name x /= findName) taskList

	return newTaskList


main = do 
	-- open the file and then read it line by line
	taskList <- readTaskList

	-- and then send it off to the main loop!
	mainLoop taskList