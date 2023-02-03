-- Import the necessary modules
import MultiSet
import System.IO

-- Define the readMSet function
readMSet :: String -> IO (MSet String)
readMSet fileName = do
  -- Open the file and read its contents
  contents <- readFile fileName
  -- Split the contents into a list of words
  let contentwords = words contents
  -- Create an empty multiset
  let mset = empty
  -- Add each word to the multiset
  let mset' = foldr (\x acc -> add acc x) mset contentwords
  -- Return the resulting multiset
  return mset'

-- Define the writeMSet function
writeMSet :: MSet String -> String -> IO ()
writeMSet mset fileName = do
  -- Open the file in write mode
  fileHandle <- openFile fileName WriteMode
  -- Write each element of the multiset to the file in the format "<elem> - <multiplicity>"
  mapM_ (\(x, n) -> hPutStrLn fileHandle (x ++ " - " ++ show n)) (elems mset)
  -- Close the file
  hClose fileHandle

-- Define the main function
main :: IO ()
main = do
  -- Load the anagram.txt file into a multiset
  m1 <- readMSet "aux_files/anagram.txt"
  -- Load the anagram_s1.txt and anagram_s2.txt files into multisets
  m2 <- readMSet "aux_files/anagram-s1.txt"
  m3 <- readMSet "aux_files/anagram-s2.txt"
  -- Load the margana2.txt file into a multiset
  m4 <- readMSet "aux_files/margana2.txt"
  -- Check that m1 and m4 are not equal, but have the same elements
  if m1 /= m4
    then putStrLn "m1 and m4 are not equal"
    else putStrLn "m1 and m4 are equal"
  if subeq m1 m4 && subeq m4 m1
    then putStrLn "m1 and m4 have the same elements"
    else putStrLn "m1 and m4 do not have the same elements"
  -- Check that m1 is equal to the union of m2 and m3
  if m1 == union m2 m3
    then putStrLn "m1 is equal to the union of m2 and m3"
    else putStrLn "m1 is not equal to the union of m2 and m3"
  -- Write m1 and m4 to files anag-out.txt and gana-out.txt, respectively
  writeMSet m1 "aux_files/anag-out.txt"
  writeMSet m4 "aux_files/gana-out.txt"
