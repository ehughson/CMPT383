-- Part A --

-- Question #1
-- append x to the end of a list. 

snoc :: a -> [a] -> [a]
snoc n []     = n : []  -- this is where the appendage happens to the end of the list using recursion. 
snoc n (x:xs) = x: (snoc n (xs))

-- Question #2
-- write version of ++ which makes two lists into one. 
myappend :: [a] -> [a] -> [a]
myappend [] lst2      = lst2 
myappend  (x:xs) lst2 = x: (myappend (xs) lst2) -- this is where the ++ happens

-- Question #3
-- reverse a list
myreverse :: [a] -> [a]
myreverse []     = []
myreverse (x:xs) = (myappend (myreverse xs) [x]) 

--Question #4
--determine prime by checking its divisors
divPrime :: Integer -> Integer -> Bool
divPrime n div 
       | div == 1         = True
       | n `mod` 2 == 0   = False
       | n `mod` 3 == 0   = False
       | n `mod` 5 == 0   = False
       | n `mod` div == 0 = False
       | otherwise        = (divPrime n (div-1))

-- reverse the prime
reversePrime:: Integer -> Integer
reversePrime n = read ( myreverse (show n)) -- read is used to read the number


-- check if the prime is a prime by calling the helper functions
check_emirp :: Integer -> Bool
check_emirp  n 
        | n < 13                  = False
        | n == reversePrime n     = False -- check to make sure the prime reverse is not the same as
        | otherwise               = (divPrime n (n-1) && divPrime (reversePrime n) (reversePrime (n-1)))

count_emirps:: Integer -> Integer
count_emirps n 
            |n < 0 = error "Primes cannot be negative" 
            |n == 0         = 0
            |check_emirp  n = 1 + count_emirps (n - 1)
            |otherwise      = count_emirps (n-1)



-- Question #5
--use the grab function to determine the biggest sum
biggest_sum :: [[Int]] -> [Int]
biggest_sum (x:xs)    = grab_func sum x xs



grab_func :: (a -> Int) -> a -> [a] -> a
grab_func f x xs
        | length xs == 0      = x
        | f x < f (head xs)   = grab_func f (head xs) (tail xs)
        | otherwise           = grab_func f x (tail xs) 




 -- Question #6
 -- use grab function to determine value of function on list. 
greatest :: (a -> Int) -> [a] -> a
greatest f (x:xs) = grab_func f x xs   



-- Part B --

-- Question #7
--check bit value
is_bit :: Int -> Bool
is_bit n 
      | n == 0    = True
      | n == 1    = True
      | otherwise = False


-- Question #8
--reverse bit value
flip_bit :: Int -> Int
flip_bit n 
        | n == 0    = 1
        | n == 1    = 0
        | otherwise = error "input is not a bit"


-- Question #9a
is_bit_seq1 :: [Int] -> Bool
is_bit_seq1 n
           | length n == 0               = True
           | (is_bit (head n)) == False  = False
           | otherwise                   = is_bit_seq1 (tail n)

-- Question #9b
is_bit_seq2 :: [Int] -> Bool
is_bit_seq2 [] = True
is_bit_seq2 n  = if (is_bit(head n)) == False then False else is_bit_seq2(tail n)

-- Question #9c
is_bit_seq3 :: [Int] -> Bool
is_bit_seq3 [] = True
is_bit_seq3 n  = all is_bit n

-- Question #10a
invert_bits1 :: [Int] -> [Int]
invert_bits1 []     = []
invert_bits1 (x:xs) = (flip_bit x) : (invert_bits1 xs)

-- Question #10b
invert_bits2 :: [Int] -> [Int]
invert_bits2 n = map (flip_bit) n

--Question #10c
invert_bits3 :: [Int] -> [Int]
invert_bits3 n = [flip_bit x | x <- n]

-- Question #11
bit_count :: [Int] -> (Int, Int)
bit_count n
         | ((length n) == 0) = (0, 0)
         | otherwise         = ((counter 0 n),(counter 1 n))


counter :: Int -> [Int] -> Int
counter bit n 
       | ((length n) == 0)  = 0
       | ((head n) == bit)  = 1 + (counter bit (tail n))
       | otherwise          = (counter bit (tail n))  



--Question #12
-- using the map function to map out the given valus for 0 and 1 given the current input to all_basic_bit_seqs. 
-- create a list using myappend of the sequences of zeros and ones recursively to make a list of all possible lists. 
all_basic_bit_seqs :: Int -> [[Int]]
all_basic_bit_seqs n
                  |n == 0      = []
                  |n == 1      = [[0], [1]]
                  |otherwise   = myappend (map (\x-> 0:x) (all_basic_bit_seqs (n-1))) (map (\x ->1:x) (all_basic_bit_seqs (n-1))) 


-- Part C -- 

data List a = Empty | Cons a (List a)
    deriving Show

--Question #13
toList :: [a] -> List a
toList a 
      | ((length a) == 0) = Empty
      | otherwise = Cons (head a) (toList (tail a))

--Question #14
toHaskellList :: List a -> [a]
toHaskellList Empty = []
toHaskellList (Cons first rest) = first : (toHaskellList rest)


-- Question #15
append :: List a -> List a -> List a
append Empty (Cons first rest)                 = (Cons first rest)
append (Cons first rest) Empty                 = (Cons first rest)
append (Cons first1 rest1) (Cons first2 rest2) =  Cons first1 (append rest1 (Cons first2 rest2)) 

--Question #16
removeAll :: (a -> Bool) -> List a -> List a
removeAll f Empty = Empty
removeAll f (Cons first rest) 
          |((f first) == False) = Cons first (removeAll f rest)
          |((f first) == True)  = removeAll f rest
                             

--Question #17
sort :: Ord a => List a -> List a
sort Empty = Empty
sort (Cons first rest) = append firstFew (Cons first (sort lastFew))
                         where firstFew = (removeAll (\x -> x > first) rest)
                              -- middle =  Cons first (removeAll (\x -> x /= first) rest)
                               lastFew = (removeAll (\x -> x <= first) rest)


















