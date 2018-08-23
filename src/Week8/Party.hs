module Week8.Party where

import Data.Monoid
import Data.Tree
import Week8.Employee

---------- ex 1 ----------
glCons :: Employee -> GuestList -> GuestList
glCons x (GL xs fun) = GL (x:xs) $ fun + empFun x

instance Monoid GuestList where
    mempty = GL [] 0
    mappend (GL xs xf) (GL ys yf) = GL (xs ++ ys) (xf + yf)

moreFun :: GuestList -> GuestList -> GuestList
moreFun gx@(GL _ x) gy@(GL _ y) = if x > y then gx else gy

---------- ex 2 ----------
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node x xs) = f x $ map (treeFold f) xs

---------- ex 3 ----------
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList,GuestList)
nextLevel e [] = (GL [e] $ empFun e, mempty)
nextLevel e xs = (withEmp, withoutEmp)
    where
        addFun x = empFun e + x
        (withList, withoutList) = unzip xs
        withEmp = glCons e $ mconcat withoutList
        withoutEmp = mconcat withList

---------- ex 4 ----------
maxFun :: Tree Employee -> GuestList
maxFun tree = moreFun x y
    where
        (x, y) = foldTree nextLevel tree

---------- ex 5 ----------
main :: IO ()
main = readFile "src/Week8/company.txt" >>= \s ->
        let gl = getGuestList s
        in putStrLn (fun gl) >>
            putStrLn "Invited guests:" >>
            mapM_ putStrLn (guests gl)

getGuestList :: String -> GuestList
getGuestList = maxFun . read

fun :: GuestList -> String
fun (GL _ f) = "Total fun: " ++ show f

guests :: GuestList -> [String]
guests (GL xs _) = map empName xs
