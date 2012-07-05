import System.Random
import System

primes = sieve [2..]
  where sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p > 0]

oddPrimes=tail primes
smallOddPrimes=takeWhile (\x->x<100) oddPrimes

repeatKTimesWhileTrue:: (a->IO Bool)->a->Int->IO Bool
repeatKTimesWhileTrue f x k=repeatKTimesWhileTrue' f x k k

repeatKTimesWhileTrue' f x k i
    = if i==0 then return True
      else do
        y<-f x
        if y then repeatKTimesWhileTrue' f x k (i-1)
        else return False
-- a^b (mod n)
powerMod:: Integer->Integer->Integer->Integer
powerMod a b n
    = if b==0 then 1
      else let c=powerMod a (b `div` 2) n in
           if b `mod` 2==0 then (c*c) `mod` n
           else (a*c*c) `mod` n

isPrimeM n=if devidedBySmallOddPrimes n then return False else  millerRabinCheckM n
devidedBySmallOddPrimes n=or $ map (\x->n `mod` x==0) smallOddPrimes
millerRabinCheckM n = repeatKTimesWhileTrue millerRabinCheckOnce n 50

millerRabinCheckOnce n=do
  a<-getStdRandom $ randomR (2,n-1)
  (let (s,d)=g (n-1) 0
       m=powerMod a d n in
   return $ not ( m/=1 && (and $ map ((/=) (n-1))
                          $ take s $ iterate (\y->(y*y) `mod` n) m)))
    where g n s=if n `mod` 2==0 then g (n `div` 2) (s+1)
                else (s,n)

main' n m=mapM_ (\x->do z<-isPrimeM x; if z then print x else return ())
                      $ f $ iterate ((+) 2) (if n `mod` 2==0 then n+1 else n)
    where f=if m<=0 then id else takeWhile (< m)

main=do
  a<-getArgs
  if length a==0 then
      putStrLn "Usage"
  else if length a==1 then
           main' (read $ a!!0) (-1)
       else
           main' (read $ a!!0) (read $ a!!1)
        
