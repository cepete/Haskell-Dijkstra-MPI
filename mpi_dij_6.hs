{- Final Project
Carl Petersen
CSC-503
3/17/2012-}

module Main where


import Control.Parallel.MPI.Simple
import qualified Data.List.Key as KYS
import Data.Map (Map, fromListWith, fromList, (!), adjust, keys)
import Data.Char
import Data.List


root :: Rank
root = 0


{- The intent was that I would pass the route to the indivual processes, and
that I would then start the algorithm in each process to calculate the path for each group
extending from the nodes nearest to the source node and let each process follow from there to the 
destination and then the process that followed the shortest distance to the target would report back, and
then the source node would be prepended (src:route).
-}

main :: IO ()
main = mpiWorld $ \size rank ->
   if size < 2
      then putStrLn "At least two processes are needed"
      else case rank of
            0 -> do numProcs <- commSize commWorld
                    let rlst = [('d','c',23), ('d','w',3), ('d', 'g', 18), ('g','d',3)
							 ,('g','w',12), ('c','w',4), ('c','m',7), ('c','e',20)
							 ,('w','m',23),('m','e',4)]
                    scatterSend commWorld root $ replicate numProcs $ show rlst
                    mapM_ putStrLn =<< gatherRecv commWorld root [fromRank rank :: Char]
                    return()
            _ -> do path <- (scatterRecv commWorld root)
                    gatherSend commWorld root $ show $ (fPth 'd' 'm' $ bM (read path))
                    
--Convert Route List into something consumable by a Map
bC (s, d, l) = [(s, [(d, l)]), (d ,[(s, l)])]

--Convert the list of Routes to a Map
bM mp = fromListWith (++) $ mp >>= bC

--Set the source to zero, and everything else to infinity
setSrc frm mp = (fromList [(vrt, (if vrt==frm then 0 else (1/0), Nothing))
                            | vrt <- keys mp])

-- Follow Path (in reverse) of vertexes of shortest distance
fPth src dst mp = reverse $ fn dst where
	fn xs = xs:maybe [] fn (snd $ dij src mp ! xs)
            
{- First call function to init map
Second fold over list of routes and calculate distances
Then report minimum route from the source
Finally remove source from Map so that it will not be visited again
-}
dij frm mp = do
        fn (setSrc frm mp) (keys mp) where
        fn ssrc [] = ssrc
        fn ssrc mn = fn (foldr calcDst ssrc $ mp ! ky) (delete ky mn) where
            calcDst (nd, imp) = adjust (min (fst(ssrc ! ky)+imp, Just ky)) nd
            ky = KYS.minimum (fst.(ssrc !)) mn
 


