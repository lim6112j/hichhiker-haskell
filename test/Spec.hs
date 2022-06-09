import Control.Monad (liftM2, replicateM)
import Lib2
import Test.QuickCheck

instance Arbitrary Dir where
  arbitrary = liftM2 Dir gen_size gen_name
    where
      gen_size = do
        s <- choose (10, 14000)
        return (s * 1024 * 1024)
      gen_name = do
        n <- choose (1, 300)
        replicateM n (elements "fubar/")

prop_greedy_pack_is_fixpoint ds =
  let pack = greedy_pack ds
   in pack_size pack == pack_size (greedy_pack (dirs pack))

main :: IO ()
main = do
  putStrLn "Lib2 test started -----"
  quickCheck prop_greedy_pack_is_fixpoint
