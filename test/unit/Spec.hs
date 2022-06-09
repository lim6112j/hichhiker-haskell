import Control.Monad (liftM2, replicateM)
import Lib3
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

--prop_dynamic_pack_is_fixpoint ds =
--let pack = dynamicPack ds
--in pack_size pack == pack_size (dynamicPack (dirs pack))

prop_dynamic_pack_small_disk ds =
  let pack = dynamicPack 50000 ds
   in pack_size pack == pack_size (dynamicPack 50000 (dirs pack))

main :: IO ()
main = do
  putStrLn "Test Lib3 --------------"
  --quickCheck prop_dynamic_pack_is_fixpoint
  quickCheck prop_dynamic_pack_small_disk
