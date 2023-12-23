import Test.Hspec
import qualified  P1 (sumOfMultiples)


t1Cases :: [(Int, Int)]  -- (输入, 预期输出)
t1Cases = [(10, 23), (20, 78), (30, 195)]  -- 示例数据

-- 测试用例
t1 :: Spec
t1 = describe "sumOfMultiples" $ do
  mapM_ (\(input, expected) ->
    it ("calculates the sum of multiples of 3 and 5 below " ++ show input) $
      P1.sumOfMultiples input `shouldBe` expected) t1Cases

main :: IO ()
main = hspec $ do
  t1
