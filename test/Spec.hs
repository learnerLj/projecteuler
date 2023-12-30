import Test.Hspec
-- import qualified  P1 (sumOfMultiples)


-- t1Cases :: [(Int, Int)]  -- (输入, 预期输出)
-- t1Cases = [(10, 23), (20, 78), (30, 195)]  -- 示例数据

-- -- 测试用例
-- t1 :: Spec
-- t1 = describe "sumOfMultiples" $ do
--   mapM_ (\(input, expected) ->
--     it ("calculates the sum of multiples of 3 and 5 below " ++ show input) $
--       P1.sumOfMultiples input `shouldBe` expected) t1Cases

import Criterion.Main
import P5 (lcmRangeImproved, lcmRangeImproved2)

main :: IO ()
main = defaultMain [
    bgroup "lcmRangeImproved" [ bench "10" $ nf lcmRangeImproved 10
                              , bench "100" $ nf lcmRangeImproved 100
                              , bench "1000" $ nf lcmRangeImproved 1000
                              ],
    bgroup "lcmRangeImproved2" [ bench "10" $ nf lcmRangeImproved2 10
                               , bench "100" $ nf lcmRangeImproved2 100
                               , bench "1000" $ nf lcmRangeImproved2 1000
                               ]
  ]


-- main :: IO ()
-- main = hspec $ do
--   t1
