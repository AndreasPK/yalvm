import Criterion.Main
import Criterion.Types
import LVM


main = defaultMainWith defaultConfig { timeLimit = 30 } [
  bgroup "yalvm" [ --bench "count"  $ whnfIO $ LVM.runLuaCode "test\\testFiles\\counter_perf.luac"
               --, bench "fac_perv"  $ whnfIO $ LVM.runLuaCode "test\\testFiles\\fac_perf.luac"
               --, 
                  bench "all" $ whnfIO $ do
                  LVM.runLuaCode "test\\testFiles\\counter_perf.luac"
                  return $ LVM.runLuaCode "test\\testFiles\\counter_perf.luac"
               --, bench "count"  $ whnf $ LVM.testFile "counter.luac"
               --, bench "count"  $ whnf $ LVM.testFile "counter.luac"
               ]
  ]
