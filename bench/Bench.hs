import Criterion.Main
import Criterion.Types
import LVM


main = defaultMainWith defaultConfig { timeLimit = 60 } [
  bgroup "yalvm" [ bench "count"  $ whnfIO $ LVM.runLuaCode "test\\testFiles\\counter.luac"
               , bench "fac_perv"  $ whnfIO $ LVM.runLuaCode "test\\testFiles\\fac_perf.luac"
               --, bench "count"  $ whnf $ LVM.testFile "counter.luac"
               --, bench "count"  $ whnf $ LVM.testFile "counter.luac"
               ]
  ]