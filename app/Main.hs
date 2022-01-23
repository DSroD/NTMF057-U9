module Main where

import Integration (integrate, henon, integrateRK45Step, integrateCashKarpStep, PSPoint, IntegrationType(..), IntegrationStepParameters(..))
import Utils (FourTuple(FourTuple), toList)

main :: IO ()
main = do 
    --chainNTimes 15 init 1e-5 1e-8
    print "Euler:"
    print $ toList finalEuler
    print $ henon finalEuler
    print "-----------------------"
    print "RK4:"
    print $ toList finalRK4
    print $ henon finalRK4
    print "-----------------------"
    print "RK45:"
    print $ toList finalRK45
    print $ henon finalRK45
    print "-----------------------"
    print "CashKarp:"
    print $ toList finalCashKarp
    print $ henon finalCashKarp
    print "-----------------------"
    print $ henon init
    where 
        finalEuler = integrate init IntegrationStepParameters { dt = 1e-6, maxt = 1, tol = Nothing } Euler
        finalRK4 = integrate init IntegrationStepParameters { dt = 1e-5, maxt = 1, tol = Nothing } RK4
        finalRK45 = integrate init IntegrationStepParameters { dt = 1e-5, maxt = 1, tol = Just 1e-10 } RK45
        finalCashKarp = integrate init IntegrationStepParameters { dt = 1e-5, maxt = 1, tol = Just 1e-10 } CashKarp
        init = FourTuple 0.6 0 0 0.03

chainNTimes :: Int -> PSPoint -> Double -> Double -> IO ()
chainNTimes n pt dt tol = do
    if n == 0 then return ()
    else do
        print $ toList na
        print ns
        print $ henon na
        chainNTimes (n-1) na ns tol
    -- where (na, ns) = integrateRK45Step pt dt tol
    where (na, ns) = integrateCashKarpStep pt dt tol
        