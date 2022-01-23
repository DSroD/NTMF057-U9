module Integration (integrate, henon, integrateRK45Step, integrateCashKarpStep, PSPoint, IntegrationType(..), IntegrationStepParameters(..)) where

import Utils (ftabs, ftabs0, ftabsinf, FourTuple (FourTuple), (*.), (+.), (-.))
import Control.Monad.Trans.State

type PSPoint = FourTuple Double

data IntegrationStepParameters = IntegrationStepParameters {
    dt :: Double,
    maxt :: Double,
    tol :: Maybe Double
}

data IntegrationType = Euler
    | RK4
    | RK45
    | CashKarp
    | RK4HALF

xdot :: PSPoint -> Double
ydot :: PSPoint -> Double
pxdot :: PSPoint -> Double
pydot :: PSPoint -> Double

xdot (FourTuple _ _ c _) = c
ydot (FourTuple _ _ _ d) = d
pxdot (FourTuple a b _ _) = - a - 2 * (a * b)
pydot (FourTuple a b _ _) = - b - a * a + b * b

henon :: PSPoint -> Double
henon (FourTuple x y px py) = (px * px + py * py) / 2 + (x * x + y * y) / 2 + x * x * y - y * y * y / 3

derivativeOfHenon :: PSPoint -> PSPoint
derivativeOfHenon a = FourTuple (xdot a) (ydot a) (pxdot a) (pydot a)

integrate :: PSPoint -> IntegrationStepParameters -> IntegrationType -> PSPoint
integrate a params t =
    evalState (integrateStateful a params t) (- dt params)

integrateStateful :: PSPoint -> IntegrationStepParameters -> IntegrationType -> State Double PSPoint
integrateStateful a params itype = do
    time <- get
    if time >= maxt params
        then return a
        else do
            put (time + dt params)
            case (itype, tol params) of
                (Euler, _) -> integrateStateful (integrateEulerStep a (dt params)) params itype
                (RK4, _) -> integrateStateful (integrateRK4Step a (dt params)) params itype
                (RK45, Nothing) -> error "RK45 requires a tolerance"
                (RK45, Just tol) ->
                    let (na, nstep) = integrateRK45Step a (dt params) tol
                    in integrateStateful na (IntegrationStepParameters {dt = nstep, tol = Just tol, maxt = maxt params}) itype
                (CashKarp, Just tol) ->
                    let (na, nstep) = integrateCashKarpStep a (dt params) tol
                    in integrateStateful na (IntegrationStepParameters {dt = nstep, tol = Just tol, maxt = maxt params}) itype
                _ -> error "Wrong parameters"

integrateEulerStep :: PSPoint -> Double -> PSPoint
integrateEulerStep (FourTuple a b c d) dt = FourTuple (a + a') (b + b') (c + c') (d + d')
    where (FourTuple a' b' c' d') = (*) dt <$> derivativeOfHenon (FourTuple a b c d)

integrateRK4Step :: PSPoint -> Double -> PSPoint
integrateRK4Step a dt = a +. ((1/6) *. (k1 +. (2 *. k2) +. (2 *. k3) +. k4))
    where k1 = dt *. derivativeOfHenon a
          k2 = dt *. derivativeOfHenon (a +. ((1/2) *. k1))
          k3 = dt *. derivativeOfHenon (a +. ((1/2) *. k2))
          k4 = dt *. derivativeOfHenon (a +. k3)

integrateRK45Step :: PSPoint -> Double -> Double -> (PSPoint, Double)
integrateRK45Step a dt tol =
    let k1 = dt *. derivativeOfHenon a
        k2 = dt *. derivativeOfHenon (a +. ((1/4) *. k1))
        k3 = dt *. derivativeOfHenon (a +. ((3/32) *. k1) +. ((9/32) *. k2))
        k4 = dt *. derivativeOfHenon (a +. ((1932/2197) *. k1) +. ((-7200/2197) *. k2) +. ((7296/2197) *. k3))
        k5 = dt *. derivativeOfHenon (a +. ((439/216) *. k1) +. ((-8) *. k2) +. ((3680/513) *. k3) +. ((-845/4104) *. k4))
        k6 = dt *. derivativeOfHenon (a +. ((-8/27) *. k1) +. (2 *. k2) +. ((-3544/2565) *. k3) +. ((1859/4104) *. k4) +. ((-11/40) *. k5))

        dy4 = a +. ((25/216) *. k1) +. ((1408/2565) *. k3) +. ((2197/4104) *. k4) +. ((-1/5) *. k5)
        dy5 = a +. ((16/135) *. k1) +. ((6656/12825) *. k3) +. ((28561/56430) *. k4) +. ((-9/50) *. k5) +. ((2/55) *. k6)

        terr = ftabsinf (dy5 -. dy4) + tol * 1e-2 -- Adding small integer in order to avoid NaN on division by zero
        nstep = 0.84 * dt * (tol / terr) ** (1/4)
    in if
        terr > tol
        then
            integrateRK45Step a nstep tol
        else
            (dy4, nstep)

-- | Stiff solver
integrateCashKarpStep :: PSPoint -> Double -> Double -> (PSPoint, Double)
integrateCashKarpStep a dt tol =
    let k1 = dt *. derivativeOfHenon a
        k2 = dt *. derivativeOfHenon (a +. ((1/5) *. k1))
        k3 = dt *. derivativeOfHenon (a +. ((3/40) *. k1) +. ((9/40) *. k2))
        k4 = dt *. derivativeOfHenon (a +. ((3/10) *. k1) -. ((9/10) *. k2) +. ((6/5) *. k3))
        k5 = dt *. derivativeOfHenon (a -. ((11/54) *. k1) +. ((5/2) *. k2) -. ((70/27) *. k3) +. ((35/27) *. k4))
        k6 = dt *. derivativeOfHenon (a +. ((1631/55296) *. k1) +. ((175/512) *. k2) +. ((575/13824) *. k3) +. ((44275/110592) *. k4) +. ((253/4096) *. k5))
        
        dy4 = a +. ((37/378) *. k1) +. ((250/621) *. k3) +. ((125/594) *. k4) +. ((512/1771) *. k6)
        dy5 = a +. ((2825/27648) *. k1) +. ((18575/48384) *. k3) +. ((13525/55296) *. k4) +. ((277/14336) *. k5) +. ((1/4) *. k6)
        
        terr = ftabsinf (dy5 -. dy4) + tol * 1e-2
        nstep = 0.8 * dt * (tol / terr) ** (1/4)
    in if
        terr > tol
        then
            integrateCashKarpStep a nstep tol
        else
            (dy4, nstep)