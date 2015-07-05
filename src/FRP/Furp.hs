{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module FRP.Furp where

data Temporal a =
    Event a
  | Signal a
  | None

data SignalVector a =
    Temp (Temporal a)
  | Product (SignalVector a) (SignalVector a)

-- data K (f :: Temporal u -> *) (sv :: SignalVector u) (a :: *) where
--   TemporalK :: (f t -> a) -> K f (Temp t) a
--   ProductK :: K f sv1 a
--            -> K f sv2 b
--            -> (a -> b -> c)
--            -> K f (Product sv1 sv2) c

-- type family Continuous' (t :: Temporal *) where
--   Continuous' (Event a)  = ()
--   Continuous' (Signal a) = a
--   Continuous' None       = ()

-- type family Discrete' (t :: Temporal *) where
--   Discrete' (Event a)  = a
--   Discrete' (Signal a) = ()
--   Discrete' None       = ()

-- newtype Continuous a =
--   Continuous { _continuous :: Continuous' a }

-- newtype Discrete a =
--   Discrete { _discrete :: Discrete' a }

-- type ContinuousK sv a = K Continuous sv a
-- type DiscreteK sv a   = K Discrete sv a

-- type family Sample (f :: Temporal u -> *)
--                    (sv :: SignalVector u) where
--   Sample f (Temp t) = f t
--   Sample f (Product sv1 sv2) = (Sample f sv1, Sample f sv2)

-- evalK :: K f sv a -> Sample f sv -> a
-- evalK (TemporalK k) x = k x
-- evalK (ProductK k k' f) (x, y) = f (evalK k x) (evalK k' y)

-- optimusPrime :: signal -> otherSignal -> transformer -> signalThingy
-- optimusPrime = undefined

type family Interp m (t :: Temporal *) where
  Interp m (Signal a) = m a
  Interp m (Event a)  = (a -> m ()) -> m ()
  Interp m None       = ()

type family InterpSV m (sv :: SignalVector *) where
  InterpSV m (Temp t) = Interp m t
  InterpSV m (Product sv1 sv2) = (InterpSV m sv1, InterpSV m sv2)

-- newtype SignalFunction m (svIn :: SignalVector *) (svOut :: SignalVector *) =
--   SignalFunction (InterpSV m svIn
--               -> (InterpSV m svOut, SignalFunction m svIn svOut))

-- newtype SignalFunction m (svIn :: SignalVector *) (svOut :: SignalVector *) =
--   SignalFunction (InterpSV m svIn -> InterpSV m svOut)

-- compSV :: SignalFunction m b c
--        -> SignalFunction m a b
--        -> SignalFunction m a c
-- compSV (SignalFunction b)
--        (SignalFunction a) =
--   SignalFunction (b . a)

-- hold :: a -> SignalFunction m (Event a) (Signal a)
-- hold = undefined

-- Inputs to the SF implementation function from the input-side sv
type family SignalIn m (sv :: SignalVector *) (k :: *) where
  SignalIn m (Temp (Signal a)) k = m a -> k
  SignalIn m (Temp (Event a)) k =  k
  SignalIn m (Temp None) k = k
  SignalIn m (Product sv1 sv2) k = SignalIn m sv1 (SignalIn m sv2 k)

-- Outputs to the SF implementation function from the output-side sv
type family SignalOut (sv :: SignalVector *) where
  SignalOut (Temp (Signal a)) = a
  SignalOut (Temp (Event a)) = ()
  SignalOut (Temp None) = ()
  SignalOut (Product sv1 sv2) = (SignalOut sv1, SignalOut sv2)

-- Inputs from the output-side sv
type family EventIn m (sv :: SignalVector *) k where
  EventIn m (Temp (Event a)) k = (a -> m ()) -> k
  EventIn m (Temp (Signal a)) k = k
  EventIn m (Temp None) k = k
  EventIn m (Product sv1 sv2) k = EventIn m sv1 (EventIn m sv2 k)

-- Outputs from the input-side sv
type family EventOut m (sv :: SignalVector *) where
  EventOut m (Temp (Signal a)) = ()
  EventOut m (Temp (Event a)) = a -> m ()
  EventOut m (Temp None) = ()
  EventOut m (Product sv1 sv2) = (EventOut m sv1, EventOut m sv2)

newtype SignalFunction m svIn svOut =
  SignalFunction {
    _signalFunction :: SignalIn m svIn
                       (EventIn m svOut (SignalOut svOut, EventOut m svIn))
    }
