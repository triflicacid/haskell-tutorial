{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
import Data.Kind (Type)

data Nat = Z | S Nat

type family (+) (a :: Nat) (b :: Nat) :: Nat where
    'Z + y = y
    ('S x) + y = 'S (x + y)

data SNat (v :: Nat) where
    SZ :: SNat 'Z
    SS :: SNat n -> SNat ('S n)

add :: SNat a -> SNat b -> SNat (a + b)
add SZ y = y
add (SS x) y = SS (add x y)  


type family (**) (a :: Nat) (b :: Nat) :: Nat where
    'Z ** y = 'Z
    ('S a) ** b = a + (a ** b)

data Vector (size :: Nat) (a :: Type) where
    VNil :: Vector 'Z a
    VCons :: a -> Vector n a -> Vector ('S n) a

vappend :: Vector m a -> Vector n a -> Vector (m + n) a
vappend VNil ys = ys
vappend (VCons x xs) ys = VCons x (vappend xs ys)

flatMap :: Vector n a -> (a -> Vector m b) -> Vector (n ** m) b
flatMap VNil _ = VNil
flatMap (VCons x xs) f = f x `vappend` flatMap xs f