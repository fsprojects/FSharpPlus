namespace FSharpPlus.Tests

open System
open NUnit.Framework
open FSharpPlus
open FSharpPlus.TypeLevel
open FSharpPlus.TypeLevel.TypeBool


module BoolTests =

    [<Test>]
    let boolTests =
        
        let inline f0 x = Not x
        Assert (Not (f0 True)); Assert (f0 False)
        let inline f1 x = x &&^ True
        Assert (f1 True); Assert (Not (f1 False))
        let inline f2 x = x &&^ False
        Assert (Not (f2 False)); Assert (Not (f2 True))
        let inline f3 x = True &&^ x
        Assert (f3 True); Assert (Not (f3 False))
        let inline f4 x = False &&^ x
        Assert (Not (f4 False)); Assert (Not (f4 True))
        let inline g1 x = x ||^ True
        Assert (g1 True); Assert (g1 False)
        let inline g2 x = x ||^ False
        Assert (g2 True); Assert (Not (g2 False))
        let inline g3 x = True ||^ x
        Assert (g3 True); Assert (g3 False)
        let inline g4 x = False ||^ x
        Assert (g4 True); Assert (Not (g4 False))
        let inline h1 x = x =^ True
        Assert (h1 True); Assert (Not (h1 False))
        let inline h2 x = x =^ False
        Assert (Not (h2 True)); Assert (h2 False)
        let inline h3 x = True =^ x
        Assert (h3 True); Assert (Not (h3 False))
        let inline h4 x = False =^ x
        Assert (Not (h4 True)); Assert (h4 False)


    
type CaseZ = CaseZ with
    static member inline Gcd (x: ^X, Z) = x

and  CaseS = CaseS with
    static member inline Gcd (x: ^X, y: ^Y) =
        let _ : ^Case = TypeBool.IfThenElse (TypeNat.IsZero (x %^ y)) CaseZ CaseS
        (^Case: (static member Gcd: _*_->_) y, x %^ y)


module NatTests =
    
    let inline hasType<'t> (_: 't) = True
    
    [<Test>]
    let natTests =
        
        let two = S (S Z)
        let inline f1 x = x +^ two
        Assert (f1 Z =^ two); Assert (f1 (S Z) =^ S two); Assert (f1 (S (S Z)) =^ S (S two))
        let inline f2 x = S (S Z) +^ x
        Assert (f2 Z =^ two); Assert (f2 (S Z) =^ S two); Assert (f2 (S (S Z)) =^ S (S two))
        
        let inline f3 x = x -^ two
        Assert (hasType<OverflowError> (f3 Z)); Assert (hasType<OverflowError> (f3 (S Z)))
        Assert (f3 two =^ Z); Assert (f3 (S two) =^ S Z); Assert (f3 (S (S two)) =^ two)
        let inline f4 x = two -^ x
        Assert (f4 Z =^ two); Assert (f4 (S Z) =^ S Z); Assert (f4 (S (S Z)) =^ Z)
        Assert (hasType<OverflowError> (f4 (S (S (S Z)))))
        
        let inline f5 x = x *^ two
        Assert (f5 Z =^ Z); Assert (f5 (S Z) =^ two); Assert (f5 two =^ S (S two))
        let inline f6 x = two *^ x
        Assert (f6 Z =^ Z); Assert (f6 (S Z) =^ two); Assert (f6 two =^ S (S two))
        
        let inline f71  x = x =^ two
        Assert (Not (f71 Z)); Assert (Not (f71 (S Z))); Assert (f71 two); Assert (Not (f71 (S two)))
        let inline f72 x = x =^ Z
        Assert (f72 Z); Assert (Not (f72 (S Z))); Assert (Not (f72 two))
        let inline f73  x = two =^ x
        Assert (Not (f73 Z))
        Assert (Not (f73 (S Z)))
        Assert (f73 two)
        Assert (Not (f73 (S two)))
        let inline f74 x = Z =^ x
        Assert (f74 Z); Assert (Not (f74 (S Z))); Assert (Not (f74 two))
        
        let inline f81  x = x <^ two
        Assert (f81 Z)
        Assert (f81 (S Z))
        Assert (Not (f81 two))
        Assert (Not (f81 (S two)))
        let inline f82 x = x <^ Z
        Assert (Not (f82 Z)); Assert (Not (f82 (S Z)))
        let inline f83 x = two <^ x
        Assert (Not (f83 Z))
        Assert (Not (f83 (S Z)))
        Assert (Not (f83 two))
        Assert (f83 (S two))
        let inline f84 x = Z <^ x
        Assert (Not (f84 Z)); Assert (f84 (S Z)); Assert (f84 two)
        
        let inline f91  x = x <=^ two
        Assert (f91 Z)
        Assert (f91 (S Z))
        Assert (f91 two)
        Assert (Not (f91 (S two)))
        let inline f92 x = x <=^ Z
        Assert (f92 Z); Assert (Not (f92 (S Z)))
        let inline f93 x = two <=^ x
        Assert (Not (f93 Z))
        Assert (Not (f93 (S Z)))
        Assert (f93 two)
        Assert (f93 (S two))
        let inline f94 x = Z <=^ x
        Assert (f94 Z); Assert (f94 (S Z)); Assert (f94 two)
        
        // let inline fa x = x /^ two
        // Assert (fa Z =^ Z)
        // Assert (fa (S Z) =^ Z)
        // Assert (fa two =^ S Z)
        let inline fb x = S (S Z) /^ x
        Assert (hasType<DividedByZeroError> (fb Z))
        Assert (fb (S Z) =^ two)
        Assert (fb two =^ (S Z))
        
        // let inline fc x = x %^ S (S Z)
        // Assert (fc Z =^ Z)
        // Assert (fc (S Z) =^ (S Z))
        // Assert (fc two =^ Z)
        let inline fd x = S (S Z) %^ x
        Assert (hasType<DividedByZeroError> (fd Z))
        Assert (fd (S Z) =^ Z)
        Assert (fd two =^ Z)
        Assert (fd (S two) =^ two)
        

        
        let inline gcd x y =
          let _ : ^Case =
            TypeBool.IfThenElse (TypeNat.IsZero y) CaseZ CaseS
          (^Case: (static member Gcd: _*_->_) x,y)
        let inline lcm x y = (x *^ y) /^ gcd x y
        
        let g1 = gcd (S(S(S(S Z)))) two
        Assert (g1 =^ two)
        let g2 = lcm (S(S(S Z))) two
        Assert (g2 =^ S(S(S(S(S(S Z))))))


open FSharpPlus.Data

module MatrixTests =
    [<Test>]
    let matrixTests =
        let v1 = vector (1,2,3,4,5)
        let v2 = vector (1,2,3,4,5,6,7,8,9,0,1,2,3,4,5)
        let (Vector(_,_,_,_,_)) = v1
        let (Vector(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_)) = v2
        
        let m1 =
          matrix (
            (1,0,0,0),
            (0,1,0,0),
            (0,0,1,0)
          )
        let m2 =
          matrix (
            (1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
            (0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
            (0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0),
            (0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0),
            (0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0),
            (0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0),
            (0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0),
            (0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0)
          )
        let (Matrix(_x1,_x2,_x3)) = m1
        let (Matrix(_y1: int*int*int*int*int*int*int*int*int*int*int*int*int*int*int*int,_y2,_y3,_y4,_y5,_y6,_y7,_y8)) = m2
        ()