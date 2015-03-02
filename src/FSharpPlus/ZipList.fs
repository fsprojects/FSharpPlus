namespace FSharpPlus

open System.Text
open System.Runtime.InteropServices
open FsControl.Core.TypeMethods
open FSharpPlus.Operators
open FSharpPlus.Extensions

type ZipList<'s> = ZipList of 's seq with
    member this.Item n = let (ZipList s) = this in Seq.nth n s

[<RequireQualifiedAccess>]
module ZipList =
    let run   (ZipList x) = x
    let map f (ZipList x) = ZipList (Seq.map f x)
    let singleton x = ZipList (Seq.singleton x)

type ZipList with
    static member Map (ZipList x, f:'a->'b) = ZipList (Seq.map f x)
    static member Return (x:'a)     = ZipList (Seq.initInfinite (konst x))
    static member (<*>) (ZipList (f:seq<'a->'b>), ZipList x) = ZipList (Seq.zip f x |> Seq.map (fun (f,x) -> f x)) :ZipList<'b>
    static member inline Mempty () = result (mempty()) :ZipList<'a>
    static member inline Mappend (x:ZipList<'a>, y:ZipList<'a>) = liftA2 mappend x y :ZipList<'a>
    static member Skip (ZipList s:ZipList<'a> , n, [<Optional>]impl:Skip) = ZipList (Seq.skip n s) :ZipList<'a>
    static member Take (ZipList s:ZipList<'a> , n, [<Optional>]impl:Take) = ZipList (Seq.take n s) :ZipList<'a>
    static member Extract (ZipList s:ZipList<'a>) = Seq.head s
    member this.GetSlice = function
        | None  , None   -> this
        | Some a, None   -> this |> skip a 
        | None  , Some b -> this |> take b 
        | Some a, Some b -> this |> skip a |> take (b-a+1)

    static member inline ToString (s:ZipList<'a>, [<Optional>]impl:ToList) = fun (k:System.Globalization.CultureInfo) ->
            let b = StringBuilder()
            let inline append (s:string) = b.Append s |> ignore
            append "ZipList ["
            let withSemiColons = ZipList.run s |> Seq.map (toStringWithCulture k) |> Seq.intersperse "; "
            Seq.iter append withSemiColons
            append "]"
            b.ToString()