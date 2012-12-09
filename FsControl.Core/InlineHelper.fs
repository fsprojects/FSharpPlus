[<AutoOpen>]
module InlineHelper

module Overloads =
    let inline instance_1 (a:^a                         ) = 
        ( ^a                                : (static member instance: ^a                     -> _) (a          ))
    let inline instance_2 (a:^a,b:^b                    ) =                                                      
        ((^a or ^b                        ) : (static member instance: ^a* ^b                 -> _) (a,b        ))
    let inline instance_3 (a:^a,b:^b,c:^c               ) =                                                          
        ((^a or ^b or ^c                  ) : (static member instance: ^a* ^b* ^c             -> _) (a,b,c      ))
    let inline instance_4 (a:^a,b:^b,c:^c,d:^d          ) =                                                          
        ((^a or ^b or ^c or ^d            ) : (static member instance: ^a* ^b* ^c* ^d         -> _) (a,b,c,d    ))
    let inline instance_5 (a:^a,b:^b,c:^c,d:^d,e:^e     ) =                                                          
        ((^a or ^b or ^c or ^d or ^e      ) : (static member instance: ^a* ^b* ^c* ^d* ^e     -> _) (a,b,c,d,e  ))
    let inline instance_6 (a:^a,b:^b,c:^c,d:^d,e:^e,f:^f) =                                   
        ((^a or ^b or ^c or ^d or ^e or ^f) : (static member instance: ^a* ^b* ^c* ^d* ^e* ^f -> _) (a,b,c,d,e,f))

open Overloads

type Inline = Inline with
    static member inline instance (                            ) = fun (x:'x) -> instance_1(          Unchecked.defaultof<'r>) x :'r
    static member inline instance (a:'a                        ) = fun (x:'x) -> instance_2(a        ,Unchecked.defaultof<'r>) x :'r
    static member inline instance (a:'a, b:'b                  ) = fun (x:'x) -> instance_3(a,b      ,Unchecked.defaultof<'r>) x :'r
    static member inline instance (a:'a, b:'b, c:'c            ) = fun (x:'x) -> instance_4(a,b,c    ,Unchecked.defaultof<'r>) x :'r
    static member inline instance (a:'a, b:'b, c:'c, d:'d      ) = fun (x:'x) -> instance_5(a,b,c,d  ,Unchecked.defaultof<'r>) x :'r
    static member inline instance (a:'a, b:'b, c:'c, d:'d, e:'e) = fun (x:'x) -> instance_6(a,b,c,d,e,Unchecked.defaultof<'r>) x :'r