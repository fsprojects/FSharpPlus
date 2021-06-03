namespace FSharpPlus.Internals
#nowarn "0042" // retype

module internal Prelude =
    let inline retype (x: 'T) : 'U = (# "" x: 'U #)
