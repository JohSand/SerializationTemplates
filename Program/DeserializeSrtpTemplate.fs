namespace TypeClassTests

open System.Runtime.InteropServices

#nowarn "3535"



type JsonReader(s: string) = class end 

type JsonReader<'x>(s) =
    inherit JsonReader(s)

type Deserializable<'a when 'a :> Deserializable<'a>> =
    static abstract member Deserialize: JsonReader<'x> -> 'a

type ExtendedDeserializable<'a> =
    static abstract member Deserialize: JsonReader<'x> -> 'a

type DeserInstance =
    | DeserInstance

    interface ExtendedDeserializable<bool> with
        static member Deserialize(arg1: JsonReader<'x>) : bool = true

    interface ExtendedDeserializable<string> with
        static member Deserialize(arg1: JsonReader<'x>) : string =
            "1"

    interface ExtendedDeserializable<int> with
        static member Deserialize(arg1: JsonReader<'x>) : int =
            1

    interface ExtendedDeserializable<float> with
        static member Deserialize(arg1: JsonReader<'x>) : float =
            2

type JsonReader with
    //type implements itself
    static member inline Deserialize(_: ^a, r: JsonReader<'x>, _: DeserInstance) : ^a = 
        (^a: (static member Deserialize: JsonReader<'x> -> ^a)(r))


    static member Deserialize<'a, 'x when 'a :> Deserializable<'a>>
        (_: 'a list, r: JsonReader<'x>, _: DeserInstance)
        : 'a list =
        [ 'a.Deserialize r ]

    static member Deserialize<'a, 'x when 'a :> Deserializable<'a>>
        (_: 'a option, r: JsonReader<'x>, _: DeserInstance)
        : 'a option =
        Some('a.Deserialize r)


    //base implementations (int, string, bool etc)
    static member Deserialize<'a, 'b, 'x when 'b :> ExtendedDeserializable<'a>>
        (_: 'a, r: JsonReader<'x>, _BuiltIns: 'b)
        : 'a =
        'b.Deserialize r

    static member Deserialize<'a, 'b, 'x when 'b :> ExtendedDeserializable<'a>>
        (_: 'a list, r: JsonReader<'x>, _BuiltIns: 'b)
        : 'a list =
        [ 'b.Deserialize r ]


    //extended implementations
    //static member Deserialize<'a, 'b when 'b :> ExtendedDeserializable<'a>>
    //    (_: 'a, r: JsonReader<'b>, _BuiltIns: obj)
    //    : 'a =
    //    'b.Deserialize r

    //static member Deserialize<'a, 'b when 'b :> ExtendedDeserializable<'a>>
    //    (_: 'a list, r: JsonReader<'b>, _BuiltIns: obj)
    //    : 'a list =
    //    [ 'b.Deserialize r ]

    //static member Deserialize<'a, 'b when 'b :> ExtendedDeserializable<'a>>
    //    (_: 'a option, r: JsonReader<'b>, _BuiltIns: obj)
    //    : 'a option =
    //    Some('b.Deserialize r)



[<AutoOpen>]
module Abc =


    let inline deserializeCore x (s: JsonReader<_>) =

        let inline call (_output: ^R, reader: ^M, inst: ^I, _ext: ^X) =
            ((^M or ^R): (static member Deserialize: ^R * _ * _ -> ^R) _output, reader, inst)

        call ((Unchecked.defaultof<_>), s, (DeserInstance), x)

    let inline (?) (r: JsonReader<'x>) (fieldName: string) =
        deserializeCore (Unchecked.defaultof<'x>) r


    type Type1 = {
        asd1: int
        asd2: bool
        asd3: string
        asd4: float
    } with        
        static member Deserialize(r: JsonReader<'x>): Type1 =
            let asd1 = r?asd1
            let asd2 = r?asd2
            let asd3 = r?asd3
            let asd4 = r?asd4
            { asd1 = asd1; asd2 = asd2; asd3 = asd3; asd4 = asd4 }

    let inline x (r: JsonReader<'x>) : Type1 = r?asdasd