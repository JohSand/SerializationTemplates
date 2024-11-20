namespace TypeClassTests.TextJson

#nowarn "3535"

open System.Text.Json

type JsonReader(s: JsonElement) =
    let mutable s = s
    member _.Current = &s

type JsonReader<'x>(s) =
    inherit JsonReader(s)


type SourceReader<'a> =
    static abstract member Deserialize: JsonReader<'x> -> 'a

type JsonSource =
    | TextJson

    interface SourceReader<bool> with
        static member Deserialize(r: JsonReader<'x>) : bool = r.Current.GetBoolean()

    interface SourceReader<string> with
        static member Deserialize(r: JsonReader<'x>) : string = r.Current.GetString()

    interface SourceReader<int> with
        static member Deserialize(r: JsonReader<'x>) : int = r.Current.GetInt32()

    interface SourceReader<float> with
        static member Deserialize(r: JsonReader<'x>) : float = r.Current.GetDouble()


type Deserializable<'a when 'a :> Deserializable<'a>> =
    static abstract member Deserialize: JsonReader<'x> -> 'a

type JsonReader with
    //type implements itself
    static member Deserialize(_: #Deserializable<'a>, r: JsonReader<'x>, _: JsonSource) : 'a =
        //
        'a.Deserialize r

    //list
    static member Deserialize(_: #Deserializable<'a> list, r: JsonReader<'x>, _: JsonSource) =
        let size = r.Current.GetArrayLength()
        let rar = ResizeArray<_>(size)
        let mutable x = r.Current.EnumerateArray()

        while x.MoveNext() do
            let curr = x.Current
            let childReader = (JsonReader<'x>(curr))
            let result = 'a.Deserialize childReader
            rar.Add(result)

        List.ofArray (rar.ToArray())

    //option
    static member Deserialize(_: #Deserializable<'a> option, r: JsonReader<'x>, _: JsonSource) =
        if
            r.Current.ValueKind = JsonValueKind.Null
            || r.Current.ValueKind = JsonValueKind.Undefined
        then
            None
        else
            Some('a.Deserialize r)


    //base implementations (int, string, bool etc)
    static member Deserialize<'a, 'b, 'x when 'b :> SourceReader<'a>>(_: 'a, r: JsonReader<'x>, _: 'b) =
        'b.Deserialize r

    static member Deserialize<'a, 'b, 'x when 'b :> SourceReader<'a>>(_: option<'a>, r: JsonReader<'x>, _: 'b) =
        if
            r.Current.ValueKind = JsonValueKind.Null
            || r.Current.ValueKind = JsonValueKind.Undefined
        then
            None
        else
            Some('b.Deserialize r)

    static member Deserialize<'a, 'b, 'x when 'b :> SourceReader<'a>>(_: 'a list, r: JsonReader<'x>, _: 'b) =
        let size = r.Current.GetArrayLength()
        let rar = ResizeArray<_>(size)
        let mutable x = r.Current.EnumerateArray()

        while x.MoveNext() do
            let curr = x.Current
            let childReader = (JsonReader<'x>(curr))
            let result = 'b.Deserialize childReader
            rar.Add(result)

        List.ofArray (rar.ToArray())


    //extended implementations
    static member Deserialize<'a, 'b when 'b :> SourceReader<'a>>(_: 'a, r: JsonReader<'b>, _: obj) : 'a =
        'b.Deserialize r

    static member Deserialize<'a, 'b when 'b :> SourceReader<'a>>(_: 'a list, r: JsonReader<'b>, _: obj) =
        let size = r.Current.GetArrayLength()
        let rar = ResizeArray<_>(size)
        let mutable x = r.Current.EnumerateArray()

        while x.MoveNext() do
            let curr = x.Current
            let childReader = (JsonReader<'b>(curr))
            let result = 'b.Deserialize childReader
            rar.Add(result)

        List.ofArray (rar.ToArray())

    static member Deserialize<'a, 'b when 'b :> SourceReader<'a>>(_: 'a option, r: JsonReader<'b>, _: obj) =
        if
            r.Current.ValueKind = JsonValueKind.Null
            || r.Current.ValueKind = JsonValueKind.Undefined
        then
            None
        else
            Some('b.Deserialize r)

module JsonReader =
    let inline deserializeCore x (s: JsonReader<_>) =

        let inline call (_output: ^R, reader: ^M, inst: ^I, _ext: ^X) =
            ((^M or ^R): (static member Deserialize: ^R * _ * _ -> ^R) _output, reader, inst)

        call ((Unchecked.defaultof<_>), s, (TextJson), x)

[<AutoOpen>]
module Operator =
    open JsonReader

    let inline (?) (r: JsonReader<'x>) (fieldName: string) =
        let mutable prop = Unchecked.defaultof<_>

        if r.Current.TryGetProperty(fieldName, &prop) then
            let childReader = JsonReader<'x>(prop)
            deserializeCore (Unchecked.defaultof<'x>) childReader
        else
            let childReader = JsonReader<'x>(JsonElement())
            deserializeCore (Unchecked.defaultof<'x>) childReader

module Demo =
    type Type2 = {
        asd1: int
        asd2: bool
        asd3: string
        asd4: float
    } with

        interface Deserializable<Type2> with
            static member Deserialize(r: JsonReader<'x>) : Type2 =
                let asd1 = r?asd1
                let asd2 = r?asd2
                let asd3 = r?asd3
                let asd4 = r?asd4

                { asd1 = asd1; asd2 = asd2; asd3 = asd3; asd4 = asd4 }


    type Type3 = { x: Type2 option; y: int option }

    type Custom =
        | Custom

        interface SourceReader<Type3> with
            static member Deserialize(r: JsonReader<'x>) : Type3 =
                let x = r?x
                let y = r?y
                { x = x; y = y }

    let inline deserialize (s: string) =
        let doc = JsonDocument.Parse(s)
        let reader = JsonReader<Custom>(doc.RootElement)

        JsonReader.deserializeCore (Unchecked.defaultof<Custom>) reader


    let proof () : Type3 =
        deserialize """ { "x": { "asd1": 1, "asd2": true, "asd3": "hi", "asd4": 4 } } """
