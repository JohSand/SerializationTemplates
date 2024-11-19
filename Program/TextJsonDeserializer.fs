namespace TypeClassTests.TextJson

open System.Runtime.InteropServices

#nowarn "3535"
open System.Text.Json
open System.Text.Json.Serialization.Metadata

type JsonReader(s: JsonElement) = 
    let mutable s = s
    member _.Current = &s

type JsonReader<'x>(s) =
    inherit JsonReader(s)

type Deserializable<'a when 'a :> Deserializable<'a>> =
    static abstract member Deserialize: JsonReader<'x> -> 'a

type ExtendedDeserializable<'a> =
    static abstract member Deserialize: JsonReader<'x> -> 'a

type DeserInstance =
    | DeserInstance

    interface ExtendedDeserializable<bool> with
        static member Deserialize(r: JsonReader<'x>) : bool = r.Current.GetBoolean()

    interface ExtendedDeserializable<string> with
        static member Deserialize(r: JsonReader<'x>) : string =
            r.Current.GetString()

    interface ExtendedDeserializable<int> with
        static member Deserialize(r: JsonReader<'x>) : int =
            r.Current.GetInt32()

    interface ExtendedDeserializable<float> with
        static member Deserialize(r: JsonReader<'x>) : float =
            r.Current.GetDouble()

type JsonReader with
    //type implements itself
    static member Deserialize<'a, 'x when 'a :> Deserializable<'a>>(_: 'a, r: JsonReader<'x>, _: DeserInstance) : 'a =
        'a.Deserialize r

    static member Deserialize<'a, 'x when 'a :> Deserializable<'a>>(_: 'a list, r: JsonReader<'x>, _: DeserInstance) : 'a list =
        let size = r.Current.GetArrayLength()
        let rar = ResizeArray<_>(size)
        let mutable x = r.Current.EnumerateArray()
        while x.MoveNext() do
            let curr = x.Current
            let childReader = (JsonReader<'x>(curr) )
            let result = 'a.Deserialize childReader
            rar.Add(result)            

        List.ofArray (rar.ToArray())

    static member Deserialize<'a, 'x when 'a :> Deserializable<'a>>(_: 'a option, r: JsonReader<'x>, _: DeserInstance)
        : 'a option =
        if r.Current.ValueKind = JsonValueKind.Null || r.Current.ValueKind = JsonValueKind.Undefined then
            None
        else
            Some('a.Deserialize r)


    //base implementations (int, string, bool etc)
    static member Deserialize<'a, 'b, 'x when 'b :> ExtendedDeserializable<'a>>(_: 'a, r: JsonReader<'x>, _BuiltIns: 'b) : 'a =
        'b.Deserialize r

    static member Deserialize<'a, 'b, 'x when 'b :> ExtendedDeserializable<'a>>(_: 'a option, r: JsonReader<'x>, _BuiltIns: 'b) : 'a option =
        if r.Current.ValueKind = JsonValueKind.Null || r.Current.ValueKind = JsonValueKind.Undefined then
            None
        else
            Some('b.Deserialize r)

    static member Deserialize<'a, 'b, 'x when 'b :> ExtendedDeserializable<'a>>(_: 'a list, r: JsonReader<'x>, _BuiltIns: 'b) : 'a list =
        let size = r.Current.GetArrayLength()
        let rar = ResizeArray<_>(size)
        let mutable x = r.Current.EnumerateArray()
        while x.MoveNext() do
            let curr = x.Current
            let childReader = (JsonReader<'x>(curr) )
            let result = 'b.Deserialize childReader
            rar.Add(result)            

        List.ofArray (rar.ToArray())


    //extended implementations
    static member Deserialize<'a, 'b when 'b :> ExtendedDeserializable<'a>>(_: 'a, r: JsonReader<'b>, _BuiltIns: obj) : 'a =
        'b.Deserialize r

    static member Deserialize<'a, 'b when 'b :> ExtendedDeserializable<'a>>(_: 'a list, r: JsonReader<'b>, _BuiltIns: obj) : 'a list =
        let size = r.Current.GetArrayLength()
        let rar = ResizeArray<_>(size)
        let mutable x = r.Current.EnumerateArray()
        while x.MoveNext() do
            let curr = x.Current
            let childReader = (JsonReader<'b>(curr) )
            let result = 'b.Deserialize childReader
            rar.Add(result)            

        List.ofArray (rar.ToArray())

    static member Deserialize<'a, 'b when 'b :> ExtendedDeserializable<'a>>(_: 'a option, r: JsonReader<'b>, _BuiltIns: obj) : 'a option =
        if r.Current.ValueKind = JsonValueKind.Null || r.Current.ValueKind = JsonValueKind.Undefined then
            None
        else
            Some('b.Deserialize r)



[<AutoOpen>]
module Abc =
    let inline deserializeCore x (s: JsonReader<_>) =

        let inline call (_output: ^R, reader: ^M, inst: ^I, _ext: ^X) =
            ((^M or ^R): (static member Deserialize: ^R * _ * _ -> ^R) _output, reader, inst)

        call ((Unchecked.defaultof<_>), s, (DeserInstance), x)

    let inline (?) (r: JsonReader<'x>) (fieldName: string) =
        let mutable prop = Unchecked.defaultof<_>
        if r.Current.TryGetProperty(fieldName, &prop) then
            let childReader = JsonReader<'x>(prop)
            deserializeCore (Unchecked.defaultof<'x>) childReader
        else
            let childReader = JsonReader<'x>(JsonElement())
            deserializeCore (Unchecked.defaultof<'x>) childReader

    let inline x (r: JsonReader<'x>) : ^a = r?asdasd