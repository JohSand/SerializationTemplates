namespace TypeClassTests.SpanJson

open SpanJson
open System
open System.Runtime.CompilerServices
open Microsoft.FSharp.NativeInterop

#nowarn "3535"

type DeserializableTo<'a> =
    static abstract member Deserialize: byref<JsonReader<byte>> -> 'a

module Core =
    let deserializeCore<'a, 'result when 'a :> DeserializableTo<'result>> (_: 'a) (bytes: byte array) : 'result =
        let span = ReadOnlySpan(bytes)
        let mutable (reader: JsonReader<byte>) = JsonReader<byte>(&span)
        'a.Deserialize(&reader)

    let deserializeProp<'a, 'result when 'a :> DeserializableTo<'result>>
        (_: 'a)
        (r: byref<JsonReader<byte>>)
        : 'result =
        'a.Deserialize(&r)


type Test = { test: int }

type Test2 = { test: Test }

module Instance =
    ()
type A =
    | Witness
    static member Self = Witness

    interface DeserializableTo<bool> with
        static member Deserialize(r: byref<JsonReader<byte>>) : bool = r.ReadBoolean()

    interface DeserializableTo<int> with
        static member Deserialize(r: byref<JsonReader<byte>>) : int = r.ReadInt32()

    interface DeserializableTo<string> with
        static member Deserialize(r: byref<JsonReader<byte>>) : string = r.ReadString()

    interface DeserializableTo<Test> with
        static member Deserialize(r: byref<JsonReader<byte>>) : Test =
            r.ReadBeginObjectOrThrow()
            let mutable test = Unchecked.defaultof<int>
            let testName = ReadOnlySpan<byte>("test"B)
            let mutable readProps = 0

            while not (r.ReadUtf8IsEndObject()) do

                let prop = r.ReadEscapedNameSpan()

                if prop.SequenceEqual testName then
                    test <- Core.deserializeProp A.Self (&r)
                    readProps <- readProps + 1
                else
                    r.ReadUtf8Dynamic() |> ignore

            { Test.test = test }

    interface DeserializableTo<Test2> with
        static member Deserialize(r: byref<JsonReader<byte>>) : Test2 =
            let mutable test = Unchecked.defaultof<Test>
            let testName = ReadOnlySpan<byte>("test"B)
            let mutable readProps = 0

            r.ReadBeginObjectOrThrow()
            while not (r.ReadUtf8IsEndObject()) do

                let prop = r.ReadEscapedNameSpan()

                if prop.SequenceEqual testName then
                    test <- Core.deserializeProp A.Self (&r)
                    readProps <- readProps + 1

                else
                    r.ReadUtf8Dynamic() |> ignore

            // if readProps = 1 then else failwith "
            { Test2.test = test }




module Test =

    let inline deserialize (bytes: byte array) =
        Core.deserializeCore Witness bytes

    let z () : Test2 =
        let json = """ { "test": {"test": 1 } } """
        let bytes = Text.UTF8Encoding.UTF8.GetBytes(json)
        Core.deserializeCore A.Self bytes
