namespace TypeClassTests.SpanJson

open SpanJson
open System
open System.Runtime.CompilerServices
open Microsoft.FSharp.NativeInterop

type Test = { test: int }

type Test2 = { test: Test }

#nowarn "3535"

type DeserializableTo<'a> =
    //No srtp, as JsonReader is a ref struct. 
    static abstract member Deserialize: byref<JsonReader<byte>> -> 'a

module Core =
    //No srtp, as JsonReader is a ref struct. 
    let resolve<'a, 'result when 'a :> DeserializableTo<'result>> (_: 'a) (r: byref<JsonReader<byte>>) : 'result =
        'a.Deserialize(&r)

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
                    test <- Core.resolve A.Self (&r)
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
                    test <- Core.resolve A.Self (&r)
                    readProps <- readProps + 1

                else
                    r.ReadUtf8Dynamic() |> ignore

            // if readProps = 1 then else failwith "
            { Test2.test = test }


module Demo =
    let deserialize<'a, 'result when 'a :> DeserializableTo<'result>> (_: 'a) (json: string) : 'result =
        let bytes = Text.UTF8Encoding.UTF8.GetBytes(json)
        let span = ReadOnlySpan(bytes)
        let mutable (reader: JsonReader<byte>) = JsonReader<byte>(&span)
        'a.Deserialize(&reader)


    let proof () : Test =
        deserialize A.Self """ { "test": {"test": 1 } } """
