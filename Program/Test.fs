module Repro
open TypeClassTests.TestJson
type Type1 = {
    asd1: int
    asd2: bool
    asd3: string
    asd4: float
} with
    interface Deserializable<Type1> with
        static member Deserialize(r: JsonReader<'x>): Type1 =
            let asd1 = r?asd1
            let asd2 = r?asd2
            let asd3 = r?asd3
            let asd4 = r?asd4
            { asd1 = asd1; asd2 = asd2; asd3 = asd3; asd4 = asd4 }

type Type2 = {
    asd1: int
    asd2: bool
    asd3: string
    asd4: float
} with
    interface Deserializable<Type2> with
        static member Deserialize(r: JsonReader<'x>): Type2 =
            let asd1 = r?asd1
            let asd2 = r?asd2
            let asd3 = r?asd3
            let asd4 = r?asd4
            { asd1 = asd1; asd2 = asd2; asd3 = asd3; asd4 = asd4 }

open System.Text.Json

type Type3 = {
    x: Type2 option
    y: int option
}

type Custom = 
    | Custom
    interface ExtendedDeserializable<Type3> with
        static member Deserialize(r: JsonReader<'x>) : Type3 = 
            let x = r?x
            let y = r?y
            { x = x; y = y }

let proof (doc: JsonDocument) : Type3  =
    let reader = JsonReader<Custom>  (doc.RootElement)

    deserializeCore  (Unchecked.defaultof<Custom>) reader

let proof2 (s: string) =
    JsonDocument.Parse(s) |> proof

let prof3 () = proof2 """ { "x": { "asd1": 1, "asd2": true, "asd3": "hi", "asd4": 4 } } """