
module Asd

open TypeClassTests

type CustomType =
    | CustomType
    interface Deserializable<CustomType> with
        static member Test(_) = CustomType

type CustomType2 =
    | CustomType2
    interface Deserializable<CustomType2> with
        static member Test(_) = CustomType2

type CustomType3 =
    | CustomType3
    interface Deserializable<CustomType3> with
        static member Test(_) = CustomType3


type DeserInstance2 = 
    | DeserInstance2
    interface ExtendedDeserializable<DeserInstance2> with
        static member Test(arg1: JsonReader<'x>): DeserInstance2 = 
            DeserInstance2


let inline deserialize (s: string) =
    let reader = JsonReader<DeserInstance2>(s)
    deserializeCore DeserInstance2 reader

let x () =
    let (b: DeserInstance2 option) = deserialize("")
    1


        //let inline call (mthd: ^M, source: ^I, _output: ^R) = ((^M or ^I or ^R) : (static member Dimap : _*_*_*_ -> _) source, ab, cd, mthd)
        //call (Unchecked.defaultof<Dimap>, source, Unchecked.defaultof<'``Profunctor<'A,'D>``>)
