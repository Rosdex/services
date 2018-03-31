namespace Rosdex.Parser.Csv

[<RequireQualifiedAccess>]
type CsvCell =
    | String of string
    | Int of int
    | Float of float

type 'a CsvField = {
    Name : string
    Mapping : 'a -> CsvCell option
}

module CsvField =
    let map mapping field =
        { field with Mapping = field.Mapping >> Option.map mapping }

    let bind binder field =
        { field with Mapping = field.Mapping >> Option.bind binder }

    let mapOption mapping field =
        { field with Mapping = field.Mapping >> mapping }

    let create name mapping = {
        Name = name
        Mapping = mapping
    }

module Operators =
    /// Промежуточный тип необходимый только для DSL.
    module MappingCaster =
        type 'a MappingCaster =
            | Mapping of ('a -> CsvCell option)
        let value (Mapping value) = value

        let ofValue toCell f =
            f >> toCell >> Some |> Mapping
        let ofOption toCell f =
            f >> Option.map toCell |> Mapping

        type 'a MappingCaster with
            static member op_Implicit f = ofValue CsvCell.String f
            static member op_Implicit f = ofValue CsvCell.Int f
            static member op_Implicit f = ofValue CsvCell.Float f
            static member op_Implicit f = ofOption CsvCell.String f
            static member op_Implicit f = ofOption CsvCell.Int f
            static member op_Implicit f = ofOption CsvCell.Float f
            static member op_Implicit f = Mapping f
            static member op_Implicit f = f >> Some |> Mapping

    let inline private (==>)
            (mapping : 'a -> ^b)
            (factory : ^c -> 'a CsvField) =
        ((^b or ^c) : (static member op_Implicit : ('a -> ^b) -> ^c) mapping)
        |> factory

    let inline (=>) mapping name =
        mapping ==> (MappingCaster.value >> CsvField.create name)

type CsvConfig = {
    Separator : string
    AllowQuote : string option
    Write : CsvCell -> string
}

module CsvConfig =
    let default' = {
        Separator = ","
        AllowQuote = Some "\""
        Write = function
            | CsvCell.Int p -> string p
            | CsvCell.Float p -> p.ToString(System.Globalization.CultureInfo.InvariantCulture)
            | CsvCell.String p -> p
    }

    let tryCanonicalize config (str : string) =
        match
            config.AllowQuote |> Option.exists str.Contains
            || str.Contains config.Separator
            || str.Contains "\n"
            , config.AllowQuote
            with
        | true, Some quote ->
            let doubleQuote = quote + quote
            sprintf "%s%s%s"
                quote
                (String.replace quote doubleQuote str)
                quote
            |> Some
        | true, None -> None
        | false, _ -> Some str

type 'a CsvWriter = {
    Fields : 'a CsvField list
    Config : CsvConfig
}

module CsvWriter =
    let tryStringifyField config field item =
        item
        |> field.Mapping
        |> Option.map config.Write
        |> Option.defaultValue ""
        |> CsvConfig.tryCanonicalize config

    let private tryBuildRow writer mapping =
        let rec f fields acc =
            match fields with
            | [] -> acc |> List.rev |> Ok
            | field::fields ->
                match field |> mapping with
                | None -> Error field
                | Some p -> p::acc |> f fields
        f writer.Fields []
        |> Result.map (String.concat writer.Config.Separator)

    let tryStringifyItem writer item =
        tryBuildRow writer (fun field ->
            tryStringifyField writer.Config field item)

    let tryStringfyHeaders writer =
        tryBuildRow writer (fun field ->
            field.Name |> CsvConfig.tryCanonicalize writer.Config)