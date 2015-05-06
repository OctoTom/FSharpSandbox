namespace FEMBoxLibrary
open System
open System.IO

/// This module contains a routines for parsing GEO FEM input file.
module GeoInput =
  let currentDirectory = __SOURCE_DIRECTORY__
  let inputFilePath = currentDirectory + @"\Input\001_FEM5_Input.txt"
  
  let allLines = MyTools.readFile inputFilePath
  let allLinesTrimmed = allLines |> Seq.map(fun item -> item.Trim()) |> Seq.toList

  /// Enumerates list from given (inclusive) element. 
  let rec readFrom keyword list =
    match list with
    | [] -> failwith "Empty list in readFrom."
    | h::t when h = keyword -> list
    | _::t -> readFrom keyword t

  /// Enumerates list up to given (inclusive) element. 
  let rec readTo keyword list =
    match list with
    | [] -> failwith "Empty list in readTo."
    | h::t when h = keyword -> [h]
    | h::t -> h :: readTo keyword t
  
  /// Enumerates list from a given element up to given element.
  /// The boundary elements are inclusive.
  let readFromTo a b = (readFrom a) >> (readTo b)

  /// let res = ['a' .. 'z'] |> readFromTo 'e' 'i'
  let res1 = allLinesTrimmed |> readFromTo "problem_description" "end"







  // Splits a line composed of space-separated words.
  let splitToWords (line:string) =
    let line = line.Trim() // Shadowing the original "line" value.
    line.Split(' ') |> Array.toList

  // Checks, if the list has exactly two elementes and if yes, it returns corresponting tuple.
  let arrayToPair a =
    match a with
    | value :: key :: [] -> Some(value, key)
    | _ -> None
  

    
    
  
  
  // Parses a key value pair separated by equals sign (=).
  let parseKeyValuePair (word:string) =
    let keyAndValue = word.Split('=') |> Array.toList
    let pair = keyAndValue |> arrayToPair
    let key = pair.Value |> fst
    let value = pair.Value |> snd |> Double.Parse
    (key, value)

  let res = parseKeyValuePair "asdf=123.32"


  // Parses line in the following format: "    1 x=5.000000E+01 y=1.000000E+01"
  // to tuple.
  let parseNodeLine (line:string) =
    let words = line |> splitToWords
    let number = Int32.Parse(words.[0])
    let coordX = parseKeyValuePair words.[1]
    let coordY = parseKeyValuePair words.[2]
    (number, (snd coordX, snd coordY))
    
  let line = "    1 x=5.000000E+01 y=1.000000E+01"
  let res2 = parseNodeLine line
