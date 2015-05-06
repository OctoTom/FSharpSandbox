namespace FEMBoxLibrary

open System
open System.IO

/// This modlule contains some often used generally useful tools.
module MyTools =
  /// Takes given number of first elements of sequence and converts them to list.
  let take numer sequence =
    sequence |> Seq.take numer |> Seq.toList
  
  /// Tests if file with given path exists.
  let fileExists path = System.IO.File.Exists(path)

  /// Returns the conntent of a file as sequence of lines.
  /// If the file is not found the function fails with message "File not found: + path".
  let readFile path =
    if fileExists path
      then System.IO.File.ReadLines(path) |> Seq.cast<string>
    else
      failwith ("File not found: " + path)

