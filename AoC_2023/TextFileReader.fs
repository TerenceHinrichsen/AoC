module TextFileReader
  open System.IO

  let readFileContents (file: string) =
    seq {
      use sr = new StreamReader (file)
      while not sr.EndOfStream do
        yield sr.ReadLine()
    }
    |> Seq.toList
