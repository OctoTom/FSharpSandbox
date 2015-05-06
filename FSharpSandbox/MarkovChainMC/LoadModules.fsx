// Script to load all relevant modules into F# Interactive
#if INTERACTIVE
#r "../packages/MathNet.Numerics.2.6.1/lib/net40/MathNet.Numerics.dll"
#r "../packages/MathNet.Numerics.FSharp.2.6.0/lib/net40/MathNet.Numerics.FSharp.dll"
#r "../packages/FSharp.Charting.0.90.7/lib/net40/FSharp.Charting.dll"
#load "../packages/FSharp.Charting.0.90.7/FSharp.Charting.fsx"
#load "MatrixModule.fs"
#load "Histogram.fs"
#load "MyDistributions.fs"
#load "MetropolisAlgorithm.fs"
open FSharp.Charting
open MatrixModule
open Histogram
open MyDistributions
open MetropolisAlgorithm
#endif 

// Reference the libraries when using F# Interactive Console. Only in .fsx file.
//#if INTERACTIVE
//#r "../packages/MathNet.Numerics.2.6.1/lib/net40/MathNet.Numerics.dll"
//#r "../packages/MathNet.Numerics.FSharp.2.6.0/lib/net40/MathNet.Numerics.FSharp.dll"
//#r "../packages/FSharp.Charting.0.84/lib/net40/FSharp.Charting.dll"
//#load "../packages/FSharp.Charting.0.84/FSharp.Charting.fsx"
//#endif

