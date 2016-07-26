#nowarn "211"
#I "packages/MathNet.Symbolics/lib/net40"
#I "packages/MathNet.Symbolics.0.9.0/lib/net40"
#I "../packages/MathNet.Symbolics/lib/net40"
#I "../packages/MathNet.Symbolics.0.9.0/lib/net40"
#I "../../packages/MathNet.Symbolics/lib/net40"
#I "../../packages/MathNet.Symbolics.0.9.0/lib/net40"
#I "../../../packages/MathNet.Symbolics/lib/net40"
#I "../../../packages/MathNet.Symbolics.0.9.0/lib/net40"

#I @"c:\Users\Tomas\OneDrive\OneSync\Projects\FSharpSandbox\FSharpSandbox\packages\MathNet.Numerics.3.8.0\lib\net40\"
#I @"c:\Users\Tomas\OneDrive\OneSync\Projects\FSharpSandbox\FSharpSandbox\packages\MathNet.Numerics.FSharp.3.8.0\lib\net40\"

#r "MathNet.Numerics.dll"
#r "MathNet.Numerics.FSharp.dll"
#r "MathNet.Symbolics.dll"

open MathNet.Symbolics

fsi.AddPrinter Infix.print