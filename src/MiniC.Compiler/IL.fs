module MiniC.Compiler.IL

open System

type ILClass = 
    {
        Fields  : ILVariable list;
        Methods : ILMethod list;
    }

and ILMethod =
    {
        Name       : string;
        ReturnType : Type;
        Parameters : ILVariable list;
        Locals     : ILVariable list;
        Body       : ILOpCode list;
    }

and ILVariable =
    {
        Type  : Type;
        Name  : string;
    }

and ILOpCode =
    | Add
    | Ldarg of int16
    | Ldc_I4 of int
    | Ldfld of ILVariable
    | Ldloc of int16
    | Mul
    | Ret
    | Starg of int16
    | Stfld of ILVariable
    | Stloc of int16
    | Sub