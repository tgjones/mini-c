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

and ILLabel = int

and ILOpCode =
    | Add
    | Br of ILLabel
    | Brtrue of ILLabel
    | Call of string
    | Ceq
    | Cgt
    | Clt
    | Label of ILLabel
    | Ldarg of int16
    | Ldc_I4 of int
    | Ldsfld of ILVariable
    | Ldloc of int16
    | Mul
    | Ret
    | Starg of int16
    | Stsfld of ILVariable
    | Stloc of int16
    | Sub