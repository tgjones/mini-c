module MiniC.Compiler.IL

open System

type ILClass = 
    {
        Fields  : ILField list;
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
        Type : Type;
        Name : string;
    }

and ILOpCode =
    | Add
    | Ldc_I4 of int
    | Ldfld of ILField
    | Ret

and ILField =
    {
        Type : Type;
        Name : string;
    }