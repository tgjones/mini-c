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
        Parameters : ILParameter list;
        Body       : ILOpCode list;
    }

and ILParameter =
    {
        Type : Type;
        Name : string;
    }

and ILOpCode =
    | Ldc_I4 of int
    | Ldfld of ILField
    | Ret

and ILField =
    {
        Type : Type;
        Name : string;
    }