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
    | Brfalse of ILLabel
    | Brtrue of ILLabel
    | Call of string
    | CallClr of System.Reflection.MethodInfo
    | Ceq
    | Cge
    | Cgt
    | Cle
    | Clt
    | Dup
    | Div
    | Label of ILLabel
    | Ldarg of int16
    | Ldc_I4 of int
    | Ldc_R8 of float
    | Ldelem of Type
    | Ldlen
    | Ldloc of int16
    | Ldsfld of ILVariable
    | Mul
    | Neg
    | Newarr of Type
    | Pop
    | Rem
    | Ret
    | Starg of int16
    | Stelem of Type
    | Stloc of int16
    | Stsfld of ILVariable
    | Sub