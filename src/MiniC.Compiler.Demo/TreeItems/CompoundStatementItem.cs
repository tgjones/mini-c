using System;
using System.Linq;
using Microsoft.FSharp.Collections;

namespace MiniC.Compiler.Demo.TreeItems
{
	public class CompoundStatementItem : TreeItemBase
	{
		public CompoundStatementItem(Tuple<FSharpList<Ast.VariableDeclaration>, FSharpList<Ast.Statement>> compoundStatement,
			string name = "Compound Statement")
		{
			Text = name;
			Children = new TreeItemBase[]
			{
				new SimpleItem("Local Declarations", compoundStatement.Item1.Select(x => new VariableDeclarationItem(x))),
				new SimpleItem("Statements", compoundStatement.Item2.Select(x => new StatementItem(x)))
			};
		}
	}
}