using System.Linq;

namespace MiniC.Compiler.Demo.TreeItems
{
	public class DeclarationItem : TreeItemBase
	{
		public DeclarationItem(Ast.Declaration declaration)
		{
			if (declaration.IsFunctionDeclaration)
			{
				var typedDeclaration = (Ast.Declaration.FunctionDeclaration) declaration;
				Text = "Function Declaration (Name = " + typedDeclaration.Item.Item2 + ")";
				Children = new TreeItemBase[]
				{
					new SimpleItem("Return Type: " + typedDeclaration.Item.Item1),
					new SimpleItem("Name: " + typedDeclaration.Item.Item2),
					new SimpleItem("Parameters", typedDeclaration.Item.Item3.Select(x => new VariableDeclarationItem(x))),
					new CompoundStatementItem(typedDeclaration.Item.Item4, "Body")
				};
			}
			else if (declaration.IsStaticVariableDeclaration)
			{
				Text = "Static Variable Declaration";
				var typedDeclaration = (Ast.Declaration.StaticVariableDeclaration) declaration;
				if (typedDeclaration.Item.IsScalarVariableDeclaration)
					Children = new[] { new VariableDeclarationItem(typedDeclaration.Item) };
			}
		}
	}
}