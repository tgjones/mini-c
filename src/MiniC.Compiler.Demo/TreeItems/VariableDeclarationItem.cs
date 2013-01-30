namespace MiniC.Compiler.Demo.TreeItems
{
	public class VariableDeclarationItem : TreeItemBase
	{
		public VariableDeclarationItem(Ast.VariableDeclaration variableDeclaration)
		{
			if (variableDeclaration.IsScalarVariableDeclaration)
			{
				var scalarDeclaration = (Ast.VariableDeclaration.ScalarVariableDeclaration) variableDeclaration;
				Text = scalarDeclaration.Item1 + " " + scalarDeclaration.Item2;
			}
			else
			{
				var arrayDeclaration = (Ast.VariableDeclaration.ArrayVariableDeclaration) variableDeclaration;
				Text = arrayDeclaration.Item1 + " " + arrayDeclaration.Item2;
			}
		}
	}
}