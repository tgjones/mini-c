using System.Linq;

namespace MiniC.Compiler.Demo.TreeItems
{
	public class ExpressionItem : TreeItemBase
	{
		public ExpressionItem(Ast.Expression expression)
		{
			if (expression.IsArrayAllocationExpression)
			{
				var typedExpression = (Ast.Expression.ArrayAllocationExpression) expression;
				Text = "Array Allocation Expression";
				Children = new TreeItemBase[]
				{
					new SimpleItem("Type: " + typedExpression.Item1),
					new SimpleItem("Size", new[] { new ExpressionItem(typedExpression.Item2) })
				};
			}
			else if (expression.IsArrayIdentifierExpression)
			{
				var typedExpression = (Ast.Expression.ArrayIdentifierExpression) expression;
				Text = "Array Identifier Expression: " + typedExpression.Item1.Identifier;
				Children = new[]
				{
					new SimpleItem("Index", new[] { new ExpressionItem(typedExpression.Item2) })
				};
			}
			else if (expression.IsArraySizeExpression)
			{
				var typedExpression = (Ast.Expression.ArraySizeExpression) expression;
				Text = "Array Size Expression: " + typedExpression.Item.Identifier;
			}
			else if (expression.IsAssignmentExpression)
			{
				var typedExpression = (Ast.Expression.AssignmentExpression) expression;
				if (typedExpression.Item.IsScalarAssignmentExpression)
				{
					var typedExpression2 = (Ast.AssignmentExpression.ScalarAssignmentExpression) typedExpression.Item;
					Text = "Scalar Assignment Expression";
					Children = new TreeItemBase[]
					{
						new SimpleItem("Variable: " + typedExpression2.Item1.Identifier),
						new SimpleItem("Expression", new[] { new ExpressionItem(typedExpression2.Item2) })
					};
				}
				if (typedExpression.Item.IsArrayAssignmentExpression)
				{
					var typedExpression2 = (Ast.AssignmentExpression.ArrayAssignmentExpression) typedExpression.Item;
					Text = "Array Assignment Expression";
					Children = new TreeItemBase[]
					{
						new SimpleItem("Variable: " + typedExpression2.Item1.Identifier),
						new SimpleItem("Index", new[] { new ExpressionItem(typedExpression2.Item2) }),
						new SimpleItem("Expression", new[] { new ExpressionItem(typedExpression2.Item3) })
					};
				}
			}
			else if (expression.IsBinaryExpression)
			{
				var typedExpression = (Ast.Expression.BinaryExpression) expression;
				Text = "Binary Expression";
				Children = new TreeItemBase[]
				{
					new SimpleItem("Left", new[] { new ExpressionItem(typedExpression.Item1) }),
					new SimpleItem("Operator: " + typedExpression.Item2),
					new SimpleItem("Right", new[] { new ExpressionItem(typedExpression.Item3) })
				};
			}
			else if (expression.IsFunctionCallExpression)
			{
				var typedExpression = (Ast.Expression.FunctionCallExpression) expression;
				Text = "Function Call Expression";
				Children = new TreeItemBase[]
				{
					new SimpleItem("Name: " + typedExpression.Item1),
					new SimpleItem("Arguments", typedExpression.Item2.Select(x => new ExpressionItem(x)))
				};
			}
			else if (expression.IsIdentifierExpression)
			{
				Text = "Identifier Expression: " + ((Ast.Expression.IdentifierExpression) expression).Item.Identifier;
			}
			else if (expression.IsLiteralExpression)
			{
				Text = "Literal Expression: " + ((Ast.Expression.LiteralExpression) expression).Item;
			}
			else if (expression.IsUnaryExpression)
			{
				var typedExpression = (Ast.Expression.UnaryExpression) expression;
				Text = "Unary Expression";
				Children = new TreeItemBase[]
				{
					new SimpleItem("Operator: " + typedExpression.Item1),
					new SimpleItem("Expression", new[] { new ExpressionItem(typedExpression.Item2) })
				};
			}
			else
				throw new System.NotImplementedException();
		}
	}
}