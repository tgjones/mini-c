using System;
using System.Linq;
using Microsoft.FSharp.Core;

namespace MiniC.Compiler.Demo.TreeItems
{
	public class StatementItem : TreeItemBase
	{
		public StatementItem(Ast.Statement statement)
		{
			if (statement.IsBreakStatement)
			{
				Text = "Break Statement";
			}
			else if (statement.IsCompoundStatement)
			{
				var typedStatement = (Ast.Statement.CompoundStatement) statement;
				Text = "Compound Statement";
				Children = new TreeItemBase[]
				{
					new SimpleItem("Local Declarations", typedStatement.Item.Item1.Select(x => new VariableDeclarationItem(x))),
					new SimpleItem("Statements", typedStatement.Item.Item2.Select(x => new StatementItem(x)))
				};
			}
			else if (statement.IsExpressionStatement)
			{
				var typedStatement = (Ast.Statement.ExpressionStatement) statement;
				Text = "Expression Statement";
				if (typedStatement.Item.IsNop)
				{
					Children = new[] { new SimpleItem("Nop") };
				}
				else if (typedStatement.Item.IsExpression)
				{
					var typedStatement2 = (Ast.ExpressionStatement.Expression) typedStatement.Item;
					Children = new[] { new ExpressionItem(typedStatement2.Item) };
				}
			}
			else if (statement.IsIfStatement)
			{
				var typedStatement = (Ast.Statement.IfStatement) statement;
				Text = "If Statement";
				Children = new TreeItemBase[]
				{
					new SimpleItem("Condition", new[] { new ExpressionItem(typedStatement.Item.Item1) }),
					new SimpleItem("Then", new[] { new StatementItem(typedStatement.Item.Item2) })
				};
				if (FSharpOption<Ast.Statement>.get_IsSome(typedStatement.Item.Item3))
					Children = Children.Union(new[]
					{
						new SimpleItem("Else", new[]
						{
							new StatementItem(typedStatement.Item.Item3.Value)
						})
					});
			}
			else if (statement.IsReturnStatement)
			{
				var typedStatement = (Ast.Statement.ReturnStatement) statement;
				Text = "Return Statement";
				if (FSharpOption<Ast.Expression>.get_IsSome(typedStatement.Item))
					Children = new[] { new ExpressionItem(typedStatement.Item.Value) };
			}
			else if (statement.IsWhileStatement)
			{
				var typedStatement = (Ast.Statement.WhileStatement) statement;
				Text = "While Statement";
				Children = new TreeItemBase[]
				{
					new SimpleItem("Condition", new[] { new ExpressionItem(typedStatement.Item.Item1) }),
					new SimpleItem("Body", new[] { new StatementItem(typedStatement.Item.Item2) })
				};
			}
			else
				throw new NotImplementedException();
		}
	}
}