using System.Collections.Generic;

namespace MiniC.Compiler.Demo.TreeItems
{
	public abstract class TreeItemBase
	{
		public string Text { get; protected set; }
		public IEnumerable<TreeItemBase> Children { get; protected set; }
	}
}