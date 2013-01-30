using System.Collections.Generic;

namespace MiniC.Compiler.Demo.TreeItems
{
	public class SimpleItem : TreeItemBase
	{
		public SimpleItem(string value, IEnumerable<TreeItemBase> children = null)
		{
			Text = value;
			Children = children;
		}
	}
}