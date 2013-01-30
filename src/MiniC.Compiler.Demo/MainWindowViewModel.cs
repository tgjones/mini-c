using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using MiniC.Compiler.Demo.Annotations;
using MiniC.Compiler.Demo.TreeItems;

namespace MiniC.Compiler.Demo
{
	public class MainWindowViewModel : INotifyPropertyChanged
	{
		private string _sourceCode;
		public string SourceCode
		{
			get { return _sourceCode; }
			set
			{
				_sourceCode = value;
				Recompile();
				OnPropertyChanged("SourceCode");
			}
		}

		private string _compilerErrors;
		public string CompilerErrors
		{
			get { return _compilerErrors; }
			set
			{
				_compilerErrors = value;
				OnPropertyChanged("CompilerErrors");
			}
		}

		private IEnumerable<TreeItemBase> _abstractSyntaxTree;
		public IEnumerable<TreeItemBase> AbstractSyntaxTree
		{
			get { return _abstractSyntaxTree; }
			private set
			{
				_abstractSyntaxTree = value;
				OnPropertyChanged("AbstractSyntaxTree");
			}
		}

		private IL.ILClass _intermediateLanguage;
		public IL.ILClass IntermediateLanguage
		{
			get { return _intermediateLanguage; }
			set
			{
				_intermediateLanguage = value;
				OnPropertyChanged("IntermediateLanguage");
			}
		}

		public MainWindowViewModel()
		{
			_sourceCode = @"int fib(int n) {
	if (n == 0) 
		return 0;
	if (n == 1)
		return 1;
     return fib(n - 1) + fib(n - 2);
}

int main(void) {
	return fib(10);
}";
			Recompile();
		}

		private void Recompile()
		{
			AbstractSyntaxTree = null;
			IntermediateLanguage = null;
			CompilerErrors = string.Empty;
			try
			{
				var program = Parser.parse(_sourceCode);
				AbstractSyntaxTree = program.Select(x => new DeclarationItem(x));
				var semanticAnalysisResult = SemanticAnalysis.analyze(program);
				IntermediateLanguage = new ILBuilder(semanticAnalysisResult).BuildClass(program);
			}
			catch (CompilerException ex)
			{
				CompilerErrors = ex.Message;
			}
		}

		#region INotifyPropertyChanged

		public event PropertyChangedEventHandler PropertyChanged;

		[NotifyPropertyChangedInvocator]
		protected virtual void OnPropertyChanged(string propertyName)
		{
			PropertyChangedEventHandler handler = PropertyChanged;
			if (handler != null) handler(this, new PropertyChangedEventArgs(propertyName));
		}

		#endregion
	}
}