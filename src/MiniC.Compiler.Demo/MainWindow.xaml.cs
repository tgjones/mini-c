using System;
using MahApps.Metro.Controls;

namespace MiniC.Compiler.Demo
{
	/// <summary>
	/// Interaction logic for MainWindow.xaml
	/// </summary>
	public partial class MainWindow : MetroWindow
	{
		public MainWindow()
		{
			InitializeComponent();
			TextEditor.Text = ((MainWindowViewModel) DataContext).SourceCode;
		}

		private void TextEditor_OnTextChanged(object sender, EventArgs e)
		{
			((MainWindowViewModel) DataContext).SourceCode = TextEditor.Text;
		}
	}
}
