using System;
using Avalonia.Controls;
using Avalonia.Markup.Xaml;
using System.IO;
using System.Threading.Tasks;
using Arithm;
using Avalonia.Interactivity;
using Avalonia.Threading;


namespace GUI.Views
{
    public partial class MainWindow : Window
    {
        private readonly TextBox _codeBox;
        private readonly TextBox _consoleBox;
        private readonly Button _runButton;
        private string? _openedFile;

        public MainWindow()
        {
            InitializeComponent();
            _consoleBox = this.Find<TextBox>("ConsoleBox");
            _codeBox = this.Find<TextBox>("CodeBox");
            _runButton = this.Find<Button>("Run");
        }

        private void InitializeComponent()
        {
            AvaloniaXamlLoader.Load(this);
        }
        
        private async void Open(object sender, RoutedEventArgs e)
        {
            var openedFileDialog = new OpenFileDialog();
            openedFileDialog.Filters.Add(new FileDialogFilter { Extensions = { "txt" } });
            var path = await openedFileDialog.ShowAsync(this);
            if (path is not { Length: > 0 }) return;
            _codeBox.Text = await File.ReadAllTextAsync(path[0]);
            _openedFile = path[0];
        }

        private void New(object sender, RoutedEventArgs e)
        {
            Save(sender, e);
            _codeBox.Text = "";
            _openedFile = "";
            Save(sender, e);
        }

        private async void Save(object sender, RoutedEventArgs e)
        {
            var saveFileDialog = new SaveFileDialog { InitialFileName = _openedFile };
            var path = await saveFileDialog.ShowAsync(this);
            if (path != null)
                await File.WriteAllTextAsync(path, _codeBox.Text);
        }

        private void Run(object sender, RoutedEventArgs e)
        {
            _runButton.IsEnabled = false;
            _consoleBox.Text = "Execution started:\n";
            var code = _codeBox.Text;
            var task = new Task<string>(() => Interpreter.runPrint(Interpreter.parse(code)));
            task.ContinueWith(t =>
                Dispatcher.UIThread.Post(() =>
                {
                    try
                    {
                        _consoleBox.Text += t.Result + "\n";
                        _runButton.IsEnabled = true;
                        _consoleBox.Text += "Execution finished." + "\n";
                        
                    }
                    catch (Exception exception)
                    {
                         _consoleBox.Text += "Execution failed:" + "\n";
                        _consoleBox.Text += exception.Message + "\n";
                        _runButton.IsEnabled = true;
                    }
                }));
            task.Start();
        }
    }
}
