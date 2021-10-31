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
        private string _openedFile;

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
            _codeBox.Text = File.ReadAllText(path[0]);
            _openedFile = path[0];
        }

        private async void New(object sender, RoutedEventArgs e)
        {
            Save(sender, e);
            var saveFileDialog = new SaveFileDialog { InitialFileName = _openedFile };
            var path = await saveFileDialog.ShowAsync(this);
            if (path != null)
                File.WriteAllText(path, _codeBox.Text);
        }

        private async void Save(object sender, RoutedEventArgs e)
        {
            var saveFileDialog = new SaveFileDialog { InitialFileName = _openedFile };
            var path = await saveFileDialog.ShowAsync(this);
            if (path != null)
                File.WriteAllText(path, _codeBox.Text);
        }

        private async void Run(object sender, RoutedEventArgs e)
        {
            _runButton.IsEnabled = false;
            var code = _codeBox.Text;
            _consoleBox.Text = "Interpretation is started! Please wait for finish...\n";
            var task = new Task<string>(() =>
                {
                    var d = Interpreter.runPrint(Interpreter.parse(code));
                    return d;
                }
            );
            task.ContinueWith(x =>
                Dispatcher.UIThread.Post(() =>
                {
                    try { _consoleBox.Text += x.Result + "Interpretation is finished!\n"; }
                    catch (Exception ex) { _consoleBox.Text = ex.Message; }
                    _runButton.IsEnabled = true;
                }));
            task.Start();
        
            _codeBox.Text = "";
            if (_codeBox.Text.Trim() == "") return;
            
            try
            {
                Run(sender, e);
            }
            catch (Exception ex)
            {
                _consoleBox.Text = ex.Message;
                _runButton.IsEnabled = true;
            }
            
        }
    }
}
