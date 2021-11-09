using System;
using Avalonia.Controls;
using Avalonia.Markup.Xaml;
using System.IO;
using System.Threading.Tasks;
using Arithm;
using Avalonia.Input;
using Avalonia.Interactivity;
using Avalonia.Threading;


namespace GUI.Views
{
    public class MainWindow : Window
    {
        private readonly TextBox _codeBox;
        private readonly TextBox _consoleBox;
        private readonly Button _runButton;
        private string? _openedFile;
        private readonly Grid _grid;
        private bool _ctrlIsPressed;

        public MainWindow()
        {
            InitializeComponent();
            Interpreter.printed.Subscribe(PrintToConsole);
            _consoleBox = this.Find<TextBox>("ConsoleBox");
            _codeBox = this.Find<TextBox>("CodeBox");
            _runButton = this.Find<Button>("Run");
            _grid = this.FindControl<Grid>("Grid");
            _grid.KeyDown += SaveKeyBoardEvent;
            _grid.KeyUp += LetterPress;
        }

        private void SaveKeyBoardEvent(object? sender, KeyEventArgs e)
        {
            if (e.Key != Key.LeftCtrl) return;
            _ctrlIsPressed = true;
        }
        
        void LetterPress(object? sender, KeyEventArgs e)
        {
            if (!_ctrlIsPressed) return;
            switch (e.Key)
            {
                case Key.S:
                    Save();
                    break;
                case Key.O:
                    Open();
                    break;
                case Key.N:
                    New();
                    break;
            }
            _ctrlIsPressed = false;
        }

        private void InitializeComponent()
        {
            AvaloniaXamlLoader.Load(this);
        }
        
        private void OpenEvent(object sender, RoutedEventArgs e)
        {
            Open();
        }
        
        private async void Open()
        {
            Save();
            var openedFileDialog = new OpenFileDialog();
            openedFileDialog.Filters.Add(new FileDialogFilter { Extensions = { "txt" } });
            var path = await openedFileDialog.ShowAsync(this);
            if (path is not { Length: > 0 }) return;
            _codeBox.Text = await File.ReadAllTextAsync(path[0]);
            _openedFile = path[0];
        }

        private void NewEvent(object sender, RoutedEventArgs e)
        {
            New();
        }
        
        private void New()
        {
            if (String.IsNullOrEmpty(_codeBox.Text) && String.IsNullOrEmpty(_openedFile))
            {
                Save();
            }
            else
            {
                SaveTo();
                _codeBox.Text = "";
                _openedFile = "";
                SaveTo();
            }
        }

        private void SaveToEvent(object sender, RoutedEventArgs e)
        {
            SaveTo();
        }
        
        private async void SaveTo()
        {
            var saveFileDialog = new SaveFileDialog { InitialFileName = _openedFile };
            var path = await saveFileDialog.ShowAsync(this);
            if (path != null)
            {
                _openedFile = path;
                await File.WriteAllTextAsync(path, _codeBox.Text);
            }
        }
        
        private void SaveEvent(object sender, RoutedEventArgs e)
        {
            Save();
        }
        
        private async void Save()
        {
            if (!string.IsNullOrEmpty(_openedFile))
            {
                await File.WriteAllTextAsync(_openedFile, _codeBox.Text);
            }
            else
            {
                SaveTo();
            }
        }

        private void PrintToConsole(string str)
        {
            Dispatcher.UIThread.Post(() => _consoleBox.Text += str + '\n');
        }

        private void Run(object sender, RoutedEventArgs e)
        {
            _runButton.IsEnabled = false;
            _consoleBox.Text = "Execution started:\n";
            var code = _codeBox.Text;
            var task = new Task(() =>
            {
                try
                {
                    Interpreter.runPrint(Lexer.parse(code));
                    Dispatcher.UIThread.Post(() =>
                    {
                        _runButton.IsEnabled = true;
                        _consoleBox.Text += "Execution finished." + "\n";
                    });
                }
                catch (Exception exception)
                {
                    Dispatcher.UIThread.Post(() =>
                    {
                         _consoleBox.Text += "Execution failed:" + "\n";
                         _consoleBox.Text += exception.Message + "\n";
                         _runButton.IsEnabled = true;
                    });
                }
            });
            task.Start();
        }

        
    }
}
