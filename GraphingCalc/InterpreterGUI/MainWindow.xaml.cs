using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;
 

namespace InterpreterGUI
{
    public partial class MainWindow : Window
    {
        public MainWindow()
        {
            InitializeComponent();
        }

        private void Evaluate_Click(object sender, RoutedEventArgs e)
        {
            string input = InputBox.Text;
            var s = InterpreterCore.eval(input);
            if (s.Item1)
            { 
                ResultBox.Text = s.Item2;
                
                ErrorBox.Text = "";
            }
            else
            {
                ErrorBox.Text = s.Item2;
                ResultBox.Text = "";
            }

        }
        private void btnOpenPlot_Click(object sender, RoutedEventArgs e)
        {
            PlotWindow plotWindow = new PlotWindow();
            plotWindow.Owner = this;

            plotWindow.Show();
        }


        private void ResultBox_TextChanged(object sender, TextChangedEventArgs e)
        {
            String s = "y = 1x + c";

        }

    }
}