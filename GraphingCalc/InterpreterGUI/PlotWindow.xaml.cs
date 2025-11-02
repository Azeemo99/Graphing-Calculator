using System;
using System.Linq;
using System.Windows;
using System.Windows.Media;
using System.Windows.Shapes;

namespace InterpreterGUI
{
    public partial class PlotWindow : Window
    {
        public PlotWindow()
        {
            InitializeComponent();
        }

        // Call this to plot a polynomial function string, e.g., "1 0 -2" for 1*x^2 + 0*x + -2
        public void PlotFunction(string funcExpr)
        {
            try
            {
                //parse coefficients from input
                //format: "a b c ..." -> a*x^n + b*x^(n-1) + ...
                var coeffs = funcExpr.Split(new[] { ' ' }, StringSplitOptions.RemoveEmptyEntries)
                                     .Select(s => double.Parse(s.Trim()))
                                     .ToArray();

                //generate points (x, y)
                double xMin = -10.0;
                double xMax = 10.0;
                double step = 0.1;
                var points = Enumerable.Range(0, (int)((xMax - xMin) / step) + 1)
                                       .Select(i =>
                                       {
                                           double x = xMin + i * step;
                                           double y = 0.0;
                                           int deg = coeffs.Length - 1;
                                           for (int j = 0; j < coeffs.Length; j++)
                                           {
                                               y += coeffs[j] * Math.Pow(x, deg - j);
                                           }
                                           return new Point(x, y);
                                       })
                                       .ToList();

                //find y min/max for scaling
                double yMin = points.Min(p => p.Y);
                double yMax = points.Max(p => p.Y);

                //add padding
                double yPadding = (yMax - yMin) * 0.1;
                yMin -= yPadding;
                yMax += yPadding;

                //clear canvas
                PlotCanvas.Children.Clear();

                double canvasWidth = PlotCanvas.ActualWidth;
                double canvasHeight = PlotCanvas.ActualHeight;

                //draw axes
                Line xAxis = new Line
                {
                    X1 = 0,
                    X2 = canvasWidth,
                    Y1 = canvasHeight * (yMax / (yMax - yMin)),
                    Y2 = canvasHeight * (yMax / (yMax - yMin)),
                    Stroke = Brushes.Black,
                    StrokeThickness = 1
                };
                PlotCanvas.Children.Add(xAxis);

                Line yAxis = new Line
                {
                    X1 = canvasWidth * (-xMin / (xMax - xMin)),
                    X2 = canvasWidth * (-xMin / (xMax - xMin)),
                    Y1 = 0,
                    Y2 = canvasHeight,
                    Stroke = Brushes.Black,
                    StrokeThickness = 1
                };
                PlotCanvas.Children.Add(yAxis);

                //draw polynomial curve
                Polyline polyline = new Polyline
                {
                    Stroke = Brushes.Blue,
                    StrokeThickness = 2
                };

                foreach (var p in points)
                {
                    double xCanvas = (p.X - xMin) / (xMax - xMin) * canvasWidth;
                    double yCanvas = canvasHeight - ((p.Y - yMin) / (yMax - yMin) * canvasHeight);
                    polyline.Points.Add(new Point(xCanvas, yCanvas));
                }

                PlotCanvas.Children.Add(polyline);
            }
            catch (Exception ex)
            {
                MessageBox.Show("Error plotting function: " + ex.Message);
            }
        }

        private void DrawButton_Click(object sender, RoutedEventArgs e)
        {
            PlotFunction(FunctionBox.Text);
        }
    }
}
