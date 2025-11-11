using System;
using System.Collections.Generic;
using System.Linq;
using System.Windows;
using System.Windows.Controls;
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

        //Main plotting function using Catmull-Rom spline for smooth curve
        public void PlotFunction(string funcExpr, double xStart = -10.0, double xEnd = 10.0)
        {
            try
            {
                //parse coefficients from input
                //example input: "1 0 -2"  ->  1*x^2 + 0*x - 2
                var coeffs = funcExpr.Split(new[] { ' ' }, StringSplitOptions.RemoveEmptyEntries)
                                     .Select(s => double.Parse(s.Trim()))
                                     .ToArray();

                //generate raw polynomial points
                double step = 0.5;
                var rawPoints = Enumerable.Range(0, (int)((xEnd - xStart) / step) + 1)
                                          .Select(i =>
                                          {
                                              double x = xStart + i * step;
                                              double y = 0.0;
                                              int deg = coeffs.Length - 1;
                                              for (int j = 0; j < coeffs.Length; j++)
                                              {
                                                  y += coeffs[j] * Math.Pow(x, deg - j);
                                              }
                                              return new Point(x, y);
                                          })
                                          .ToList();

                if (rawPoints.Count < 4)
                    throw new Exception("Need at least 4 points for Catmull-Rom interpolation.");

                //compute smooth spline points
                List<Point> smoothPoints = new List<Point>();
                for (int i = 0; i < rawPoints.Count - 3; i++)
                {
                    for (double t = 0; t < 1.0; t += 0.05)
                    {
                        smoothPoints.Add(CatmullRom(rawPoints[i], rawPoints[i + 1], rawPoints[i + 2], rawPoints[i + 3], t));
                    }
                }

                //find y min/max for scaling
                double yMin = smoothPoints.Min(p => p.Y);
                double yMax = smoothPoints.Max(p => p.Y);
                double yPadding = (yMax - yMin) * 0.1;
                yMin -= yPadding;
                yMax += yPadding;

                //clear canvas
                PlotCanvas.Children.Clear();
                double canvasWidth = PlotCanvas.ActualWidth;
                double canvasHeight = PlotCanvas.ActualHeight;


                //draw axes with fallback dashed reference lines

                //  X-Axis (Y = 0):
                double xAxisPos; // Y position for X-axis
                Line xAxis = new Line { X1 = 0, X2 = canvasWidth, StrokeThickness = 1 };

                if (0 >= yMin && 0 <= yMax)
                {
                    xAxisPos = canvasHeight - ((0 - yMin) / (yMax - yMin) * canvasHeight);
                    xAxis.Y1 = xAxis.Y2 = xAxisPos;
                    xAxis.Stroke = Brushes.Black;
                }
                else
                {
                    xAxisPos = (0 < yMin) ? canvasHeight - 10 : 10;
                    xAxis.Y1 = xAxis.Y2 = xAxisPos;
                    xAxis.Stroke = Brushes.Gray;
                    xAxis.StrokeDashArray = new DoubleCollection { 4, 4 };
                }
                PlotCanvas.Children.Add(xAxis);

                //  Y-Axis (X = 0):
                double xZero = canvasWidth * (-xStart / (xEnd - xStart));
                Line yAxis = new Line
                {
                    Y1 = 0,
                    Y2 = canvasHeight,
                    StrokeThickness = 1
                };

                if (0 >= xStart && 0 <= xEnd)
                {
                    //X=0 is within visible range
                    yAxis.X1 = xZero;
                    yAxis.X2 = xZero;
                    yAxis.Stroke = Brushes.Black;
                }
                else
                {
                    //X=0 is outside visible range
                    double xPos = (0 < xStart) ? 10 : canvasWidth - 10;
                    yAxis.X1 = xPos;
                    yAxis.X2 = xPos;
                    yAxis.Stroke = Brushes.Gray;
                    yAxis.StrokeDashArray = new DoubleCollection { 4, 4 };
                }
                PlotCanvas.Children.Add(yAxis);

                //Draw axis ticks and labels
                DrawAxisTicks(xStart, xEnd, yMin, yMax, canvasWidth, canvasHeight, xAxisPos);


                //draw smooth curve
                Polyline polyline = new Polyline
                {
                    Stroke = Brushes.Blue,
                    StrokeThickness = 2
                };

                foreach (var p in smoothPoints)
                {
                    double xCanvas = (p.X - xStart) / (xEnd - xStart) * canvasWidth;
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

        //Catmull-Rom interpolation helper
        private Point CatmullRom(Point p0, Point p1, Point p2, Point p3, double t)
        {
            double t2 = t * t;
            double t3 = t2 * t;

            double x = 0.5 * ((2 * p1.X) +
                              (-p0.X + p2.X) * t +
                              (2 * p0.X - 5 * p1.X + 4 * p2.X - p3.X) * t2 +
                              (-p0.X + 3 * p1.X - 3 * p2.X + p3.X) * t3);

            double y = 0.5 * ((2 * p1.Y) +
                              (-p0.Y + p2.Y) * t +
                              (2 * p0.Y - 5 * p1.Y + 4 * p2.Y - p3.Y) * t2 +
                              (-p0.Y + 3 * p1.Y - 3 * p2.Y + p3.Y) * t3);

            return new Point(x, y);
        }

        //Button click handler
        private void DrawButton_Click(object sender, RoutedEventArgs e)
        {
            //check for default or empty text
            string defaultXStart = "-10";
            string defaultXEnd = "10";
            bool startEmptyOrDefault = string.IsNullOrWhiteSpace(xStartBox.Text) || xStartBox.Text == defaultXStart;
            bool endEmptyOrDefault = string.IsNullOrWhiteSpace(xEndBox.Text) || xEndBox.Text == defaultXEnd;

            //try parsing data in textbox
            bool xStartSuccess = double.TryParse(xStartBox.Text, out var xs);
            double xStart = xStartSuccess ? xs : -10;
            bool xEndSuccess = double.TryParse(xEndBox.Text, out var xe);
            double xEnd = xEndSuccess ? xe : 10;

            if (!xStartSuccess)
            {
                MessageBox.Show("Invalid start value. Using default -10.");
            }

            if (!xEndSuccess)
            {
                MessageBox.Show("Invalid end value. Using default 10.");
            }

            PlotFunction(FunctionBox.Text, xStart, xEnd);
        }

        //Helper function to draw axes
        private void DrawAxisTicks(double xStart, double xEnd, double yMin, double yMax, double canvasWidth, double canvasHeight, double xAxisPos)
        {
            int numXTicks = 10;     //number of ticks
            int numYTicks = 10;

            //round tick step sizes to "nice" numbers
            double xRange = xEnd - xStart;
            double yRange = yMax - yMin;

            double xStep = Math.Round(xRange / numXTicks);
            double yStep = Math.Round(yRange / numYTicks);

            //make sure steps aren't zero
            if (xStep == 0) xStep = 1;
            if (yStep == 0) yStep = 1;

            //align start/end to rounded multiples for cleaner labels
            double xStartAligned = Math.Ceiling(xStart / xStep) * xStep;
            double xEndAligned = Math.Floor(xEnd / xStep) * xStep;
            double yStartAligned = Math.Ceiling(yMin / yStep) * yStep;
            double yEndAligned = Math.Floor(yMax / yStep) * yStep;

            //X-axis ticks
            for (double xVal = xStartAligned; xVal <= xEndAligned + 0.0001; xVal += xStep)
            {
                double xCanvas = (xVal - xStart) / (xEnd - xStart) * canvasWidth;
                double yCanvas = xAxisPos;

                Line tick = new Line
                {
                    X1 = xCanvas,
                    X2 = xCanvas,
                    Y1 = yCanvas - 4,
                    Y2 = yCanvas + 4,
                    Stroke = Brushes.Black,
                    StrokeThickness = 1
                };
                PlotCanvas.Children.Add(tick);

                TextBlock label = new TextBlock
                {
                    Text = xVal.ToString("0"),
                    FontSize = 10
                };
                Canvas.SetLeft(label, xCanvas - 8);
                Canvas.SetTop(label, yCanvas + 6);
                PlotCanvas.Children.Add(label);
            }

            // === Y-axis ticks ===
            for (double yVal = yStartAligned; yVal <= yEndAligned + 0.0001; yVal += yStep)
            {
                double yCanvas = canvasHeight - ((yVal - yMin) / (yMax - yMin) * canvasHeight);
                double xCanvas = canvasWidth * (-xStart / (xEnd - xStart));

                Line tick = new Line
                {
                    X1 = xCanvas - 4,
                    X2 = xCanvas + 4,
                    Y1 = yCanvas,
                    Y2 = yCanvas,
                    Stroke = Brushes.Black,
                    StrokeThickness = 1
                };
                PlotCanvas.Children.Add(tick);

                TextBlock label = new TextBlock
                {
                    Text = yVal.ToString("0"),
                    FontSize = 10
                };
                Canvas.SetLeft(label, xCanvas + 6);
                Canvas.SetTop(label, yCanvas - 8);
                PlotCanvas.Children.Add(label);
            }
        }
    }
}
