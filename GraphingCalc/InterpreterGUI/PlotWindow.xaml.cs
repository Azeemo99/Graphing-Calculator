using Microsoft.FSharp.Collections;
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
        private FSharpMap<string, InterpreterCore.v> symTable;

        public PlotWindow(FSharpMap<string, InterpreterCore.v> sharedTable)
        {
            InitializeComponent();
            symTable = sharedTable;
        }

        //Main plotting function using Catmull-Rom spline for smooth curve
        public void PlotFunction(string funcExpr, double xStart = -10.0, double xEnd = 10.0)
        {
            try
            {
                // --- 1. Build x samples ---
                double step = 0.25;
                List<double> xVals = new List<double>();
                for (double x = xStart; x <= xEnd; x += step)
                    xVals.Add(x);

                var fsharpXVals = Microsoft.FSharp.Collections.ListModule.OfSeq(xVals);

                // --- 2. Call F# plotEval ---
                var result = InterpreterCore.plotEval(symTable, funcExpr, fsharpXVals);

                // --- 3. Filter invalid points ---
                var rawPoints = result
                    .Where(p => !double.IsNaN(p.Item2) && !double.IsInfinity(p.Item2))
                    .Select(p => new Point(p.Item1, p.Item2))
                    .ToList();

                if (rawPoints.Count == 0)
                {
                    MessageBox.Show("No valid points to plot.");
                    return;
                }

                // --- 4. Check for horizontal line ---
                List<Point> smoothPoints;
                if (rawPoints.All(p => p.Y == rawPoints[0].Y))
                {
                    // Constant function: no smoothing needed
                    smoothPoints = new List<Point>(rawPoints);
                }
                else if (rawPoints.Count < 4)
                {
                    // Not enough points for Catmull-Rom
                    smoothPoints = new List<Point>(rawPoints);
                }
                else
                {
                    // Apply Catmull-Rom smoothing
                    smoothPoints = new List<Point>();
                    for (int i = 0; i < rawPoints.Count - 3; i++)
                        for (double t = 0; t <= 1.0; t += 0.05)
                            smoothPoints.Add(CatmullRom(rawPoints[i], rawPoints[i + 1],
                                                        rawPoints[i + 2], rawPoints[i + 3], t));
                }

                // --- 5. Find y-min/max and prevent zero range ---
                double yMin = smoothPoints.Min(p => p.Y);
                double yMax = smoothPoints.Max(p => p.Y);
                if (yMax == yMin)
                {
                    yMin -= 1.0;
                    yMax += 1.0;
                }
                double padding = (yMax - yMin) * 0.1;
                yMin -= padding;
                yMax += padding;

                // --- 6. Clear canvas ---
                PlotCanvas.Children.Clear();
                double canvasWidth = PlotCanvas.ActualWidth;
                double canvasHeight = PlotCanvas.ActualHeight;

                double xAxisPos;
                if (0 >= yMin && 0 <= yMax)
                {
                    xAxisPos = canvasHeight - ((0 - yMin) / (yMax - yMin) * canvasHeight);
                }
                else
                {
                    xAxisPos = (0 < yMin) ? canvasHeight - 10 : 10;
                }
                // --- Draw X-axis line ---
                Line xAxisLine = new Line
                {
                    X1 = 0,
                    X2 = canvasWidth,
                    Y1 = xAxisPos,
                    Y2 = xAxisPos,
                    Stroke = Brushes.Black,
                    StrokeThickness = 1
                };
                PlotCanvas.Children.Add(xAxisLine);

                // --- Draw Y-axis line ---
                double xZero = canvasWidth * (-xStart / (xEnd - xStart));
                Line yAxisLine = new Line
                {
                    X1 = xZero,
                    X2 = xZero,
                    Y1 = 0,
                    Y2 = canvasHeight,
                    Stroke = Brushes.Black,
                    StrokeThickness = 1
                };
                PlotCanvas.Children.Add(yAxisLine);

                DrawAxisTicks(xStart, xEnd, yMin, yMax, canvasWidth, canvasHeight, xAxisPos);
                // --- 7. Draw axes ---
                

                // --- 8. Draw curve ---
                Polyline polyline = new Polyline
                {
                    Stroke = Brushes.Blue,
                    StrokeThickness = 2
                };

                double yRange = yMax - yMin;
                foreach (var p in smoothPoints)
                {
                    double xc = (p.X - xStart) / (xEnd - xStart) * canvasWidth;
                    double yc = canvasHeight - ((p.Y - yMin) / yRange * canvasHeight);
                    polyline.Points.Add(new Point(xc, yc));
                }

                PlotCanvas.Children.Add(polyline);
            }
            catch (Exception ex)
            {
                MessageBox.Show("Error plotting function: " + ex.Message);
            }
        }


        public void UpdateSymbolTable(FSharpMap<string, InterpreterCore.v> newTable)
        {
            symTable = newTable;
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

        private void DiffButton_click(object sender, RoutedEventArgs e) {
            var result = InterpreterCore.differentiate("1");


        }

        
    }


}
