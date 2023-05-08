Imports System.Drawing
Imports System.Drawing.Drawing2D
Imports System.Drawing.Image
Imports System.IO

Partial Public Class graphic
    Inherits System.Web.UI.Page
    Dim nID As Int32

    Const nFn As Int32 = 4
    Const nDiv As Int32 = 10
    Public Const w As Int32 = 580
    Public Const h As Int32 = 580
    Dim vMtxParser(nFn - 1) As matrixParser
    Dim vPoly(nFn - 1) As Polynomial, iP As Int32
    Dim oVars(nFn - 1) As VarsAndFns
    Dim vFn() As String
    Dim vIsEmptyStr(nFn - 1) As Boolean
    Dim vIsPoly(nFn - 1) As Boolean
    Dim vDbl(3) As Double
    Dim sErrMsg As String = "Graphic n/a"
    Dim min As Double = Double.MaxValue
    Dim max As Double = -min
    Dim cImg As HtmlImage
    Dim origH, origW As Int32
    Dim marginPt0 As New Point(110, 60)
    Dim marginPt1 As New Point(100, 50)
    Dim iComplexPts, iOutOfRange As Int32
    Const margDch As Int32 = 65
    Dim cfg As Config = Config.cfg
    Dim multiplier As Double

    Sub logfile(ex As Exception)
        Try
            Dim sFn() As String = {f1.Text, _
                       f2.Text, _
                       f3.Text, _
                       f4.Text}
            Dim sTB() As String = {tbLeft.Text, _
                       tbRight.Text, _
                       tbBottom.Text, _
                       tbTop.Text, _
                       lblMessage.Text}
            C_SessionWeb.logFile(ex, _
                 Join(sFn, "|"), _
                Join(sTB, "|"), Page, nID, "polycalc.txt")
        Catch ex2 As Exception

        End Try
    End Sub
    Private Sub Page_PreRender(sender As Object, e As EventArgs) Handles Me.PreRender
        Dim sImagePath As String = MapPath("~/images/grTable.png")
        Try
            lblMessage.Visible = False
            lblMessage.Text = ""
            iComplexPts = 0 : iOutOfRange = 0
            cImg = New UI.HtmlControls.HtmlImage
            cImg.Alt = "Graphic"
            cImg.Attributes.Add("class", "grPos")
            C_SessionWeb.contador( _
                Page, _
                Request, _
                Response)
            nID = C_SessionWeb.getID(Page)
            If Page.IsPostBack Then
                Me.logfile(Nothing)
                GetGraphic(sImagePath)
            Else
                getSessionParams()
                Me.logfile(Nothing)
                GetGraphic(sImagePath)
            End If
            Me.Panel1.Controls.Add(cImg)
            If iComplexPts OrElse iOutOfRange Then

                If iComplexPts Then
                    lblMessage.Text = iComplexPts.ToString + " complex points couldn't be drawn."
                    ' case "es" " puntos complejos no se pudieron trazar."
                    ' case "ca" " punts complexes no es pogueren dibuixar."
                End If
                If iOutOfRange Then
                    lblMessage.Text += iOutOfRange.ToString + " out of range points couldn't be drawn."
                    ' case "es" " puntos no se pudieron trazar, por estar fuera de rango."
                    ' case "ca" " punts no es pogueren dibuixar, per ser-hi fora de rang."
                End If
            End If
        Catch ex As Exception
            If lblMessage.Text.Length = 0 Then
                If iComplexPts OrElse iOutOfRange Then
                    If iComplexPts Then
                        lblMessage.Text = iComplexPts.ToString + " complex points couldn't be drawn."
                        ' case "es" " puntos complejos no se pudieron trazar."
                        ' case "ca" " punts complexes no es pogueren dibuixar."
                    End If
                    If iOutOfRange Then
                        lblMessage.Text += iOutOfRange.ToString + " out of range points couldn't be drawn."
                        ' case "es" " puntos no se pudieron trazar, por estar fuera de rango."
                        ' case "ca" " punts no es pogueren dibuixar, per ser-hi fora de rang."
                    End If
                Else
                    lblMessage.Text = "n/a"
                End If
            End If
            logfile(ex)
        Finally
            If lblMessage.Text.Length Then
                lblMessage.Visible = True
            End If
        End Try
    End Sub
    Sub getSessionParams()
        Dim xi, xf, yi, yf, xAxis, yAxis As Double, fn(2) As String
        Dim yAuto As Double
        Try
            xi = C_SessionWeb.xi(nID)
            xf = C_SessionWeb.xf(nID)
            If xi >= xf Then
                xi = -4
                xf = 4
                fn(2) = "0"
            End If
            yi = C_SessionWeb.yi(nID)
            yf = C_SessionWeb.yf(nID)
            fn(0) = C_SessionWeb.sfn(nID)
            fn(1) = C_SessionWeb.sf2(nID)
            fn(2) = C_SessionWeb.sf3(nID)
            xAxis = C_SessionWeb.xAxis(nID)
            yAxis = C_SessionWeb.yAxis(nID)
            yAuto = C_SessionWeb.yAuto(nID)
            If xAxis < 0.5 Then xAxis = 1.0
            If xAxis > 2.0 Then xAxis = 2.0
            If yAxis < 0.5 Then yAxis = 1.0
            If yAxis > 2.0 Then yAxis = 2.0
            If yAuto = 1 Then
                yi = 0
                yf = 0
            End If
        Catch ex As Exception

        End Try
        Try
            tbLeft.Text = xi.ToString(MathGlobal8.us)
            tbRight.Text = xf.ToString(MathGlobal8.us)
            tbTop.Text = yf.ToString(MathGlobal8.us)
            tbBottom.Text = yi.ToString(MathGlobal8.us)
            If Len(fn(0)) Then
                f1.Text = fn(0)
            End If
            If Len(fn(1)) Then
                f2.Text = fn(1)
            End If
        Catch ex As Exception

        End Try
    End Sub

    Private Sub btnDraw_Click(sender As Object, e As EventArgs) Handles btnDraw.Click
        'Dim sImagePath As String = MapPath("~/images/grTable.png")
        'GetGraphic(sImagePath)
    End Sub
    Sub GetGraphic(sGridImagePath As String)
        Try

            If Not validateInputs() Then
                lblMessage.Text = "n/a: invalid input values."
                ' case "es" "n/a: valores de entrada no válidos."
                ' case "ca" "n/a: valors d'entrada no valids."
                Exit Try
            End If
            vFn = {f1.Text, _
                   f2.Text, _
                   f3.Text, _
                   f4.Text}
            Me.getvMtxP_and_vPoly(vFn)

            ' Load a square grid of ten rows
            ' and columns (i.e. 11 horizontal lines
            ' and 11 vertical lines):
            Dim bmp As New Bitmap(sGridImagePath)
            origH = bmp.Height
            origW = bmp.Width
            Dim gr As Graphics = Graphics.FromImage(bmp)
            gr.SmoothingMode = Drawing2D.SmoothingMode.AntiAlias
            gr.InterpolationMode = InterpolationMode.HighQualityBicubic

            Dim vPen() As Pen = {New Pen(Brushes.Black, 0), _
                                 New Pen(Brushes.Blue, 0), _
                                 New Pen(Brushes.Green, 0), _
                                 New Pen(Brushes.Red, 0)}


            Dim vPts(-1)()() As PointF, iv As Int32
            ' vPts(i) holds all the points of a function (for ex. f(x)=1/x)
            ' vPts(i)(j) has all the points of a continous interval of the
            '            function. If f(x)=1/x, point x=0 is a discontinuity (f(x)=infinity):
            '            vPts(i)(0) has all the points f(x)=1/x where x<0
            '            vPts(i)(0) has all the points f(x)=1/x where x>0
            ' vPts(i)(j)(k) is a concrete point of the function, for example:
            '            vPts(i)(0)(0) may be point (x=-4, y=1/-4)
            '            vPts(i)(0)(1) may be point (x=-3.9, y=1/-3.9)
            '            ....
            '            vPts(i)(1)(0) may be point (x=0.1, y=1/0.1)
            '            vPts(i)(1)(1) may be point (x=0.2, y=1/0.2)
            '            ....
            ' (here, the increment 0.1 is ficticious, really the step depends on
            ' the interval Xleft, Xright and the number of points to draw,
            ' see method evalFnToGraphicsPath())
            Dim bExit As Boolean = False
            multiplier = 1.0
            Dim max2 As Double
            Dim min2 As Double
            Do
                max2 = -Double.MaxValue
                min2 = Double.MaxValue
                iv = 0
                For i As Int32 = 0 To vIsEmptyStr.Length - 1
                    If Not vIsEmptyStr(i) Then

                        ' Get function's path points:
                        ReDim Preserve vPts(iv)
                        vPts(iv) = _
                            Me.evalFnToGraphicsPath( _
                            i, vDbl(0), vDbl(1), w)
                        iv += 1
                        max2 = Math.Max(max2, max)
                        min2 = Math.Min(min2, min)
                    End If
                Next
                If max2 / multiplier > Single.MaxValue AndAlso _
                min2 / multiplier > -Single.MaxValue Then
                    multiplier *= (max2 + min2) / 2.0
                ElseIf max2 < -Single.MaxValue Then
                    multiplier *= (max2 + min2) / 2.0
                Else
                    Exit Do
                End If
            Loop
            If Me.chkAutoTopBottom.Checked Then
                max = max2 : min = min2
            Else
                max = vDbl(2) : min = vDbl(3)
            End If
            iv = 0
            For i As Int32 = 0 To vIsEmptyStr.Length - 1
                If Not vIsEmptyStr(i) Then

                    ' Get function's path points:
                    For j As Int32 = 0 To vPts(iv).GetLength(0) - 1
                        gr.TranslateTransform(0, -min, MatrixOrder.Append)
                        ' Scale X, Y axis:
                        If Math.Abs(max - min) > 10 ^ -10 Then
                            gr.ScaleTransform(bmp.Width / w, _
                                              bmp.Height / ((max - min) / multiplier), _
                                              MatrixOrder.Append)
                        End If
                        ' Add into image's Graphics gr:
                        gr.DrawCurve(vPen(i), vPts(iv)(j))

                        If chkAutoTopBottom.Checked Then
                        End If
                        ' restore origin and scale:
                        gr.ResetTransform()
                    Next
                    iv += 1
                End If
            Next

            If chkAutoTopBottom.Checked Then
                vDbl(2) = max
                vDbl(3) = min
                tbTop.Text = vDbl(2).ToString(MathGlobal8.us)
                tbBottom.Text = vDbl(3).ToString(MathGlobal8.us)
            Else
                max = vDbl(2)
                min = vDbl(3)
            End If
            If max = min Then
                If vDbl(2) = vDbl(3) Then
                    lblMessage.Text = "n/a: top and bottom values can't be equal or void."
                    ' case "es" " los valores superior e inferior no pueden ser iguales"
                    ' case "ca" " els valors superior i inferior no poden ésser iguals."
                Else
                    lblMessage.Text = iComplexPts.ToString + " complex points couldn't be drawn."
                    ' case "es" " puntos no se pudieron trazar."
                    ' case "ca" " punts no és pogueren dibuixar."
                End If
                Throw New Exception(lblMessage.Text)
            End If
            mouseeventInit(bmp.Width, bmp.Height)

            ' Flip image because gr's vertical axis increments
            ' downwards and graphic's Y axis will increment
            ' upwards:
            bmp.RotateFlip(RotateFlipType.Rotate180FlipX)

            ' Get a bigger bitmap...
            Dim bmp1 As New Bitmap(w + margDch, h) ' bmp.Width + marginPt0.X, bmp.Height + marginPt0.Y)
            Dim gr1 As Graphics = Graphics.FromImage(bmp1)
            gr1.Clear(Color.White)

            ' ...and copy former image to (100,50) to allow 
            ' space on the left and top for the strings:
            gr1.DrawImage(bmp, New Point(marginPt1.X, marginPt1.Y))
            gr1.SmoothingMode = Drawing2D.SmoothingMode.AntiAlias

            ' Draw X's and Y's coordinates values as strings:
            drawStrings(gr1)

            ' Release resources:
            gr.Dispose()
            bmp.Dispose()

            Using ms As New MemoryStream
                ' Save bmp1 into memory...
                bmp1.Save(ms, Imaging.ImageFormat.Png)
                ' ...and set, to the image control on the
                ' aspx page, the memory file in base64 string format:
                cImg.Src = "data:image/png;base64," + Convert.ToBase64String(ms.ToArray)
            End Using

            ' Release resources:
            gr1.Dispose()
            bmp1.Dispose()
        Catch ex As Exception
            Throw ex
            'lblMessage.Text = sErrMsg
        End Try
    End Sub
    Sub mouseeventInit(bmpW As Int32, bmpH As Int32)
        Try
            'If Not Page.IsPostBack Then
            '    Exit Sub
            'End If
            cImg.Attributes.Add("onmousemove", "coord(event,this);")

            ' Set Xm,Xb,Ym,Yb for mouseover event:
            Dim m As Double = (max - min) / bmpH
            Ym.Value = m.ToString(MathGlobal8.us)
            Yb.Value = ((max + min + m * bmpH) / 2.0).ToString(MathGlobal8.us)
            Ymargin.Value = ((marginPt0.Y + marginPt1.Y) / 2.0 * bmpH / h).ToString(MathGlobal8.us)
            m = (vDbl(1) - vDbl(0)) / bmpW
            Xm.Value = m.ToString(MathGlobal8.us)
            Xb.Value = ((vDbl(1) + vDbl(0) - m * bmpW) / 2.0).ToString(MathGlobal8.us)
            Xmargin.Value = (marginPt1.X).ToString(MathGlobal8.us)
            Xmin.Value = min '.ToString(MathGlobal8.us)
        Catch ex As Exception
            Throw ex
        End Try
    End Sub
    Function validateInputs() As Boolean
        Dim mP As New matrixParser
        Try
            ' Axis' X values will be in the
            ' interval [vDbl(0), vDbl(1)]

            ' Verify tbLeft -- tbRight
            ' ------------------------
            ' eval eventual arithmetic (for ex. 2/3) ...
            If tbLeft.Text = "" Then tbLeft.Text = "-4"
            If tbRight.Text = "" Then tbRight.Text = "4"
            If tbTop.Text = "" Then tbLeft.Text = "4"
            If tbBottom.Text = "" Then tbLeft.Text = "-4"
            cfg.doTimeOut = timeout.never
            cfg.timeOutms = 45 * 1000
            System.Threading.Thread.Sleep(0)
            cfg.doTimeOut = timeout.whenTimeIsOver
            System.Threading.Thread.Sleep(0)
            'cfg.bUseUnits = False
            mP.parse(tbLeft.Text, cfg:=cfg)
            ' ...but must reduce to a double value:
            cfg.doTimeOut = timeout.never
            vDbl(0) = mP.retCjo(0).pRe.ToDouble

            cfg.doTimeOut = timeout.whenTimeIsOver
            'cfg.bUseUnits = False
            mP.parse(tbRight.Text, cfg:=cfg)
            vDbl(1) = mP.retCjo(0).pRe.ToDouble
            cfg.doTimeOut = timeout.never
            If vDbl(1) < vDbl(0) Then
                ' If left < right, swap values:
                Dim dbl As Double = vDbl(0)
                vDbl(0) = vDbl(1)
                vDbl(1) = dbl
            ElseIf vDbl(0) = vDbl(1) Then
                Return False
            End If
            If vDbl(1) - vDbl(0) <= 1000 * Double.MinValue Then
                Return False
            End If

            If chkAutoTopBottom.Checked = False Then
                ' Axis' Y values will be in the
                ' interval [vDbl(2), vDbl(3)]:
                ' values outside these bounds 
                ' will not be graphed.

                ' If not auto-Top-Bottom checked, consider
                ' inputs in textboxes tbTop, tbBottom.

                ' Verify tbTop -- tbBottom
                ' ------------------------
                mP.parse(tbTop.Text)
                vDbl(3) = mP.retCjo(0).pRe.ToDouble
                mP.parse(tbBottom.Text)
                vDbl(2) = mP.retCjo(0).pRe.ToDouble
                If vDbl(2) < vDbl(3) Then
                    ' If top < bottom, swap values:
                    Dim dbl As Double = vDbl(2)
                    vDbl(2) = vDbl(3)
                    vDbl(3) = dbl
                ElseIf vDbl(2) = vDbl(3) Then
                    Return False
                End If
                If vDbl(2) - vDbl(3) <= 1000 * Double.MinValue Then
                    Return False
                End If
            Else
                ' If auto-Top-Bottom checked
                ' allow any double value:
                vDbl(2) = Double.MaxValue
                vDbl(3) = -Double.MaxValue
            End If
        Catch ex As Exception
            Throw ex
            'Return False
        End Try
        Return True
    End Function
    Private Sub getvMtxP_and_vPoly(sFn() As String)
        Try
            For i As Int32 = 0 To nFn - 1
                If sFn(i) IsNot Nothing AndAlso
                sFn(i).Length Then
                    vMtxParser(i) = New matrixParser()
                    vMtxParser(i).parse(sFn(i), "", oVars(i), cfg)
                    If oVars(i).length <= 1 Then
                        ' Find out if sFn is a Polynomial expression
                        ' because, if so, evaluation will be less 
                        ' time consuming:
                        If vMtxParser(i).ret.exprMtx.IsPolynomial Then
                            vIsPoly(i) = True
                            vPoly(i) = vMtxParser(i).ret.curExpr.getPolynomial
                        End If
                    Else
                        ' more than 1 variable, discard:
                        'vIsEmptyStr(i) = True
                        lblMessage.Text = "<span style=""color:black"">n/a: more than one variable {"
                        ' case "es" "n/a: hay presente más de una variable"
                        ' case "ca" "n/a: n'hi ha més d'una variable."
                        For nV As Int32 = 0 To oVars(i).length - 1
                            lblMessage.Text += oVars(i).getVarNameByID(nV)
                            If nV < oVars(i).length - 1 Then
                                lblMessage.Text += ","
                            Else
                                lblMessage.Text += "}"
                            End If
                        Next
                        lblMessage.Text += " in function " + sFn(i)
                        ' case "es" " en la función "
                        ' case "ca" " a la funció "
                        lblMessage.Text += " <br />Tip: for ex. enter x^2-1, do NOT enter the function name" +
                            " <span style=""color:red"">f(x)=</span>x^2-1</span>"
                        ' case "es" " <br />Ayuda: entre por ejemplo x^2-1, y NO la definición <span style=""color:red"">f(x)=</span>x^2-1"
                        ' case "ca" " <br />Ajut: entreu per exemple x^2-1 y NO pas la definició <span style=""color:red"">f(x)=</span>x^2-1"
                        Throw New Exception(lblMessage.Text)
                    End If
                Else
                    vIsEmptyStr(i) = True
                End If

            Next
        Catch ex As Exception
            Throw ex
        End Try
    End Sub
    Private Sub drawStrings(ByRef gr As Graphics) ' Get graph's strings
        Try
            Dim iv As Int32 = 0
            Dim incrV As Double = (max - min) / nDiv
            Dim incrH As Double = (vDbl(1) - vDbl(0)) / nDiv
            Dim dist As Double = origH / nDiv
            Dim fntStr As New Font("Calibri", 10)

            Dim ptImg As New Point(100, 30)

            Dim oldsX(10) As String
            Dim Xdec(10) As Int32
            Dim oldsY(10) As String
            Dim Ydec(10) As Int32
            Dim nDec As Int32 = 3
            Dim maxDec As Int32 = 9
            Dim curPos As Single = 0
repite:
            Dim Vert As Double = max
            Dim Horiz As Double = vDbl(0)
            For row = 0 To 10
                ' Left side strings:
                Dim sX As String = ""
                If Vert < 100 AndAlso Xdec(row) < 2 Then
                    Xdec(row) = 2
                ElseIf Vert < 10 AndAlso Xdec(row) < 3 Then
                    Xdec(row) = 3
                End If
                If Math.Abs(Vert) > 10 ^ 5 OrElse _
                Math.Abs(Vert) < 10 ^ -5 Then
                    If Xdec(row) < 2 Then Xdec(row) = 2
                    sX = Vert.ToString("g" + Xdec(row).ToString, MathGlobal8.us)
                Else
                    sX = Math.Round(Vert, Xdec(row)).ToString(MathGlobal8.us)
                End If
                For i As Int32 = 0 To row - 1
                    If oldsX(i) = sX Then
                        If Xdec(row) < maxDec Then
                            Xdec(i) += 1
                            Xdec(row) += 1
                            GoTo repite
                        End If
                    End If
                Next
                oldsX(row) = sX
                Vert = max - incrV * (row + 1)

                ' Strings at the top:
                Dim sY As String = ""
                If Horiz < 100 AndAlso Ydec(row) < 2 Then
                    Ydec(row) = 2
                ElseIf Horiz < 10 AndAlso Ydec(row) < 3 Then
                    Ydec(row) = 3
                End If
                If Math.Abs(Horiz) > 10 ^ 5 OrElse _
                Math.Abs(Horiz) < 10 ^ -5 Then
                    If Ydec(row) < 2 Then Ydec(row) = 2
                    sY = Horiz.ToString("g" + Ydec(row).ToString, MathGlobal8.us)
                Else
                    sY = Math.Round(Horiz, Ydec(row)).ToString(MathGlobal8.us)
                End If
                For i As Int32 = 0 To row - 1
                    If oldsY(i) = sY Then
                        If Ydec(row) < maxDec Then
                            Ydec(i) += 1
                            Ydec(row) += 1
                            GoTo repite
                        End If
                    End If
                Next
                oldsY(row) = sY
                Horiz = vDbl(0) + incrH * (row + 1)
            Next
            For row = 0 To 10
                Dim s As SizeF = gr.MeasureString(oldsX(row), fntStr)
                gr.DrawString(oldsX(row), fntStr, _
                               Brushes.Black, _
                               New Point(ptImg.X - s.Width - 3, _
                                         ptImg.Y + s.Height / 2 + dist * row))

                s = gr.MeasureString(oldsY(row), fntStr)
                gr.DrawString(oldsY(row), fntStr, _
                               Brushes.Black, _
                               New Point(ptImg.X - s.Width / 2 - 1 + dist * row, _
                                         ptImg.Y - 2))
            Next
            Dim sngMin As Double = Single.MinValue
            If Math.Abs(max - min) > Math.Abs(sngMin) Then
                gr.ScaleTransform(origW / w, _
                                  origH / ((max - min) / multiplier), MatrixOrder.Append)
            End If
            gr.ResetTransform()
        Catch ex As Exception
            Throw ex
        End Try
    End Sub
    Function evalFnToGraphicsPath(iFn As Int32, x1 As Double, x2 As Double, nPts As Int32) As PointF()()
        Dim pts(0)() As PointF, iP0, iP1 As Int32

        Try
            Dim xStep As Double = (x2 - x1) / nPts
            Dim x As Double = x1
            Dim y As Double
            Dim i As Int32
            ReDim pts(0)(nPts)

            Dim bClosed As Boolean = False
            Dim bExclude As Boolean = False
            Dim PaZeros(-1) As Double
            Dim nZeros As Int32 = 0
            Dim iCurZero As Int32 = 0
            Dim DenP As Polynomial = Nothing
            Dim curPt As New PointF
            Dim oldPt0, oldPt1 As PointF
            Dim diff As Double = Double.MaxValue
            Dim cjo As Complex
            Dim expr As Expression = vMtxParser(iFn).ret.exprMtx.getExpr(0, 0)
            Dim Pa As Polynomial = vPoly(iFn)
            Do
                x = xStep * CDbl(i)
                ' Try-catch in case of for ex. singular points:
                Try
                    If vIsPoly(iFn) Then
                        ' Evaluate Pa(x+x1):
                        y = Pa.evalPrecis(New Rational(x + x1)).ToDouble
                    Else
                        cjo = expr.eval_1var_DblToCjo(x + x1)
                        If cjo IsNot Nothing Then
                            y = cjo.pRe.ToDouble
                            Dim dbIm As Double = cjo.pIm.ToDouble
                            If (Not cjo.pRe.IsZero AndAlso _
                            Math.Abs(cjo.pIm.ToDouble / cjo.pRe.ToDouble) > 10 ^ -8) OrElse _
                           (cjo.pRe.IsZero AndAlso Not cjo.pIm.IsZero) Then
                                bExclude = True
                                iComplexPts += 1
                            End If
                        Else
                            bExclude = True
                        End If
                    End If
                Catch ex As Exception
                    bExclude = True
                End Try
                If Not bExclude AndAlso _
                (vDbl(3) > y OrElse vDbl(2) < y) Then
                    bExclude = True
                End If
                If iP1 > 2 AndAlso Not bExclude AndAlso _
                vDbl(3) <= y AndAlso y <= vDbl(2) Then
                    ' Get the slope of line oldPt0-oldPt1:
                    Dim m1 As Double = (oldPt1.Y - oldPt0.Y) / 1
                    ' ...and of line oldPt1-(x,y):
                    Dim m2 As Double = (y - oldPt1.Y) / 1
                    Dim atg As Double = Math.Abs(Math.PI / 2 - Math.Abs(Math.Atan2(m1, m2)))
                    'If atg < diff Then
                    '    diff = atg
                    '    lblMessage.Text = diff.ToString(MathGlobal8.us)
                    'End If
                    If Double.IsInfinity(m1) OrElse _
                    Double.IsNaN(m1) OrElse _
                    Double.IsInfinity(m2) OrElse _
                    Double.IsNaN(m2) Then
                        bExclude = True
                    ElseIf Math.Sign(m1) <> Math.Sign(m2) AndAlso _
                    Math.Abs(m1) > 10 AndAlso Math.Abs(m2) > 10 AndAlso _
                    atg < 1.6 Then
                        bExclude = True
                    End If
                End If
                If iP1 Then
                    oldPt1.X = pts(iP0)(iP1 - 1).X
                    oldPt1.Y = pts(iP0)(iP1 - 1).Y
                    If iP1 > 1 Then
                        oldPt0.X = pts(iP0)(iP1 - 2).X
                        oldPt0.Y = pts(iP0)(iP1 - 2).Y
                    End If
                End If
                If bExclude OrElse Double.IsInfinity(y) _
                OrElse Double.IsNaN(y) Then
                    If Double.IsInfinity(y) OrElse _
                    Double.IsNaN(y) Then
                        iOutOfRange += 1
                    End If
                    ' 'y' value out of bounds 
                    If Not bClosed Then
                        If iP1 Then
                            If iP1 > 1 Then
                                ReDim Preserve pts(iP0)(iP1 - 1)
                                iP0 += 1
                            End If
                            ReDim Preserve pts(iP0), pts(iP0)(nPts - 1)
                            iP1 = 0
                        End If
                        bClosed = True
                    End If
                    bExclude = False
                Else
                    pts(iP0)(iP1).X = i
                    pts(iP0)(iP1).Y = y / multiplier
                    iP1 += 1
                    min = Math.Min(min, y)
                    max = Math.Max(max, y)
                    bClosed = False
                End If
                i += 1
            Loop While i <= nPts
            If iP1 Then
                ReDim Preserve pts(iP0)(iP1 - 1)
            ElseIf iP0 Then
                ReDim Preserve pts(iP0 - 1)
            Else
                ReDim Preserve pts(-1)
            End If

        Catch ex As Exception
            Throw ex
        End Try
        Return pts
    End Function

End Class