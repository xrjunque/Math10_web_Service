Imports System.Text

Public Class Interpolation

    Public Sub New()
        MyBase.New()
    End Sub

    Public nId As Int64
    Dim PolyFn As Polynomial
    Dim exprFn As Expression
    Dim ptsMtx As ExprMatrix
    Dim nIntervals1 As Int64
    Public retPoly As Polynomial
    Public An As ExprMatrix
    Public Bn As ExprMatrix
    Const maxIntervals As Int64 = 110
    Public Sub New(ByVal pointsMtx As ExprMatrix)
        Try
            If pointsMtx Is Nothing Then
                Throw New Exception(Msg8.num(66))
            End If
            'nIntervals = Me.ptsMtx.vVect.Length - 1
            nIntervals = pointsMtx.Rows - 1
            ptsMtx = New ExprMatrix(pointsMtx)
        Catch ex As Exception
            Throw
        End Try
    End Sub

    Private Function getPointsExpr(ByVal PolyFn As Polynomial,
                                         ByVal a As Double,
                                         ByVal b As Double,
                                         ByVal nIntervals As Int64) As ExprMatrix
        Dim Mc As ExprMatrix
        Dim db As Double
        Try
            If Me.nIntervals = 0 Then
                If nIntervals < 1 Then
                    nIntervals = 1
                End If
                Me.nIntervals = nIntervals
            End If
            ' ReDim Mc.vVect(nIntervals), An.vVect(nIntervals), Bn.vVect(nIntervals)
            Mc = New ExprMatrix(nIntervals, nIntervals)
            An = New ExprMatrix(nIntervals, nIntervals)
            Bn = New ExprMatrix(nIntervals, nIntervals)
            Dim i As Int64
            For i = 0 To nIntervals
                '    Mc.vVect(i) = New Vector(Complex.zero)
                For j = 0 To Mc.Columns(i)
                    Mc.Item(i, j) = New Expression(Complex.zero)
                Next
                'ReDim Mc.vVect(i).vPoly(1)
                db = ((nIntervals - i) * a + b * i) / nIntervals
                Dim cjo As New Complex(db, 0)
                'Mc.vVect(i).vPoly(0) = New Polynomial(cjo)
                'Mc.vVect(i).vPoly(1) = New Polynomial(PolyFn.evalCjo(cjo))
                'An.vVect(i).vPoly(0) = New Polynomial(cjo)
                'Bn.vVect(i).vPoly(0) = New Polynomial(PolyFn.evalCjo(cjo))
                Mc.Item(i, 0) = New Expression(cjo)
                Mc.Item(i, 1) = New Expression(PolyFn.EvalPrecisCplx(cjo)) ' PolyFn.EvalCplx(cjo))
                An.Item(i, 0) = New Expression(cjo)
                Bn.Item(i, 0) = New Expression(PolyFn.EvalPrecisCplx(cjo)) 'PolyFn.EvalCplx(cjo))
            Next
        Catch ex As Exception
            Throw New Exception(
            String.Format(Msg8.msg(24), db.ToString))
        End Try
        Return Mc
    End Function
    Public Property nIntervals As Int64
        Get
            Return nIntervals1
        End Get
        Set(value As Int64)
            If value <> nIntervals1 Then
                If value > maxIntervals Then
                    Throw New Exception(String.Format(
                     Msg8.num(65), value, maxIntervals))
                End If
                nIntervals1 = value
            End If
        End Set
    End Property

    ''' <summary>
    ''' Given the interval [a,b], a number of subintervals 'nItervals' 
    ''' and the expression of a function f(x), obtains the Chebyshev
    ''' nodes (xi,f(xi)) -i ranging from 0 to nIntervals- in the
    ''' ptsMtx member of the return Interpolation object. 
    ''' Typically, after calling this method, the return's value 
    ''' lagrangianInterpolation() method should be called to obtain 
    ''' a polynomial of degree equal to 'nIntervals', approximating 
    ''' f(x) at [a,b].
    ''' </summary>
    ''' <param name="a">Starting point</param>
    ''' <param name="b">Ending point</param>
    ''' <param name="nIntervals">Number of subintervals in [a,b]</param>
    ''' <param name="expr">expression class instance containing the function f(x)</param>
    ''' <returns>an interpolation class instance with Chebyshev nodes inside expr.ptsMtx</returns>
    ''' <remarks>See http://svn.assembla.com/svn/mna/tps/tp4ej5Paulina.pdf for an explanation about Chebyshev nodes</remarks>
    Shared Function getChebyshevNodes(
                                         ByVal a As Double,
                                         ByVal b As Double,
                                         ByVal nIntervals As Int64,
                                         ByVal expr As Expression) As Interpolation
        Dim Mc As ExprMatrix
        Dim db As Double
        Dim Intp As New Interpolation
        Try
            Intp.nIntervals = nIntervals
            Intp.exprFn = expr
            'ReDim Mc.vVect(nIntervals)
            Mc = New ExprMatrix(nIntervals + 1, 2)
            Dim i As Int64
            Dim ptsChebyshev(nIntervals) As Double
            For i = 0 To nIntervals
                ' Obtain root-i for the Chebyshev Polynomial of nIntervals+1 degree.
                ' This will provide to lagrangianInterpolation(), in a later call,
                ' a number of subintervals in [a,b] equal to 'nIntervals'
                db = Math.Cos((2.0 * i + 1) * Math.PI / (2 * (nIntervals + 1)))
                ptsChebyshev(i) = db
            Next
            Array.Sort(ptsChebyshev)
            For i = 0 To nIntervals
                ' Convert interval [-1,1] --> to interval [a,b]
                ' y=alfa*x+beta
                ' a=-1*alfa+beta
                ' b= 1*alfa+beta =>
                ' y=0.5*((b-a)*x+(a+b))
                db = ptsChebyshev(i)
                db = 0.5 * ((b - a) * db + (a + b))
                Dim cjo As New Complex(db, 0)

                ' Set Chebyshev node (xi,f(xi)) as a new row in Mc.vVect(i):
                'Mc.vVect(i) = New Vector()
                'ReDim Mc.vVect(i).vPoly(1)
                'Mc.vVect(i).vPoly(0) = New Polynomial(cjo) ' xi
                'Mc.vVect(i).vPoly(1) = New Polynomial(Intp.exprFn.evalExpression(cjo)) ' f(xi)
                Dim dic As New Dictionary(Of String, Expression)
                dic.Add(expr.GetVars(0), New Expression(cjo))
                Mc.Item(i, 0) = New Expression(cjo) ' xi
                'Mc.Item(i, 1) = New Expression(Intp.exprFn.ToPolynomial.EvalPrecisCplx(cjo)) ' f(xi)
                Dim pE As New ParseExpression
                Mc.Item(i, 1) = pE.Evaluate(dic, expr.ToString).vExpr(0)
            Next
            Intp.ptsMtx = Mc
        Catch ex As Exception
            Throw New Exception(
            String.Format(Msg8.msg(24), db.ToString))
        End Try
        Return Intp
    End Function

    Public Function lagrangianinterpolation(bHTMLdetail As Boolean) As Polynomial
        Try
            retPoly = New Polynomial(0.0)
            ' For Lagrangian interpolation let's obtain Lagragian polynomial, this is,
            ' calculate Gn(x) = retPoly = sum[k=0 to n] of( products(j=0 to n, j<>k) of ((x-xj)/(xk - xj)).
            ' i.e.:
            '                     n              n
            '                   ------        -------
            '                   \              |   |  (x - xj)
            ' Gn(x) = retPoly = /      f(xk) * |   | ----------
            '                   -----          |   |  (xk- xj)
            '                   k=0             k<>j
            '
            ' where x is the variable of our Polynomial
            ' and xj, xk (0<=j,k<=n  are the given points
            ' (we have previouly taken nInterval=n points equidistant or
            ' whatsoever, for example, Chebyshev nodes xi=cos((2*k-1)*pi /(2*n))
            ' and f(xk) is the value, in the point xk, of the function to approximate.
            ' The pairs (xk,f(xk)) are supposed to be already present in ptsMtx as an input
            ' in the class constructor New(ByVal pointsMtx As Matrix) -see above this
            ' class constructor-.
            '
            Dim j, k As Int64
            Dim x As New Polynomial("x", 1)

            Dim sSummands As String = ""
            For k = 0 To nIntervals
                Dim polyNumerator As New Polynomial(1.0)
                Dim cjoDenominator As New Complex(1.0, 0.0) ' f(xk)
                Dim sNum As String = ""
                Dim sDen As String = ""
                For j = 0 To nIntervals
                    'If ptsMtx.vVect(j).vPoly.Length <> 2 Then
                    '    Throw New Exception(Msg8.num(70))
                    'End If
                    If ptsMtx.Columns(j) <> 2 Then
                        Throw New Exception(Msg8.num(70))
                    End If
                    If j <> k Then
                        'If cfg.bDetail Then
                        '    If Len(sNum) Then
                        '        sNum += "*"
                        '        sDen += "*"
                        '    End If
                        '    sNum += "(x-" + ptsMtx.vVect(j).vPoly(0).toStringPoly(cfg) + ")"
                        '    sDen +=
                        '      "(" + ptsMtx.vVect(k).vPoly(0).cf(0).toStringComplex(cfg) + "-" _
                        '       + ptsMtx.vVect(j).vPoly(0).cf(0).toStringComplex(cfg) + ")"
                        'End If

                        'polyNumerator *= (x - ptsMtx.vVect(j).vPoly(0))
                        'cjoDenominator *= (ptsMtx.vVect(k).vPoly(0).cf(0) -
                        '           ptsMtx.vVect(j).vPoly(0).cf(0))
                        polyNumerator *= (x - ptsMtx.Item(j, 0).ToPolynomial)
                        cjoDenominator *= (ptsMtx.Item(k, 0).ToComplex -
                                   ptsMtx.Item(j, 0).ToComplex)

                    End If
                Next
                'If cfg.bDetail Then
                '    sNum = Replace(sNum, "--", "+")
                '    sDen = Replace(sDen, "--", "+")
                '    cfg.oDetail.Add("Numerator: " + sNum + " =")
                '    cfg.oDetail.Add("= " + polyNumerator.toStringPoly(cfg))
                '    cfg.oDetail.Add("Denominator: " + sDen + " =")
                '    cfg.oDetail.Add("= " + cjoDenominator.ToStringComplex(cfg))
                'End If
                If cjoDenominator.IsZero Then
                    Throw New DivideByZeroException
                End If
                Dim curPoly As Polynomial = Nothing
                'If cfg.bDetail Then
                '    Dim e1 As String = "(" + polyNumerator.toStringPoly(cfg) + ")"
                '    e1 += "*(" + ptsMtx.vVect(k).vPoly(1).toStringPoly(cfg)
                '    e1 += ")/(" + New Polynomial(cjoDenominator).toStringPoly(cfg) + ")"
                '    cfg.oDetail.Add(e1 + " =")
                'End If

                'curPoly = polyNumerator * ptsMtx.vVect(k).vPoly(1) / New Polynomial(cjoDenominator)
                curPoly = polyNumerator * ptsMtx.Item(k, 1).ToPolynomial / New Polynomial(cjoDenominator)

                'If cfg.bDetail Then
                '    Dim e1 As String = curPoly.toStringPoly(cfg)
                '    If bHTMLdetail Then
                '        cfg.oDetail.Add("= <span style=""color:brown"">" + e1 + "</span>")
                '    Else
                '        cfg.oDetail.Add("= " + e1)
                '    End If
                '    If e1.Chars(0) = "-" OrElse
                '    Len(sSummands) = 0 Then
                '        sSummands += e1
                '    Else
                '        sSummands += "+" + e1
                '    End If
                'End If
                retPoly += curPoly
            Next
            'If cfg.bDetail Then
            '    cfg.oDetail.Add(sSummands + " =")
            '    If bHTMLdetail Then
            '        cfg.oDetail.Add("= <span style=""color:red"">" + retPoly.toStringPoly(cfg) + "</span>")
            '    Else
            '        cfg.oDetail.Add("= " + retPoly.toStringPoly(cfg))
            '    End If
            'End If
        Catch ex As Exception
            Throw
        End Try
        Return retPoly
    End Function
    ''' <summary>
    '''   Find a lineal system of equations that approximates a non-linear system of equations.
    ''' </summary>
    ''' <param name="eG">eG = expression matrix containing a non-linear system of equations.
    ''' eG has 1 column and n rows = n equations.
    ''' Each element at each row i of eG is the left hand of an equation: Fi(x,y,...) = 0</param>
    ''' <param name="ini">The starting real value of the interval to interpolate.</param>
    ''' <param name="fin">The ending real value of the interval to interpolate.</param>
    ''' <returns>Returns a approximating lineal Sys.Of.Equations matrix to the given eG.</returns>
    ''' <remarks></remarks>
    Public Shared Function aproxNonToLinearSOEinterpolating(ByVal eG As ExprMatrix, ByVal ini As Double, ByVal fin As Double) As ExprMatrix
        Dim eMtx As ExprMatrix
        Try
            ' For each row (equation) in eG we'll establish a system of (n+m)!/(m!*(n+m-m)!)
            ' linear equations.
            ' Being m = 1 (linear system), this reduces to (n+1)!/(1!*n!) = n+1 equations,
            ' where n is the number of equations in eG = # of rows.
            '  http://www.google.es/url?sa=t&rct=j&q=&esrc=s&frm=1&source=web&cd=2&ved=0CEIQFjAB&url=http%3A%2F%2Fwww.siam.org%2Fstudents%2Fsiuro%2Fvol1issue1%2FS01002.pdf&ei=CwA4Ur2dFsK2hQf68IH4CA&usg=AFQjCNE1ckVB74h9WD5n6Av-VpfDeX8Ruw 

            ' The solution to this s.o.eq., say variables r,s and t, will set
            ' the first row of the return matrix, retMtx, -for ex.- r*x+s*y+t*z = 0, i.e.,
            ' (r,s,t) would be the first row of retMtx.
            ' For the remainding rows, we'll iterate over the same procedure.

            ' 1.a) Given the points (ini,ini,...,ini),(ini,ini,...,fin),...,(ini,fin,...,fin),(fin,fin,...,fin)
            ' 1.b) ...evaluate eG at those points y1=f(ini,ini,...,ini),y2=f(ini,ini,...,fin),...,yn=f(fin,fin,...,fin)
            '      and conform the
            '      points (ini,ini,...,ini,y1),(ini,ini,...,fin,y2),...,(fin,fin,...,fin,yn) of the
            '      matrix to resolve.
            Dim i, j As Int64
            Dim n As Int64 = eG.Rows
            Dim vVar() As String = eG.GetVars
            If n <> vVar.Length Then
                Throw New Exception(Msg8.num(13)) ' n/a
            End If
            eMtx = New ExprMatrix(n, 1)
            For row As Int64 = 0 To n - 1
                Dim mtx As New ExprMatrix(n + 1, n + 1)
                'mtx.setRowAndCol(n, n) ' n+1 x n+1 matrix =cols(0,1,..,n) x rows(0,1,...,n)
                Dim vIndepTerm As New ExprMatrix(1, n + 1)  ' Complex
                For i = 0 To n
                    Dim vCjo(n - 1) As Complex
                    For iPoint = 0 To n
                        ' 1.a) set the points:
                        'Dim oVar As New VarsAndFns(eG.cfg)
                        Dim oVars As New Dictionary(Of String, Expression)
                        For j = 0 To n
                            If j = n Then
                                'mtx.vVect(i).vPoly(j).cf(0) = New Complex(1.0)
                                mtx.Item(i, j) = New Expression(1.0)
                            ElseIf j < n - i Then
                                'oVar.AddVar(vVar(j), New Expression(ini))
                                'mtx.vVect(i).vPoly(j).cf(0) = New Complex(ini)
                                oVars.Add(vVar(j), New Expression(ini))
                                mtx.Item(i, j) = New Expression(ini)
                            Else
                                'oVar.AddVar(vVar(j), New Expression(fin))
                                'mtx.vVect(i).vPoly(j).cf(0) = New Complex(fin)
                                oVars.Add(vVar(j), New Expression(fin))
                                mtx.Item(i, j) = New Expression(fin)
                            End If
                        Next
                        ' 1.b) evaluate:
                        'vIndepTerm(i) = eG.getExpr(row, 0).evalExpression(Nothing, oVar)
                        Dim pE As New ParseExpression
                        vIndepTerm.Item(0, i) = pE.Evaluate(oVars, eG.Item(row, 0).ToString).vExpr(0)
                    Next
                Next
                ' 2) Resolve the linear system:
                'Dim invMtx As Matrix = mtx ^ -1
                Dim invMtx As ExprMatrix = mtx ^ New ExprMatrix(-1)
                'Dim ITmtx As New Matrix(New Vector(vIndepTerm)) ' 1 row x n columns
                Dim ITmtx As New ExprMatrix(1, n)
                ITmtx.Row(0) = vIndepTerm.row(0)
                'ITmtx = Matrix.opTranspose(ITmtx) ' n rows x 1 column
                ITmtx = ExprMatrix.opTranspose(ITmtx)
                Dim vColumn As ExprMatrix = invMtx * ITmtx
                'Dim vRow As Matrix = Matrix.opTranspose(vColumn)
                Dim vRow As ExprMatrix = ExprMatrix.opTranspose(vColumn)

                'eMtx.getExpr(row, 0) = New Expression(vColumn.vVect(n).vPoly(0))
                eMtx.Item(row, 0) = New Expression(vColumn.Item(n, 0))
                For i = 0 To n - 1
                    'eMtx.getExpr(row, 0) += New Expression(
                    '    vColumn.vVect(i).vPoly(0) *
                    '    Polynomial.GetPolynomial(vVar(i)))
                    eMtx.Item(row, 0) += vColumn.Item(i, 0) * New Expression(vVar(i), 1)
                Next
            Next
        Catch ex As Exception
            Throw
        End Try
        Return eMtx
    End Function
    Public Function getVandermondeMatrix() As ExprMatrix
        Dim VM As New ExprMatrix(nIntervals, nIntervals)
        Try
            ' ReDim VM.vVect(nIntervals)
            For row = 0 To nIntervals - 1
                'VM.vVect(row) = New Vector
                'ReDim Preserve VM.vVect(row).vPoly(nIntervals)
                For col = 0 To nIntervals
                    'VM.vVect(row).vPoly(col) = Me.An.vVect(row).vPoly(0)
                    VM.Item(row, col) = Me.An.Item(row, 0)
                Next
            Next
        Catch ex As Exception
            Throw
        End Try
        Return VM
    End Function
    Public Overloads Function ToString() As String
        If retPoly Is Nothing Then
            lagrangianinterpolation(False)
        End If
        Dim e1 As String = "Polynomial approximation:" + vbCrLf
        e1 += retPoly.ToString()
        Return e1
    End Function
End Class
