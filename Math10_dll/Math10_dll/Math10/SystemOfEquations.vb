Imports System.Text.RegularExpressions

Public Class SystemOfEquations
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
                Dim vIndepTerm(n) As Complex
                For i = 0 To n
                    Dim vCjo(n - 1) As Complex
                    For iPoint = 0 To n
                        ' 1.a) set the points:
                        '   Dim oVar As New VarsAndFns(eG.cfg)
                        Dim var As New Dictionary(Of String, Expression)
                        For j = 0 To n
                            If j = n Then
                                'mtx.vVect(i).vPoly(j).cf(0) = New Complex(1.0)
                                mtx.Item(i, j) = New Expression(1.0)
                            ElseIf j < n - i Then
                                'oVar.AddVar(vVar(j), New Expression(ini))
                                'mtx.vVect(i).vPoly(j).cf(0) = New Complex(ini)
                                mtx.Item(i, j) = New Expression(ini)
                                var.Add(vVar(j), New Expression(ini))
                            Else
                                'oVar.AddVar(vVar(j), New Expression(fin))
                                'mtx.vVect(i).vPoly(j).cf(0) = New Complex(fin)
                                mtx.Item(i, j) = New Expression(fin)
                                var.Add(vVar(j), New Expression(fin))
                            End If
                        Next
                        ' 1.b) evaluate:
                        'vIndepTerm(i) = eG.getExpr(row, 0).evalExpression(Nothing, oVar)
                        Dim pE As New ParseExpression
                        Dim b As Boolean = G10.mathml
                        G10.mathml = False
                        Dim r As ParseExpression.Result = pE.Evaluate(var, eG.Item(row, 0).ToString)
                        vIndepTerm(i) = r.vExpr(0).ToComplex
                        G10.mathml = b
                    Next
                Next
                ' 2) Resolve the linear system:
                Dim invMtx As ExprMatrix = mtx ^ New ExprMatrix(-1.0)
                'Dim ITmtx As New ExprMatrix(New Vector(vIndepTerm)) ' 1 row x n columns
                Dim ITmtx As New ExprMatrix(vIndepTerm) ' 1 row x n columns
                'ITmtx = Matrix.opTranspose(ITmtx) ' n rows x 1 column
                ITmtx = ExprMatrix.opTranspose(ITmtx) ' n rows x 1 column
                Dim vColumn As ExprMatrix = invMtx * ITmtx
                Dim vRow As ExprMatrix = ExprMatrix.opTranspose(vColumn)

                'eMtx.item(row, 0) = New Expression(vColumn.vVect(n).vPoly(0))
                eMtx.Item(row, 0) = New Expression(vColumn.Item(n, 0))
                For i = 0 To n - 1
                    eMtx.Item(row, 0) +=
                        vColumn.Item(i, 0) *
                        New Expression(vVar(i), 1)
                Next
            Next
        Catch ex As Exception
            Throw
        End Try
        Return eMtx
    End Function
    Public Shared Function Newton_Raphson(ByRef eG As ExprMatrix,
                                  Optional ByVal initialPts() As Complex = Nothing) As Boolean
        ' eG contents are the non-linear equations like the following:
        ' eG:
        '   row 0, column 0: x^2+3y-3 (=0)
        '   row 1, column 0: x-y+2    (=0)
        Dim bRet As Boolean = False
        Dim Mresult As ExprMatrix
        Dim bMathML As Boolean = G10.mathml
        G10.mathml = False
        Dim vars() As String = eG.GetVars
        Dim ptN(vars.Length - 1) As Complex
        Try
            If vars.Length <> eG.Rows Then
                Throw New Exception(Msg8.num(13))
            End If
            If initialPts Is Nothing Then
                Dim eMLnAprox As ExprMatrix = Nothing
                Try
                    eMLnAprox = aproxNonToLinearSOEinterpolating(eG, 0, 1)
                    Dim solution As ExprMatrix = ExprMatrix.resolveLinearSystemOfEquations(eMLnAprox, vars)
                    ReDim initialPts(solution.Rows - 1)
                    For k2 As Int32 = 0 To solution.Rows - 1
                        vars(k2) = solution.Item(k2, 0).GetVars(0).Replace("=", "")
                        If Not solution.Item(k2, 1).IsComplex Then
                            initialPts(k2) = eMLnAprox.Item(k2, 0).ToComplex
                        Else
                            initialPts(k2) = solution.Item(k2, 1).ToComplex
                        End If
                    Next
                Catch ex As Exception
                    ReDim initialPts(eG.Rows - 1)
                    For i = 0 To initialPts.Length - 1
                        initialPts(i) = Complex.zero
                    Next
                End Try
            End If
            ptN = initialPts
            Dim nMaxIter As Int32 = 500
            Dim nIter As Int32 = 0
            Dim err As Double = Double.MaxValue
            Dim F As New ExprMatrix(eG)
            Dim sF As String = F.ToString.Replace(" ", "")
            Dim J As New ExprMatrix(eG.Rows, eG.Rows)
            For i = 0 To eG.Rows - 1
                For c As Int32 = 0 To eG.Rows - 1
                    J.Item(i, c) = New Expression(eG.Item(i, 0))
                Next
            Next
            Dim Jacobian As ExprMatrix = J.opJacobian(vars)
            Do While nIter < nMaxIter
                nIter += 1
                ' Queremos calcular:
                ' (xn+1) = (xn) - ((J)xn)^-1 * (F)xn
                ' donde
                ' xn+1 es la incognita
                ' xn es el vector pts()
                ' ((J)xn)^-1 es el Jacobiano en el punto xn y tomamos su matriz inversa
                ' 

                'Dim Jacobian_in_point_xn As ExprMatrix

                Dim oVars As New Dictionary(Of String, Expression)
                For i = 0 To ptN.Length - 1
                    oVars.Add(vars(i), New Expression(ptN(i)))
                Next
                Dim pOM As New ParseOneMatrix
                Dim s As String = Jacobian.ToString().Replace(" ", "")
                Dim r As ParseOneMatrix.Result = pOM.Evaluate(oVars, s)(0)(0)
                'Jacobian_in_point_xn = r.eMtx
                Dim invJ_in_xn = r.eMtx ^ New ExprMatrix(-1.0)

                Dim F_in_point_xn As ExprMatrix
                r = pOM.Evaluate(oVars, sF)(0)(0)
                F_in_point_xn = r.eMtx

                Dim invJ_in_xn_Times_F As ExprMatrix = invJ_in_xn * F_in_point_xn
                err = 0
                For i = 0 To ptN.Length - 1
                    Dim cjo As Complex = invJ_in_xn_Times_F.Item(i, 0).ToComplex
                    err += cjo.opModulo
                    ptN(i) -= cjo
                Next
                If err = 0 Then Exit Do
            Loop
            bRet = True


        Catch ex As Exception
            'Throw New Exception("System of equations resolution: n/a.")
            'Throw New Exception(msg8.num(12))
            bRet = False
        Finally
            If bRet Then
                Mresult = New ExprMatrix(ptN.Length, 2)
                For i = 0 To ptN.Length - 1
                    Mresult.Item(i, 0) = New Expression(vars(i) + "=", 1)
                    Mresult.Item(i, 1) = New Expression(ptN(i))
                Next
                eG = Mresult
            End If
            G10.mathml = bMathML
        End Try
        Return bRet
    End Function

End Class
