Imports System.Text


Public Class SystemOfEquations
    Public Sub New()
        MyBase.New()
    End Sub

    Dim mtx As Matrix
    Dim Jxn, inv_Jxn, Mvar, F As Matrix
    Public Mresult As Matrix
    Dim var(-1) As String
    Dim vMtx() As Matrix
    Dim tipo1 As eqSysType
    Dim varEqExp(var.Length - 1)() As String
    Friend eG As ExprMatrix
    Dim poly1 As Polynomial
    Dim vars As VarsAndFns
    Dim sMtx As String
    Dim poly1varSolution As Polynomial = Nothing
    Dim sFactorsSolution As String
    Public sSolutionVars(-1)() As String
    Public resultVars(-1) As String
    Public resultValues(-1) As Complex
    Public sErrMsg As String = ""
    Dim sPreInfo As String = ""
    Dim cur As currentMatch = Nothing
    Dim cfg As Config
    Friend unit As Expression = Nothing

    Public Sub New(ByVal Mtx As Matrix,
                   ByVal eG As ExprMatrix,
                   ByVal vars As VarsAndFns,
                   ByVal cur As currentMatch,
                   ByVal cfg As Config)
        Dim err As Exception = Nothing
        Try

            Dim i As Int64
            If eG IsNot Nothing AndAlso eG.Cols = 1 AndAlso
               eG.Rows = 1 Then
                Dim expr As Expression = eG.getExpr(0, 0)
                If expr.getMatchStr = "sqr" Then
                    eG.getExpr(0, 0) = expr.getArgs(0)
                End If
            End If
            Me.eG = eG
            Me.vars = New VarsAndFns(cfg)
            Dim allVars() As String = eG.getAllVars
            For i = 0 To allVars.Length - 1
                Me.vars.AddVar(allVars(i), Nothing)
            Next
            Me.var = allVars
            Me.cur = cur
            Me.cfg = cfg
            If Mtx Is Nothing Then
                Try
                    Mtx = Me.eG.getMatrix
                Catch ex As Exception
                    Mtx = Nothing
                End Try
            End If
            If Mtx IsNot Nothing Then
                If Mtx.vVect.Length = 1 AndAlso Mtx.vVect(0).vPoly.Length = 1 Then
                    Dim Pa As Polynomial = Mtx.vVect(0).vPoly(0)
                    If Pa.PolyResto IsNot Nothing Then
                        Dim resto As Polynomial = New Polynomial(Pa.PolyResto)
                        Dim div As Polynomial = New Polynomial(Pa.PolyDivisor)
                        Pa.PolyResto = Nothing
                        Pa.PolyDivisor = Nothing
                        Mtx.vVect(0).vPoly(0) = New Polynomial(Pa * div + resto)
                    End If
                End If
                Me.mtx = New Matrix(Mtx)
                Me.var = Me.mtx.getAllVars
                'Array.Sort(Me.var)
                Jxn = New Matrix()
                Jxn.setVars(vars)
                F = New Matrix()
                Me.vMtx = vMtx
            Else
                Dim bIsPoly As Boolean = True
                For i = 0 To eG.Rows - 1
                    'Dim rup As New ReduceExprUsingPolynomials
                    'eG.getExpr(i, 0) = rup.ReduceUsingPolynomials(eG.getExpr(i, 0))
                    If Not eG.getExpr(i, 0).IsEquation Then
                        ' expected an equation
                        err = New Exception(msg8.num(20) + " (" +
                        eG.getExpr(i, 0).ToStringExpr(Config.cfg) + ")")
                        Throw err
                    ElseIf Not eG.getExpr(i, 0).IsPolynomial Then
                        bIsPoly = False
                    End If
                Next
                If bIsPoly Then
                    Me.mtx = New Matrix(0.0)
                    ReDim Me.mtx.vVect(eG.Rows - 1)
                    For i = 0 To eG.Rows - 1
                        Me.mtx.vVect(i) = New Vector(eG.getExpr(i, 0).getPolynomial)
                    Next
                End If
                var = eG.getAllVars
                sysType()
                Jxn = New Matrix
                Jxn.setVars(vars)
            End If
        Catch ex As Exception
            If err IsNot Nothing Then
                Throw err
            End If
            tipo1 = eqSysType.nonLinear
        End Try
    End Sub
    Public Property tipo() As eqSysType
        Get
            Return tipo1
        End Get
        Set(ByVal value As eqSysType)
            tipo1 = value
        End Set
    End Property
    Public ReadOnly Property getPolynomial() As Polynomial
        Get
            Return poly1
        End Get
    End Property
    Public Function sysType() As eqSysType
        Try
            tipo1 = 0
            Dim cff As New Complex(0.0)
            If mtx Is Nothing OrElse mtx.vVect Is Nothing Then
                Dim Expr1 As Expression = Nothing
                Dim CommExpr As Expression = Nothing
                Dim commStr As String = ""
                Dim expr2 As New Expression(eG.getExpr(0, 0))
                If expr2.getMatchStr = "sqr" Then
                    expr2 = expr2.getArgs(0)
                    eG.getExpr(0, 0) = expr2
                End If
                If expr2.getCommonFactor(Me.cfg, CommExpr, Expr1, commStr) Then
                    'expr2 /= CommExpr
                End If
                Dim vvar(-1) As String
                expr2.getAllVars(vvar)
                Dim vSum() As Expression = expr2.splitIntoTerms
                For iv As Int64 = 0 To vvar.Length - 1
                    Dim sVar As String = vvar(iv)
                    Dim vResult(-1) As Expression
                    If expr2.tryToIsolateToExpression(sVar, vResult) Then
                        If vResult.Length = 1 Then
                            tipo1 = eqSysType.PolyDegreeOneCoefAsVars
                            eG = New ExprMatrix(cfg, 1, 2)
                            eG.getExpr(0, 0) = New Expression(
                                Polynomial.GetPolynomial(sVar))
                            eG.getExpr(0, 1) = vResult(0)
                            Exit Try
                        ElseIf vResult.Length = 2 Then
                            tipo1 = eqSysType.PolyDegreeTwoCoefAsVars
                            eG = New ExprMatrix(cfg, 2, 2)
                            eG.getExpr(0, 0) = New Expression(
                                Polynomial.GetPolynomial(sVar))
                            eG.getExpr(0, 1) = vResult(0)
                            eG.getExpr(1, 0) = New Expression(
                                Polynomial.GetPolynomial(sVar))
                            eG.getExpr(1, 1) = vResult(1)
                            Exit Try
                        End If
                    End If
                Next

                If isLinearIndeterminate() Then
                    'Return eqSysType.linearIndeterminate
                End If
                Return eqSysType.nonLinear
            End If
            Dim i As Int64
            If mtx.vVect.Length = 1 AndAlso
            mtx.vVect(0).vPoly.Length = 1 _
            AndAlso mtx.vVect(0).IsPolynomial Then
                Dim Expr1 As Expression = Nothing
                Dim CommExpr As Expression = Nothing
                Dim commStr As String = ""
                Dim expr2 As New Expression(mtx.vVect(0).vPoly(0))
                If expr2.getCommonFactor(Me.cfg, CommExpr, Expr1, commStr) Then
                    expr2 /= CommExpr
                    mtx.vVect(0).vPoly(0) = New Polynomial(expr2.getPolynomial)
                End If
                Dim Pa As Polynomial = mtx.vVect(0).vPoly(0)
                If Pa.varAll.Length > 1 Then
                    Dim vvar() As String = cfg.cur.vars.getNamesList
                    For i = 0 To var.Length - 1
                        Dim j As Int64 = Array.IndexOf(vvar, var(i))
                        If j = -1 Then
                            ReDim Preserve vvar(vvar.Length)
                            vvar(vvar.Length - 1) = var(i)
                        End If
                    Next
                    Dim vMaxDeg(vvar.Length - 1) As Int64
                    For i = 0 To vMaxDeg.Length - 1
                        vMaxDeg(i) = Pa.getMaxExponentOfVar(vvar(i))
                    Next
                    For i = 0 To vMaxDeg.Length - 1
                        If vMaxDeg(i) AndAlso vMaxDeg(i) <= 3 Then
                            Exit For
                        End If
                    Next
                    Dim sVar As String = ""
                    If i < vvar.Length Then
                        sVar = vvar(i)
                    End If
                    Dim pamaxDeg As Int64 = Pa.getMaxExponentOfVar(sVar)
                    If pamaxDeg = 1 Then
                        Dim expr As Expression = Nothing
                        If Pa.tryToIsolateToPolynomial(sVar, expr) Then
                            If expr.IsReal AndAlso expr.toDouble = 0 Then
                                Throw New Exception(msg8.num(13))
                            End If
                            tipo1 = eqSysType.PolyDegreeOneCoefAsVars
                            eG = New ExprMatrix(cfg, 1, 2)
                            eG.getExpr(0, 0) = New Expression(
                                Polynomial.GetPolynomial(sVar))
                            eG.getExpr(0, 1) = expr
                            Exit Try
                        End If
                    ElseIf pamaxDeg = 2 OrElse pamaxDeg = 3 Then
                        'Dim vVar() As String = Pa.varAll
                        Dim sDeg2 As String = ""
                        Dim sDeg3 As String = ""
                        Dim vPa() As Polynomial = Pa.splitIntoTerms
                        Dim i3 As Int64 = -1
                        Dim i2 As Int64 = -1
                        Dim i1 As Int64 = -1
                        Dim i0 As Int64 = -1
                        Dim x As Polynomial = Polynomial.GetPolynomial(sVar)
                        sDeg3 = sVar
                        sDeg2 = sDeg3
                        'For i = 0 To vPa.Length - 1
                        '    If pamaxDeg = 3 AndAlso vPa(i).getMaxExponent(sDeg3) = 3 Then
                        '        'vPa(i).getMaxExponent(sDeg3)
                        '        x = Polynomial.GetPolynomial(sDeg3)
                        '        Exit For
                        '    ElseIf pamaxDeg = 2 AndAlso vPa(i).getMaxExponent(sDeg2) = 2 Then
                        '        'vPa(i).getMaxExponent(sDeg2)
                        '        x = Polynomial.GetPolynomial(sDeg2)
                        '        Exit For
                        '    End If
                        'Next
                        Dim vPaB(pamaxDeg) As Polynomial
                        For i = 0 To vPaB.Length - 1
                            vPaB(i) = New Polynomial(0.0)
                        Next
                        For i = 0 To vPa.Length - 1
                            Dim exp As Int64 = vPa(i).getMaxExponentOfVar(sDeg2)
                            Dim exp2 As Int64 = exp
                            Do While exp2
                                vPa(i) /= x
                                exp2 -= 1
                            Loop
                            vPaB(pamaxDeg - exp) += vPa(i)
                        Next
                        vPa = vPaB
                        If pamaxDeg = 3 Then
                            i3 = 0
                            i2 = 1
                            i1 = 2
                            i0 = 3
                            ' ax3+bx2+cx+d=0 
                            Dim a As New Expression(vPa(i3))
                            Dim b As Expression = Nothing
                            If i2 = -1 Then
                                b = New Expression(0.0)
                            Else
                                b = New Expression(vPa(i2))
                            End If
                            Dim c As Expression = Nothing
                            If i1 = -1 Then
                                c = New Expression(0.0)
                            Else
                                c = New Expression(vPa(i1))
                            End If
                            Dim d As Expression = Nothing
                            If i0 = -1 Then
                                d = New Expression(0.0)
                            Else
                                d = New Expression(vPa(i0))
                            End If
                            Dim two As New Expression(2.0)
                            Dim three As New Expression(3.0)
                            Dim six As New Expression(6.0)
                            Dim half As New Expression(0.5)
                            Dim nine As New Expression(9.0)
                            Dim twSeven As New Expression(27.0)
                            Dim one3 As New Expression(1 / 3)
                            Dim img As New Expression(Complex.i)
                            '
                            ' Método de Cardano:
                            ' https://es.wikipedia.org/wiki/Ecuaci%C3%B3n_de_tercer_grado
                            Dim a1 As New Expression(b / a)
                            Dim a2 As New Expression(c / a)
                            Dim a3 As New Expression(d / a)
                            Dim Q As New Expression((three * a2 - a1 * a1) / nine)
                            Dim R As New Expression(nine * a1 * a2 - twSeven * a3 - two * a1 * a1 * a1)
                            R /= New Expression(54.0)
                            Dim S1 As Expression = (R + (Q * Q * Q + R * R) ^ half) ^ one3
                            Dim S2 As Expression = (R - (Q * Q * Q + R * R) ^ half) ^ one3


                            eG = New ExprMatrix(cfg, 3, 2)
                            eG.getExpr(0, 0) = New Expression(x)
                            eG.getExpr(1, 0) = New Expression(x)
                            eG.getExpr(2, 0) = New Expression(x)


                            ' x1 = S1 + S2 - a1/3
                            eG.getExpr(0, 1) = S1 + S2 - a1 / three
                            ' x2 = -(s1+s2)*0.5 - a1/3 + 0.5*sqr(3)*i*(S1 - S2)
                            eG.getExpr(1, 1) =
                                 -(S1 + S2) / two - a1 / three + (three ^ half) * img * (S1 - S2) / two
                            ' x3 = -(s1+s2)*0.5 - a1/3 - 0.5*sqr(3)*i*(S1 - S2)
                            eG.getExpr(2, 1) =
                                 -(S1 + S2) / two - a1 / three - (three ^ half) * img * (S1 - S2) / two


                            tipo1 = eqSysType.threeDegreePolyCoefAsVars
                            If pamaxDeg = 3 AndAlso b.IsReal AndAlso b.toDouble = 0 AndAlso
                            c.IsReal AndAlso c.toDouble = 0 Then
                                eG = New ExprMatrix(cfg, 1, 2)
                                eG.getExpr(0, 0) = New Expression(x)
                                eG.getExpr(0, 1) = New Expression(-d / a) ^ New Expression(1 / 3)
                            End If
                            Return tipo1
                        End If
                        i2 = 0
                        i1 = 1
                        i0 = 2

                        If vPa(i0).isReal AndAlso vPa(i0).ToDouble = 0.0 Then
                            ' A*x2+B*x = 0 => A*x+B=0 => x=-B/A
                            eG = New ExprMatrix(cfg, 1, 2)
                            eG.getExpr(0, 0) = New Expression(x)
                            eG.getExpr(0, 1) = New Expression(-vPa(i1) / vPa(i2))
                        ElseIf vPa(i1).isReal AndAlso vPa(i1).ToDouble = 0.0 Then
                            ' A*x2+C = 0 => x=+/-sqr(-C/A)
                            eG = New ExprMatrix(cfg, 2, 2)
                            eG.getExpr(0, 0) = New Expression(x)
                            Dim div As Polynomial = -vPa(i0) / vPa(i2)
                            eG.getExpr(0, 1) = -Expression.AddFnAndArg0(
                                           "sqr", New Expression(div))
                            eG.getExpr(1, 0) = New Expression(x)
                            eG.getExpr(1, 1) = Expression.AddFnAndArg0(
                                           "sqr", New Expression(div))
                        Else
                            eG = New ExprMatrix(cfg, 2, 2)
                            ' A*x2+B*x+C = 0 => x = -b/(2a) - sqr(b2-4*ac)/(2a)
                            '                   x = -b/(2a) + sqr(b2-4*ac)/(2a)
                            Dim sum1 As New Expression(-vPa(i1) / (2 * vPa(i2)))
                            Dim sum2 As Expression = Expression.AddFnAndArg0(
                                "sqr", New Expression(vPa(1) * vPa(1) - 4 * vPa(i2) * vPa(i0)))
                            sum2 /= New Expression(2 * vPa(i2))
                            eG.getExpr(0, 0) = New Expression(x)
                            eG.getExpr(0, 1) = sum1 - sum2
                            eG.getExpr(1, 0) = New Expression(x)
                            eG.getExpr(1, 1) = sum1 + sum2
                        End If
                        tipo1 = eqSysType.PolyDegreeTwoCoefAsVars
                        Return tipo1
                    End If
                End If
            End If

            If mtx.vVect.Length = 1 AndAlso Me.mtx.vVect(0).vPoly.Length = 1 _
                AndAlso mtx.vVect(0).vPoly(0).IsMultiVar = False Then
                Dim degree As Int64 = mtx.vVect(0).vPoly(0).getDegree
                If degree > MathGlobal8.maxPolyDegree Then
                    tipo = eqSysType.nonLinear
                    Exit Try
                End If
                If degree >= 1 Then
                    tipo1 = eqSysType.isAPolynomial
                    poly1 = mtx.vVect(0).vPoly(0)
                    Mresult = New Matrix(poly1)
                    resultVars = poly1.varAll
                    'Dim Va As New Vector(poly1)
                    var = poly1.varAll
                    Dim oRoots As retOpRoots = Polynomial.opRoots(poly1, False, Me.cfg)
                    ReDim resultValues(oRoots.cjo.Length - 1), resultVars(oRoots.cjo.Length - 1)
                    For i = 0 To oRoots.cjo.Length - 1
                        resultValues(i) = oRoots.cjo(i)
                        resultVars(i) = var(0)
                    Next
                    Exit Try
                ElseIf degree = 0 Then
                    poly1 = mtx.vVect(0).vPoly(0)
                    tipo1 = eqSysType.isAPolynomial
                    Mresult = New Matrix(poly1)
                    resultVars = poly1.varAll
                    Exit Try
                End If
            End If

            ' The system of linear equations will be represented by
            ' Ma() * var() = b()
            ' for ex. if user types: 2x=4| x-y=5
            ' this is:
            ' 2x +   0y = 4
            ' 1x +(-1)y = 5
            ' then Ma=(2,0|1-1), var()=(0,1) b=(4,5)  (sVar=("x","y")


            ' Obtain matrix Ma() and b():
            Dim IsLinear As Boolean = True
            Jxn = New Matrix()
            Jxn.setVars(vars)
            Jxn.setRowAndCol(mtx.vVect.Length - 1, mtx.vVect.Length - 1)
            F = New Matrix()
            ReDim F.vVect(mtx.vVect.Length - 1)
            Dim row, col As Int64
            For row = 0 To mtx.vVect.Length - 1
                'ReDim Preserve Ma.vVect(row).vPoly(mtx.vVect.Length - 1)
                For col = 0 To mtx.vVect(row).vPoly(0).cf.Length - 1
                    Dim exp As Int64 = 0
                    Dim iColExp As Int64 = -1
                    Dim cf As Complex = Nothing
                    If col < mtx.vVect(row).vPoly(0).exp.Length Then
                        For i = 0 To mtx.vVect(row).vPoly(0).exp(col).Length - 1
                            exp += mtx.vVect(row).vPoly(0).exp(col)(i)
                            If mtx.vVect(row).vPoly(0).exp(col)(i) Then
                                Dim sV() As String = mtx.vVect(row).vPoly(0).var
                                If Me.var.Length Then
                                    iColExp = Array.IndexOf(Me.var, sV(i))
                                ElseIf Me.vars.getNamesList.Length Then
                                    iColExp = Array.IndexOf(Me.vars.getNamesList, sV(i))
                                End If
                                cf = mtx.vVect(row).vPoly(0).cf(col)
                            End If
                        Next
                    End If
                    If exp = 0 Then
                        ' se trata de una constante, luego
                        ' se incorpora en la matriz B la constante:
                        F.vVect(row) = New Vector(-mtx.vVect(row).vPoly(0).cf(col))
                        ' y suprimir la misma de matriz Ma:
                        'j = Ma.vVect(row).vPoly(0).cf.Length
                        'For i = col To j - 2
                        'Ma.vVect(row).vPoly(0).cf(0) = Ma.vVect(row).vPoly(col).cf(0)
                        'Next
                        'ReDim Preserve Ma.vVect(row).vPoly(0).cf(j - 2)
                        ' (si todo va bien, a la salida, Ma debería ser una matriz cuadrada)
                    ElseIf exp = 1 Then
                        '  ----------   no hacer nada
                        '  situar el coef. en la columna que
                        ' le correponde (iColExp):
                        'Ma.vVect(row).vPoly(col) = New Polynomial(0.0)
                        If iColExp >= Jxn.vVect(row).vPoly.Length Then
                            For k As Int64 = 0 To Jxn.vVect(row).vPoly.Length - 1
                                If iColExp >= Jxn.vVect(row).vPoly.Length Then
                                    ReDim Preserve Jxn.vVect(row).vPoly(iColExp)
                                End If
                            Next
                        End If
                        Jxn.vVect(row).vPoly(iColExp) = New Polynomial(cf)
                    Else
                        ' el sistema no es lineal:
                        IsLinear = False
                        tipo1 = eqSysType.nonLinear
                        Exit Try
                    End If

                Next
                If F.vVect(row) Is Nothing Then
                    F.vVect(row) = New Vector(0.0)
                End If
            Next
            eG = New ExprMatrix(Jxn)
            For i = 0 To eG.Rows - 1
                For j = 0 To eG.Cols - 1
                    eG.getExpr(i, j) = eG.getExpr(i, j) *
                       New Expression(Polynomial.GetPolynomial(Me.var(j)))
                Next
            Next
            tipo1 = eqSysType.linear
        Catch ex As Exception
            Try
                tipo1 = eqSysType.nonLinear
                'If isLinearIndeterminate() Then
                '    tipo1 = eqSysType.linearIndeterminate
                'Else
                '    tipo1 = eqSysType.nonLinear
                'End If
            Catch ex2 As Exception
                tipo1 = eqSysType.nonLinear
            End Try
        End Try
        Return tipo1
    End Function
    Function isLinearIndeterminate() As Boolean
        Try
            Dim row, col As Int64
            For row = 0 To eG.Rows - 1
                For col = 0 To eG.Cols - 1
                    If Not eG.getExpr(row, col).IsPolynomial Then
                        Return False
                    End If
                    Dim degree As Int64 = mtx.vVect(row).vPoly(col).getDegree
                    If degree > 1 Then Return False
                Next
            Next
            'resolveLinearIndeterminateByCramersRule()
        Catch ex As Exception
            Throw ex
        End Try
        Return True
    End Function
    Function resolveLinearIndeterminateByCramersRule() As ExprMatrix
        Dim ret As ExprMatrix = Nothing
        Try
            Dim A As New Matrix
            Dim B As New Matrix
            Dim vVar() As String = eG.getAllVars
            Dim n As Int64 = vVar.Length
            A.setRowAndCol(n, n)
            B.setRowAndCol(n, 1)
            Dim i, row, col As Int64
            For i = 0 To n - 1
                B.vVect(i).vPoly(0) = New Polynomial(0.0)
            Next
            For row = 0 To n - 1
                For col = 0 To n - 1
                    A.vVect(i).vPoly(col) = New Polynomial(0.0)
                Next
            Next
            For row = 0 To eG.Rows - 1
                For col = 0 To eG.Cols - 1
                    Dim P As Polynomial = New Polynomial(mtx.vVect(row).vPoly(col))
                    Dim vP() As Polynomial = P.splitIntoTerms
                    For i = 0 To vP.Length - 1
                        Dim deg As Int64 = vP(i).getDegree
                        If deg = 0 Then
                            B.vVect(row).vPoly(0) -= vP(i)
                        Else ' deg = 1
                            Dim sVarP() As String = vP(i).varAll
                            Dim pos As Int64 = Array.IndexOf(vVar, sVarP)
                            ' set into A only the coefficient:
                            A.vVect(row).vPoly(col) += New Polynomial(vP(i).cf(0))
                        End If
                    Next
                Next
            Next
            ret = New ExprMatrix(cfg, n, 2)

            Dim detA As Polynomial = Matrix.opDeterminant(A)
            For i = 0 To n - 1
                Dim Acol As New Matrix
                Acol.setRowAndCol(n, n)
                For row = 0 To n - 1
                    For col = 0 To n - 1
                        If i = col Then
                            Acol.vVect(row).vPoly(col) =
                                New Polynomial(B.vVect(row).vPoly(0))
                        Else
                            Acol.vVect(row).vPoly(col) =
                                New Polynomial(A.vVect(row).vPoly(col))
                        End If
                    Next
                Next
                ' Solution to variable vVar(i)
                ret.getExpr(i, 0) = New Expression(Polynomial.GetPolynomial(vVar(i)))
                ' is applying Cramer's rule:
                ret.getExpr(i, 1) = New Expression(Matrix.opDeterminant(Acol) / detA)
            Next
            eG = ret
        Catch ex As Exception
            Throw ex
        End Try
        Return ret
    End Function

    '    Public Function roots_fn_1var_Newton_RaphsonOLD( _
    '                            ByVal cfg As Config, _
    '                            ByVal sFn As String, _
    '                            ByRef roots() As Complex) As Boolean
    '        Dim polySolution As New Polynomial(1.0)
    '        Try
    '            ReDim roots(-1)
    '            Dim oVars As VarsAndFns = Nothing
    '            Dim epFn As New exprParser(cfg)
    '            epFn.parse(sFn, "", oVars)
    '            Dim exprOrig As New Expression(epFn.ret.exprMtx.getExpr(0, 0))
    '            If oVars.getNamesList.Length <> 1 Then
    '                Return False
    '            End If
    '            Dim sVar As String = oVars.getNamesList(0)
    '            Dim exprFn As Expression = epFn.ret.exprMtx.getExpr(0, 0)
    '            Dim exprFnOrig As New Expression(exprFn)
    '            ' obtener la derivada respecto a la 1a. variable (IDvar=0);
    '            ' param. 'True' to rewind current match sequence:
    '            Dim exprDer As Expression = exprFn.opDeriv(sVar)
    '            Dim initialValues() As Complex = { _
    '                New Complex(0.9, -0.5), Complex.one, Complex.minusOne, _
    '                Complex.minusi, Complex.i}
    '            Dim rnd As New Random(1)
    '            Dim ts As New TimeSpan(Now.Ticks)
    '            Dim bFound As Boolean = False
    '            Do
    '                bFound = False
    '                Dim iVal(10) As Complex
    '                Array.Copy(initialValues, 0, iVal, 0, initialValues.Length)
    '                For i As Int64 = initialValues.Length To iVal.Length - 1
    '                    If rnd.Next(0, 2) = 0 Then
    '                        iVal(i) = New Complex( _
    '                            (-1) ^ rnd.Next(0, 2) * _
    '                            rnd.NextDouble * _
    '                            10 ^ rnd.Next(-2, 2))
    '                    Else
    '                        iVal(i) = New Complex( _
    '                            (-1) ^ rnd.Next(0, 2) * _
    '                            rnd.NextDouble * _
    '                            10 ^ rnd.Next(-2, 2), _
    '                            (-1) ^ rnd.Next(0, 2) * _
    '                            rnd.NextDouble * _
    '                            10 ^ rnd.Next(-2, 2))
    '                    End If
    '                Next
    '                For I = 0 To iVal.Length - 1
    '                    Dim x0 As Complex = iVal(I) ' As New Complex(0.9, -0.5)
    '                    Dim dif As Complex = Nothing
    '                    Dim nVueltas As Int64 = 0
    '                    Dim maxVueltas As Int64 = 40
    '                    Dim bCumple As Boolean = False
    '                    Do
    '                        Dim x As Complex = x0 - _
    '                         exprFn.evalExpression(x0) / _
    '                            exprDer.evalExpression(x0)
    '                        dif = x - x0
    '                        x0 = x
    '                        nVueltas += 1
    '                        If dif.opNorm < 10 ^ -12 AndAlso _
    '                            x0.opNorm > 10 ^ -12 Then
    '                            bCumple = True
    '                            Exit Do
    '                        End If
    '                        Dim ts2 As New TimeSpan(Now.Ticks - ts.Ticks)
    '                        If ts2.TotalSeconds > 12 Then
    '                            Exit Try
    '                        End If
    '                    Loop While dif.opNorm < 10 ^ 7 AndAlso _
    '                    nVueltas < maxVueltas
    '                    Dim fnx As Complex
    '                    Try
    '                        fnx = exprFnOrig.evalExpression(x0)
    '                    Catch ex As Exception
    '                        GoTo sig
    '                    End Try
    '                    Dim x1 As Complex = Nothing
    '                    If (bCumple AndAlso _
    '                    fnx.opNorm < 10 ^ -6) OrElse _
    '                    (dif.opNorm < 10 ^ -8 AndAlso _
    '                            x0.opNorm > 10 ^ -8) Then
    '                        bFound = True
    '                        ReDim Preserve roots(roots.Length)
    '                        roots(roots.Length - 1) = x0
    '                        If Math.Abs(x0.pIm.ToDouble) > 0.0001 Then
    '                            x1 = Complex.opConjugate(x0)
    '                            fnx = _
    '                                exprFnOrig.evalExpression(x1)
    '                            If fnx.opNorm < 10 ^ -6 Then
    '                                ReDim Preserve roots(roots.Length)
    '                                roots(roots.Length - 1) = x1
    '                            End If
    '                        End If
    '                        Dim sX As Polynomial = Polynomial.GetPolyomial(sVar)
    '                        sX -= New Polynomial(x0)
    '                        Dim divByRootExpr As Expression = Nothing
    '                        If Len(Me.sFactorsSolution) Then
    '                            Me.sFactorsSolution += "*"
    '                        End If
    '                        If x1 Is Nothing Then
    '                            divByRootExpr = New Expression(sX)
    '                            polySolution *= sX
    '                            Me.sFactorsSolution += "(" + sX.toStringPoly(cfg) + ")"
    '                        Else
    '                            Dim sX1 = Polynomial.GetPolyomial(sVar)
    '                            sX1 -= New Polynomial(x1)
    '                            Dim prod As Polynomial = sX * sX1
    '                            divByRootExpr = New Expression(prod)
    '                            polySolution *= sX * sX1
    '                            Me.sFactorsSolution += "(" + prod.toStringPoly(cfg) + ")"
    '                        End If
    '                        exprFn = exprOrig / New Expression(polySolution)
    '                        oVars.setValue(0, Nothing)
    '                        exprDer = exprFn.opDeriv(sVar)
    '                        'xit For
    '                    Else

    '                    End If
    'sig:
    '                Next
    '            Loop While roots.Length < 10 AndAlso bFound = True
    '        Catch ex As Exception
    '            Throw ex
    '        End Try
    '        If roots.Length = 0 Then
    '            Return False
    '        End If
    '        Me.poly1varSolution = polySolution
    '        Return True
    '    End Function

    Public Function NewtonRaphson(ByVal cfg As Config,
                                  Optional ByVal initialPts() As Complex = Nothing) As Boolean
        Dim bRet As Boolean = False
        Try

            'MathGlobal8.SaveAll()

            ' ========================================================
            ' Método de Newton-Raphson:
            '   Xn+1 = Xn - [Jacobiano(Xn)]^(-1) * Xn

            'Dado que calcular explícitamente 
            'la matriz inversa del Jacobiano no es un
            'proceso muy eficiente desde el punto de vista
            'numérico, a la hora de implementar
            'el método se hace en dos pasos:

            '1. se resuelve el sistema
            '   Jaccobian(Xn) * Δxn+1 = −F(Xn),

            '2. se calcula
            '   xn+1 = xn + Δxn+1
            ' ========================================================

            ' Obtengamos las matrices (F) y (J)
            ' eG.retEqsExprMtx contiene las ecuaciones
            Dim i, k As Int64
            Dim dt As Detall = Nothing ' cfg.oDetail
            'MathGlobal8.detail = Nothing
            Dim vvars() As String = eG.getAllVars
            If vvars.Length > cfg.cur.vars.getNamesList.Length Then
                For i = 0 To vvars.Length - 1
                    cfg.cur.vars.AddVar(vvars(i), Nothing)
                Next
            End If
            Dim varOrig() As String = cfg.cur.vars.getNamesList ' eG.getAllVars
            Dim vFixedValues(varOrig.Length - 1) As Complex
            Dim vFixedIndex(varOrig.Length - 1) As Int64
            For i = 0 To vFixedIndex.Length - 1
                vFixedIndex(i) = -1
            Next
            For i = 0 To eG.Rows - 1
                Dim expr As Expression = eG.getExpr(i, 0)
                Dim vResult(-1) As Expression
                For j = 0 To varOrig.Length - 1
                    If expr.tryToIsolateToExpression(varOrig(j), vResult) Then
                        If vResult.Length = 1 AndAlso vResult(0).IsComplex Then
                            vFixedValues(i) = vResult(0).toComplex
                            vFixedIndex(j) = i
                            Exit For
                        End If
                    End If
                Next
            Next
            Dim eGorig As New ExprMatrix(eG)
reStart:
            'var = eG.getAllVars ' eG.retEqsExprMtx.getVars.toStringArray
            var = cfg.cur.vars.getNamesList
            vars = New VarsAndFns(cfg)
            For i = 0 To var.Length - 1
                If vFixedIndex(i) = -1 Then
                    vars.AddVar(var(i), Nothing)
                Else
                    vars.AddVar(var(i), New Expression(vFixedValues(vFixedIndex(i))))
                End If
            Next
            var = vars.getNamesList

            Dim Fn As New ExprMatrix(cfg, eG.Rows, eG.Rows)
            Dim rnd As New Random(1)
            Dim FXn As ExprMatrix
            ' Tomaremos como punto inicial X = (0, 0, 0, ...,0)
            Dim ptN(var.Length - 1) As Complex
            Dim nConst As Int64 = 0
            Dim numVueltasSinError As Int64 = 0
            Dim maxSinErr As Int64 = 0
            For j = 0 To var.Length - 1
                Dim sCur As String = var(j)
                For i = 0 To eG.Rows - 1
                    Dim vR(-1) As Expression
                    If eG.getExpr(i, 0).tryToIsolateToExpression(sCur, vR) Then
                        If vR(0).IsComplex Then
                            Dim pos As Int64 = vars.getVarIDByName(sCur)
                            vars.setValue(pos, New ExprMatrix(vR(0)))
                            nConst += 1
                        End If
                    End If
                Next
            Next
            For i = 0 To ptN.Length - 1
                If initialPts IsNot Nothing Then
                    ptN(i) = New Complex(initialPts(i))
                Else
                    ptN(i) = New Complex(0.0)
                End If
                For k = 0 To ptN.Length - 1
                    Fn.getExpr(i, k) = New Expression(
                        eG.getExpr(i, 0))
                Next
            Next
            Mresult = New Matrix
            ReDim Mresult.vVect(eG.Rows - 1)

            Dim bInitialIsSolution As Boolean = False
            For i = 0 To ptN.Length - 1
                If Not eG.getExpr(i, 0).evalToCjo(var, ptN).IsZero Then
                    Exit For
                End If
            Next
            If i >= ptN.Length Then
                Mresult = New Matrix()
                ReDim Mresult.vVect(ptN.Length - 1)
                For i = 0 To ptN.Length - 1
                    Mresult.vVect(i) = New Vector(initialPts(i))
                Next
                bRet = True
                Exit Try
            End If
            Dim jacobianXn As New ExprMatrix(cfg, Fn.Rows, Fn.Cols)
            Dim maxVueltas As Int64 = 500
            Dim curVuelta As Int64 = 0
            Dim iCur As Int64 = 0
            If dt IsNot Nothing Then
                dt.AddAlways("<br>Newton Raphson method<br>")
                dt.AddAlways("Xn+1 = Xn - [Jacobiano(Xn)]^(-1) * " +
                "F(Xn)<br>Dado que calcular explícitamente <br>" +
                "la matriz inversa del Jacobiano no es un<br>proceso" +
                " muy eficiente desde el punto de vista<br>numérico," +
                " a la hora de implementar<br>el método se hace en dos pasos:" +
                "<br><br>1. se resuelve el sistema<br>J(Xn) * Δxn+1 = −F (Xn) ," +
                "<br>2. se calcula<br>xn+1 = xn +Δxn+1<br>")
            End If
            Dim ts As New TimeSpan(Now.Ticks)
            Dim bestPt(ptN.Length - 1) As Complex
            Dim FnJacobian As ExprMatrix = Fn.opJacobian(vars)

            Dim F As New ExprMatrix(Fn)
            Dim jacobian As New ExprMatrix(FnJacobian)
            Dim eGvars() As String = eG.getAllVars
            Do
                jacobianXn = New ExprMatrix(FnJacobian)
                If dt IsNot Nothing Then
                    dt.Add("X" + iCur.ToString + "=" + Complex.toStringV2(ptN))
                    dt.Add("Jacobian=" + MathGlobal8.getTable(jacobianXn.ToString, "green"))
                End If
                iCur += 1
                ' evaluamos el jacobiano en el punto ptN, esto es, Jacobian(Xn)
                ' y, al mismo tiempo, evaluamos F(Xn):
                FXn = New ExprMatrix(Fn)
                'Dim vVars(var.Length - 1) As String
                'Dim slenVars(var.Length - 1) As String
                'For i = 0 To ptN.Length - 1
                '    vVars(i) = var(i) + "=" + ptN(i).toString
                '    slenVars(i) = String.Format("{0:0000}{1}", 10000 - Len(var(i)), var(i))
                'Next
                'Array.Sort(slenVars, vVars)
                'Dim sVars As String = Join(vVars, vbCrLf)
                Dim bFoundError As Boolean = False
                Dim vars1 As New VarsAndFns(vars)
                For i = 0 To vars1.getNamesList.Length - 1
                    If vars1.getValueByID(i) Is Nothing Then
                        vars1.setValue(i, New ExprMatrix(ptN(i)))
                    End If
                Next
                For i = 0 To ptN.Length - 1
                    For k = 0 To ptN.Length - 1
                        jacobianXn.getExpr(i, k) = jacobian.getExpr(i, k).evalExprToExpr(vars1)
                        FXn.getExpr(i, k) = F.getExpr(i, k).evalExprToExpr(vars1)

                        'If vFixedValues(i) IsNot Nothing Then
                        '    FXn.getExpr(i, k) = New Expression(-vFixedValues(i))
                        'Else
                        '    Try
                        '        Dim cjo As Complex = Nothing
                        '        If jacobianXn.getExpr(i, k).IsPolynomial Then
                        '            cjo = jacobianXn.getExpr(i, k).getPolynomial.evalMulti(ptN)
                        '        Else
                        '            cjo = jacobianXn.getExpr(i, k).evalToCjo(var, ptN)
                        '        End If
                        '        jacobianXn.getExpr(i, k) = New Expression(cjo)
                        '        If FXn.getExpr(i, k).IsPolynomial Then
                        '            cjo =
                        '            FXn.getExpr(i, k).getPolynomial.evalMulti(ptN)
                        '        Else
                        '            cjo = FXn.getExpr(i, k).evalToCjo(var, ptN)
                        '        End If
                        '        FXn.getExpr(i, k) = New Expression(cjo)
                        '    Catch ex As Exception
                        '        bFoundError = True
                        '        GoTo cont
                        '    End Try
                        'End If
                    Next
                Next
                If Not jacobianXn.tryGetMatrix(Jxn) Then
                    GoTo cont
                    'exit do
                End If
                Dim F1 As New Matrix
                If Not FXn.tryGetMatrix(F1) Then
                    GoTo cont
                    'Exit Do
                End If
                Try
                    'Dim ts2 As New TimeSpan(Now.Ticks - ts.Ticks)
                    'If ts2.TotalMilliseconds > cfg.timeOutms / 3 Then
                    If cfg.isTimeoutFraction(0.33) Then
                        Exit Do
                    End If

                    'Dim det As Double = _
                    '    New ExprMatrix(Jxn).opDeterminant.toDouble
                    'If Math.Abs(det) > 10 ^ 50 Then
                    '    GoTo cont
                    'End If



                    ' resolvemos el sistema
                    inv_Jxn = Jxn ^ -1
                    Mresult = inv_Jxn * F1



                    If dt IsNot Nothing Then
                        dt.Add("(<br>" + Jxn.toStringMtxHTML(cfg) + "<br>)^-1<br>= " +
                               inv_Jxn.toStringMtxHTML(cfg))
                        dt.Add("<br>" + inv_Jxn.toStringMtxHTML(cfg) + "<br>*<br>" +
                               (-F1).toStringMtxHTML(cfg) + "<br>=" +
                               Mresult.toStringMtxHTML(cfg))
                    End If
                    numVueltasSinError += 1
                    If numVueltasSinError > maxSinErr Then
                        Array.Copy(ptN, bestPt, ptN.Length)
                        maxSinErr = numVueltasSinError
                    End If
                Catch ex2 As Exception
                    bFoundError = True
                    GoTo cont
                End Try

                Dim err As Double = 0.0

                ' Xn+1 = Xn - J^-1(Xn)*F(Xn)
                For j = 0 To ptN.Length - 1
                    i = Array.IndexOf(var, eGvars(j))
                    err += ptN(i).opModulo + Mresult.vVect(i).vPoly(0).cf(0).opModulo
                    If vFixedIndex(i) <> -1 Then
                        ptN(i) = Complex.zero
                    Else
                        ptN(i) -= Mresult.vVect(i).vPoly(0).cf(0)
                    End If
                Next
                If dt IsNot Nothing Then
                    dt.Add("err=" + err.ToString(MathGlobal8.us))
                End If
                'If err < 10 ^ -15 AndAlso curVuelta > 15 Then
                'Exit Do
                'End If

cont:
                If Double.IsNaN(err) OrElse bFoundError Then
                    bFoundError = False
                    numVueltasSinError = 0
                    For i = 0 To ptN.Length - 1
                        If vFixedIndex(i) <> -1 Then
                            ptN(i) = Complex.zero
                        Else
                            ptN(i) = New Complex(rnd.NextDouble * (-1) ^ rnd.Next(1, 3) * 10 ^ rnd.Next(0, 3),
                                             rnd.NextDouble * (-1) ^ rnd.Next(1, 3) * 10 ^ rnd.Next(0, 3))
                        End If
                    Next
                    If curVuelta >= maxVueltas / 2 Then
                        Exit Do
                    End If
                    'If dt IsNot Nothing Then
                    '    dt.Add("(<br>Couldn't obtain inverse matrix, trying again...<br>" + _
                    '           MaInv.toStringMtxHTML(cfg))
                    'End If
                    iCur = 0
                    Mresult = New Matrix
                    ReDim Mresult.vVect(-1)
                    'Exit Do
                End If
                Dim ts3 As New TimeSpan(Now.Ticks - ts.Ticks)
                If ts3.TotalMilliseconds > cfg.timeOutms / 3 Then
                    Exit Do
                End If
                curVuelta += 1
            Loop While curVuelta < maxVueltas
            If dt IsNot Nothing Then
                dt.Add("X" + iCur.ToString + "=" + Complex.toStringV2(ptN))
            End If
            'cfg.oDetail = dt
salida:
            If (numVueltasSinError < 20 OrElse numVueltasSinError + 10 < curVuelta) Then
                ReDim Mresult.vVect(-1)
                bRet = False
            Else
                For i = 0 To vars.getNamesList.Length - 1
                    If vars.getValueByID(i) IsNot Nothing Then
                        ptN(i) = vars.getValueByID(i).toComplex
                    End If
                Next
                For i = 0 To ptN.Length - 1
                    Mresult.vVect(i) = New Vector(ptN(i))
                Next
                bRet = True
            End If

            'MathGlobal8.RestoreAll()

        Catch ex As Exception
            'Throw New Exception("System of equations resolution: n/a.")
            'Throw New Exception(msg8.num(12))
            bRet = False
        End Try
        Return bRet
    End Function




    Public Function roots_fn_1var_Newton_Raphson(
                            ByVal exprOrig As Expression,
                            ByVal cfg As Config,
                            ByVal sFn As String,
                            ByRef roots() As Complex,
                            ByRef Pa As Polynomial,
                            Optional timeOutFraction As Double = 1 / 3) As Boolean
        Static nCall As Int64 = 0
        Dim polySolution As New Polynomial(1.0)
        Dim bSimplify As Boolean = False
        Pa = Nothing
        Dim t0 As New TimeSpan(Now.Ticks)
        'Try
        '    Dim vIsMn() As Boolean = Nothing
        '    Dim vSum() As Expression = exprOrig.exprToSummands(vIsMn)
        '    If vSum.Length = 1 Then
        '        Return False
        '    End If
        'Catch ex As Exception

        'End Try
        Dim i As Int64
doSimplify:
        Try

            If bSimplify Then
                Dim exprSmp As Expression = exprOrig.tryToSimplifyEquation
                If exprSmp Is Nothing Then
                    Return False
                End If
                exprOrig = exprSmp
                If exprOrig.IsPolynomial Then
                    Pa = exprOrig.getPolynomial
                    If Pa.isComplex Then
                        ' (Pa complex => (complex)=0 => n/a
                        Pa = Nothing
                    End If
                    Return False
                End If
            End If
            ReDim roots(-1)
            Dim oVars As New VarsAndFns(cfg)
            'Dim epFn As New exprParser
            'epFn.parse(sFn, "", oVars)
            'Dim exprOrig As New Expression(epFn.ret.exprMtx.getExpr(0, 0))
            Dim vVar() As String = Nothing
            exprOrig.getAllVars(vVar)
            If vVar.Length <> 1 Then
                Return False
            End If
            'If oVars.getNamesList.Length <> 1 Then
            '    Return False
            'End If

            Dim sVar As String = vVar(0) ' oVars.getNamesList(0)
            oVars.AddVar(sVar, Nothing, "")
            'Dim exprFn As Expression =  epFn.ret.exprMtx.getExpr(0, 0)
            Dim exprFn As New Expression(exprOrig)
            Dim exprFnOrig As New Expression(exprFn)
            ' obtener la derivada respecto a la 1a. variable (IDvar=0);
            ' param. 'True' to rewind current match sequence:
            Dim exprDer As Expression = exprFn.opDeriv(sVar)
            Dim exprDerOrig As New Expression(exprDer)
            'Dim initialValues() As Complex = { _
            '    Complex.one, Complex.minusOne, New Complex(0.9, -0.5), _
            '    Complex.minusi, Complex.i, Complex.zero, New Complex(0.5), New Complex(-0.5),
            '    New Complex(3), New Complex(10.0), New Complex(-3), New Complex(-10.0)}
            Dim initialValues() As Complex = {
                 New Complex(0.9, -0.5),
                Complex.minusi, Complex.i, Complex.zero, New Complex(0.5), New Complex(-0.5),
                New Complex(0.01), New Complex(-0.01), New Complex(0.00001), -New Complex(0.00001)
                }
            Dim rnd As New Random(1)
            Dim ts As New TimeSpan(Now.Ticks)
            Dim bFound As Boolean = False

            'Dim polySolutionRe As New Polynomial(1.0)
            Dim bIsPolynomial As Boolean = exprOrig.IsPolynomial
            Dim degree As Int64 = 0
            Dim paOrig As Polynomial = Nothing
            If bIsPolynomial Then
                degree = exprOrig.getPolynomial.getDegree
                paOrig = exprOrig.getPolynomial
            End If
            If bIsPolynomial AndAlso exprOrig.getPolynomial.getDegree < MathGlobal8.maxPolyDegree AndAlso
            Not exprOrig.getPolynomial.hasComplexCoeff Then
                Dim deg As Int64 = paOrig.getDegree
                'Dim Va As New Vector(paOrig)
                Dim rOpR As retOpRoots = Polynomial.opRoots(
                        paOrig, True, cfg) ' find ALL real roots and imaginary pure (0+b1*i,0-b1*i,...)
                Dim rRoots() As Complex = rOpR.cjo
                'Dim degree As Int64 = Vc.vPoly(i).opDegree(Nothing, Va.vPoly(i).varAll, New Int64() {0})
                'Dim degree As Int64 = paOrig.getDegree
                If rRoots.Length > 0 Then
                    Dim vDen As New Vector(rRoots)
                    Dim polyDen As Polynomial = vDen.RootsToPolynomial
                    polyDen.setVars(New String() {sVar})
                    paOrig /= polyDen
                    paOrig.PolyResto = Nothing
                    paOrig.PolyDivisor = Nothing
                    For i = 0 To rRoots.Length - 1
                        ReDim Preserve roots(roots.Length)
                        roots(roots.Length - 1) = rRoots(i)
                        Dim sX As Polynomial = Polynomial.GetPolynomial(sVar)
                        sX -= New Polynomial(rRoots(i))
                        If i + 1 < rRoots.Length AndAlso
                        Not rRoots(i).pIm.IsZero AndAlso
                        rRoots(i).pIm.ToDouble = -rRoots(i + 1).pIm.ToDouble Then
                            ReDim Preserve roots(roots.Length)
                            roots(roots.Length - 1) = rRoots(i + 1)
                            Dim sX1 = Polynomial.GetPolynomial(sVar)
                            sX1 -= New Polynomial(rRoots(i + 1))
                            Dim prod As Polynomial = sX * sX1
                            polySolution *= prod
                            Me.sFactorsSolution += "(" + prod.toStringPoly(cfg) + ")"
                            i += 1
                        Else
                            polySolution *= sX
                            Me.sFactorsSolution += "(" + sX.toStringPoly(cfg) + ")"
                        End If
                    Next
                    Pa = exprOrig.getPolynomial
                    If roots.Length = degree Then
                        Exit Try
                    End If
                    exprOrig = New Expression(paOrig)
                    exprFn = New Expression(paOrig)
                    exprDer = New Expression(paOrig.opDerivative(sVar))
                    Dim ts2 As New TimeSpan(Now.Ticks - ts.Ticks)
                    If ts2.TotalMilliseconds > cfg.timeOutms * timeOutFraction Then
                        Return True
                    End If
                End If
            ElseIf bIsPolynomial Then
                'roots = Polynomial.opRealRoots_Junque(
                'sVar, paOrig, New Polynomial(paOrig), degree)
                'exprFn = Expression.suprFractionsInEquation(exprFn)
            End If
            ' Deflate of eventual zeros:
            Dim rup As New ReduceExprUsingPolynomials
            Try
                Dim P_xN As Polynomial = Polynomial.GetPolynomial(sVar)
                Dim nZeros As Int64 = 0
                Dim curExpr As Expression = exprFn
                Dim cjo As Complex
                Do
                    If nZeros Then
                        P_xN = Polynomial.GetPolynomial(sVar) ^ New Polynomial(CDbl(nZeros))
                        curExpr = exprFn / New Expression(P_xN)
                        curExpr = rup.ReduceUsingPolynomials(curExpr)
                    End If
                    Try
                        cjo = curExpr.evalExpression(Complex.zero)
                    Catch ex As Exception
                        Exit Do
                    End Try
                    If Not cjo.IsZero Then
                        Exit Do
                    End If
                    nZeros += 1
                Loop
                If nZeros Then
                    nZeros = P_xN.getDegree
                    'exprOrig = curExpr
                    exprFn = New Expression(curExpr)
                    P_xN = Polynomial.GetPolynomial(sVar) ^ New Polynomial(CDbl(nZeros))
                    polySolution = P_xN
                    Me.sFactorsSolution += P_xN.toStringPoly(cfg)
                    Dim P_xN_expr As New Expression(P_xN)
                    exprFn /= P_xN_expr
                    exprFn = rup.ReduceUsingPolynomials(exprFn)
                    exprFnOrig /= P_xN_expr
                    exprFnOrig = rup.ReduceUsingPolynomials(exprFnOrig)
                    curExpr = exprFn
                    exprDer = curExpr.opDeriv(sVar)
                    ReDim roots(nZeros - 1)
                    For iZ As Int64 = 0 To nZeros - 1
                        roots(iZ) = Complex.zero
                    Next
                End If
            Catch ex As Exception

            End Try
            Dim bIsMn(-1) As Boolean
            Dim vSum() As Expression = exprFn.exprToSummands(bIsMn)
            For i = 0 To vSum.Length - 1
                If vSum(i).IsReal Then
                    ReDim Preserve initialValues(initialValues.Length + 4)
                    initialValues(initialValues.Length - 5) =
                       New Complex(-vSum(i).toDouble)
                    initialValues(initialValues.Length - 4) =
                       New Complex(-vSum(i).toDouble ^ 2)
                    initialValues(initialValues.Length - 3) =
                       New Complex(vSum(i).toDouble ^ 2)
                    initialValues(initialValues.Length - 2) =
                       New Complex(Math.Sqrt(Math.Abs(vSum(i).toDouble)))
                    initialValues(initialValues.Length - 1) =
                       New Complex(vSum(i).toDouble)
                    Exit For
                End If
            Next
            Dim iVal(40 + initialValues.Length - 1) As Complex
            Array.Copy(initialValues, 0, iVal, 0, initialValues.Length)
            bFound = False
            For i = initialValues.Length To iVal.Length - 1 - 40
                If rnd.Next(0, 2) = 0 Then
                    iVal(i) = New Complex(
                            (-1) ^ rnd.Next(0, 2) *
                            rnd.NextDouble *
                            10 ^ rnd.Next(-2, 2))
                Else
                    iVal(i) = New Complex(
                            (-1) ^ rnd.Next(0, 2) *
                            rnd.NextDouble *
                            10 ^ rnd.Next(-2, 2),
                            (-1) ^ rnd.Next(0, 2) *
                            rnd.NextDouble *
                            10 ^ rnd.Next(-2, 2))
                End If
            Next
            Dim k As Int64 = 1
            For i = iVal.Length - 1 - 39 To iVal.Length - 30
                iVal(i) = New Complex(k)
                k += 1
            Next
            k = -1
            For i = iVal.Length - 1 - 29 To iVal.Length - 20
                iVal(i) = New Complex(k)
                k -= 1
            Next
            k = 0
            For i = iVal.Length - 1 - 20 To iVal.Length - 1
                iVal(i) = New Complex(k * (-1) ^ rnd.Next(0, 2),
                                          k * (-1) ^ rnd.Next(0, 2))
                k += 1
            Next
            Dim iV3(iVal.Length - 1) As Complex
            For i = 0 To iVal.Length - 1
                If Not iVal(i).IsZero Then
                    iV3(i) = Complex.one / iVal(i)
                Else
                    iV3(i) = Complex.zero
                End If
            Next
            i = iVal.Length
            ReDim Preserve iVal(i * 2 - 1)
            For i = 0 To iV3.Length - 1
                iVal(iV3.Length + i) = iV3(i)
            Next
            i = iVal.Length
            ReDim Preserve iVal(i * 2 - 1)
            For i = 0 To iVal.Length / 2 - 1
                iVal(i + iVal.Length / 2) = -iVal(i)
            Next
            Dim nIter As Int64 = 0
            Dim bSumFnOrig(-1) As Boolean
            Dim vSummFnOrig() As Expression = exprFnOrig.exprToSummands(bSumFnOrig)
            Dim absIndepTerm As Double = 0.0
            For i = 0 To vSummFnOrig.Length - 1
                If vSummFnOrig(i).IsReal Then
                    absIndepTerm = Math.Abs(vSummFnOrig(i).toDouble)
                End If
            Next
            Do
                bFound = False
                For i = 0 To iVal.Length - 1
                    'Trace.WriteLine(i.ToString)
                    Dim x0 As Complex = iVal(i)
                    'Trace.WriteLine(x0.toString)
                    Dim dif As Complex = Nothing
                    Dim nVueltas As Int64 = 0
                    Dim maxVueltas As Int64 = 30
                    Dim bCumple As Boolean = False
                    Dim ts2 As New TimeSpan(Now.Ticks)
                    Dim norm As Double
                    Do
                        Dim x As Complex = Nothing
                        Try
                            Dim fnx0 As Complex = exprFn.evalExpression(x0, Nothing, True)
                            If fnx0 Is Nothing Then GoTo sig
                            If fnx0.IsZero Then
                                dif = Complex.zero
                            Else
                                Dim den As Complex = exprDer.evalExpression(x0)
                                If den.IsZero Then
                                    GoTo sig
                                End If
                                dif = exprFn.evalExpression(x0) / den
                                x = x0 - dif
                                x0 = x

                            End If
                        Catch ex As Exception
                            GoTo sig
                        End Try
                        nVueltas += 1
                        If (nVueltas > maxVueltas - 3 AndAlso dif.opNorm < 10 ^ -4 AndAlso
                                    x0.opNorm > 10 ^ -12) OrElse dif.IsZero OrElse dif.opNorm < absIndepTerm / 10 ^ 7 Then
                            bCumple = True
                            'Exit Do

                            If Math.Abs(x0.pIm.ToDouble) < 10 ^ -3 Then
                                Dim x02 As New Complex(x0.pRe, New Rational(0.0))
                                Dim j As Int64 = 0
                                For j = 0 To roots.Length - 1
                                    If Math.Abs(roots(j).pRe.ToDouble - x02.pRe.ToDouble) < 10 ^ -3 _
                                            AndAlso roots(j).pIm.IsZero Then
                                        Exit For
                                    End If
                                Next
                                If j >= roots.Length Then
                                    Try
                                        Dim x2 As Complex = exprFn.evalExpression(x0)
                                        If x2.opNorm < dif.opNorm Then
                                            x0 = x02
                                        End If
                                    Catch ex As Exception

                                    End Try
                                End If
                            End If
                            Exit Do
                        ElseIf nVueltas >= maxVueltas Then
                            GoTo sig
                        End If

                        'Dim ts2b As New TimeSpan(Now.Ticks - ts.Ticks)
                        'If ts2b.TotalMilliseconds > cfg.timeOutms * 0.3 * I / iVal.Length Then
                        '    nVueltas = maxVueltas
                        'End If
                        'Dim ts2 As New TimeSpan(Now.Ticks - ts.Ticks)
                        'If ts2.TotalMilliseconds > 1 * cfg.timeOutms / 4 Then
                        '    nVueltas = maxVueltas
                        'End If
                        If cfg IsNot Nothing AndAlso cfg.doTimeOut <> timeout.never _
                        AndAlso cfg.isTimeout(False) Then
                            Exit Try
                        End If
                        Try
                            norm = dif.opNorm
                        Catch ex As Exception
                            norm = 10 ^ 10
                            GoTo sig
                        End Try
                    Loop While norm < 10 ^ 50 AndAlso nVueltas < maxVueltas
                    'Loop While nVueltas < maxVueltas
                    If norm >= 10 ^ 50 Then
                        GoTo sig
                        'Exit Do
                    End If
                    If nVueltas < maxVueltas Then
                        'GoTo sig
                    End If

                    Dim fnx As Complex = Nothing
                    Dim norm1 As Double = Double.MaxValue
                    If bCumple Then
                        Try
                            fnx = exprFnOrig.evalExpression(x0)
                            Dim x02 As Complex = Nothing
                            If Math.Abs(x0.pRe.ToDouble) > Math.Abs(x0.pIm.ToDouble) Then
                                x02 = New Complex(x0.pRe, New Rational(0.0))
                            Else
                                x02 = New Complex(New Rational(0.0), x0.pIm)
                            End If
                            Dim fnx02 As Complex = exprFnOrig.evalExpression(x02)
                            If fnx02.opModulo < fnx.opModulo Then
                                x0 = x02
                            End If
                        Catch ex As Exception
                            GoTo sig
                        End Try
                        Try
                            norm1 = fnx.opNorm
                        Catch ex As Exception

                        End Try
                    End If
                    Dim x1 As Complex = Nothing
                    If bCumple AndAlso
                            (norm1 < 1 OrElse norm1 < absIndepTerm / 10 ^ 7) Then
                        bFound = True
                        'ReDim Preserve roots(roots.Length)
                        Dim re As Double
                        If x0.IsReal OrElse
                                (Math.Abs(x0.pRe.ToDouble) > 10 ^ -3 AndAlso
                                Math.Abs(x0.pIm.ToDouble) < 10 ^ -6) Then
                            re = x0.pRe.ToDouble
                            ' intentar afinar, si es posible,
                            ' con una búsqueda dicotómica:
                            Dim abs As Double = Math.Abs(re)
                            Dim a As Double = Math.Sign(re) * (abs - abs * 0.001)
                            Dim fa As Double = exprFn.eval_1var_DblToCjo(a).pRe.ToDouble
                            Dim sA As Int64 = Math.Sign(fa)
                            Dim b As Double = Math.Sign(re) * (abs + abs * 0.001)
                            Dim fb As Double = exprFn.eval_1var_DblToCjo(b).pRe.ToDouble
                            Dim sB As Int64 = Math.Sign(fb)
                            If sA * sB = 1 Then
                                a = Math.Sign(re) * (abs - abs * 0.1)
                                fa = exprFn.eval_1var_DblToCjo(a).pRe.ToDouble
                                sA = Math.Sign(fa)
                                If sA = sB Then
                                    b = Math.Sign(re) * (abs + abs * 0.1)
                                    fb = exprFn.eval_1var_DblToCjo(b).pRe.ToDouble
                                    sB = Math.Sign(fb)
                                End If
                            End If
                            If sA * sB = -1 Then
                                Dim m As Double
                                Dim fm As Double
                                Dim sM As Int64
                                Dim nVueltas2 As Int64 = 0
                                Try
                                    Do
                                        m = (a + b) / 2
                                        fm = exprFn.eval_1var_DblToCjo(m).pRe.ToDouble
                                        sM = Math.Sign(fm)
                                        If sA = sM Then
                                            If a = m Then
                                                Exit Do
                                            End If
                                            a = m : fa = fm
                                        ElseIf sB = sM Then
                                            If b = m Then
                                                Exit Do
                                            End If
                                            b = m : fb = fm
                                        Else
                                            Exit Do
                                        End If
                                        nVueltas2 += 1
                                    Loop While nVueltas2 < 2000
                                    If Math.Abs(fb) < Math.Abs(fm) Then
                                        m = b : fm = fb
                                    ElseIf Math.Abs(fa) < Math.Abs(fm) Then
                                        m = a : fm = fa
                                    End If
                                    x0 = New Complex(m)
                                Catch ex As Exception

                                End Try
                            End If

                        End If

                        Try
                            fnx = exprFnOrig.evalExpression(x0)
                            If fnx.opModulo > 10 ^ -6 AndAlso fnx.opModulo > absIndepTerm / 10 ^ 7 Then
                                GoTo sig
                            End If
                        Catch ex As Exception
                            GoTo sig
                        End Try
                        Dim vX0() As Complex = {x0, -x0, Complex.opConjugate(x0), Complex.opConjugate(-x0)}
                        For iv As Int64 = 0 To vX0.Length - 1
                            For iv2 As Int64 = 0 To iv - 1
                                If (vX0(iv2) - vX0(iv)).opModulo < 10 ^ -3 Then
                                    GoTo nextIV
                                End If
                            Next
                            x0 = vX0(iv)
                            Try
                                fnx = exprFnOrig.evalExpression(x0)
                                If fnx.opModulo > 10 ^ -6 AndAlso fnx.opModulo > absIndepTerm / 10 ^ 7 Then
                                    GoTo nextIV
                                End If
                            Catch ex As Exception
                                GoTo nextIV
                            End Try
                            ReDim Preserve roots(roots.Length)
                            roots(roots.Length - 1) = x0
                            If bIsPolynomial AndAlso roots.Length = paOrig.getDegree Then
                                Exit Try
                            End If
                            'If Math.Abs(x0.pIm.ToDouble) > 0.0001 Then
                            '    x1 = Complex.opConjugate(x0)
                            '    fnx = _
                            '        exprFnOrig.evalExpression(x1)
                            '    If fnx.opNorm < 10 ^ -6 Then
                            '        ReDim Preserve roots(roots.Length)
                            '        roots(roots.Length - 1) = x1
                            '    End If
                            'End If
                            Dim sX As Polynomial = Polynomial.GetPolynomial(sVar)
                            sX -= New Polynomial(x0)
                            Dim divByRootExpr As Expression = Nothing
                            If Len(Me.sFactorsSolution) Then
                                Me.sFactorsSolution += "*"
                            End If
                            If x1 Is Nothing Then
                                divByRootExpr = New Expression(sX)
                                polySolution *= sX
                                Me.sFactorsSolution += "(" + sX.toStringPoly(cfg) + ")"
                            Else
                                Dim sX1 = Polynomial.GetPolynomial(sVar)
                                sX1 -= New Polynomial(x1)
                                Dim prod As Polynomial = sX * sX1
                                divByRootExpr = New Expression(prod)
                                polySolution *= sX * sX1
                                Me.sFactorsSolution += "(" + prod.toStringPoly(cfg) + ")"
                            End If
nextIV:
                        Next
                        'exprFn = exprOrig / New Expression(polySolution)
                        If exprOrig.getMatchStr = "/" Then
                            exprFn = exprOrig.getArgs(0) * New Expression(polySolution) / exprOrig.getArgs(1)
                        Else
                            exprFn = Expression.exprOp("/", exprOrig, New Expression(polySolution))
                        End If
                        If exprOrig.IsPolynomial Then
                            exprFn.getPolynomial.PolyResto = Nothing
                            exprFn.getPolynomial.PolyDivisor = Nothing
                        End If
                        oVars.setValue(0, Nothing)
                        Try
                            'exprDer = exprFn.opDeriv(sVar)
                            ' D( f/polySolution ) =
                            '  = (Df * polySolution - f* D(polySolution) ) / (polySolution)^2
                            exprDer = Expression.exprOp("/", (exprDerOrig * New Expression(polySolution) _
                                               - exprOrig * New Expression(polySolution.opDerivative(sVar))
                                               ), New Expression(polySolution * polySolution))
                        Catch ex As Exception
                            Exit Try
                        End Try
                        'i -= 1
                        'If nSolutions > 8 Then
                        '    'Exit Do
                        'End If

                    Else

                    End If
sig:
                    Dim ts3 As New TimeSpan(Now.Ticks - t0.Ticks)
                    If ts3.TotalMilliseconds >= cfg.timeOutms / 3 AndAlso
                             cfg.doTimeOut <> timeout.never Then
                        Exit Try
                    End If
                    'Dim ts3 As New TimeSpan(Now.Ticks - ts.Ticks)
                    'If ts3.TotalMilliseconds >= 1 * CDbl(cfg.timeOutms) / 4.0 Then
                    '    Exit Do
                    'End If
                Next ' next i
                nIter += 1
                If bFound Then nIter = 0
            Loop While bFound = True AndAlso nIter < 3
        Catch exT As System.Threading.ThreadAbortException
            If roots.Length = 0 Then
                Throw exT
            Else
                cfg.ticksStart = Now.Ticks
            End If
        Catch ex As Exception
            If roots.Length = 0 Then
                Throw ex
            Else
                cfg.ticksStart = Now.Ticks
            End If
        End Try

        If nCall Then
            nCall = 0
        ElseIf roots.Length = 0 Then
            If Not bSimplify Then
                bSimplify = True
                GoTo doSimplify
            End If
            Return False
        End If
        roots = Complex.sortRoots(roots)
        Me.poly1varSolution = polySolution
        Return True
    End Function

    Public Sub byPolynApprox()
        Try
            var = eG.getAllVars
            Dim lenVars As Int64 = var.Length

        Catch ex As Exception

        End Try
    End Sub
    Public Function resolveSysOfEqs(ByVal cfgOrig As Config) As Boolean
        ' sysType() --> get Ma() & b()
        Dim cfg As Config = cfgOrig
        Dim bRet As Boolean = False
        Dim doTimeOut As timeout = cfg.doTimeOut
        Try
            If Me.unit IsNot Nothing Then
                bRet = True
                Exit Try
            End If
            If Me.tipo1 = eqSysType.PolyDegreeTwoCoefAsVars OrElse
            Me.tipo1 = eqSysType.threeDegreePolyCoefAsVars OrElse
            Me.tipo1 = eqSysType.PolyDegreeOneCoefAsVars Then
                bRet = True
                Exit Try
            End If
            If Me.tipo1 <> eqSysType.nonLinear Then
                tipo1 = sysType()
            End If
            If Me.tipo1 = eqSysType.PolyDegreeTwoCoefAsVars OrElse
            Me.tipo1 = eqSysType.threeDegreePolyCoefAsVars OrElse
            Me.tipo1 = eqSysType.PolyDegreeOneCoefAsVars Then
                bRet = True
                Exit Try
            End If
            If var.Length = 0 AndAlso
            eG IsNot Nothing Then
                var = eG.getAllVars
                If (var Is Nothing OrElse var.Length = 0) AndAlso
                vars IsNot Nothing Then
                    var = vars.getNamesList
                End If
            End If

            If Me.tipo1 = eqSysType.isAPolynomial Then
                If poly1.getDegree = 0 Then
                    If poly1.hasComplexCoeff OrElse poly1.ToDouble <> 0.0 Then
                        sErrMsg = String.Format(msg8.num(56), poly1.toStringPoly(cfg))
                    Else
                        sErrMsg = msg8.num(60)
                    End If
                    bRet = False
                    Exit Try
                End If
                Mresult = New Matrix(poly1.roots.mtx)
                Me.resultValues = poly1.roots.cjo
                Me.resultVars = poly1.var
                bRet = True
                Exit Try
            ElseIf (tipo1 And eqSysType.linear) OrElse
            (tipo1 = eqSysType.linear AndAlso
            Jxn.vVect.Length <> var.Length) OrElse
            (tipo1 AndAlso eqSysType.nonLinear AndAlso
             eG.Rows <> var.Length) Then
                If (tipo1 And eqSysType.linear AndAlso
                Jxn.vVect.Length = var.Length) Then
                    Try
                        ' Verify eqs. are independent
                        Dim emJ As New ExprMatrix(Jxn)
                        emJ.cfg.bRounding = cfg.bRounding
                        Dim rank As Int64 = emJ.opRank()
                        If rank < emJ.Rows Then
                            Throw New Exception(msg8.num(51)) ' eqs. are not indepent
                        End If
                    Catch ex As Exception
                        Throw ex
                    End Try
                    Try
                        'MaInv = Ma ^ -1
                        Dim errMsg As String = ""
                        'inv_Jxn = Functions.Inverse_Matrix_GaussPivotTotal_poly(Jxn, errMsg)
                        Matrix.TryParseMatrix((New ExprMatrix(Jxn)) ^ New ExprMatrix(-1), inv_Jxn)
                        If Len(errMsg) Then
                            sErrMsg = errMsg
                            Return False
                        End If
                        Mresult = inv_Jxn * F

                        ReDim resultValues(Mresult.vVect.Length - 1), resultVars(resultValues.Length - 1)
                        For i As Int64 = 0 To Mresult.vVect.Length - 1
                            resultValues(i) = Mresult.vVect(i).vPoly(0).cf(0)
                            resultVars(i) = var(i)
                        Next
                        bRet = True
                    Catch ex2 As Exception
                        'Throw New Exception("System of equations resolution: n/a.")
                        Throw New Exception(msg8.num(12))
                    End Try
                Else
                    If eG.bHasEqsAndExpr Then
                        'bRet = False
                        'sInfo = msg8.num(53)
                        'Exit Try
                    End If
                    bRet = False
                    Dim sErr As String = msg8.num(40)
                    Dim sV As String = ""
                    If var.Length <> 1 Then sV = "s"
                    Dim sEq() As String
                    If Jxn.vVect.Length = 1 Then
                        sEq = New String() {"is one", ""}
                        'sErr = "n/a: There {0} equation{1} and {2} variable{3}: "
                        sErr = String.Format(sErr,
                            sEq(0), sEq(1), var.Length, sV)
                    Else
                        sEq = New String() {"are", "s"}
                        'sErr = "n/a: There {0} equation{1} and {2} variable{3}: "
                        sErr = String.Format(sErr,
                             sEq(0) + " " + Jxn.vVect.Length.ToString + " ",
                            sEq(1), var.Length, sV)
                    End If
                    If tipo1 = eqSysType.linear Then
                        Try
                            ' try resolve indeterminate system:

                            Dim oVars As New VarsAndFns(cfg)
                            oVars.bVerifyName = False
                            For i As Int64 = 0 To var.Length - 1
                                oVars.AddVar(var(i), Nothing)
                            Next
                            Jxn = New Matrix
                            ReDim Jxn.vVect(eG.Rows - 1)
                            For i As Int64 = 0 To eG.Rows - 1
                                Dim indep As New Polynomial(0.0)
                                Jxn.vVect(i) = New Vector
                                ReDim Jxn.vVect(i).vPoly(var.Length)
                                For j = 0 To Jxn.vVect(i).vPoly.Length - 1
                                    Jxn.vVect(i).vPoly(j) = New Polynomial(0.0)
                                Next
                                For j As Int64 = 0 To eG.Cols - 1
                                    Dim Pa As New Polynomial(eG.getExpr(i, j).getPolynomial)
                                    Dim vPa() As Polynomial = Pa.splitIntoTerms
                                    For k As Int64 = 0 To vPa.Length - 1
                                        Dim deg As Int64 = vPa(k).getDegree
                                        If deg = 0 Then
                                            indep -= vPa(k)
                                        ElseIf deg = 1 Then
                                            Dim col As Int64 = Array.IndexOf(var, vPa(k).varAll(0))
                                            Jxn.vVect(i).vPoly(col) = New Polynomial(vPa(k).cf(0))
                                        Else
                                            Exit Try
                                        End If
                                    Next
                                Next
                                Jxn.vVect(i).vPoly(var.Length) = indep
                            Next
                            Dim mtx As Matrix =
                                Functions.Inverse_Matrix_GaussPartial_poly(Jxn, sErr, False, oVars)
                            If mtx IsNot Nothing Then
                                eG = New ExprMatrix(cfg, var.Length, 2)
                                Dim vVar() As String = oVars.getNamesList
                                For i As Int64 = 0 To vVar.Length - 1
                                    eG.getExpr(i, 0) = New Expression(Polynomial.GetPolynomial(vVar(i)))
                                    eG.getExpr(i, 1) = oVars.getValueByID(i).getExpr(0, 0)
                                Next
                                'Trace.WriteLine(oVars.ToString)
                                sErrMsg = ""
                                sErr = ""
                                tipo1 = eqSysType.linearIndeterminate
                                bRet = True
                                Return True
                            ElseIf sErr.Length Then
                                sErrMsg = sErr
                                bRet = False
                                Return False
                            End If
                        Catch ex3 As Exception

                        End Try
                    End If
                    If var.Length Then
                        sErr += "{"
                        ' add vars' names:
                        For i As Int64 = 0 To var.Length - 1
                            sErr += "'" + var(i) + "'"
                            If i < var.Length - 1 Then
                                sErr += ", "
                            End If
                        Next
                        sErr += "}"
                    Else
                        sErr = Replace(sErr, "variables:", "variables.")
                    End If
                    sErrMsg = sErr
                End If
            ElseIf (tipo1 And eqSysType.nonLinear) Then
                If Me.var Is Nothing OrElse Me.var.Length = 0 Then
                    Me.var = eG.getAllVars
                End If
                If Me.var.Length = 1 Then
                    ' 1 non-linear equation
                    Dim bIsDivF() As Boolean = Nothing
                    Dim rup As New ReduceExprUsingPolynomials
                    eG.getExpr(0, 0) = rup.ReduceUsingPolynomials(eG.getCurExpr)
                    Dim result(-1) As Expression
                    Dim Pvar As Polynomial = Polynomial.GetPolynomial(var(0))
                    Dim altExpr As Expression = Nothing
                    If eG.getCurExpr.tryToIsolateToExpression(var(0), result, altExpr) AndAlso
                        result.Length = 1 Then
                        eG.getExpr(0, 0) = New Expression(Pvar) - result(0)
                    ElseIf altExpr IsNot Nothing Then
                        eG.getExpr(0, 0) = altExpr
                    End If
                    Dim vFact() As Expression = eG.getCurExpr.exprToFactors(bIsDivF)
                    If True Then
                        Dim vfact2(-1) As Expression
                        Dim bIsDiv2(-1) As Boolean
                        Dim i2 As Int64 = 0
                        For i1 As Int64 = 0 To vFact.Length - 1
                            ReDim Preserve vfact2(i2), bIsDiv2(i2)
                            vfact2(i2) = New Expression(vFact(i1))
                            bIsDiv2(i2) = bIsDivF(i1)
                            i2 += 1
                            Dim common As Expression = Nothing
                            Dim expr2 As Expression = Nothing
                            Dim commonStr As String = ""
                            Dim Expr1 As Expression = Nothing
                            Dim CommExpr As Expression = Nothing
                            Dim commStr As String = ""
                            If vfact2(i2 - 1).getCommonFactor(cfg, CommExpr, Expr1, commStr) Then
                                ReDim Preserve vfact2(i2), bIsDiv2(i2)
                                vfact2(i2 - 1) = CommExpr
                                vfact2(i2) = Expr1
                                bIsDiv2(i2) = bIsDivF(i1)
                                i2 += 1
                            End If
                        Next
                        vFact = vfact2
                        bIsDivF = bIsDiv2
                    End If

                    Dim k1 As Int64
                    Dim div As Double = 1.0
                    Mresult = New Matrix
                    ReDim Mresult.vVect(-1)
                    k1 = 0
                    Do
                        If vFact(k1).IsComplex Then
                            For j = k1 + 1 To vFact.Length - 1
                                vFact(j - 1) = vFact(j)
                            Next
                            ReDim Preserve vFact(vFact.Length - 2)
                        Else
                            k1 += 1
                        End If
                    Loop While k1 < vFact.Length
                    Dim bSum2(-1) As Boolean
                    Dim vSumm2() As Expression = eG.getExpr(0, 0).exprToSummands(bSum2)
                    Dim absIndepTerm As Double = 0.0
                    For i = 0 To vSumm2.Length - 1
                        If vSumm2(i).IsReal Then
                            absIndepTerm = Math.Abs(vSumm2(i).toDouble)
                        End If
                    Next
                    If vFact.Length = 1 Then
                        Dim sOp As String = vFact(0).getMatchStr
                        If sOp = "sqr" Then
                            vFact(0) = vFact(0).getArgs(0)
                            eG.getExpr(0, 0) = vFact(0)
                        End If
                        Dim common As Expression = Nothing
                        Dim expr As Expression = Nothing
                        Dim commonStr As String = ""
                        Dim Expr1 As Expression = Nothing
                        Dim CommExpr As Expression = Nothing
                        Dim commStr As String = ""
                        If eG.getExpr(0, 0).getCommonFactor(cfg, CommExpr, Expr1, commStr) Then
                            Dim commVars(-1) As String
                            If CommExpr.getAllVars(commVars) AndAlso
                            Array.IndexOf(commVars, var(0)) > -1 Then
                                CommExpr.IsEquation = True
                                Dim cfg1 As New Config(cfg)
                                cfg1.timeOutms = cfg.timeOutms / 2.0
                                Dim soe1 As New SystemOfEquations(
                                Nothing, New ExprMatrix(CommExpr), Nothing, cur, cfg1)
                                Me.Mresult = New Matrix
                                ReDim Me.resultValues(-1), Mresult.vVect(-1)
                                Try
                                    If soe1.resolveSysOfEqs(cfg) Then
                                        ReDim Me.resultValues(
                                            soe1.resultValues.Length - 1)
                                        Array.Copy(soe1.resultValues,
                                            Me.resultValues, soe1.resultValues.Length)
                                        bRet = True
                                    End If
                                Catch ex As Exception

                                End Try
                                Expr1.IsEquation = True
                                Dim soe2 As New SystemOfEquations(
                                Nothing, New ExprMatrix(Expr1), Nothing, cur, cfg1)
                                Try
                                    If soe2.resolveSysOfEqs(cfg) Then
                                        'Dim i As Int64 = Me.resultValues.Length
                                        'ReDim Preserve Me.resultValues( _
                                        '    i + soe2.resultValues.Length - 1)
                                        'Array.Copy(soe2.resultValues, 0, _
                                        '    Me.resultValues, i, soe2.resultValues.Length)
                                        For i = 0 To soe2.resultValues.Length - 1
                                            AddSolutionIfNotAlreadyPresent(soe2.resultValues(i))
                                        Next
                                        bRet = True
                                    End If
                                Catch ex As Exception

                                End Try
                                'If bRet Then
                                '    ReDim Me.resultVars(0)
                                '    Me.resultVars(0) = var(0)
                                '    ReDim Me.Mresult.vVect(Me.resultValues.Length - 1)
                                '    For i = 0 To Me.resultValues.Length - 1
                                '        Me.Mresult.vVect(i) = _
                                '            New Vector(Me.resultValues(i))
                                '    Next
                                'End If
                                Exit Try
                            End If
                        End If
                        If True Then
                            Dim sFn2 As String = eG.getCurExpr.ToStringExpr(cfg)
                            Dim Pa As Polynomial = Nothing
                            Dim roots2(-1) As Complex
                            Dim trigo As Expression = eG.getExpr(0, 0).trigonometricToExponential
                            If trigo.getMatchStr = "/" Then
                                trigo = trigo.getArgs(0)
                            End If
                            If roots_fn_1var_Newton_Raphson(
                            trigo,
                            cfg, sFn2, roots2, Pa) Then
                                For i As Int64 = 0 To roots2.Length - 1
                                    Try
                                        Dim fx As Complex = eG.getExpr(0, 0).evalToCjo(
                                        var, New Complex() {roots2(i)})
                                        If fx.opModulo = 0.0 OrElse
                                    (roots2(i).opModulo / fx.opModulo) > 10 ^ 3 OrElse fx.opModulo < absIndepTerm / 10 ^ 7 Then
                                            AddSolutionIfNotAlreadyPresent(roots2(i), False)
                                        End If
                                    Catch ex As Exception

                                    End Try
                                Next
                                bRet = True
                            End If
                            If roots_fn_1var_Newton_Raphson(
                            eG.getExpr(0, 0),
                            cfg, sFn2, roots2, Pa) Then
                                For i As Int64 = 0 To roots2.Length - 1
                                    Try
                                        Dim fx As Complex = eG.getExpr(0, 0).evalToCjo(
                                        var, New Complex() {roots2(i)})
                                        If fx.opModulo = 0.0 OrElse
                                    (roots2(i).opModulo / fx.opModulo) > 10 ^ 3 OrElse fx.opModulo < absIndepTerm / 10 ^ 7 Then
                                            AddSolutionIfNotAlreadyPresent(roots2(i))
                                        End If
                                    Catch ex As Exception

                                    End Try
                                Next
                                bRet = True
                            End If
                        End If
                    Else
                        Dim t As Long = cfg.timeOutms
                        cfg.timeOutms = t / (vFact.Length + 1)
                        Dim vfact2(-1) As Expression
                        Dim bIsDiv2(-1) As Boolean
                        Dim i2 As Int64 = 0
                        For i1 As Int64 = 0 To vFact.Length - 1
                            ReDim Preserve vfact2(i2), bIsDiv2(i2)
                            vfact2(i2) = New Expression(vFact(i1))
                            bIsDiv2(i2) = bIsDivF(i1)
                            i2 += 1
                            Dim common As Expression = Nothing
                            Dim expr2 As Expression = Nothing
                            Dim commonStr As String = ""
                            Dim Expr1 As Expression = Nothing
                            Dim CommExpr As Expression = Nothing
                            Dim commStr As String = ""
                            If vfact2(i2 - 1).getCommonFactor(cfg, CommExpr, Expr1, commStr) Then
                                ReDim Preserve vfact2(i2), bIsDiv2(i2)
                                vfact2(i2 - 1) = CommExpr
                                vfact2(i2) = Expr1
                                bIsDiv2(i2) = bIsDivF(i1)
                                i2 += 1
                            End If
                        Next
                        vFact = vfact2
                        bIsDivF = bIsDiv2
                        For i1 As Int64 = 0 To vFact.Length - 1
                            'Dim eM As New ExprMatrix(eG.getExpr(0, 0).trigonometricToExponential)
                            'eM.getExpr(0, 0).IsEquation = True

                            If bIsDivF(i1) = False Then
                                Dim expr As New Expression(vFact(i1))
                                expr.IsEquation = True
                                Dim cfg2 As New Config
                                cfg2.doTimeOut = timeout.whenTimeIsOver
                                cfg2.timeOutms = cfg.timeOutms / (vFact.Length + 2)
                                Dim soe As New SystemOfEquations(Nothing, New ExprMatrix(expr), Nothing, Nothing, cfg2)
                                soe.resolveSysOfEqs(cfg2)
                                Dim eGorig As Expression = eG.getExpr(0, 0)
                                For i = 0 To soe.resultValues.Length - 1
                                    Dim RVi As Complex = soe.resultValues(i) ^ New Complex(div)
                                    Dim zero As Complex = vFact(i1).evalExpression(soe.resultValues(i))
                                    Try
                                        Dim zeroOrig As Complex = eGorig.evalExpression(RVi)
                                        If zeroOrig.opModulo < 10 ^ -6 Then
                                            AddSolutionIfNotAlreadyPresent(RVi)
                                        End If
                                    Catch ex As Exception

                                    End Try
                                Next
                            End If
                        Next
                    End If
                    If k1 > 0 Then
                        bRet = True
                        Me.poly1varSolution = Nothing
                        Me.sFactorsSolution = ""
                    End If
                    If True Then
                        Dim sFn2 As String = eG.getCurExpr.ToStringExpr(cfg)
                        Dim Pa As Polynomial = Nothing
                        Dim roots2(-1) As Complex
                        Dim trigo As Expression = eG.getExpr(0, 0).trigonometricToExponential
                        If trigo.getMatchStr = "/" Then
                            trigo = trigo.getArgs(0)
                        End If
                        If roots_fn_1var_Newton_Raphson(
                            trigo,
                            cfg, sFn2, roots2, Pa) Then
                            For i As Int64 = 0 To roots2.Length - 1
                                Try
                                    Dim fx As Complex = eG.getExpr(0, 0).evalToCjo(
                                        var, New Complex() {roots2(i)})
                                    If fx.opModulo = 0.0 OrElse
                                    (roots2(i).opModulo / fx.opModulo) > 10 ^ 3 OrElse fx.opModulo < absIndepTerm / 10 ^ 7 Then
                                        AddSolutionIfNotAlreadyPresent(roots2(i), True)
                                    End If
                                Catch ex As Exception

                                End Try
                            Next
                            bRet = True
                        End If
                    End If

                    Exit Try

                ElseIf Me.var.Length > 1 Then

                    ' 2 or more non-linear equations:

                    Dim vResult() As Expression = Nothing
                    If tryToIsolateSystemVars(vResult) Then
                        'ReDim resultValues(vResult.Length - 1), resultVars(resultValues.Length - 1)
                        'For i As Int64 = 0 To vResult.Length - 1
                        '    resultValues(i) = vResult(i).toComplex
                        '    resultVars(i) = var(i)
                        'Next
                        bRet = True
                    Else
                        Me.var = eG.getAllVars
                        Dim bFirstTry As Boolean = True
                        Dim initialPts(var.Length - 1) As Complex
anothertry:
                        Try
                            Dim eMLnAprox As ExprMatrix = Nothing
                            Try
                                eMLnAprox = Interpolation.aproxNonToLinearSOEinterpolating(eG, 0, 1)
                            Catch ex As Exception

                            End Try
                            Dim soe As New SystemOfEquations(Nothing, eMLnAprox, vars, Nothing, cfg)
                            'Dim cfg2 As New Config
                            soe.resolveSysOfEqs(cfg)
                            If soe.resultValues IsNot Nothing AndAlso
                            soe.resultValues.Length = eG.Rows Then
                                Dim rnd As New Random()
                                For i = 0 To initialPts.Length - 1
                                    initialPts(i) = soe.resultValues(i)
                                    If initialPts(i).IsZero Then
                                        initialPts(i) = New Complex(rnd.NextDouble)
                                    End If
                                Next
                            Else
                                Dim rnd As New Random
                                For i = 0 To initialPts.Length - 1
                                    initialPts(i) = New Complex(rnd.NextDouble)
                                Next
                            End If
                            If Me.NewtonRaphson(cfg, initialPts) Then
                                ReDim resultValues(Mresult.vVect.Length - 1), resultVars(resultValues.Length - 1)
                                For i As Int64 = 0 To Mresult.vVect.Length - 1
                                    resultValues(i) = Mresult.vVect(i).vPoly(0).cf(0)
                                    resultVars(i) = var(i)
                                Next
                                bRet = True
                            ElseIf bFirstTry Then
                                bFirstTry = False
                                ' cambiamos el orden de las expresiones:
                                Dim vExpr(eG.Rows - 1) As Expression
                                For i = 0 To eG.Rows - 1
                                    vExpr(i) = eG.getExpr(i, 0)
                                Next
                                Array.Reverse(vExpr)
                                For i = 0 To eG.Rows - 1
                                    eG.getExpr(i, 0) = vExpr(i)
                                Next
                                GoTo anothertry
                            End If
                        Catch ex As Exception

                        End Try
                    End If
                End If
            Else
                Dim e1 As String = String.Format(msg8.num(12) +
                " Found {0} equations and {1} variables (", mtx.vVect.Length, var.Length)
                e1 += Join(var, ",") + ")."
                Throw New Exception(e1)
            End If

        Catch ex As Exception
            If InStr(ex.ToString, "n/a") Then
                Throw ex
            End If
            If resultValues.Length = 0 Then
                Mresult = Nothing
                Return False
            End If
        Finally
            If Not bRet AndAlso Me.sErrMsg = "" Then
                Me.sErrMsg = msg8.num(47)
            ElseIf bRet AndAlso Me.var.Length = 1 AndAlso
            tipo <> eqSysType.PolyDegreeTwoCoefAsVars AndAlso
            tipo <> eqSysType.linearIndeterminate Then
                Dim vCjo()() As Complex =
                    Complex.sortRootsByType(Me.resultValues, cfg, Me.resultVars)
                Dim k As Int64
                Mresult = New Matrix
                ReDim Mresult.vVect(Me.resultValues.Length - 1)
                For i As Int64 = 0 To vCjo.Length - 1
                    For j As Int64 = 0 To vCjo(i).Length - 1
                        Me.Mresult.vVect(k) = New Vector(vCjo(i)(j))
                        Me.resultValues(k) = New Complex(vCjo(i)(j))
                        k += 1
                    Next
                Next
            End If
            cfg.doTimeOut = doTimeOut
        End Try
        Return bRet
    End Function
    Public ReadOnly Property poly As Polynomial
        Get
            Return Me.poly1
        End Get
    End Property
    Public Function ToStringInfo(
                                ByVal cfg As Config,
                                Optional ByVal bUseOptions As Boolean = True) As String
        Dim e1 As String = ""
        If Len(sErrMsg) Then
            Return sErrMsg
        End If
        Dim bDetail As Boolean = cfg.bDetail
        cfg.bDetail = False
        Try
            If Me.unit IsNot Nothing Then
                e1 += var(0) + " = " +
                 "<span style=""color:red"">" +
                    Mresult.vVect(0).vPoly(0).toStringPoly(cfg) +
                        "</span> " + Me.unit.ToString + vbCrLf
                Return e1
            End If
            e1 = sPreInfo
            Dim i, j As Int64
            If tipo1 = eqSysType.isAPolynomial Then
                'e1 = vbCrLf + Mresult.toStringMtx + vbCrLf
                'e1 += Mresult.vVect(0).vPoly(0).ToStringInfo
                'e1 += Me.poly1.ToStringInfo(cfg, Me.resultValues )
                e1 += Me.poly1.ToStringInfo(cfg)
                Exit Try
            ElseIf tipo1 = eqSysType.PolyDegreeTwoCoefAsVars OrElse
            tipo1 = eqSysType.linearIndeterminate OrElse
            tipo1 = eqSysType.threeDegreePolyCoefAsVars OrElse
            tipo1 = eqSysType.PolyDegreeOneCoefAsVars Then
                For i = 0 To Me.eG.Rows - 1
                    e1 += Me.eG.getExpr(i, 0).ToString + " =" +
                        Me.eG.getExpr(i, 1).ToStringExpr(cfg) + vbCrLf
                Next
                Exit Try
            ElseIf tipo1 = eqSysType.general Then
                e1 = String.Empty
                For i = 0 To eG.Rows - 1
                    If i Then e1 += vbCrLf
                    For j = 0 To eG.Cols - 1
                        If j Then e1 += "; "
                        e1 += eG.getExpr(i, j).ToStringExpr(cfg)
                    Next
                Next
                Exit Try
            End If
            If Mresult Is Nothing OrElse Mresult.vVect.Length = 0 Then
                e1 = msg8.num(47) 'e1 = "Found no solution."
                Exit Try
            End If
            'e1 = "The solution found out from the system of equations:" + vbCrLf
            If tipo1 = eqSysType.isAPolynomial OrElse
            (tipo1 And eqSysType.linear) Then

                ' LINEAR SYSTEM INFO:

                'e1 = "The solution found out from the (linear) system of equations:" + vbCrLf
                e1 += msg8.num(45) + vbCrLf
                Dim e4 As String = ""
                For i = 0 To Jxn.vVect.Length - 1
                    For j = 0 To Jxn.vVect(i).vPoly.Length - 1
                        If j Then e4 += "+"
                        Dim e2 As String =
                            Jxn.vVect(i).vPoly(j).cf(0).toStringComplex(cfg) +
                            "*" + var(j) + " "
                        Dim e5 = Replace(e2, " ", "")
                        If e5 <> "-0" AndAlso e5 <> "+0" Then
                            e4 += e2
                        End If
                    Next
                    e4 += " = " + F.vVect(i).vPoly(0).cf(0).toStringComplex(cfg) + vbCrLf
                Next
                e1 += MathGlobal8.getTable(e4, "navy")
            Else

                ' NON-LINEAR INFO:

                'e1 = "The solution found out from the (non linear) system of equations:" + vbCrLf
                e1 += msg8.num(46) + vbCrLf
                Dim e2() As String = Split(eG.ToString, vbCrLf)
                Dim e4 As String = ""
                For i = 0 To e2.Length - 1
                    Dim e3 As String = Replace(Trim(e2(i)), " ", "")
                    Do While Right(e3, 2) = "=0"
                        e3 = Mid(e3, 1, Len(e3) - 2)
                    Loop
                    Do While Right(e3, 2) = "-0"
                        e3 = Mid(e3, 1, Len(e3) - 2)
                    Loop
                    e4 += e3 + "= 0" + vbCrLf
                Next
                e1 += MathGlobal8.getTable(e4, "navy")
                'For i = 0 To Me.vPolyqsNonLinear.Length - 1
                '    e1 += Me.vPolyqsNonLineal(i) + " = 0" + vbCrLf
                'Next
            End If
            'e1 += vbCrLf + "follows: " + vbCrLf
            e1 += vbCrLf + msg8.num(48) + vbCrLf
            If var.Length = 1 Then
                Dim e4 As String = ""
                ReDim sSolutionVars(Mresult.vVect.Length - 1)
                For i = 0 To Mresult.vVect.Length - 1
                    ReDim Preserve sSolutionVars(i)(1)
                    If tipo = eqSysType.nonLinear Then
                        e4 += var(0) + String.Format("{0}= ", i) +
                         "<span style=""color:red"">" +
                         Mresult.vVect(i).vPoly(0).toStringPoly(cfg) +
                         "</span>" + vbCrLf
                        sSolutionVars(i)(0) = var(0) + String.Format("{0}= ", i)
                        sSolutionVars(i)(1) =
                            Mresult.vVect(i).vPoly(0).toStringPoly(cfg)
                    Else
                        e4 += var(0) + " = " +
                         "<span style=""color:red"">" +
                         Mresult.vVect(i).vPoly(0).toStringPoly(cfg) +
                         "</span>" + vbCrLf
                        sSolutionVars(i)(0) = var(0)
                        sSolutionVars(i)(1) =
                            Mresult.vVect(i).vPoly(0).toStringPoly(cfg)
                    End If
                Next
                e1 += MathGlobal8.getTable(e4, "navy")
                If Me.poly1varSolution IsNot Nothing AndAlso
                    Me.poly1varSolution.getDegree > 1 Then
                    'e1 += "These roots " + _
                    'String.Format("({0}-{0}0)*({0}-{0}1)...", var(0)) + " constitute the polynomial:" + vbCrLf
                    e1 += String.Format(msg8.num(49), var(0)) + vbCrLf
                    e1 += Me.poly1varSolution.toStringPoly(cfg) + " = " + vbCrLf
                    e1 += "=" + Me.sFactorsSolution + vbCrLf
                End If
            Else
                ReDim sSolutionVars(Mresult.vVect.Length - 1)
                Dim e4 As String = ""
                For i = 0 To Mresult.vVect.Length - 1
                    ReDim Preserve sSolutionVars(i)(1)
                    e4 += var(i) + "= " +
                        Mresult.vVect(i).vPoly(0).toStringPoly(cfg) + vbCrLf
                    sSolutionVars(i)(0) = var(i)
                    sSolutionVars(i)(1) =
                        Mresult.vVect(i).vPoly(0).toStringPoly(cfg)
                Next
                e1 += MathGlobal8.getTable(e4, "navy")
            End If
            e1 = Replace(e1, "+-", "-")
        Catch ex As Exception
            Throw ex
        End Try
        cfg.bDetail = bDetail
        Return e1
    End Function
    Public Function tryToIsolateSystemVars(ByRef result() As Expression) As Boolean
        Dim bRet As Boolean = False
        Dim index As Int64 = 0
        Dim lenVar As Int64 = 0
        Dim eG As New ExprMatrix(Me.eG)
        Dim vVarAll2() As String = Nothing
        Dim vVarAll() As String = Nothing
        Dim eG2 As New ExprMatrix(eG)
        Dim varian3() As Int64 = New Int64() {
                0, 1, 2,
                0, 2, 1,
                1, 0, 2,
                1, 2, 0,
                2, 0, 1,
                2, 1, 0}
        'tryAgain:
        Try
            If index = 0 Then
                vVarAll2 = eG.getAllVars()
                ReDim vVarAll(vVarAll2.Length - 1)
                Array.Copy(vVarAll2, vVarAll, vVarAll.Length)
            End If
            lenVar = vVarAll.Length
            Dim vExpr(vVarAll.Length - 1) As Expression
            Dim msg As String = ""
            Dim i, j As Int64

            Dim retRoots(vVarAll.Length - 1) As retOpRoots
            For i = 0 To vVarAll.Length - 1
                Dim sCurVar As String = vVarAll(i)
                For j = 0 To eG.Rows - 1
                    If vExpr(j) Is Nothing Then ' if not already treated
                        If eG.getExpr(j, 0).tryToIsolateToPolynomial(sCurVar, vExpr(i)) Then
                            ' polynomial vExpr(i) has been isolated 
                            'curIdx(i) = j
                            Me.vars.AddVar(sCurVar, vExpr(i))
                            Exit For
                        Else
                            Exit Try
                        End If
                    End If
                Next
                If j >= eG.Rows Then
                    Exit Try
                Else
                End If
            Next
            For i = 0 To vExpr.Length - 1
                Dim oVars As New VarsAndFns(Config.cfg)
                For j = 0 To i
                    If i <> j Then
                        oVars.AddVar(vVarAll(j), vExpr(j))
                    End If
                Next
                vExpr(i) = vExpr(i).evalExprToExpr(oVars)
                Dim vCurAll(-1) As String
                Dim vCurAll2(-1) As String
                vExpr(i).getAllVars(vCurAll2)
                Do
                    vCurAll = vCurAll2
                    vExpr(i) = vExpr(i).evalExprToExpr(oVars)
                    vExpr(i).getAllVars(vCurAll2)
                Loop While vCurAll2.Length <> vCurAll.Length
                vCurAll = vCurAll2
                If vCurAll.Length > 1 OrElse
                (vCurAll.Length = 1 AndAlso
                 vCurAll(0) <> vVarAll(i)) Then
                    Dim Pi As Polynomial = Polynomial.GetPolynomial(vVarAll(i)) _
                                            - vExpr(i).getPolynomial
                    If Pi.tryToIsolateToPolynomial(vVarAll(i), vExpr(i)) Then
                        ' polynomial vExpr(i) has been isolated 
                        'curIdx(i) = j
                    Else
                        Exit Try
                    End If
                End If
            Next


            ReDim result(vVarAll.Length - 1)
            ReDim Me.resultValues(vVarAll.Length - 1)
            ReDim Me.resultVars(vVarAll.Length - 1)
            ' Restore original order in result():
            Mresult = New Matrix()
            ReDim Mresult.vVect(vVarAll.Length - 1), var(vVarAll.Length)
            For i = vVarAll.Length - 1 To 0 Step -1
                If Not vExpr(i).IsComplex Then
                    Dim oVars As New VarsAndFns(cfg)
                    For j = 0 To vVarAll.Length - 1
                        oVars.AddVar(vVarAll(j), vExpr(j))
                    Next
                    vExpr(i) = vExpr(i).evalExprToExpr(oVars)
                    Dim vCurAll(-1) As String
                    Dim vCurAll2(-1) As String
                    vExpr(i).getAllVars(vCurAll2)
                    Do
                        vCurAll = vCurAll2
                        vExpr(i) = vExpr(i).evalExprToExpr(oVars)
                        vExpr(i).getAllVars(vCurAll2)
                    Loop While vCurAll2.Length <> vCurAll.Length
                    vCurAll = vCurAll2
                End If
                If Not vExpr(i).IsComplex Then
                    Exit Try
                End If
                result(i) = vExpr(i)
                Me.resultVars(i) = vVarAll(i)
                Me.resultValues(i) = vExpr(i).toComplex
                Mresult.vVect(i) = New Vector(resultValues(i))
                var(i) = vVarAll(i)
            Next
            bRet = True
        Catch ex As Exception
            bRet = False
        End Try
        'If Not bRet AndAlso lenVar = 3 Then
        '    index += 1
        '    If index < 6 Then
        '        For i As Int64 = index * 3 To index * 3 + 2
        '            vVarAll(i - index * 3) = vVarAll2(varian3(i))
        '            eG.getExpr(i - index * 3, 0) = eG2.getExpr(varian3(i), 0)
        '        Next
        '        GoTo tryAgain
        '    End If
        'End If
        Return bRet
    End Function
    Private Sub AddSolutionIfNotAlreadyPresent(RVi As Complex, Optional bVerify As Boolean = True)
        If Mresult Is Nothing Then
            Mresult = New Matrix
            ReDim Mresult.vVect(-1)
        End If
        Dim k2 As Int64 = 0
        Dim k1 As Int64 = Mresult.vVect.Length
        Dim RViRe As Double = RVi.pRe.ToDouble
        Dim RViIm As Double = RVi.pIm.ToDouble
        If bVerify Then
            For k2 = 0 To k1 - 1
                Dim Re As Double = resultValues(k2).pRe.ToDouble
                Dim Im As Double = resultValues(k2).pIm.ToDouble
                If Im * Re * RViRe * RViIm AndAlso
            Math.Abs((Re - RViRe) / Re) < 0.1 AndAlso
            Math.Abs((Im - RViIm) / Im) < 0.1 Then
                    Exit For
                End If
                If Re AndAlso Im = 0 AndAlso RViIm = 0 AndAlso
            Math.Abs((Re - RViRe) / Re) < 0.1 Then
                    Exit For
                End If
                If Im AndAlso Re = 0 AndAlso RViRe = 0 AndAlso
            Math.Abs((Im - RViIm) / Im) < 0.1 Then
                    Exit For
                End If
            Next
        Else
            k2 = k1
        End If
        If k2 >= k1 Then
            Try
                Dim bSum(-1) As Boolean
                Dim vSumm() As Expression = eG.getExpr(0, 0).exprToSummands(bSum)
                Dim absIndepTerm As Double = 0.0
                For i = 0 To vSumm.Length - 1
                    If vSumm(i).IsReal Then
                        absIndepTerm = Math.Abs(vSumm(i).toDouble)
                    End If
                Next
                Dim m As Double =
                    eG.getExpr(0, 0).evalToCjo(var, New Complex() {RVi}).opModulo
                If m > 10 ^ -3 AndAlso m > absIndepTerm / 10 ^ 7 Then
                    Exit Sub
                End If
            Catch ex As Exception
                Exit Sub
            End Try
            ReDim Preserve Mresult.vVect(k1)
            ReDim Preserve resultValues(k1), resultVars(k1)
            Mresult.vVect(k1) = New Vector(RVi)
            resultValues(k1) = RVi
            resultVars(k1) = var(0)
            k1 += 1
        End If
        k1 += 1
    End Sub
    Public Shared Sub resolveDeterminateSysOfEqs(
                            ByRef sysOfEqs As ExprMatrix,
                            ByRef oVar As VarsAndFns,
                            Optional b As ExprMatrix = Nothing)

        Try
            ' sysOfEqs is the matrix with the coefficients of the variables
            ' b is the matrix of independent values (coefficients or constants)
            ' For the system:
            '     x + 2y + z = 3
            '     x - 2y -2z = 8
            '    2x + 4y +2z = 6
            ' sysOfEqs = (1,2,1|1,-2,-2|2,4,2) and b=(3|8|6)

            Dim j As Int64
            Dim rows As Int64 = sysOfEqs.Rows
            Dim cols As Int64 = sysOfEqs.Cols
            Dim vIndexRows(rows - 1), iv As Int64
            Dim usedRows(rows - 1) As Int64
            Dim bIsComplex As Boolean = True
            For j = 0 To cols - 1
                Dim indexRow As Int64 = -1
                ' Look for pivot:
                Dim max As Double = 0
                For row As Int64 = 0 To rows - 1
                    If usedRows(row) = 0 Then
                        Dim cExpr1 As Expression = sysOfEqs.getExpr(row, j)
                        If Not cExpr1.IsComplex Then
                            indexRow = row
                            Exit For
                        Else
                            If cExpr1.toComplex.opModulo > max Then
                                max = cExpr1.toComplex.opModulo
                                indexRow = row
                            End If
                        End If
                    End If
                Next
                If indexRow = -1 Then
                    GoTo sigJ
                End If
                ' Mark current row:
                usedRows(indexRow) = 1
                vIndexRows(iv) = indexRow
                iv += 1
                ' Zero rows not marked and with coeff. j not zero:
                Dim mxExpr As Expression = sysOfEqs.getExpr(indexRow, j)
                For row As Int64 = 0 To rows - 1
                    If usedRows(row) = 0 Then ' not marked?
                        Dim cExpr As Expression = sysOfEqs.getExpr(row, j)
                        If Not cExpr.IsComplex OrElse cExpr.toComplex.opModulo <> 0.0 Then ' not zero?
                            ' Zero column j of row #row:
                            Dim vect1(cols - 1) As Expression
                            Dim vect2(cols - 1) As Expression
                            'Dim cExpr2 As Expression = sysOfEqs.getExpr(row, j)
                            For col As Int64 = 0 To cols - 1
                                vect1(col) = mxExpr * sysOfEqs.getExpr(row, col)
                                vect2(col) = cExpr * sysOfEqs.getExpr(indexRow, col)
                                sysOfEqs.getExpr(row, col) = vect1(col) - vect2(col)
                                If sysOfEqs.cfg.bRounding AndAlso
                                sysOfEqs.getExpr(row, col).IsComplex AndAlso
                                sysOfEqs.getExpr(row, col).toComplex.opModulo < 10 ^ -12 Then
                                    sysOfEqs.getExpr(row, col) = New Expression(0.0)
                                End If
                                If b IsNot Nothing Then
                                    ' operate similarly over b:
                                    vect1(col) = mxExpr * b.getExpr(row, 0)
                                    vect2(col) = cExpr * b.getExpr(indexRow, 0)
                                    b.getExpr(row, 0) = vect1(col) - vect2(col)
                                End If
                            Next
                        End If
                    End If
                Next
sigJ:
            Next
            oVar = New VarsAndFns(sysOfEqs.cfg)
            For row As Int64 = 0 To rows - 1
                oVar.AddVar("x" + row.ToString, Nothing)
            Next
            Dim nZeros(cols - 1) As Int64
            Dim vMtxR(rows - 1) As Object
            For row As Int64 = 0 To iv - 1
                Dim MtxR(cols - 1) As Expression
                Dim i As Int64 = vIndexRows(row)
                Dim cExpr As New Expression(0.0)
                For col As Int64 = 0 To cols - 1
                    Dim colExpr As Expression = sysOfEqs.getExpr(i, col)
                    If Not (colExpr.IsReal AndAlso colExpr.toDouble = 0) Then
                        MtxR(col) = New Expression(sysOfEqs.getExpr(i, col))
                        If Not colExpr.IsReal OrElse
                        colExpr.toDouble <> 0.0 Then
                            sysOfEqs.getExpr(i, col) /= colExpr
                            MtxR(col) = New Expression(sysOfEqs.getExpr(i, col))
                            For col1 As Int64 = col + 1 To cols - 1
                                sysOfEqs.getExpr(i, col1) /= colExpr
                                MtxR(col1) = New Expression(sysOfEqs.getExpr(i, col1))
                            Next
                            Exit For
                        End If
                    End If
                    nZeros(i) += 1
                Next
                vMtxR(i) = MtxR
                If b IsNot Nothing Then
                    cExpr = b.getExpr(i, 0)
                End If
                For col As Int64 = nZeros(i) + 1 To cols - 1
                    cExpr -= sysOfEqs.getExpr(i, col) *
                        New Expression(Polynomial.GetPolynomial("x" + col.ToString))
                Next

                Dim Id As Int64 = -1
                oVar.tryGetVarIDByName("x" + nZeros(i).ToString, Id)
                If Id > -1 Then
                    oVar.setValue(Id, New ExprMatrix(cExpr))
                Else
                    Exit For
                End If
            Next
            ReDim Preserve nZeros(rows - 1)
            For row As Int64 = 0 To vMtxR.Length - 1
                If vMtxR(row) Is Nothing Then
                    'oVar.AddVar("x" + vIndexRows(row).ToString, Nothing)
                    Dim mtxR(cols - 1) As Expression
                    For col As Int64 = 0 To cols - 1
                        mtxR(col) = New Expression(0.0)
                    Next
                    vMtxR(row) = mtxR
                    nZeros(row) = cols
                End If
            Next
            Array.Sort(nZeros, vMtxR)
            For row As Int64 = 0 To rows - 1
                For col As Int64 = 0 To cols - 1
                    If vMtxR(row)(col) IsNot Nothing Then
                        sysOfEqs.getExpr(row, col) = CType(
                           vMtxR(row)(col), Expression)
                    Else
                        sysOfEqs.getExpr(row, col) =
                            New Expression(0.0)
                    End If
                Next
            Next
        Catch ex As Exception
            Throw ex
        End Try
    End Sub
    Public Overrides Function ToString() As String
        Return ToStringInfo(Config.cfg)
    End Function
End Class
Public Enum eqSysType
    isAPolynomial = 1
    linear = 2
    nonLinear = 4
    PolyDegreeOneCoefAsVars = 8
    PolyDegreeTwoCoefAsVars = 16
    linearIndeterminate = 32
    threeDegreePolyCoefAsVars = 64
    general = 128
End Enum
Public Enum systemOfLinearEquations
    Determined = 4
    OverDetermined = 8
    Compatible = 16
    Uncompatible = 32
End Enum
