Imports System.Text.RegularExpressions

Partial Public Class Expression
    Public Function Roots_Muller_Method() As Complex()
        Dim vCplx(-1) As Complex
        Dim bMathML As Boolean = G10.mathml
        Try
            Dim vVars() As String = Me.GetVars
            If vVars.Length <> 1 Then
                Throw New ArgumentOutOfRangeException
            End If
            If IsPolynomial() Then
                Return ToPolynomial.RootsNumerator
            End If
            Dim Fr As Expression = CopyFrom(Me)
            Dim sVar As String = vVars(0)
            Dim pX As New Polynomial(sVar)
            Try
                Dim P_xN As New Polynomial(sVar)
                Dim nZeros As Int64 = 0
                Dim curExpr As Expression = Fr
                Dim cjo As Complex
                Do
                    If nZeros Then
                        P_xN = New Polynomial(sVar) ^ New Polynomial(CDbl(nZeros))
                        curExpr = Fr / New Expression(P_xN)
                    End If
                    Dim x0 As New Complex(0.0)
                    Try
                        cjo = curExpr.EvalToComplex(x0)
                    Catch ex As Exception
                        Exit Do
                    End Try
                    If Not cjo.opModulo < 0.0000000001 Then
                        Exit Do
                    Else
                        If AveriguarSignos(Fr, x0) Then
                            ReDim Preserve vCplx(vCplx.Length)
                            vCplx(vCplx.Length - 1) = Complex.zero
                        End If
                    End If
                    nZeros += 1
                Loop
                If nZeros Then
                    nZeros = P_xN.Degree
                    P_xN = New Polynomial(sVar) ^ New Polynomial(CDbl(nZeros))
                    Dim P_xN_expr As New Expression(P_xN)
                    Fr /= P_xN_expr ' divide function by "x"
                    ReDim vCplx(nZeros - 1)
                    For iZ As Int64 = 0 To nZeros - 1
                        vCplx(iZ) = Complex.zero
                    Next
                End If
            Catch ex As Exception

            End Try

            Dim rnd As New Random(1)
            For i As Int32 = 1 To 2
                Dim maxIter As Int32 = 40
                Dim incr As Double = 0.1
                Dim two As New Complex(2.0)
                For j As Double = -5 To 5 Step incr
                    Dim nIter As Int32 = 0
                    Dim sgn As Int32 = (-1) ^ Math.Floor(j)
                    Dim dbl As Double = rnd.NextDouble * sgn
                    Dim z1 As New Complex(-1 + j)
                    Dim z2 As New Complex(dbl + j)
                    Dim z3 As New Complex(1 + j)
                    Try
                        Do
                            Dim f1 As Complex = Fr.EvalToComplex(z1)
                            Dim f2 As Complex = Fr.EvalToComplex(z2)
                            Dim f3 As Complex = Fr.EvalToComplex(z3)
                            Dim q As Complex = (z3 - z2) / (z2 - z1)
                            Dim a As Complex = q * f3 - q * (1 + q) * f2 + q * q * f1
                            Dim b As Complex = (2 * q + 1) * f3 - (1 + q) ^ two * f2 + q * q * f1
                            Dim c As Complex = (1 + q) * f3
                            Dim max As Complex = b + (b * b - 4 * a * c) ^ Complex.oneHalf
                            Dim min As Complex = b - (b * b - 4 * a * c) ^ Complex.oneHalf
                            If max.opModulo < min.opModulo Then
                                max = min
                            End If
                            Dim mod1 As Double = (z3 - z2).opModulo
                            Dim f3Orig As Double = EvalToComplex(z3).opModulo
                            If i = 1 AndAlso (mod1 = 0 OrElse mod1 < 10 ^ -10 AndAlso f3.opModulo < 10 ^ -10 _
                            AndAlso f3Orig < 10 ^ -15) Then
                                Dim x0 As Complex = z3
                                If z3.pIm.ToDouble < 10 ^ -15 AndAlso f3Orig < 10 ^ -10 Then
                                    Dim rZero As New Rational(0.0)
                                    x0 = New Complex(z3.pRe, rZero)
                                    If EvalToComplex(x0).opModulo <= EvalToComplex(z3).opModulo Then
                                        z3 = x0
                                        addRoot(vCplx, z3)
                                        ' divide function by "(x-root)"
                                        Fr /= (New Expression(pX) - New Expression(z3))
                                        j -= incr
                                        Exit Do
                                    End If
                                End If
                                If mod1 = 0 Then Exit Do
                            End If
                            If i > 1 AndAlso ((mod1 = 0 AndAlso f3Orig < 10 ^ -10) _
                            OrElse mod1 < 10 ^ -10 AndAlso f3.opModulo < 10 ^ -10 _
                            AndAlso f3Orig < 10 ^ -15) Then
                                Dim x0 As New Complex(New Rational(0.0), z3.pIm)
                                If z3.pRe.ToDouble < 10 ^ -15 Then
                                    If EvalToComplex(x0).opModulo <= EvalToComplex(z3).opModulo Then
                                        z3 = x0
                                    End If
                                End If
                                If z3.pIm.ToDouble < 10 ^ -15 Then
                                    x0 = New Complex(z3.pRe, New Rational(0.0))
                                    If EvalToComplex(x0).opModulo <= EvalToComplex(z3).opModulo Then
                                        z3 = x0
                                    End If
                                End If

                                addRoot(vCplx, z3)
                                ' divide function by "(x-root)"
                                Fr /= (New Expression(pX) - New Expression(z3))
                                j -= incr
                                Exit Do
                            End If
                            If max.IsZero Then Exit Do
                            Dim dif As Complex = (z3 - z2) * 2 * c / max
                            If i = 1 Then
                                dif.pIm = New Rational(0.0)
                            End If
                            Dim z4 As Complex = z3 - dif
                            z1 = z2 : z2 = z3 : z3 = z4
                            nIter += 1
                        Loop While nIter < maxIter
                    Catch ex As Exception
                        Throw
                    End Try
                Next
            Next
            Dim sR As New SortRoots
            Array.Sort(vCplx, sR)
        Catch ex As Exception
            'Throw
        End Try
        G10.mathml = bMathML
        Return vCplx
    End Function
    Private Sub addRoot(ByRef vCplx() As Complex, x0 As Complex)
        Dim j As Int32
        For j = 0 To vCplx.Length - 1
            If (vCplx(j) - x0).opModulo < 10 ^ -15 Then
                Exit For
            End If
        Next
        If j >= vCplx.Length Then
            ReDim Preserve vCplx(vCplx.Length)
            vCplx(vCplx.Length - 1) = x0
        End If
    End Sub
    Private Function AveriguarSignos(Fn As Expression, ByRef x0 As Complex) As Boolean
        Try
            Dim re As Double = x0.pRe.ToDouble
            If x0.pIm.ToDouble > 10 ^ -15 Then
                Return False
            End If

            ' intentar afinar, si es posible,
            ' con una búsqueda dicotómica:
            Dim abs As Double = Math.Abs(re)
            Dim a1 As Double = Math.Sign(re) * (abs - abs * 0.001)
            If re = 0.0 Then a1 = -0.001
            Dim fa As Double = EvalToComplex(New Complex(a1)).pRe.ToDouble
            Dim sA As Int64 = Math.Sign(fa)
            Dim b1 As Double = Math.Sign(re) * (abs + abs * 0.001)
            If re = 0.0 Then b1 = 0.001
            Dim fb As Double = EvalToComplex(New Complex(b1)).pRe.ToDouble
            Dim sB As Int64 = Math.Sign(fb)
            If sA * sB = 1 Then
                a1 = Math.Sign(re) * (abs - abs * 0.1)
                fa = EvalToComplex(New Complex(a1)).pRe.ToDouble
                sA = Math.Sign(fa)
                If sA = sB Then
                    b1 = Math.Sign(re) * (abs + abs * 0.1)
                    fb = EvalToComplex(New Complex(b1)).pRe.ToDouble
                    sB = Math.Sign(fb)
                End If
            End If
            If sA * sB = -1 Then
                Dim m As Double
                Dim fm As Double
                Dim sM As Int64
                Dim nVueltas2 As Int64 = 0
                Do
                    m = (a1 + b1) / 2
                    fm = EvalToComplex(New Complex(m)).pRe.ToDouble
                    sM = Math.Sign(fm)
                    If sA = sM Then
                        If a1 = m Then
                            Exit Do
                        End If
                        a1 = m : fa = fm
                    ElseIf sB = sM Then
                        If b1 = m Then
                            Exit Do
                        End If
                        b1 = m : fb = fm
                    Else
                        Exit Do
                    End If
                    nVueltas2 += 1
                Loop While nVueltas2 < 2000
                If Math.Abs(fb) < Math.Abs(fm) Then
                    m = b1 : fm = fb
                ElseIf Math.Abs(fa) < Math.Abs(fm) Then
                    m = a1 : fm = fa
                End If
                x0 = New Complex(m)
                Return True
            End If
        Catch ex As Exception

        End Try
        Return False
    End Function

    Function OpLimit(sVar As String, limit As Expression) As Expression
        Dim ret As New Expression(0.0)
        Dim bIsInfinity As Boolean = False
        Dim lstVars As New Dictionary(Of String, Expression)
        Dim rup As New ReduceExprUsingPolynomials
        Dim me2 As New Expression(Me)
        Dim limit2 As New Expression(limit)
        Dim bMathML As Boolean = G10.mathml
        Dim eA As New Expression()
        Try
            G10.mathml = False
            Dim PL As Polynomial = limit.ToPolynomial
            If Not PL.IsComplex AndAlso
             Regex.IsMatch(LCase(PL.GetVars(0)), "∞|infinity") Then
                bIsInfinity = True
                If InStr(PL.ToString, "-") Then
                    lstVars.Add(sVar, New Expression(-1.0E+80))
                    PL = New Polynomial(Term.mnInfinity)
                Else
                    lstVars.Add(sVar, New Expression(1.0E+80))
                    PL = New Polynomial(Term.Infinity)
                End If
                limit = New Expression(PL)
            Else
                lstVars.Add(sVar, limit)
            End If
            Dim cpy2me As New Expression(ReduceExprUsingPolynomials.TryToReduce(Me))
            eA.CopyExprToMe(cpy2me)
            eA.ReplaceConstants()
            If eA.IsPolynomial Then
                If limit.IsComplex Then
                    Dim pE As New ParseExpression
                    ret = pE.Evaluate(lstVars, eA.ToString()).vExpr(0)

                ElseIf limit.IsPolynomial Then
                    ret = New Expression(eA)
                    ret.resto = Nothing
                    ret.divisor = Nothing
                    Dim pE As New ParseExpression
                    ret = pE.Evaluate(lstVars, ret.ToString()).vExpr(0)
                    Dim rv() As String = ret.GetVars
                    If Array.IndexOf(rv, "∞") > -1 Then
                        ret = New Expression("∞", 1)
                        Exit Try
                    End If
                    If bIsInfinity Then
                        If eA.IsPolynomial Then
                            Dim Pa As Polynomial = eA.ToPolynomial
                            If Pa.resto IsNot Nothing Then
                                Dim jNum As Int64 = Array.IndexOf(Pa.GetVarsNumerator, sVar)
                                Dim jDen As Int64 = Array.IndexOf(Pa.GetVarsDenominator, sVar)
                                Dim expN As Int64 = 0
                                Dim expD As Int64 = 0
                                Dim cfn As Complex = Complex.zero
                                Dim cfd As Complex = Complex.zero
                                If jNum > -1 Then
                                    For Each t As Term In Pa.resto
                                        For Each f As Factor In t.f
                                            If f.var = sVar AndAlso f.exp > expN Then
                                                expN = f.exp
                                                cfn = t.cf
                                            End If
                                        Next
                                    Next
                                End If
                                If jDen > -1 Then
                                    For Each t As Term In Pa.divisor
                                        For Each f As Factor In t.f
                                            If f.var = sVar AndAlso f.exp > expN Then
                                                expN = f.exp
                                                cfn = t.cf
                                            End If
                                        Next
                                    Next
                                End If
                                Dim num As New Expression(eA.resto)
                                Dim den As New Expression(eA.divisor)
                                Dim nDegree As Int32 = num.Degree
                                Dim dDegree As Int32 = den.Degree
                                If nDegree > dDegree Then
                                    ret = New Expression("∞", 1)
                                    Exit Try
                                ElseIf nDegree < dDegree Then
                                    Exit Try
                                End If
                                If jNum < jDen Then
                                    Pa.resto = Nothing
                                    Pa.divisor = Nothing
                                    ret += New Expression(Pa)
                                ElseIf jNum = jDen Then
                                    Pa.resto = Nothing
                                    Pa.divisor = Nothing
                                    Pa += New Polynomial(cfn / cfd)
                                    ret += New Expression(Pa)
                                End If
                                Return ret
                            End If
                        End If
                        ret = New Expression(Msg10.Num(115), 1)
                        Exit Try
                    End If
                    pE = New ParseExpression
                    ret = pE.Evaluate(lstVars, Me.ToString()).vExpr(0)
                End If
            Else
                eA.CopyExprToMe(me2)
                Dim nIter0 As Int32 = 0
                Do While eA.t.Count = 1 AndAlso eA.t(0).f.Count > 1 AndAlso nIter0 < 10
                    For i = 0 To eA.t(0).f.Count - 1
                        Dim f As ExprFactor = eA.t(0).f(i)
                        If f.var.ToString = "sin" OrElse f.var.ToString = "cos" Then
                            eA.t(0).f.RemoveAt(i)
                            Exit For
                        End If
                    Next
                    nIter0 += 1
                Loop
                limit = New Expression(limit2)
                If eA.IsPolynomial Then
                    Dim Pa As Polynomial = eA.ToPolynomial()

                    If Pa.resto IsNot Nothing Then
                        Dim num As New Expression(Pa.resto)
                        Dim den As New Expression(Pa.divisor)
                        If den.IsComplex Then
                            Throw New ApplicationException("DivideByZeroException")
                        End If
                        ' apply L'Hôpital's rule:
                        Return (num / den).OpLimit(sVar, limit)
                    Else
                        Dim pE As New ParseExpression
                        ret = pE.Evaluate(lstVars, Pa.ToString).vExpr(0)
                        Exit Try
                    End If
                End If
                eA.Reduce()
                If eA.resto Is Nothing AndAlso eA.t.Count = 1 AndAlso eA.t(0).f.Count > 1 Then
                    Dim div As Expression = New Expression(New ExprTerm(1.0) / New ExprTerm(eA.t(0).f(0)))
                    Dim rto As New Expression(New ExprTerm(eA.t(0).f(1)))
                    Try
                        Dim derRto As Expression = rto.Derivative(sVar)
                        Dim derDiv As Expression = div.Derivative(sVar)
                        Dim pE As New ParseExpression
                        eA = pE.Evaluate(lstVars, (derRto / derDiv).ToString).vExpr(0)
                        ret = eA
                        Return ret
                    Catch ex As Exception

                    End Try
                End If
                If eA.resto Is Nothing Then
                    Dim pE As New ParseExpression
                    Try
                        ret = pE.Evaluate(lstVars, eA.ToString).vExpr(0)
                        If bIsInfinity AndAlso ret.IsDouble Then
                            Dim retDbl As Double = ret.ToDouble
                            If Double.IsPositiveInfinity(retDbl) Then retDbl = 1.0E+40
                            If Double.IsNegativeInfinity(retDbl) Then retDbl = -1.0E+40
                            Dim sgn As Int32 = Math.Sign(retDbl)
                            retDbl = Math.Abs(retDbl)
                            lstVars(sVar) = New Expression(0.0)
                            Dim paso As Double = retDbl / 10 + 10
                            Dim ant As Double = pE.Evaluate(lstVars, eA.ToString).vExpr(0).ToDouble
                            sgn = Math.Sign(ant)
                            Dim difAnt As Double = 0
                            Dim nErr As Int32 = 0
                            For dbl As Double = retDbl To retDbl + 100 Step paso
                                lstVars(sVar) = New Expression(dbl)
                                Dim val As Double = pE.Evaluate(lstVars, eA.ToString).vExpr(0).ToDouble
                                Dim dif As Double = Math.Abs(ant - val)
                                If dif > difAnt Then
                                    nErr += 1
                                    If nErr > 100 Then
                                        ' Sequence is not decrementing
                                        ret = New Expression(PL)
                                        Return ret
                                    End If
                                End If
                                ant = val
                                difAnt = dif
                            Next
                            Return ret
                        End If
                    Catch ex As Exception
                        ret = New Expression(Msg10.Num(115), 1)
                    End Try
                Else
                    ' apply L'Hôpital's rule: lim(num/den) = lim(Derivative(num)/Derivative(den))
                    ret = New Expression(eA)
                    Dim sgn0 As Int32 = 1
                    Try
                        Dim pE As New ParseExpression
                        ret = pE.Evaluate(lstVars, eA.ToString).vExpr(0)
                        If bIsInfinity AndAlso ret.IsDouble Then
                            Dim retDbl As Double = ret.ToDouble
                            If Double.IsPositiveInfinity(retDbl) Then retDbl = 1.0E+40
                            If Double.IsNegativeInfinity(retDbl) Then retDbl = -1.0E+40
                            Dim sgn As Int32 = Math.Sign(retDbl)
                            retDbl = Math.Abs(retDbl)
                            lstVars(sVar) = New Expression(0.0)
                            Dim paso As Double = retDbl / 10 + 10
                            Dim ant As Double = pE.Evaluate(lstVars, eA.ToString).vExpr(0).ToDouble
                            sgn = Math.Sign(ant)
                            Dim difAnt As Double = 0
                            Dim nErr As Int32 = 0
                            For dbl As Double = retDbl To retDbl + 100 Step paso
                                lstVars(sVar) = New Expression(dbl)
                                Dim val As Double = pE.Evaluate(lstVars, eA.ToString).vExpr(0).ToDouble
                                Dim dif As Double = Math.Abs(ant - val)
                                If dif > difAnt Then
                                    nErr += 1
                                    If nErr > 100 Then
                                        ' Sequence is not decrementing
                                        ret = New Expression(PL)
                                        Return ret
                                    End If
                                End If
                                ant = val
                                difAnt = dif
                            Next
                            Return ret
                        End If
                    Catch ex As Exception
                    End Try
                    ret = New Expression(eA)
                    ret.resto = Nothing
                    ret.divisor = Nothing
                    Dim num As New Expression(eA.resto)
                    Dim den As New Expression(eA.divisor)
                    Dim s As String = "(" + num.ToString + ")/(" + den.ToString + ")"
                    Try
                        Dim pE As New ParseExpression
                        Dim eN As Expression = Nothing
                        Dim bErr As Boolean = False
                        Dim vNum(-1) As String
                        Try
                            eN = pE.Evaluate(lstVars, num.ToString).vExpr(0)
                            vNum = num.GetVars()
                        Catch ex As Exception
                            bErr = True
                            Try
                                num = num.ConvertTrigToExp
                                num = ReduceExprUsingPolynomials.TryToReduce(num)
                                Dim eE As Expression = num / den
                                eE = ReduceExprUsingPolynomials.TryToReduce(eE)
                                num = New Expression(eE.resto)
                                den = New Expression(eE.divisor)
                                eE.resto = Nothing
                                eE.divisor = Nothing
                                ret += eE
                                vNum = num.GetVars()
                            Catch ex2 As Exception

                            End Try
                        End Try
                        pE = New ParseExpression
                        Dim eD As Expression = Nothing
                        Dim vDen(-1) As String
                        If Not bErr Then
                            Try
                                eD = pE.Evaluate(lstVars, den.ToString).vExpr(0)
                                vDen = den.GetVars
                                If bIsInfinity AndAlso eN.IsDouble Then
                                    If eD.IsDouble AndAlso eD.ToDouble < 1.0E+100 Then
                                        ret = New Expression(PL)
                                    End If
                                    Return ret
                                End If
                            Catch ex As Exception
                                bErr = True
                                Try
                                    If bIsInfinity AndAlso eN.IsDouble Then
                                        Return ret
                                    End If
                                    den = den.ConvertTrigToExp
                                    den = ReduceExprUsingPolynomials.TryToReduce(den)
                                    Dim eE As Expression = num / den
                                    eE = ReduceExprUsingPolynomials.TryToReduce(eE)
                                    num = New Expression(eE.resto)
                                    den = New Expression(eE.divisor)
                                    eE.resto = Nothing
                                    eE.divisor = Nothing
                                    ret += eE
                                    vNum = num.GetVars()
                                    vDen = den.GetVars()
                                Catch ex2 As Exception

                                End Try
                            End Try
                        End If
                        Dim nIter As Int32 = 0
                        If (eD.IsDouble AndAlso eD.ToDouble = 0.0) OrElse bErr Then
                            Do While nIter < 10 AndAlso bErr OrElse
                        (Array.IndexOf(vDen, sVar) > -1 AndAlso Array.IndexOf(vNum, sVar) > -1)
                                nIter += 1
                                num = num.Derivative(sVar)
                                den = den.Derivative(sVar)
                                pE = New ParseExpression
                                eN = pE.Evaluate(lstVars, num.ToString).vExpr(0)
                                pE = New ParseExpression
                                eD = pE.Evaluate(lstVars, den.ToString).vExpr(0)
                                vDen = eD.GetVars
                            Loop
                        End If
                        Dim NumDen As Expression = eN / eD
                        Dim valND As Expression = pE.Evaluate(lstVars, NumDen.ToString).vExpr(0)
                        ret += valND
                    Catch ex As Exception
                        ret = New Expression(Msg10.Num(115), 1)
                    End Try
                End If
            End If
        Catch ex As Exception
            Throw
        Finally
            G10.mathml = bMathML
        End Try
        Return ret
    End Function

End Class
