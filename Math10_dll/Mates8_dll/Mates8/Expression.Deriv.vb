Imports System.Text.RegularExpressions

Partial Public Class Expression
#Region "ExprDeriv"
    Public Function opDeriv(ByVal respVar As String) As Expression
        Dim ret As Expression = Nothing
        Dim rup As New ReduceExprUsingPolynomials
        Try
            If pA IsNot Nothing Then
                ret = New Expression(pA.opDerivative(respVar))
            ElseIf mOp.Groups("num").Success Then
                ret = New Expression(0.0)
            ElseIf mOp.Groups("var2").Success _
                OrElse mOp.Groups("var").Success Then
                If mOp.ToString <> respVar Then
                    ret = New Expression(0.0)
                Else
                    ret = New Expression(1.0)
                End If
            ElseIf mOp.Groups("fn").Success Then
                ret = FnDeriv(respVar)
            Else
                'Args(0).cfg = cfg
                'If Args.Length > 1 Then Args(1).cfg = cfg
                Select Case mOp.ToString
                    Case "-"
                        If Args.Length = 2 Then
                            ret = Args(0).opDeriv(respVar) - _
                                Args(1).opDeriv(respVar)
                        Else
                            ret = Args(0).opDeriv(respVar)
                        End If
                    Case "+" 'ret = Args(0).opDeriv(respVar) + _
                        'Args(1).opDeriv(respVar)
                        Dim dArg0 As Expression = Nothing
                        Dim dArg1 As Expression = Nothing
                        If Args.Length AndAlso _
                        Args(0) IsNot Nothing Then
                            dArg0 = Args(0).opDeriv(respVar)
                        End If
                        If Args.Length > 1 AndAlso _
                        Args(1) IsNot Nothing Then
                            dArg1 = Args(1).opDeriv(respVar)
                        End If
                        If dArg0 IsNot Nothing AndAlso _
                        dArg1 IsNot Nothing Then
                            ret = dArg0 + dArg1
                        ElseIf dArg0 IsNot Nothing Then
                            ret = dArg0
                        ElseIf dArg1 IsNot Nothing Then
                            ret = dArg1
                        Else
                            ret = New Expression(0.0)
                        End If
                    Case "*"
                        Dim f As Expression = Args(0)
                        Dim g As Expression = Args(1)
                        Dim Df As Expression = Args(0).opDeriv(respVar)
                        Dim Dg As Expression = Args(1).opDeriv(respVar)
                        ret = Df * g + f * Dg
                    Case "/"
                        Dim f As Expression = Args(0)
                        Dim g As Expression = Args(1)
                        Dim Df As Expression = Args(0).opDeriv(respVar)
                        Dim Dg As Expression = Args(1).opDeriv(respVar)
                        Dim varG() As String = Nothing
                        g.getAllVars(varG)
                        Dim varF() As String = Nothing
                        f.getAllVars(varF)
                        If varG Is Nothing OrElse _
                        varG.Length = 0 OrElse _
                        Array.IndexOf(varG, respVar) = -1 Then
                            ret = Df / g
                        ElseIf varF Is Nothing OrElse _
                        varF.Length = 0 OrElse _
                        Array.IndexOf(varF, respVar) = -1 Then
                            ret = -f * Dg / (g * g)
                        Else
                            Dim num As Expression = (Df * g - f * Dg)
                            Dim den As Expression = g ^ New Expression(2.0) 'g * g
                            If num.IsPolynomial AndAlso den.IsPolynomial Then
                                Dim pNum As Polynomial = num.getPolynomial
                                Dim pDen As Polynomial = den.getPolynomial
                                Dim gcd As Polynomial = _
                                    Polynomial.opGcd(pNum, pDen)
                                ret = New Expression((pNum / gcd) / (pDen / gcd))
                            Else
                                ret = num / den
                            End If
                        End If

                    Case "^"
                        Dim f As Expression = Args(0)
                        Dim g As Expression = Args(1)
                        If f.IsPolynomial AndAlso _
                        g.IsPolynomial AndAlso g.IsReal Then
                            Dim pRe As Rational = g.pA.An.pRe
                            Dim pReMnOne As Rational = pRe - New Rational(1.0)
                            Dim cjoRe As New Complex(pRe, New Rational(0.0))
                            Dim cjoReMnOne As New Complex(pReMnOne, New Rational(0.0))
                            ret = New Expression(cjoRe) * _
                                f.opDeriv(respVar) * f ^ New Expression(cjoReMnOne)
                        Else
                            Dim Dg As Expression = Args(1).opDeriv(respVar)
                            If False AndAlso Args(0).IsReal AndAlso _
                            Args(0).toDouble = Math.E Then
                                ' D(exp(g))= D(g)*exp(g)
                                ret = Dg * f
                            Else
                                ' y = f^g
                                ' ln(y) = g*ln(f)
                                ' D(  ) = D (  ) = Dg*ln(f) + g*Df/ f
                                ' D(ln(y)) = Dy/y
                                ' Dy/y = Dg*ln(f) + g*Df / f
                                ' Dy = y*[ ] = f^g * [ ]
                                ' D(f^g)= f^g * [ ] 
                                ' D(f^g)= f^g * [Dg*ln(f) + g*Df / f]
                                Dim Df As Expression = Args(0).opDeriv(respVar)
                                Dim lnf As Expression = Nothing
                                If f.IsReal AndAlso f.toDouble = Math.E Then
                                    lnf = New Expression(1.0)
                                Else
                                    lnf = Expression.AddFnAndArg0("ln", f)
                                End If
                                If Df Is Nothing OrElse _
                                (Df.IsReal AndAlso Df.toDouble = 0.0) Then
                                    ret = (f ^ g) * Dg * lnf
                                ElseIf Dg Is Nothing OrElse _
                                (Dg.IsReal AndAlso Dg.toDouble = 0.0) Then
                                    ret = g * Df * f ^ (g - New Expression(1.0))
                                Else
                                    ret = f ^ g * (Dg * lnf + g * Df / f)
                                End If
                            End If
                        End If
                End Select
            End If
        Catch ex As Exception
            Throw ex
        End Try
        If Me.Args.Length = 2 Then
            If Me.Args(1) Is Nothing Then
                ReDim Preserve Me.Args(0)
                mOp = Nothing
            ElseIf Me.Args(0) Is Nothing Then
                Me.Args(0) = Me.Args(1)
                ReDim Preserve Me.Args(0)
                mOp = Nothing
            End If
        End If
        If sign = -1 Then
            ret = -ret
        End If
        ret.cfg = cfg
        Return ret
        'Return rup.ReduceUsingPolynomials(ret)
        'Dim rup As New ReduceExprWithPolynomials
        'Return rup.ReduceUsingPolynomials(ret)
    End Function
    Private Function FnDeriv( _
                             ByVal respVar As String) As Expression
        Dim ret As Expression = Nothing
        Dim sgn As Int64 = Me.sign
        Dim sFn As String = LCase(mOp.ToString)
        Dim g As Expression = Args(0)
        Dim Dg As Expression = g.opDeriv(respVar) '.deriv
        'Dim sArg0 As String = "(" + arg.ToString + ")"
        'Dim sDetail As String = ""
        Try
            'If MathGlobal8.detail IsNot Nothing Then
            '    sDetail = "D" + vars.getVarNameByID(respVarID) + _
            '    "(" + Fn + "(" + arg.curexpr.ToString + "))"
            'End If
repSelectDeriv:
            Select Case Len(sFn)
                Case 6
                    Select Case sFn
                        Case "logtwo"
                            ' log2(g) = ln(g)/ln(2)
                            ' D(ln(g)) = Dg/g
                            ' D(log2(g))=D(ln(g)/ln(2))=Dg/(g*ln(2))
                            Dim ln2 As Expression = _
                                Expression.AddFnAndArg0("ln", New Expression(2.0))
                            ret = Dg / (g * ln2)
                        Case "logten"
                            ' log10(g) = ln(g)/ln(10)
                            ' D(ln(g)) = Dg/g
                            ' D(log10(g))=D(ln(g)/ln(10))=Dg/(g*ln(10))

                            Dim ln10 As Expression = _
                                Expression.AddFnAndArg0("ln", New Expression(10.0))
                            ret = Dg / (g * ln10)
                    End Select
                Case 5
                    Select Case sFn
                        Case "acosh"
                            ' D(acosh(g) = 1/sqr(g^2-1) * (Dg ),   g = arg
                            '            
                            Dim g2mn1 As Expression = g * g - New Expression(1.0)
                            ret = Dg / New Expression(g2mn1 ^ New Expression(0.5))
                        Case "acoth"
                            '  D(acoth(g)) = 1/(1-g2) * dg
                            Dim onemng2 As Expression = New Expression(1.0) - g * g
                            ret = Dg / onemng2

                        Case "acsch"
                            ' D(acsch(g)) = -1/(abs(g)*sqr(1+g^2)) * dg  
                            '            
                            Dim onePlusg2 As Expression = New Expression(1.0) + g * g
                            Dim absg As Expression = Expression.AddFnAndArg0("abs", g)
                            ret = -Dg / (absg * onePlusg2 _
                                                          ^ New Expression(0.5))

                        Case "asech"
                            '  D(asech(z)) = -Dz/(abs(z)*sqr(1+z^2))   (z = g)

                            Dim onePlusg2 As Expression = New Expression(1.0) + g * g
                            Dim absg As Expression = Expression.AddFnAndArg0("abs", g)
                            ret = -Dg / (absg * onePlusg2 ^ New Expression(0.5))

                        Case "asinh"
                            ' D(asinh(z)) = dz/sqr(1+z^2)    (z = g)

                            Dim onePlusg2 As Expression = _
                                New Expression(1.0) + g * g
                            ret = Dg / onePlusg2 ^ New Expression(0.5)
                        Case "atanh"
                            ' D atan(z) = 1/(1-z) * (Dz)   (z = g)

                            ret = Dg / (New Expression(1.0) - g)
                    End Select
                Case 4
                    Select Case sFn
                        Case "acsc"
                            ' D(acsc(u)) = (-1/(abs(u)*sqr(u^2-1)) * Du    (u=g)

                            Dim sqrOfg2mn1 As Expression = (g * g - New Expression(1.0)) _
                                                           ^ New Expression(0.5)
                            Dim absg As Expression = Expression.AddFnAndArg0("abs", g)
                            ret = -Dg / (absg * sqrOfg2mn1)
                        Case "acos"
                            ' D(acos(u)= - du /sqr(1-u^2)

                            ret = -Dg / (New Expression(1.0) - g * g) ^ New Expression(0.5)
                        Case "acot"
                            ' acot(z)) = - 1/(1+z^2) * dz

                            ret = -Dg / (New Expression(1.0) + g * g) _
                                        ^ New Expression(0.5)
                        Case "asec"
                            ' D(asec(x)) = 1/(x*sqr(x2-1)) * dx

                            ret = Dg / (g * (g * g - New Expression(1.0)) _
                                            ^ New Expression(0.5))
                        Case "asin"
                            ' D(asin(g)) = 1/sqr(1-g^2) * dg

                            ret = Dg / (New Expression(1.0) - g * g) _
                                            ^ New Expression(0.5)
                        Case "atan"
                            ' D(atan(g)) = 1/(1+g^2) * dg

                            ret = Dg / (New Expression(1.0) + g * g)
                        Case "cosh"
                            ' D(cosh(g)) = sinh(g) * D(g)

                            ret = Dg * Expression.AddFnAndArg0("sinh", g)
                        Case "coth"
                            ' D(coth(z)) = -(csch(z))^2 * D(z)   (z = g)

                            ret = -Dg * Expression.AddFnAndArg0("csch", g)
                        Case "csch"
                            ' D(csch(z)) = -csch(z)*coth(x) * D(z)

                            ret = -Dg * Expression.AddFnAndArg0("csch", g) * _
                                Expression.AddFnAndArg0("coth", g)
                        Case "sech"
                            ' D(sech(z)) = -sech(z)*tanh(z) * D(z)

                            ret = -Dg * Expression.AddFnAndArg0("sech", g) * _
                                Expression.AddFnAndArg0("tanh", g)
                        Case "sinh"
                            ' D(sinh(z)) = cosh(z) * D(z)

                            ret = Dg * Expression.AddFnAndArg0("cosh", g)
                        Case "tanh"
                            ' D(tanh(z)) = (sech(z))^2 * D(z)

                            ret = Dg * Expression.AddFnAndArg0("sech", g) _
                                ^ New Expression(0.5)
                        Case "sqrt"
                            ' D(sqrt(z)) = D( z ^ 0.5) * D(z) =
                            '           = 0.5*D(z)*z^-0.5
                            Dim oneHalf As Expression = New Expression(0.5)
                            ret = oneHalf * Dg * g ^ -oneHalf
                    End Select
                Case 3
                    Select Case sFn
                        Case "sqr"
                            sFn = "sqrt" : GoTo repSelectDeriv
                        Case "cos"
                            ' D(cos(z)) = -sin(z) * D(z)

                            ret = -Dg * Expression.AddFnAndArg0("sin", g)
                        Case "cot"
                            ' D(cot(z)) = -(csc(z))^2 * D(z)

                            Dim cscz2 = Expression.AddFnAndArg0("csc", g) _
                                        ^ New Expression(2.0)
                            ret = -Dg * cscz2
                        Case "csc"
                            ' D(csc(z)) = -csc(z)*cot(z) * D(z)

                            Dim cscg = Expression.AddFnAndArg0("csc", g)
                            Dim cotg = Expression.AddFnAndArg0("cot", g)
                            ret = -Dg * cscg * cotg
                        Case "sec"
                            ' D(sec(z)) =  sec(z)*tan(z) * D(z)

                            Dim secg = Expression.AddFnAndArg0("sec", g)
                            Dim tang = Expression.AddFnAndArg0("tan", g)
                            ret = Dg * secg * tang
                        Case "sin"
                            ' D(sin(z)) = cos(z) * D(z)

                            ret = Dg * Expression.AddFnAndArg0("cos", g)
                        Case "tan"
                            ' D(tan(z)) = (sec(z))^2 * D(z)

                            Dim secg2 = Expression.AddFnAndArg0("sec", g) _
                                        ^ New Expression(2.0)
                            ret = Dg * secg2
                        Case "exp"
                            ' D(exp(z)) = exp(z) * D(z)

                            ret = Dg * Expression.AddFnAndArg0("exp", g)
                        Case "log", "ln"
                            ' D(log(z)) = D(z)/z

                            ret = Dg / g
                        Case "abs"
                            ' D(|u|)=D(sqr(u^2) = (1/2)*(u^2)^(-1/2)*2*u*Du
                            ' = (1/sqr(u^2))*u*Du 
                            ' = (1/|u|)* u*u' =u*u'/abs(u)
                            ' D(abs(g) = g*D(g) /abs(g)
                            ret = g * Dg / Me
                    End Select
                Case 2
                    Select Case sFn
                        Case "ln" : sFn = "log" : GoTo repSelectDeriv
                    End Select
            End Select
        Catch ex As Exception
        Finally
            'If MathGlobal8.detail IsNot Nothing Then
            '    MathGlobal8.detail.Add(sDetail + " = " + eRet.curder.ToString)
            'End If
        End Try
        Return ret
    End Function
    Function opLimit(sVar As String, limit As Expression) As Expression
        Dim ret As Expression = Nothing
        Dim bIsInfinity As Boolean = False
        Dim oVar As New VarsAndFns(cfg)
        Dim rup As New ReduceExprUsingPolynomials
        Dim me2 As New Expression(Me)
        Dim limit2 As New Expression(limit)
        Try
            Try
                Dim PL As Polynomial = limit.getPolynomial
                If Not PL.isComplex AndAlso PL.var(0) = "∞" Then
                    bIsInfinity = True
                End If
                Dim cpy2me As New Expression(rup.ReduceUsingPolynomials(Me))
                CopyExprToMe(cpy2me)
                If limit.IsComplex Then
                    oVar.AddVar(sVar, limit)
                Else
                    If bIsInfinity Then
                        If Me.IsPolynomial Then
                            Dim Pa As Polynomial = Me.getPolynomial
                            If Pa.PolyResto IsNot Nothing Then
                                Dim i, j As Int64
                                Dim jNum As Int64 = Array.IndexOf(Pa.PolyResto.var, sVar)
                                Dim jDen As Int64 = Array.IndexOf(Pa.PolyDivisor.var, sVar)
                                Dim expN As Int64 = 0
                                Dim expD As Int64 = 0
                                Dim cfn As Complex = Complex.zero
                                Dim cfd As Complex = Complex.zero
                                If jNum > -1 Then
                                    For i = 0 To Pa.PolyResto.exp.Length - 1
                                        If Pa.PolyResto.exp(i) IsNot Nothing Then
                                            If j < Pa.PolyResto.exp(i).Length AndAlso _
                                            Pa.PolyResto.exp(i)(j) > expN Then
                                                expN = Pa.PolyResto.exp(i)(j)
                                                cfn = New Complex(Pa.PolyResto.cf(i))
                                            End If
                                        End If
                                    Next
                                End If
                                If jDen > -1 Then
                                    For i = 0 To Pa.PolyDivisor.exp.Length - 1
                                        If Pa.PolyDivisor.exp(i) IsNot Nothing Then
                                            If j < Pa.PolyDivisor.exp(i).Length AndAlso _
                                            Pa.PolyDivisor.exp(i)(j) > expN Then
                                                expD = Pa.PolyDivisor.exp(i)(j)
                                                cfd = New Complex(Pa.PolyDivisor.cf(i))
                                            End If
                                        End If
                                    Next
                                End If
                                If jNum < jDen Then
                                    Pa.PolyResto = Nothing
                                    Pa.PolyDivisor = Nothing
                                    Return New Expression(Pa)
                                ElseIf jNum = jDen Then
                                    Pa.PolyResto = Nothing
                                    Pa.PolyDivisor = Nothing
                                    Pa += New Polynomial(cfn / cfd)
                                    Return New Expression(Pa)
                                End If
                            End If
                        End If
                        If PL.cf(0).pRe.ToDouble < 0.0 Then
                            oVar.AddVar(sVar, New Expression(-Double.MaxValue))
                        Else
                            oVar.AddVar(sVar, New Expression(Double.MaxValue))
                        End If
                    End If
                End If
                ret = evalExprToExpr(oVar)
            Catch ex2 As Exception
                Try
                    CopyExprToMe(me2)
                    limit = New Expression(limit2)
                    If Me.IsPolynomial Then
                        Dim Pa As Polynomial = getPolynomial
                        If Pa.PolyResto IsNot Nothing Then
                            Dim num As New Expression(Pa.PolyResto)
                            Dim den As New Expression(Pa.PolyDivisor)
                            If den.IsComplex Then
                                Throw New ApplicationException("DivideByZeroException")
                            End If
                            ' apply L'Hôpital's rule:
                            num = num.opDeriv(sVar)
                            den = den.opDeriv(sVar)
                            Return (num / den).opLimit(sVar, limit)
                        End If
                    End If
                    If Regex.Match(getMatchStr, _
                            cfg.mathGlobal.sAll2).Groups("fn").Success Then
                        ' if is a function return argument's limit
                        Return Me.Args(0).opLimit(sVar, limit)
                    End If
                    Dim sOp As String = getMatchStr()
                    Select Case sOp
                        Case "/"
                            Dim num As New Expression(Args(0))
                            Dim den As New Expression(Args(1))
                            If den.IsComplex Then
                                Throw New ApplicationException("DivideByZeroException")
                            End If
                            ' apply L'Hôpital's rule:
                            num = num.opDeriv(sVar)
                            den = den.opDeriv(sVar)
                            If den.IsComplex AndAlso Not num.IsComplex AndAlso _
                            bIsInfinity Then
                                Throw New ApplicationException(msg8.num(71)) ' infinity
                            End If
                            Return (num / den).opLimit(sVar, limit)
                        Case "*"
                            Dim num As New Expression(Args(0))
                            Dim den As Expression = New Expression(1.0) / Args(1)
                            ' apply L'Hôpital's rule:
                            num = num.opDeriv(sVar)
                            den = den.opDeriv(sVar)
                            num = rup.ReduceUsingPolynomials(num)
                            den = rup.ReduceUsingPolynomials(den)
                            Try
                                ret = num / den
                                ret = rup.ReduceUsingPolynomials(ret)
                                ret = ret.evalExprToExpr(oVar)
                            Catch ex As Exception
                                num = New Expression(Args(1))
                                den = New Expression(1.0) / Args(0)
                                ' apply L'Hôpital's rule:
                                num = rup.ReduceUsingPolynomials(num.opDeriv(sVar))
                                den = rup.ReduceUsingPolynomials(den.opDeriv(sVar))
                                ret = num / den
                                ret = ret.evalExprToExpr(oVar)
                            End Try
                        Case "^"
                            ' y = a^b
                            ' ln(y) = b*ln(a)
                            ' y = exp(b*ln(a))
                            ' lim y = exp(lim(b*ln(a)) =
                            '       = exp(lim(D(ln(a))/D(1/b)) 
                            If Me.Args(0).IsComplex AndAlso Me.Args(1).IsComplex Then
                                Throw New ApplicationException("DivideByZeroException")
                            End If
                            Dim ln As Expression = Expression.AddFnAndArg0("ln", Me.Args(0))
                            Dim Dln As Expression = ln.opDeriv(sVar)
                            Dim Dinvb As Expression = (New Expression(1.0) / Args(1)).opDeriv(sVar)
                            Dim div As Expression = Dln / Dinvb
                            div = rup.ReduceUsingPolynomials(div)
                            ret = div.opLimit(sVar, limit)
                            ret = Expression.AddFnAndArg0("exp", ret)
                            ret = ret.evalExprToExpr(oVar)
                    End Select
                Catch ex4 As Exception
                    Throw ex4
                End Try
            End Try
        Catch ex As Exception
            Throw ex
        End Try
        Return ret
    End Function
    Public Function opDiff(ByVal respVar As String) As Expression
        Dim ret As Expression = Nothing
        Dim rup As New ReduceExprUsingPolynomials
        Try
            If pA IsNot Nothing Then
                Dim bIsDiv(-1) As Boolean
                Dim vFact() As Expression = Me.exprToFactors(bIsDiv, True)
                ret = New Expression(0.0)
                For i = 0 To vFact.Length - 1
                    Dim curExpr As Expression = Nothing
                    If bIsDiv(i) Then
                        curExpr = New Expression(1.0) / vFact(i)
                    Else
                        curExpr = vFact(i)
                    End If
                    If vFact.Length = 1 Then
                        curExpr = curExpr.getPolynomial.opDiff(respVar)
                    Else
                        curExpr = curExpr.opDiff(respVar)
                    End If
                    For j As Int64 = 0 To vFact.Length - 1
                        If j <> i Then
                            curExpr *= vFact(j)
                        End If
                    Next
                    ret += curExpr
                Next
            ElseIf mOp.Groups("num").Success Then
                ret = New Expression(0.0)
            ElseIf mOp.Groups("var2").Success _
                OrElse mOp.Groups("var").Success Then
                If mOp.ToString <> respVar Then
                    Dim m As Match = Regex.Match("diff", MathGlobal8.sFn)
                    ret = New Expression(m)
                    Dim arg0 As New Expression(Polynomial.GetPolynomial(mOp.ToString))
                    Dim arg1 As New Expression(Polynomial.GetPolynomial(respVar))
                    ret.Args = New Expression() {arg0, arg1}
                    ret.cfg = Me.cfg
                Else
                    ret = New Expression(1.0)
                End If
            ElseIf mOp.Groups("fn").Success Then
                ret = FnDeriv(respVar)
            Else
                'Args(0).cfg = cfg
                'If Args.Length > 1 Then Args(1).cfg = cfg
                Select Case mOp.ToString
                    Case "-"
                        If Args.Length = 2 Then
                            ret = Args(0).opDiff(respVar) - _
                                Args(1).opDiff(respVar)
                        Else
                            ret = Args(0).opDiff(respVar)
                        End If
                    Case "+" 'ret = Args(0).opDiff(respVar) + _
                        'Args(1).opDiff(respVar)
                        Dim dArg0 As Expression = Nothing
                        Dim dArg1 As Expression = Nothing
                        If Args.Length AndAlso _
                        Args(0) IsNot Nothing Then
                            dArg0 = Args(0).opDiff(respVar)
                        End If
                        If Args.Length > 1 AndAlso _
                        Args(1) IsNot Nothing Then
                            dArg1 = Args(1).opDiff(respVar)
                        End If
                        If dArg0 IsNot Nothing AndAlso _
                        dArg1 IsNot Nothing Then
                            ret = dArg0 + dArg1
                        ElseIf dArg0 IsNot Nothing Then
                            ret = dArg0
                        ElseIf dArg1 IsNot Nothing Then
                            ret = dArg1
                        Else
                            ret = New Expression(0.0)
                        End If
                    Case "*"
                        Dim f As Expression = Args(0)
                        Dim g As Expression = Args(1)
                        Dim Df As Expression = Args(0).opDiff(respVar)
                        Dim Dg As Expression = Args(1).opDiff(respVar)
                        ret = Df * g + f * Dg
                    Case "/"
                        Dim f As Expression = Args(0)
                        Dim g As Expression = Args(1)
                        Dim Df As Expression = Args(0).opDiff(respVar)
                        Dim Dg As Expression = Args(1).opDiff(respVar)
                        Dim varG() As String = Nothing
                        g.getAllVars(varG)
                        Dim varF() As String = Nothing
                        f.getAllVars(varF)
                        If varG Is Nothing OrElse _
                        varG.Length = 0 OrElse _
                        Array.IndexOf(varG, respVar) = -1 Then
                            ret = Df / g
                        ElseIf varF Is Nothing OrElse _
                        varF.Length = 0 OrElse _
                        Array.IndexOf(varF, respVar) = -1 Then
                            ret = -f * Dg / (g * g)
                        Else
                            Dim num As Expression = (Df * g - f * Dg)
                            Dim den As Expression = g ^ New Expression(2.0) 'g * g
                            If num.IsPolynomial AndAlso den.IsPolynomial Then
                                Dim pNum As Polynomial = num.getPolynomial
                                Dim pDen As Polynomial = den.getPolynomial
                                Dim gcd As Polynomial = _
                                    Polynomial.opGcd(pNum, pDen)
                                ret = New Expression((pNum / gcd) / (pDen / gcd))
                            Else
                                ret = num / den
                            End If
                        End If

                    Case "^"
                        Dim f As Expression = Args(0)
                        Dim g As Expression = Args(1)
                        If f.IsPolynomial AndAlso _
                        g.IsPolynomial AndAlso g.IsReal Then
                            Dim pRe As Rational = g.pA.An.pRe
                            Dim pReMnOne As Rational = pRe - New Rational(1.0)
                            Dim cjoRe As New Complex(pRe, New Rational(0.0))
                            Dim cjoReMnOne As New Complex(pReMnOne, New Rational(0.0))
                            ret = New Expression(cjoRe) * _
                                f.opDiff(respVar) * f ^ New Expression(cjoReMnOne)
                        Else
                            Dim Dg As Expression = Args(1).opDiff(respVar)
                            If False AndAlso Args(0).IsReal AndAlso _
                            Args(0).toDouble = Math.E Then
                                ' D(exp(g))= D(g)*exp(g)
                                ret = Dg * f
                            Else
                                ' y = f^g
                                ' ln(y) = g*ln(f)
                                ' D(  ) = D (  ) = Dg*ln(f) + g*Df/ f
                                ' D(ln(y)) = Dy/y
                                ' Dy/y = Dg*ln(f) + g*Df / f
                                ' Dy = y*[ ] = f^g * [ ]
                                ' D(f^g)= f^g * [ ] 
                                ' D(f^g)= f^g * [Dg*ln(f) + g*Df / f]
                                Dim Df As Expression = Args(0).opDiff(respVar)
                                Dim lnf As Expression = Nothing
                                If f.IsReal AndAlso f.toDouble = Math.E Then
                                    lnf = New Expression(1.0)
                                Else
                                    lnf = Expression.AddFnAndArg0("ln", f)
                                End If
                                If Df Is Nothing OrElse _
                                (Df.IsReal AndAlso Df.toDouble = 0.0) Then
                                    ret = (f ^ g) * Dg * lnf
                                ElseIf Dg Is Nothing OrElse _
                                (Dg.IsReal AndAlso Dg.toDouble = 0.0) Then
                                    ret = g * Df * f ^ (g - New Expression(1.0))
                                Else
                                    ret = f ^ g * (Dg * lnf + g * Df / f)
                                End If
                            End If
                        End If
                End Select
            End If
        Catch ex As Exception
            Throw ex
        End Try
        If Me.Args.Length = 2 Then
            If Me.Args(1) Is Nothing Then
                ReDim Preserve Me.Args(0)
                mOp = Nothing
            ElseIf Me.Args(0) Is Nothing Then
                Me.Args(0) = Me.Args(1)
                ReDim Preserve Me.Args(0)
                mOp = Nothing
            End If
        End If
        If sign = -1 Then
            ret = -ret
        End If
        ret.cfg = cfg
        Return ret
        'Return rup.ReduceUsingPolynomials(ret)
        'Dim rup As New ReduceExprWithPolynomials
        'Return rup.ReduceUsingPolynomials(ret)
    End Function

#End Region
End Class
