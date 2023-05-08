Partial Public Class Expression
    Public Function Derivative(respVar As String) As Expression
        Dim eC As Expression = Expression.CopyFrom(Me)
        Try
            eC = ReduceExprUsingPolynomials.TryToReduce(eC)
            Return eC.DerivativeB(respVar)
        Catch ex As Exception
            Throw
        End Try
    End Function
    Private Function DerivativeB(respVar As String) As Expression
        Dim eC As New Expression(0.0)
        Try
            Dim eA As Expression = CopyFrom(Me)
            For i As Int32 = 0 To eA.t.Count - 1
                For j As Int32 = 0 To eA.t(i).f.Count - 1
                    Dim eD As Expression = Derivative(eA.t(i), j, respVar)
                    'eD *= New Expression(eA.t(i))
                    eC += eD
                Next
            Next
            If eA.resto IsNot Nothing AndAlso eA.resto.Count Then
                Dim n As New Expression(eA.resto)
                Dim d As New Expression(eA.divisor)
                Dim dn As Expression = n.DerivativeB(respVar)
                Dim dd As Expression = d.DerivativeB(respVar)
                Dim eD As Expression = (d * dn - n * dd) / (d * d)
                eC += eD
            End If
            eC.Reduce()
        Catch ex As Exception
            Throw
        End Try
        Return eC
    End Function
    Public Shared Function Derivative(tA As ExprTerm, i As Int32, respVar As String) As Expression
        ' D(a*b)=Da*b+a*Db
        Dim eC As Expression
        Try
            If tA.f(i).var.GetType IsNot GetType(String) Then
                ' D (f^exp) = exp * f^(exp-1) * D f
                Dim f As New Expression(CType(tA.f(i).var, Expression))
                Dim Df As Expression = f.Derivative(respVar)
                If Not tA.f(i).exp.IsDouble Then
                    Throw New Exception(Msg8.num(13))
                End If
                Dim exp As Expression = New Expression(tA.f(i).exp.ToDouble)
                eC = Df * exp * f ^ New Expression(exp.ToDouble - 1)
            Else
                eC = Derivative(tA.f(i), respVar) * New Expression(tA.Cf)
            End If
            For j As Int32 = 0 To tA.f.Count - 1
                If i <> j Then
                    If tA.f(j).args.Count Then
                        eC *= New Expression(tA.f(j).var, tA.f(j).exp, tA.f(j).args(0))
                    ElseIf tA.f(j).var.GetType IsNot GetType(String) Then
                        Dim expr As New Expression(CType(tA.f(j).var, Expression))
                        expr ^= New Expression(tA.f(j).exp)
                        eC *= expr
                    Else
                        eC *= New Expression(tA.f(j).var, tA.f(j).exp, Nothing)
                    End If
                End If
            Next
        Catch ex As Exception
            Throw
        End Try
        Return eC
    End Function
    Public Shared Function Derivative(fA As ExprFactor, respVar As String) As Expression
        Dim ret As Expression = Nothing
        Try
            Dim vVarsExp() As String = fA.exp.GetVars
            Dim vVarsArgs(-1) As String
            If fA.args.Count = 1 Then
                vVarsArgs = fA.args(0).GetVars
            End If
            Dim i As Int32 = vVarsExp.Length
            Dim j As Int32 = vVarsArgs.Length
            If i + j = 0 Then
                If fA.var.ToString <> respVar Then
                    ret = New Expression(0.0)
                    Exit Try
                ElseIf fA.exp.IsComplex Then
                    ' D( x^num) = num * x ^ (num - 1)
                    If fA.exp.IsDouble AndAlso fA.exp.ToDouble = 1 Then
                        Dim t As New ExprTerm(fA.exp.ToComplex)
                        't.f.Add(New ExprFactor(respVar, New Expression(fA.exp.ToComplex - Complex.one)))
                        ret = New Expression(t)
                    Else
                        Dim t As New ExprTerm(fA.exp.ToComplex)
                        t.f.Add(New ExprFactor(respVar, New Expression(fA.exp.ToComplex - Complex.one)))
                        ret = New Expression(t)
                    End If
                Else
                    ' se supone que el exponente no tiene vars. luego es una constante
                    Throw New ArgumentOutOfRangeException
                End If
            ElseIf j = 0 Then
                ' D(f^g)
                ' y = f^g
                ' ln(y) = g*ln(f)
                ' D(  ) = D (  ) = Dg*ln(f) + g*Df/ f
                ' D(ln(y)) = Dy/y
                ' Dy/y = Dg*ln(f) + g*Df / f
                ' Dy = y*[ ] = f^g * [ ]
                ' D(f^g)= f^g * [ ] 
                ' D(f^g)= f^g * [Dg*ln(f) + g*Df / f]
                Dim f As New Expression(fA.var.ToString, 1)
                Dim g As Expression = fA.exp
                Dim Dg As Expression = g.DerivativeB(respVar)
                Dim lnf As New Expression(0.0) ' = Expression.AddArg("ln", 1)
                Dim fexpG As Expression = f ^ g
                Dim Df As Expression
                If fA.var.ToString = respVar Then
                    Df = New Expression(1.0)
                Else
                    Df = New Expression(0.0)
                End If
                ret = fexpG * (Dg * lnf + g * Df / f)

            ElseIf Not (fA.exp.IsDouble AndAlso fA.exp.ToDouble = 1) Then
                ' D((foh)^g)
                ' = (foh)^g * [Dg*ln(foh) + g*D(foh) / F]
                ' 
                Dim g As Expression = fA.exp
                Dim Dg As Expression = g.DerivativeB(respVar)
                Dim t As New ExprTerm(respVar, 1)
                Dim f As New Expression(fA.var, New Expression(1.0), New Expression(t))
                Dim foh As New Expression(fA.var, New Expression(1.0), fA.args(0))
                Dim h As New Expression(fA.args(0))
                Dim Dh As Expression = h.DerivativeB(respVar)
                Dim lnfoh As Expression = Expression.AddArg("ln", foh)
                Dim fexpG As Expression = foh ^ g

                Dim fact As New ExprFactor(respVar)
                Dim term As New ExprTerm(fact)
                Dim Df As Expression = f.DerivativeB(respVar)
                Dim Dfoh As Expression = foh.DerivativeB(respVar) '= CopyFrom(Df)
                If Dg.IsDouble AndAlso Dg.ToDouble = 0 Then
                    ' D((foh)^g)= (foh)^g*(0 +g*D(foh)/F) =
                    ' = (F^g/foh)*g*D(foh)
                    ' = (foh)^(g-1)*g*D(foh)
                    ret = foh ^ (g - New Expression(1.0)) * g * Dfoh
                Else
                    ' =  (foh)^g * [Dg*ln(foh) + g*D(foh) / foh]
                    ret = foh ^ g * (Dg * lnfoh + g * Dfoh / foh)
                End If
            Else
                ' ...
                ' ...

                Dim foh As New Expression(fA.var, fA.exp, fA.args(0))
                Dim g As Expression = fA.args(0)
                Dim Dg As Expression = g.DerivativeB(respVar)
                'Dim lnf As New Expression("ln", 1)
                'Dim fexpG As Expression = foh ^ g
                Dim dArg As Expression = fA.args(0).DerivativeB(respVar)
                Dim sFn As String = fA.var.ToString
                Select Case Len(sFn)
                    Case 6
                        Select Case sFn
                            Case "logtwo"
                                ' log2(g) = ln(g)/ln(2)
                                ' D(ln(g)) = Dg/g
                                ' D(log2(g))=D(ln(g)/ln(2))=Dg/(g*ln(2))
                                Dim ln2 As Expression = Expression.AddArg("ln", New Expression(2.0))
                                ret = Dg / (g * ln2)
                            Case "logten"
                                ' log10(g) = ln(g)/ln(10)
                                ' D(ln(g)) = Dg/g
                                ' D(log10(g))=D(ln(g)/ln(10))=Dg/(g*ln(10))

                                Dim ln10 As Expression = Expression.AddArg("ln", New Expression(10.0))
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
                                Dim absg As Expression = Expression.AddArg("abs", g)
                                ret = -Dg / (absg * onePlusg2 _
                                                          ^ New Expression(0.5))

                            Case "asech"
                                '  D(asech(z)) = -Dz/(abs(z)*sqr(1+z^2))   (z = g)

                                Dim onePlusg2 As Expression = New Expression(1.0) + g * g
                                Dim absg As Expression = Expression.AddArg("abs", g)
                                ret = -Dg / (absg * onePlusg2 ^ New Expression(0.5))

                            Case "asinh"
                                ' D(asinh(z)) = dz/sqr(1+z^2)    (z = g)

                                Dim onePlusg2 As Expression =
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
                                Dim absg As Expression = Expression.AddArg("abs", g)
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

                                ret = Dg * Expression.AddArg("sinh", g)
                            Case "coth"
                                ' D(coth(z)) = -(csch(z))^2 * D(z)   (z = g)

                                ret = -Dg * Expression.AddArg("csch", g)
                            Case "csch"
                                ' D(csch(z)) = -csch(z)*coth(x) * D(z)

                                ret = -Dg * Expression.AddArg("csch", g) *
                                Expression.AddArg("coth", g)
                            Case "sech"
                                ' D(sech(z)) = -sech(z)*tanh(z) * D(z)

                                ret = -Dg * Expression.AddArg("sech", g) *
                                Expression.AddArg("tanh", g)
                            Case "sinh"
                                ' D(sinh(z)) = cosh(z) * D(z)

                                ret = Dg * Expression.AddArg("cosh", g)
                            Case "tanh"
                                ' D(tanh(z)) = (sech(z))^2 * D(z)

                                ret = Dg * Expression.AddArg("sech", g) _
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
                                ' D(sqrt(z)) = D( z ^ 0.5) * D(z) =
                                '           = 0.5*D(z)*z^-0.5
                                Dim oneHalf As Expression = New Expression(0.5)
                                ret = oneHalf * Dg * g ^ -oneHalf
                            Case "cos"
                                ' D(cos(z)) = -sin(z) * D(z)

                                ret = -Dg * Expression.AddArg("sin", g)
                            Case "cot"
                                ' D(cot(z)) = -(csc(z))^2 * D(z)

                                Dim cscz2 = Expression.AddArg("csc", g) _
                                        ^ New Expression(2.0)
                                ret = -Dg * cscz2
                            Case "csc"
                                ' D(csc(z)) = -csc(z)*cot(z) * D(z)

                                Dim cscg = Expression.AddArg("csc", g)
                                Dim cotg = Expression.AddArg("cot", g)
                                ret = -Dg * cscg * cotg
                            Case "sec"
                                ' D(sec(z)) =  sec(z)*tan(z) * D(z)

                                Dim secg = Expression.AddArg("sec", g)
                                Dim tang = Expression.AddArg("tan", g)
                                ret = Dg * secg * tang
                            Case "sin"
                                ' D(sin(z)) = cos(z) * D(z)

                                ret = Dg * Expression.AddArg("cos", g)
                            Case "tan"
                                ' D(tan(z)) = (sec(z))^2 * D(z)

                                Dim secg2 = Expression.AddArg("sec", g) _
                                        ^ New Expression(2.0)
                                ret = Dg * secg2
                            Case "exp"
                                ' D(exp(z)) = exp(z) * D(z)

                                ret = Dg * Expression.AddArg("exp", g)
                            Case "log", "ln"
                                ' D(log(z)) = D(z)/z

                                ret = Dg / g
                            Case "abs"
                                ' D(|u|)=D(sqr(u^2) = (1/2)*(u^2)^(-1/2)*2*u*Du
                                ' = (1/sqr(u^2))*u*Du 
                                ' = (1/|u|)* u*u' =u*u'/abs(u)
                                ' D(abs(g) = g*D(g) /abs(g)
                                Dim absg As New Expression(g)
                                If absg.t.Count > 1 Then
                                    Throw New ArgumentOutOfRangeException
                                End If
                                For i = 0 To absg.t.Count - 1
                                    If Not absg.t(i).IsDouble Then
                                        Throw New ArgumentOutOfRangeException
                                    End If
                                    absg.t(i).Cf = New Complex(Math.Abs(absg.t(i).Cf.ToDouble))
                                Next
                                ret = g * Dg / absg
                        End Select
                    Case 2
                        Select Case sFn
                            Case "ln" : ret = Dg / g
                        End Select
                End Select

            End If
            ret.Reduce()
        Catch ex As Exception
            Throw
        End Try
        Return ret
    End Function
End Class
