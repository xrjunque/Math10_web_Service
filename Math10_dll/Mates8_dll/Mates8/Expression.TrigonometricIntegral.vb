Partial Public Class Expression
#Region "trigonometricIntegrals"
#Region "sinN * cosM"

    Public Function Is_sinN_cosM(IRespVar As String, _
                            ByRef Nsin As Int64, _
                             ByRef Mcos As Int64, _
                             ByRef arg As Expression, _
                             ByRef mult As Double, _
                             Optional bAdmitNegExp As Boolean = False) _
                            As Boolean

        ' If 'me' is a sin(x)^N * cos(x)^M type expression
        ' this function should return "True"
        Dim bRet As Boolean = False
        Dim mult2 As Double
        Try
            Dim bIsDiv() As Boolean = Nothing
            Dim vFac() As Expression = Me.exprToFactors(bIsDiv)
            Dim x As New Expression(Polynomial.GetPolynomial(IRespVar))
            Nsin = 0
            Mcos = 0
            mult = 1
            Dim arg2 As Expression = Nothing
            For i As Int64 = 0 To vFac.Length - 1
                If bIsDiv(i) Then
                    If Not vFac(i).IsReal Then
                        Exit Try
                    Else
                        mult /= vFac(i).pA.cf(0).pRe.ToDouble
                    End If
                Else
                    If vFac(i).extractFromPath(New String() {"sin", "0"}, arg) Then
                        Dim var() As String = Nothing
                        arg.getAllVars(var)
                        If var.Length AndAlso _
                        Array.IndexOf(var, IRespVar) > -1 Then ' arg contains var. IRespVar ?
                            If arg2 IsNot Nothing AndAlso _
                            Not arg2.isEqualTo(arg) Then
                                Exit Try
                            End If
                            arg2 = arg
                            If vFac(i).isEqualTo(Expression.AddFnAndArg0("sin", arg), mult2, "*") Then
                                Nsin += 1
                            End If
                            mult *= mult2
                        End If
                    ElseIf vFac(i).extractFromPath(New String() {"cos", "0"}, arg) Then
                        Dim var() As String = Nothing
                        arg.getAllVars(var)
                        If var.Length AndAlso _
                        Array.IndexOf(var, IRespVar) > -1 Then ' arg contains var. IRespVar ?
                            If arg2 IsNot Nothing AndAlso _
                            Not arg2.isEqualTo(arg) Then
                                Exit Try
                            End If
                            arg2 = arg
                            If vFac(i).isEqualTo(Expression.AddFnAndArg0("cos", arg), mult2, "*") Then
                                Mcos += 1
                            End If
                            mult *= mult2
                        End If
                    ElseIf Not vFac(i).IsReal Then
                        Exit Try
                    Else
                        mult *= vFac(i).pA.cf(0).pRe.ToDouble
                    End If
                End If
            Next
            If Nsin > 1 OrElse Mcos > 1 Then
                bRet = True
            ElseIf Nsin = 1 AndAlso Mcos = 1 Then
                bRet = True
            ElseIf bAdmitNegExp AndAlso _
            Math.Abs(Nsin) > 1 OrElse _
            Math.Abs(Mcos) > 1 Then
                bRet = True
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return bRet
    End Function
    Public Function integralSinNCosM(cfg As Config, _
                            IRespVar As String, _
                            vExprParams() As Expression, _
                            cur As currentMatch, _
                            vars As VarsAndFns, _
                            ByRef result As Expression, _
                            Optional bAdmitNegExp As Boolean = False) _
                             As Boolean
        Dim bRet As Boolean = False
        Dim mult As Double = 1.0
        Dim varId As Int64 = vars.getVarIDByName(IRespVar)
        Dim bDetail As Boolean = cfg.bDetail
        Try
            cfg.bDetail = False
            Dim Nsin As Int64 = 0
            Dim Mcos As Int64 = 0
            Dim arg As Expression = Nothing
            If Not vExprParams(0).Is_sinN_cosM(IRespVar, _
                Nsin, Mcos, arg, mult, bAdmitNegExp) Then
                ' expression's form is not sin(x)^ N * cos(x)^ M
                Exit Try
            End If
            If Nsin = 1 Then
                mult /= (Mcos + 1)
                result = _
                    Expression.AddFnAndArg0("cos", arg) ^ _
                    New Expression(Mcos + 1)
                bRet = True
                Exit Try
            ElseIf Mcos = 1 Then
                mult /= (Nsin + 1)
                result = _
                     Expression.AddFnAndArg0("sin", arg) ^ _
                    New Expression(Nsin + 1)
                bRet = True
                Exit Try
            End If
            Dim sU As String = "__" + IRespVar
            Dim u As New Expression(Polynomial.GetPolynomial(sU))
            Dim one As New Expression(1.0)
            Dim oVars2 As New VarsAndFns(vars)
            Dim msg As String = ""
            If Nsin Mod 2 Then
                ' 1) N = 2*k+1 impar (odd):

                ' sin^N * cos^M =
                ' = sin^2k * sin * cos^M
                ' = { sin(x)^2 = 1-cos(x)^2 }
                ' = (1-cos^2)^k * sin*cos^M
                ' tomando u=cos(x) --> du= - sin(x)dx
                ' = - ∫[(1-u2)^k * u^m ]du
                Dim k As Int64 = (Nsin - 1) / 2

                ' retreive - ∫[(1-u2)^k * u^m ]du 
                Dim eK As New Expression(k)
                Dim eM As New Expression(Mcos)
                Dim integrand As Expression = -((one - u * u) ^ eK * u ^ eM)
                vExprParams(0) = integrand
                oVars2.AddVar(sU, Nothing, msg)
                Dim Id_sU As Int64 = oVars2.getVarIDByName(sU)

                Dim uIntegral As Expression = _
                    integrand.opAntiDerivative( _
                     cfg, Id_sU, vExprParams, cur, oVars2, sU, Nothing, Nothing, Nothing)
                Dim eMtx As New ExprMatrix( _
                    Expression.AddFnAndArg0("cos", arg))
                ' set  cos(x) in 'u' variable:
                oVars2.setValue(Id_sU, eMtx)

                ' finally, replace 'u' by original cos(x) in uIntegral:
                result = uIntegral.evalExprToExpr(oVars2)
                bRet = True
            ElseIf Mcos Mod 2 Then
                ' 2) M = 2*k+1 impar (odd):

                ' sin^N * cos^M =
                ' = sin^N * cos(x)*(cos(x)^2)^k 
                ' = sin^N *cos(x)*(1 -sin(x)^2)^k
                ' = { performing the substitution: u=sin(x) --> du= cos(x)dx }
                ' = ∫ u^n*(1-u^2)^k du
                Dim k As Int64 = (Mcos - 1) / 2
                Dim eK As New Expression(k)
                Dim eN As New Expression(Nsin)
                ' = ∫ u^n*(1-u^2)^k du
                Dim integrand As Expression = u ^ eN * (one - u * u) ^ eK
                vExprParams(0) = integrand
                oVars2.AddVar(sU, Nothing, msg)
                Dim Id_sU As Int64 = oVars2.getVarIDByName(sU)

                ' obtain the antiderivative of 'integrand':
                Dim uIntegral As Expression = _
                    integrand.opAntiDerivative( _
                     cfg, Id_sU, vExprParams, cur, oVars2, sU, Nothing, Nothing, Nothing)
                Dim eMtx As New ExprMatrix( _
                    Expression.AddFnAndArg0("sin", New Expression( _
                                        Polynomial.GetPolynomial(IRespVar))))
                ' set  sin(x) as value for 'u', this will allow...
                oVars2.setValue(Id_sU, eMtx)

                ' ...to replace finally 'u' by the original sin(x) in uIntegral:
                result = uIntegral.evalExprToExpr(oVars2)
                bRet = True
            Else
                ' 3) N,M both even
                ' = ∫ sin(x)^N * cos(x)^M
                ' = ∫(sin(x)^2)^p *(cos(x)^2)^k
                ' = ∫[0.5-0.5cos(2x)]^p * [(0.5+0.5cos(2x))^k] dx
                Dim p As Int64 = Nsin / 2
                Dim eP As New Expression(p)
                Dim k As Int64 = Mcos / 2
                Dim eK As New Expression(k)
                Dim half As New Expression(0.5)
                Dim two As New Expression(2.0)
                Dim cos_2x As Expression = _
                    Expression.AddFnAndArg0("cos", two * arg)
                Dim bIsMinusSignA() As Boolean = Nothing

                ' sin(x)^2 = 0.5 - 0.5 cos(2x):
                Dim integrand1 As New Expression(half - half * cos_2x)
                Dim vSummandsA() As Expression = Nothing
                If p Then
                    For i As Int64 = 2 To p
                        vSummandsA = _
                                integrand1.exprToSummands(bIsMinusSignA)
                        Dim lenvs As Int64 = vSummandsA.Length
                        ReDim Preserve vSummandsA(2 * lenvs - 1)
                        Array.Copy(vSummandsA, 0, vSummandsA, _
                                  lenvs, lenvs)
                        For j As Int64 = 0 To lenvs - 1
                            vSummandsA(j) *= half
                            vSummandsA(j + lenvs) *= -half * cos_2x
                        Next
                        integrand1 = New Expression(0.0)
                        For j As Int64 = 0 To lenvs - 1
                            If bIsMinusSignA(j) Then
                                integrand1 -= (vSummandsA(j) + vSummandsA(j + lenvs))
                            Else
                                integrand1 += (vSummandsA(j) + vSummandsA(j + lenvs))
                            End If
                        Next
                        integrand1.reduceSummands()
                    Next
                Else
                    integrand1 = New Expression(0.0)
                End If
                ' cos(x)^2 = 0.5 + 0.5 cos(2x)
                Dim integrand2 As New Expression(half + half * cos_2x)
                Dim vSummandsB() As Expression = Nothing
                Dim bIsMinusSignB() As Boolean = Nothing
                If k Then
                    For i As Int64 = 2 To k
                        vSummandsB = _
                                integrand2.exprToSummands(bIsMinusSignB)
                        Dim lenvs As Int64 = vSummandsB.Length
                        ReDim Preserve vSummandsB(2 * lenvs - 1)
                        Array.Copy(vSummandsB, 0, vSummandsB, _
                                  lenvs, lenvs)
                        For j As Int64 = 0 To lenvs - 1
                            vSummandsB(j) *= half
                            vSummandsB(j + lenvs) *= half * cos_2x
                        Next
                        integrand2 = New Expression(0.0)
                        For j As Int64 = 0 To lenvs - 1
                            If bIsMinusSignB(j) Then
                                integrand2 -= (vSummandsB(j) + vSummandsB(j + lenvs))
                            Else
                                integrand2 += (vSummandsB(j) + vSummandsB(j + lenvs))
                            End If
                        Next
                    Next
                End If
                If p AndAlso k Then
                    vSummandsA = _
                            integrand1.exprToSummands(bIsMinusSignA)
                    vSummandsB = _
                            New Expression(half + _
                            half * cos_2x).exprToSummands(bIsMinusSignB)
                    Dim lenA As Int64 = vSummandsA.Length
                    Dim lenB As Int64 = vSummandsB.Length
                    ReDim Preserve vSummandsA(lenA * lenB - 1)
                    For i = 1 To lenB - 1
                        Array.Copy(vSummandsA, 0, vSummandsA, _
                                  lenA * i, lenA)
                    Next
                    For i = 0 To lenB - 1
                        For j = 0 To lenA - 1
                            vSummandsA(j + lenA * i) *= _
                                vSummandsB(i)
                        Next
                    Next
                    integrand1 = New Expression(0.0)
                    For j As Int64 = 0 To vSummandsA.Length - 1
                        integrand1 += vSummandsA(j)
                    Next
                    integrand1.reduceSummands()
                    integrand2 = New Expression(0.0)
                End If
                If p = 0 Then
                    vExprParams(0) = integrand2
                ElseIf k = 0 Then
                    vExprParams(0) = integrand1
                Else
                    vExprParams(0) = integrand1
                End If
                ' obtain, recursively (until p or k are =1 or odd), the integral:
                result = _
                    vExprParams(0).opAntiDerivative( _
                     cfg, varId, vExprParams, cur, vars, IRespVar, Nothing, Nothing, Nothing)
                bRet = True
            End If
        Catch ex As Exception
            Throw ex
        End Try
        If result IsNot Nothing Then
            mult *= Me.sign
            result *= New Expression(mult)
            result = result.convertSecCscCot(True)
        End If
        cfg.bDetail = bdetail
        Return bRet
    End Function

#End Region

#Region "secN tanM"
    Public Function Is_secN_tanM(IRespVar As String, _
                            ByRef Nsec As Int64, _
                             ByRef Mtan As Int64, _
                             ByRef arg As Expression, _
                             ByRef mult As Double) As Boolean

        ' If 'me' is a sec(x)^N * tan(x)^M type expression
        ' this function should return "True"
        ' sec(x)^N * tan(x)^M = sin(x)^M /(cos^M * cos^N)
        Dim bRet As Boolean = False
        Dim mult2 As Double
        Try
            Dim bIsDiv() As Boolean = Nothing
            Dim vFac() As Expression = Me.exprToFactors(bIsDiv)
            Dim x As New Expression(Polynomial.GetPolynomial(IRespVar))
            Dim Nsin As Int64 = 0
            Dim Mcos As Int64 = 0
            mult = 1
            Dim arg2 As Expression = Nothing
            For i As Int64 = 0 To vFac.Length - 1
                If bIsDiv(i) Then
                    If vFac(i).extractFromPath(New String() {"cos", "0"}, arg) Then
                        If arg2 IsNot Nothing AndAlso _
                        Not arg2.isEqualTo(arg) Then
                            Exit Try
                        End If
                        arg2 = arg
                        If vFac(i).isEqualTo(Expression.AddFnAndArg0("cos", arg), mult2, "*") Then
                            Mcos += 1
                        End If
                        mult *= mult2
                    ElseIf vFac(i).IsReal Then
                        mult /= vFac(i).pA.cf(0).pRe.ToDouble
                    Else
                        Exit Try
                    End If
                Else
                    If vFac(i).extractFromPath(New String() {"sin", "0"}, arg) Then
                        If arg2 IsNot Nothing AndAlso _
                        Not arg2.isEqualTo(arg) Then
                            Exit Try
                        End If
                        arg2 = arg
                        If vFac(i).isEqualTo(Expression.AddFnAndArg0("sin", arg), mult2, "*") Then
                            Nsin += 1
                        End If
                        mult *= mult2
                    ElseIf Not vFac(i).IsReal Then
                        Exit Try
                    Else
                        mult *= vFac(i).pA.cf(0).pRe.ToDouble
                    End If
                End If
            Next
            If Nsin <= Mcos Then
                Nsec = Mcos - Nsin
                Mtan = Nsin
                If Nsec > 0 OrElse Mtan > 1 Then
                    bRet = True
                End If
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return bRet
    End Function
    Public Function integralSecNtanM( _
                        cfg As Config, _
                        IRespVar As String, _
                        vExprParams() As Expression, _
                        cur As currentMatch, _
                        vars As VarsAndFns, _
                        ByRef result As Expression, _
                        ByRef origAntideriv() As Expression, _
                        ByRef bFoundOrigAntideriv() As Boolean _
                        ) As Boolean
        Dim bRet As Boolean = False
        Dim mult As Double = 1.0
        Try
            Dim Msec As Int64 = 0
            Dim Ntan As Int64 = 0
            Dim arg As Expression = Nothing
            Dim varId As Int64 = vars.getVarIDByName(IRespVar)
            If Not vExprParams(0).Is_secN_tanM(IRespVar, Msec, Ntan, arg, mult) Then
                ' expression's form is not sin(x)^ N * cos(x)^-N * cos(x)^-M
                Exit Try
            End If
            If Msec > 4 Then
                ' Algorithmic slow and not confident
                ' for Nsec>4 (exponents should be
                ' deflated first)
                ' Throw New Exception(msg8.num(13)) ' n/a 
            End If

            '  N  |  M  |
            '--------------------------------------------------
            '  1  |  0  |  ∫ tan = ln(|sec|) + K
            '  0  |  1  |  ∫ sec = ln(|sec + tan |) + K     (*)
            '  1  |  1  |  ∫ sec tan = sec + K
            '  0  |  2  |  ∫ (sec)^2 = tan + K
            '  0  | M>2 |  Recursive: Im = (sec)^(m-2)*tan/(m-1) + ((n-2)/(n-1)) * Im-2
            '              where Im = ∫(sec)^M and Im-2=∫(sec)^(m-2)
            ' N>=2|  0  |  Recursive: I(n) = (tan)^(n-2)/(n-1) - I(n-2)  ( I(n-2)=∫(tan)^(n-2) )
            ' N>0 |Meven|  Take apart sec^2 subs.by (1+(tan)^2) take u=tan,
            '              then du=(sec)^2*dx. Resolve by substitution.
            'Neven| M>0 |  N even= 2*k, take (tan)^N=(tan^2)^k=(sec^2-1)^k and there'll
            '              be only sumands of ∫(sec)^i. See (*) above.
            ' Nodd| M>0 |  Take apart sec*tan, subs.(tan)^(2*k) by (sec2-1)^k.
            '              Resolve by substitution, consider u=sec so du=sec*tan.

            If Msec = 0 AndAlso Ntan = 1 Then
                '  N  |  M  |
                '  1  |  0  |  ∫ tan = ln(|sec|) + K
                result = Expression.AddFnAndArg0("sec", arg)
                result = Expression.AddFnAndArg0("abs", result)
                result = Expression.AddFnAndArg0("ln", result)
                bRet = True
            ElseIf Ntan = 0 AndAlso Msec = 1 Then
                '  N  |  M  |
                '  0  |  1  |  ∫ sec = ln(|sec + tan |) + K     (*)
                result = Expression.AddFnAndArg0("sec", arg) + Expression.AddFnAndArg0("tan", arg)
                result = Expression.AddFnAndArg0("abs", result)
                result = Expression.AddFnAndArg0("ln", result)
                bRet = True
            ElseIf Ntan = 1 AndAlso Msec = 1 Then
                '  N  |  M  |
                '  1  |  1  |  ∫ sec tan = sec + K
                result = Expression.AddFnAndArg0("sec", arg)
                bRet = True
            ElseIf Ntan = 0 AndAlso Msec = 2 Then
                '  N  |  M  |
                '  0  |  2  |  ∫ (sec)^2 = tan + K
                result = Expression.AddFnAndArg0("tan", arg)
                bRet = True
            ElseIf Ntan = 0 Then
                '  N  |  M  |
                '  0  | M>2 |  Recursive: Im = (sec)^(m-2)*tan/(m-1) + ((n-2)/(n-1)) * Im-2
                '              where Im = ∫(sec)^M and Im-2=∫(sec)^(m-2)
                If Msec Mod 2 = 0 Then
                    ' Msec even (4,6,... Msec={0,2} solved before)
                    ' I(m=2) = ∫ (sec)^2 = tan + K
                    ' I(m=4) = (sec)^2*tan/3 + 2/3*I(m=2)
                    ' ...
                    ' Im = Im = (sec)^(m-2)*tan/(m-1) + ((n-2)/(n-1)) * Im-2
                    If cfg.bDetail Then
                        cfg.oDetail.AddAlways("Applying recursive formula: ")
                        cfg.oDetail.AddAlways("Im = (sec)^(m-2)*tan/(m-1) + ((n-2)/(n-1)) * Im-2")
                        cfg.oDetail.AddAlways("See for ex. ""Cálculo Integral 15: Integrales Secante Tangente"" ")
                        Dim sAddr As String = "https://www.youtube.com/watch?v=Dw_HsCUPhJw"
                        cfg.oDetail.AddAlways("<a target=""_new"" href=""" + sAddr + """>" + sAddr + "</a>")
                        cfg.oDetail.AddAlways("Here m is even, so:")
                        cfg.oDetail.AddAlways("I(m=2) = ∫ (sec)^2 = tan + K")
                        cfg.oDetail.AddAlways("I(m=4) = (sec)^2*tan/3 + 2/3*I(m=2) ")
                        cfg.oDetail.AddAlways(" ...")
                        cfg.oDetail.AddAlways("Im = (sec)^(m-2)*tan/(m-1) + ((n-2)/(n-1)) * Im-2")

                    End If
                    result = Expression.AddFnAndArg0("tan", arg)
                    Dim m As Int64 = 4
                    Do While m <= Msec
                        If cfg.bDetail Then
                            cfg.oDetail.AddAlways("∫ (sec)^" + (m - 2).ToString + " = " + result.ToStringExpr(cfg))
                        End If
                        result *= New Expression(m - 2) / New Expression(m - 1)
                        Dim sum As Expression = Expression.AddFnAndArg0("tan", arg) / New Expression(m - 1)
                        sum *= Expression.AddFnAndArg0("sec", arg) ^ New Expression(m - 2)
                        result += sum
                        m += 2
                    Loop
                    If cfg.bDetail Then
                        cfg.oDetail.AddAlways("∫ (sec)^" + (m - 2).ToString + " = " + result.ToStringExpr(cfg))
                    End If
                    bRet = True
                Else
                    ' Msec odd (3,5,... Msec={0,1} solved before)
                    ' I(m=1) = ∫ sec = ln(|sec + tan|)
                    ' I(m=3) = (sec)^1* tan /2 + 1/2 * ln(|sec + tan|)
                    ' I(m=5) = (sec)^3* tan/4 + (3/4)* I(m=3)
                    ' ...
                    ' Im = Im = (sec)^(m-2)*tan/(m-1) + ((n-2)/(n-1)) * Im-2
                    If cfg.bDetail Then
                        cfg.oDetail.AddAlways("Applying recursive formula: ")
                        cfg.oDetail.AddAlways("Im = (sec)^(m-2)*tan/(m-1) + ((n-2)/(n-1)) * Im-2")
                        cfg.oDetail.AddAlways("See for ex. ""Cálculo Integral 15: Integrales Secante Tangente"" ")
                        Dim sAddr As String = "https://www.youtube.com/watch?v=Dw_HsCUPhJw"
                        cfg.oDetail.AddAlways("<a target=""_new"" href=""" + sAddr + """>" + sAddr + "</a>")
                        cfg.oDetail.AddAlways("Here m is odd, therefore:")
                        cfg.oDetail.AddAlways("I(m=1) = ∫ sec = ln(|sec + tan|)")
                        cfg.oDetail.AddAlways("I(m=3) = (sec)^1* tan /2 + 1/2 * ln(|sec + tan|)")
                        cfg.oDetail.AddAlways("I(m=5) = (sec)^3* tan/4 + (3/4)* I(m=3)")
                        cfg.oDetail.AddAlways(" ...")
                        cfg.oDetail.AddAlways("Im = (sec)^(m-2)*tan/(m-1) + ((n-2)/(n-1)) * Im-2")

                    End If
                    result = Expression.AddFnAndArg0("sec", arg)
                    result += Expression.AddFnAndArg0("tan", arg)
                    result = Expression.AddFnAndArg0("ln", result)
                    Dim m As Int64 = 3
                    Do While m <= Msec
                        If cfg.bDetail Then
                            cfg.oDetail.AddAlways("∫ (sec)^" + (m - 2).ToString + " = " + result.ToStringExpr(cfg))
                        End If
                        result *= New Expression(m - 2) / New Expression(m - 1)
                        Dim sum As Expression = Expression.AddFnAndArg0("tan", arg) / New Expression(m - 1)
                        sum *= Expression.AddFnAndArg0("sec", arg) ^ New Expression(m - 2)
                        result += sum
                        m += 2
                    Loop
                    If cfg.bDetail Then
                        cfg.oDetail.AddAlways("∫ (sec)^" + (m - 2).ToString + " = " + result.ToStringExpr(cfg))
                    End If
                    bRet = True
                End If
            ElseIf Msec = 0 Then
                '  N  |  M  |
                ' N>=2|  0  |  Recursive: I(n) = (tan)^(n-2)/(n-1) - I(n-2)  ( I(n-2)=∫(tan)^(n-2) )
                '              I(1) = ∫ tan = ln|sec| + K
                '              ....
                '              I(n) = tan^(n-1) / (n-1) - I(n-2)
                If Ntan Mod 2 Then
                    If cfg.bDetail Then
                        cfg.oDetail.AddAlways("Applying recursive formula: ")
                        cfg.oDetail.AddAlways("I(n) = tan^(n-1) / (n-1) - I(n-2)")
                        cfg.oDetail.AddAlways("See for ex. ""Cálculo Integral 15: Integrales Secante Tangente"" ")
                        Dim sAddr As String = "https://www.youtube.com/watch?v=Dw_HsCUPhJw"
                        cfg.oDetail.AddAlways("<a target=""_new"" href=""" + sAddr + """>" + sAddr + "</a>")
                        cfg.oDetail.AddAlways("Here m is even, so:")
                        cfg.oDetail.AddAlways("I(1) = ∫ tan = ln|sec| + K")
                        cfg.oDetail.AddAlways(" ...")
                        cfg.oDetail.AddAlways("I(n) = tan^(n-1) / (n-1) - I(n-2)")

                    End If
                    result = Expression.AddFnAndArg0("sec", arg)
                    result = Expression.AddFnAndArg0("abs", result)
                    result = Expression.AddFnAndArg0("ln", result)
                    Dim n As Int64 = 3
                    Do While n <= Ntan
                        If cfg.bDetail Then
                            cfg.oDetail.AddAlways("∫ (tan)^" + (n - 2).ToString + " = " + result.ToStringExpr(cfg))
                        End If
                        Dim sum As Expression = Expression.AddFnAndArg0("tan", arg) ^ New Expression(n - 1)
                        sum /= New Expression(n - 1)
                        result = sum - result
                        n += 2
                    Loop
                    If cfg.bDetail Then
                        cfg.oDetail.AddAlways("∫ (tan)^" + (n - 2).ToString + " = " + result.ToStringExpr(cfg))
                    End If
                    bRet = True
                Else

                    '      I(2) = ∫ tan2 = tan(x) - x + K
                    '      ....
                    '              I(n) = tan^(n-1) / (n-1) - I(n-2)

                    If cfg.bDetail Then
                        cfg.oDetail.AddAlways("Applying recursive formula: ")
                        cfg.oDetail.AddAlways("I(n) = tan^(n-1) / (n-1) - I(n-2)")
                        cfg.oDetail.AddAlways("See for ex. ""Cálculo Integral 15: Integrales Secante Tangente"" ")
                        Dim sAddr As String = "https://www.youtube.com/watch?v=Dw_HsCUPhJw"
                        cfg.oDetail.AddAlways("<a target=""_new"" href=""" + sAddr + """>" + sAddr + "</a>")
                        cfg.oDetail.AddAlways("Here m is even, so:")
                        cfg.oDetail.AddAlways("I(2) = ∫ tan2 = tan(x) - x + K")
                        cfg.oDetail.AddAlways(" ...")
                        cfg.oDetail.AddAlways("I(n) = tan^(n-1) / (n-1) - I(n-2)")

                    End If
                    result = Expression.AddFnAndArg0("tan", arg)
                    result -= New Expression(arg)
                    Dim n As Int64 = 4
                    Do While n <= Ntan
                        If cfg.bDetail Then
                            cfg.oDetail.AddAlways("∫ (tan)^" + (n - 2).ToString + " = " + result.ToStringExpr(cfg))
                        End If
                        Dim sum As Expression = Expression.AddFnAndArg0("tan", arg) ^ New Expression(n - 1)
                        sum /= New Expression(n - 1)
                        result = sum - result
                        n += 2
                    Loop
                    If cfg.bDetail Then
                        cfg.oDetail.AddAlways("∫ (tan)^" + (n - 2).ToString + " = " + result.ToStringExpr(cfg))
                    End If
                    bRet = True
                End If
            ElseIf Msec AndAlso _
            Msec Mod 2 = 0 Then
                '  N  |  M  |
                ' N>0 |Meven|  Take apart sec^2 subs.by (1+(tan)^2) take u=tan,
                '              then du=(sec)^2*dx. Resolve by substitution.
                Dim k As Int64 = Msec / 2
                '  ∫  sec^2k * tan^m
                ' =∫sec^(2k-2) *sec^2*tan^m
                ' =∫(sec^2)^(k-1) * ...
                ' =∫(1+tan^2)^(k-1)*...
                ' {u=tan(x), du=sec(x)^2*dx }
                ' =∫(1+u^2)^(k-1) * u^m du
                Dim sU As String = "__" + IRespVar
                Dim u As New Expression(Polynomial.GetPolynomial(sU))
                Dim ue As Expression = Expression.AddFnAndArg0("tan", arg)
                Dim one As New Expression(1.0)
                Dim oVars2 As New VarsAndFns(vars)
                Dim msg As String = ""
                If cfg.bDetail Then
                    cfg.oDetail.AddAlways("∫  sec^2k * tan^n")
                    cfg.oDetail.AddAlways("=∫sec^(2k-2) *sec^2*tan^n")
                    cfg.oDetail.AddAlways("=∫(sec^2)^(k-1) * ...")
                    cfg.oDetail.AddAlways("=∫(1+tan^2)^(k-1)*...")
                    cfg.oDetail.AddAlways(" {u=tan(x), du=sec(x)^2*dx }")
                    cfg.oDetail.AddAlways("=∫(1+u^2)^(k-1) * u^n du")
                End If
                ' u = tan^2(arg), du = D(arg) * sec^2(arg) * dx
                '--> sec^2(arg) dx = du/D(arg)
                Dim DArg As Expression = arg.opDeriv(IRespVar)
                mult /= DArg.pA.cf(0).pRe.ToDouble
                Dim eKminusOne As New Expression(k - 1.0)
                Dim eM As New Expression(Ntan)
                Dim integrand As Expression = ((one + u * u) ^ eKminusOne * u ^ eM)
                vExprParams(0) = integrand
                oVars2.AddVar(sU, Nothing, msg)
                Dim Id_sU As Int64 = oVars2.getVarIDByName(sU)
                If cfg.bDetail Then
                    cfg.oDetail.AddAlways(" {k=" + k.ToString + " n=" + Ntan.ToString + "}")
                    cfg.oDetail.AddAlways("= ∫" + Replace(vExprParams(0).ToStringExpr(cfg), "__x", "u"))
                End If
                Dim bdetail As Boolean = cfg.bDetail
                cfg.bDetail = False
                Dim uIntegral As Expression = _
                    integrand.opAntiDerivative( _
                     cfg, Id_sU, vExprParams, cur, oVars2, sU, Nothing, Nothing, Nothing)
                If bdetail Then
                    cfg.oDetail.AddAlways("= " + Replace(uIntegral.ToStringExpr(cfg), "__x", "u"))
                End If
                Dim eMtx As New ExprMatrix( _
                    Expression.AddFnAndArg0("tan", arg))
                ' set  cos(x) in 'u' variable:
                oVars2.setValue(Id_sU, eMtx)

                ' finally, replace 'u' by original cos(x) in uIntegral:
                result = uIntegral.evalExprToExpr(oVars2)
                If bdetail Then
                    cfg.oDetail.AddAlways("= " + result.ToStringExpr(cfg))
                End If
                cfg.bDetail = bdetail
                bRet = True
            ElseIf Ntan Mod 2 = 0 Then
                'Neven| M>0 |  N even= 2*k, take (tan)^N=(tan^2)^k=(sec^2-1)^k and there'll
                '              be only sumands of ∫(sec)^i. See (*) above.


                If cfg.bDetail Then
                    cfg.oDetail.AddAlways("N even= 2*k, take (tan)^N=(tan^2)^k=(sec^2-1)^k and there'll")
                    cfg.oDetail.AddAlways("be only sumands of ∫(sec)^i")
                End If
                Dim bDetail As Boolean = cfg.bDetail
                cfg.bDetail = False


                Dim esec As Expression = Expression.AddFnAndArg0("sec", arg)
                esec = esec ^ New Expression(Msec)
                Dim eTan As Expression = Expression.AddFnAndArg0("sec", arg)
                eTan = New Expression(1) + eTan * eTan
                For i As Int64 = 1 To Ntan / 2
                    esec *= eTan
                Next
                result = esec.opAntiDerivative(cfg, varId, _
                             New Expression() {esec}, cur, vars, IRespVar, Nothing, Nothing, Nothing)
                bRet = True
            Else
                ' Nodd| M>0 |  Take apart sec*tan, subs.(tan)^(2*k) by (sec2-1)^k.
                '              Resolve by substitution, consider u=sec so du=sec*tan.

                If cfg.bDetail Then
                    cfg.oDetail.AddAlways("∫secM tanN = ∫sec^(m-1)tan(n-1) * sec*tan (extract sec*tan, decrement exponents)")

                    cfg.oDetail.AddAlways("= {m-1=2*k because Msec-1 is even and")
                    cfg.oDetail.AddAlways("    we know sec^2 = tan^2 + 1 then")
                    cfg.oDetail.AddAlways("(sec)^(m-1)= (sec^2+1)^k } =")
                    cfg.oDetail.AddAlways("= ∫sec^(m-1)*(sec^2-1)^k * sec*tan  ")
                    cfg.oDetail.AddAlways(" = { u=sec, du=sec*tan }")
                    cfg.oDetail.AddAlways("= ∫u^(m-1)*(u^2-1)^k * du = polinomial integration")
                    cfg.oDetail.AddAlways("= {m=" + Msec.ToString + " n=" + Ntan.ToString + "}")
                End If
                Dim bDetail As Boolean = cfg.bDetail
                cfg.bDetail = False

                ' Construct ∫u^(m-1)*(u^2-1)^k * du:
                Dim Pa As Polynomial = Polynomial.GetPolynomial("u")
                Dim fact1 As Polynomial = Pa ^ New Polynomial(Ntan - 1)
                Dim fact2 As Polynomial = Pa * Pa + New Polynomial(1.0)
                fact2 ^= New Polynomial((Msec - 1) / 2)
                Pa = fact1 * fact2
                ' Integrate polynomial Pa(u) respect "u":
                If bDetail Then
                    cfg.oDetail.AddAlways("= ∫" + Pa.toStringPoly(cfg))
                End If
                Dim uP As Polynomial = Pa.opIntegral("u")
                If bDetail Then
                    cfg.oDetail.AddAlways("= " + uP.toStringPoly(cfg))
                End If
                Dim uExpr As New Expression(uP)
                ' replace u by sec(arg):
                Dim oV As New VarsAndFns(New Config(cfg)) ' caso contrario cfg pierde sExpr()
                oV.AddVar("u", Expression.AddFnAndArg0("sec", arg))
                result = uExpr.evalExprToExpr(oV)
                If bDetail Then
                    cfg.oDetail.AddAlways("= " + result.ToStringExpr(cfg))
                End If
                cfg.bDetail = bDetail
                bRet = True

            End If
        Catch ex As Exception
            Throw New Exception(msg8.num(13)) ' n/a 
        End Try
        If bRet AndAlso mult <> 1.0 Then
            result = New Expression(mult) * result
        End If
        Return bRet
    End Function

#End Region

#Region "all the rest of trigonometric integrals"
    Public Function integralTrigonometricRemaining( _
                            cfg As Config, _
                            IRespVar As String, _
                            cur As currentMatch, _
                            vars As VarsAndFns, _
                            ByRef result As Expression, _
                            Optional bAdmitNegExp As Boolean = False, _
                            Optional ByRef cnt As Int64 = 0) _
                             As Boolean
        Dim bRet As Boolean = False
        Dim mult As Double = 1.0
        Dim varId As Int64 = vars.getVarIDByName(IRespVar)
        Try
            cnt += 1
            Dim sT As String = String.Empty
            If IRespVar <> "t" Then
                sT = "t"
            Else
                sT = "x"
            End If
            Dim dx As Expression = Nothing
            Dim vIsDiv() As Boolean = Nothing
            Dim vFac() As Expression = Me.exprToFactors(vIsDiv)
            For i = 0 To vFac.Length - 1
                If vFac(i).IsPolynomial Then
                    Dim Pa As Polynomial = vFac(i).getPolynomial
                    Dim pos As Int64 = Array.IndexOf(Pa.var1, IRespVar)
                    If pos > -1 Then
                        Exit Try
                    End If
                End If
            Next


            ' Convert into partial fractions:
            ' x = 2*atan(t) => { cos(2a)=(1-tan2(a))/(1+tan2(a))
            ' cos(x)= (1-t^2)/(1+t^2)
            ' sin(x)= 2t / (1+t^2)
            ' dx = 2/(1+t^2) dt
            ' t = tan(x/2)


            Dim pT As Polynomial = Polynomial.GetPolynomial(sT)
            Dim pX As Polynomial = Polynomial.GetPolynomial(IRespVar)

            ' cos(x)= (1-t^2)/(1+t^2):
            Dim t2plusOne As New Expression(pT * pT + New Polynomial(1.0))
            Dim OneMinust2 As New Expression(New Polynomial(1.0) - pT * pT)
            Dim cosx As Expression = New Expression( _
                    OneMinust2 / t2plusOne)
            Dim sinx As New Expression( _
                     New Expression(New Polynomial(2.0) * pT) / t2plusOne)
            ' fnX = dx = 
            dx = New Expression(2.0) / t2plusOne
            Dim arg As Expression = Nothing
            Dim multG As Complex = Nothing
            For i As Int64 = 0 To vFac.Length - 1
                If vFac(i).extractFromPath(New String() {"cos", "0"}, arg) OrElse _
                vFac(i).extractFromPath(New String() {"sin", "0"}, arg) Then
                    Dim vArgIsDiv() As Boolean = Nothing
                    Dim vArgFac() As Expression = Nothing
                    vArgFac = arg.exprToFactors(vArgIsDiv)
                    If vArgFac.Length > 2 Then
                        bRet = False
                        Exit Try
                    ElseIf vArgFac.Length = 2 AndAlso _
                    vArgFac(0).IsReal Then
                        Array.Reverse(vArgFac)
                        Array.Reverse(vArgIsDiv)
                    End If
                    Dim mult1 As Complex = Nothing
                    If vArgFac(0).IsPolynomial AndAlso _
                    vArgFac(0).getPolynomial.IsEqual(pX, mult1) Then
                        If vFac(i).extractFromPath(New String() {"cos", "0"}, arg) Then
                            vFac(i) = New Expression(cosx)
                        Else
                            vFac(i) = New Expression(sinx)
                        End If
                        If multG Is Nothing Then
                            multG = mult1
                        ElseIf Not (multG - mult1).IsZero Then
                            bRet = False
                            Exit Try
                        End If
                    Else
                        bRet = False
                        Exit Try
                    End If
                    If vArgFac.Length > 1 AndAlso _
                    vArgFac(1).IsReal Then
                        If vArgIsDiv(1) Then
                            vFac(i) /= New Expression(vArgFac(1))
                        Else
                            vFac(i) *= New Expression(vArgFac(1))
                        End If
                    End If
                End If
            Next
            Dim expr As Expression = Expression.factorsToExpr(vFac, vIsDiv)
            If expr.IsPolynomial Then
                Dim Pa As Polynomial = New Polynomial(multG) * expr.getPolynomial
                Dim exprPa As New Expression(Pa)
                exprPa.cfg = cfg
                Dim oVars2 As New VarsAndFns(cfg)
                oVars2.AddVar(sT, Nothing)
                Dim cfg2 As Config = Nothing
                If Pa.PolyResto Is Nothing Then
                    result = New Expression(Pa.opIntegral(IRespVar))
                Else 'If Not exprPa.InmediateIntegrals( _
                    'cfg, IRespVar, 0, oVars2, expr, cur) Then
                    If cfg.bDetail Then
                        cfg.oDetail.AddAlways(" ")
                        cfg.oDetail.AddAlways("Substitution of " + IRespVar + " by 2*atan(" + sT + ")")
                        cfg.oDetail.AddAlways(" so  cos(x)= (1-" + sT + "^2)/(1+" + sT + "^2) ")
                        cfg.oDetail.AddAlways(" and sin(x)= 2*" + sT + "/(1+" + sT + "^2) ")
                        cfg.oDetail.AddAlways(" =>  Converted into a partial fractions integral: ")
                        cfg2 = cfg
                        'cfg = New Config
                        'cfg.bDetail = True
                        'cfg.bRounding = cfg2.bRounding
                    End If

                    result = Expression.opIntegralDefOrPolinFraction( _
                        cfg, 0, New Expression() {exprPa}, cur, oVars2, cnt)
                End If
                ' t = tan(x/2):
                oVars2.setValue(0, New ExprMatrix( _
                         Expression.AddFnAndArg0("tan", _
                         New Expression(Polynomial.GetPolynomial(IRespVar) / New Polynomial(2.0)))))
                result = result.evalExprToExpr(oVars2)
                If cfg2 IsNot Nothing Then
                    ReDim Preserve cfg2.oDetail.sExpr(cfg2.oDetail.ie + cfg.oDetail.ie - 1)
                    Array.Copy(cfg.oDetail.sExpr, 0, _
                              cfg2.oDetail.sExpr, cfg2.oDetail.ie, cfg.oDetail.ie)
                    cfg2.oDetail.ie += cfg.oDetail.ie
                    cfg = cfg2
                End If
                bRet = True
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return bRet
    End Function
    Public Function convertRestTrigonometric( _
                        cfg As Config, _
                        iRespVar As String, _
                        vars As VarsAndFns, _
                        varId As Int64, _
                        ByRef result As Expression, _
                        ByVal sU As String, _
                        Optional ByRef arg As Expression = Nothing, _
                        Optional ByRef iLevel As Int64 = 0) _
            As Boolean
        Dim bRet As Boolean = False
        Try
            If iLevel = 0 Then
                ' first deflate degree in sin^N * cosM
                Dim sinN As Int64 = 0
                Dim cosM As Int64 = 0

            End If
            iLevel += 1
            ' By substitution:
            ' sin(x) = 2u/(1+u2)
            ' cos(x) = (1-u) / (1+u2)
            ' tan(x) = 2u / (1-u2)
            ' sec(x) = (1+u2) / (1-u2)
            ' dx = (2 / (1+u2)) du
            ' x = ∫ dx = ∫(2 / (1+u2)) du = 2*atan(u)
            If pA IsNot Nothing Then
                'Dim oVar2 As New VarsAndFns(vars)
                'Dim eU As Expression = _
                '    New Expression(2.0) * _
                '    Expression.AddFnAndArg0("atan", _
                '    New Expression(Polynomial.GetPolyomial(sU)))
                'oVar2.setValue(varId, New ExprMatrix(eU))
                'result = New Expression(pA).evalExprtoexpr( cfg, oVar2)
                'bRet = True
                bRet = False
            ElseIf mOp.Groups("fn").Success Then
                Dim sfn As String = mOp.ToString
                If arg Is Nothing Then
                    If sfn <> "sqr" Then
                        arg = Args(0)
                    End If
                ElseIf Not arg.isEqualTo(Args(0)) Then
                    result = Nothing
                    bRet = False
                    Exit Try
                End If
                Dim oVar2 As New VarsAndFns(vars)
                Dim eU As New Expression(Polynomial.GetPolynomial(sU))
                Select Case sfn
                    Case "sin"
                        result = _
                            New Expression(2.0) * eU / _
                            (New Expression(1.0) + eU * eU)
                    Case "cos"
                        result = (New Expression(1.0) - eU * eU) / _
                            (New Expression(1.0) + eU * eU)
                    Case "tan"
                        result = _
                            New Expression(2.0) * eU / _
                            (New Expression(1.0) - eU * eU)
                    Case "sec"
                        result = _
                            (New Expression(1.0) + eU * eU) / _
                            (New Expression(1.0) - eU * eU)
                    Case "sqr"

                        bRet = (Args(0) ^ _
                             New Expression(0.5)).convertRestTrigonometric( _
                             cfg, iRespVar, vars, varId, result, sU, arg, iLevel)
                        If Not bRet Then
                            Exit Try
                        End If
                    Case Else
                        result = Nothing
                        Exit Try
                End Select
                oVar2.setValue(varId, New ExprMatrix( _
                               New Expression(Polynomial.GetPolynomial(sU))))
                result = result.evalExprToExpr(oVar2)
                bRet = True
            ElseIf mOp IsNot Nothing Then
                Dim sOp As String = mOp.ToString
                If Args.Length = 1 Then
                    bRet = convertRestTrigonometric(cfg, iRespVar, _
                        vars, varId, result, sU, arg, iLevel)
                    If sOp = "-" Then
                        result = -result
                    End If
                Else
                    Dim resultA As Expression = Nothing
                    Dim resultB As Expression = Nothing
                    bRet = Args(0).convertRestTrigonometric(cfg, iRespVar, _
                        vars, varId, resultA, sU, arg, iLevel)
                    If Not bRet Then
                        Exit Try
                    End If
                    bRet = Args(1).convertRestTrigonometric(cfg, iRespVar, _
                        vars, varId, resultB, sU, arg, iLevel)
                    If Not bRet Then
                        Exit Try
                    End If
                    Select Case sOp
                        Case "-" : result = resultA - resultB
                        Case "+" : result = resultA + resultB
                        Case "*" : result = resultA * resultB
                        Case "/" : result = resultA / resultB
                        Case "^" : result = resultA ^ resultB
                    End Select
                End If
            End If
        Catch ex As Exception
            Throw New Exception(msg8.num(13)) ' n/a 
        Finally
            Try
                iLevel -= 1
                If bRet Then
                    If iLevel = 0 Then
                        ' dx = [ 1/(1+u2) ] du
                        result *= New Expression( _
                            New Polynomial(1.0) / _
                            (New Polynomial(1.0) + _
                             Polynomial.GetPolynomial(sU) ^ _
                             New Polynomial(2.0)))
                    End If
                    'Dim bIsMinus() As Boolean = Nothing
                    'Dim vSum() As Expression = result.exprToSummands(bIsMinus)
                    'For i As Int64 = 0 To vSum.Length - 1
                    '    Dim bIsDiv() As Boolean = Nothing
                    '    Dim vFac() As Expression = vSum(i).exprToFactors(bIsDiv)
                    '    For j As Int64 = 0 To vFac.Length - 1
                    '        vFac(j) = vFac(j).reduceFactors
                    '    Next
                    '    vSum(i) = Expression.factorsToExpr(vFac, bIsDiv)
                    'Next
                    'result = Expression.summandsToExpr(vSum, bIsMinus)
                End If
            Catch ex2 As Exception
                Throw New Exception(msg8.num(13)) ' n/a 
            End Try
        End Try
        If result Is Nothing Then
            bRet = False
        End If
        Return bRet
    End Function
#End Region
#End Region
End Class
