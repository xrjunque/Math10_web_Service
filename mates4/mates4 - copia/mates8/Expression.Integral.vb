
Partial Public Class Expression

#Region "Expression.Integral"

    Const maxCntRecursive As Int64 = 15
    Public Function opAntiDerivative( _
                               ByVal cfg As Config, _
                               ByVal respVarID As Int64, _
                               ByVal vExprParams() As Expression, _
                               ByVal cur As currentMatch, _
                               ByVal vars As VarsAndFns, _
                               ByVal IRespVar As String, _
                               ByRef origAntideriv() As Expression, _
                               ByRef bFoundOrigAntideriv() As Boolean, _
                               ByRef vIndex() As Int64, _
                               Optional ByRef sDetail() As String = Nothing, _
                               Optional ByRef cnt As Int64 = 0 _
                    ) As Expression
        Dim ret As Expression = Nothing
        Dim rup As New ReduceExprUsingPolynomials
        Try
            cnt += 1
            If cnt > maxCntRecursive Then
                Throw New Exception(msg8.num(13))
            End If
            vExprParams(0) = Me

            Dim bIsMinus() As Boolean = Nothing
            Dim exprReduc As Expression = rup.ReduceUsingPolynomials(Me)
            Dim vSummands() As Expression = _
                exprReduc.convertSecCscCot(False).exprToSummands(bIsMinus)
            If IRespVar = "" Then
                If cur.m.Groups("IResp").Success Then
                    IRespVar = cur.m.Groups("IResp").ToString
                End If
            Else
                vars.AddVar(IRespVar, Nothing)
            End If
            If IRespVar = "" Then
                Exit Try
            End If
            Dim i As Int64
            ' Convert:
            ' sin(a+b) into sin(a)cos(b) + cos(a)sin(b)
            ' cos(a+b) into cos(a)cos(b) - sin(a)sin(b)
            Dim bFound As Boolean
            Dim i0 As Int64
            Dim bTreated As Boolean
            Do
                bTreated = False
                For i0 = 0 To vSummands.Length - 1
                    bFound = False
                    Dim bIsDiv() As Boolean = Nothing
                    Dim vFactSum() As Expression = vSummands(i0).exprToFactors(bIsDiv)

                    Dim arg As Expression = Nothing
                    For i = 0 To vFactSum.Length - 1
                        If vFactSum(i).extractFromPath(New String() {"sin", "0"}, arg) Then
                            Dim vFactI() As Expression = arg.splitIntoTerms
                            If vFactI.Length > 1 Then
                                bFound = True
                                ' just break into 2 summands, vFactI(0)
                                ' and vFactI(1). 
                                ' Any other extra summand will be merged
                                ' into vFactI(1); (Total number of summands will
                                ' have decreased by one).
                                For j As Int64 = 2 To vFactI.Length - 1
                                    vFactI(1) += vFactI(j)
                                Next
                                Dim factI As Expression = _
                                    Expression.AddFnAndArg0("sin", vFactI(0))
                                factI *= Expression.AddFnAndArg0("cos", vFactI(1))
                                Dim factI2 As Expression = _
                                    Expression.AddFnAndArg0("cos", vFactI(0))
                                factI2 *= Expression.AddFnAndArg0("sin", vFactI(1))
                                vFactSum(i) = factI + factI2
                                bTreated = True
                                Exit For
                            End If
                        ElseIf vFactSum(i).extractFromPath(New String() {"cos", "0"}, arg) Then
                            Dim vFactI() As Expression = arg.splitIntoTerms
                            If vFactI.Length > 1 Then
                                bFound = True
                                ' just break into 2 summands, vFactI(0)
                                ' and vFactI(1). 
                                ' Any other extra summand will be merged
                                ' into vFactI(1); (Total number of summands will
                                ' have decreased by one).
                                For j As Int64 = 2 To vFactI.Length - 1
                                    vFactI(1) += vFactI(j)
                                Next
                                Dim factI As Expression = _
                                    Expression.AddFnAndArg0("cos", vFactI(0))
                                factI *= Expression.AddFnAndArg0("cos", vFactI(1))
                                Dim factI2 As Expression = _
                                    Expression.AddFnAndArg0("sin", vFactI(0))
                                factI2 *= Expression.AddFnAndArg0("sin", vFactI(1))
                                vFactSum(i) = factI - factI2
                                bTreated = True
                                Exit For
                            End If
                        End If
                    Next
                    If bTreated Then
                        vSummands(i0) = Expression.factorsToExpr(vFactSum, bIsDiv)
                        exprReduc = Expression.summandsToExpr(vSummands, bIsMinus)
                        exprReduc = rup.ReduceUsingPolynomials(exprReduc)
                        vSummands = exprReduc.exprToSummands(bIsMinus)
                        Exit For
                    End If
                Next
            Loop While bTreated
            For i = 0 To vSummands.Length - 1
                Dim curExpr As Expression = Nothing
                Dim eSum As Expression = Nothing  ' vSummands(i).reduceSummands()
                Dim bIsDiv() As Boolean = Nothing
                Dim factWithOutVarResp As New Expression(1.0)
                Dim vFact() As Expression = vSummands(i).exprToFactors(bIsDiv)
                vSummands(i) = New Expression(1.0)
                For j As Int64 = 0 To vFact.Length - 1
                    If vFact(j).IsReal Then
                        Dim db As Double = vFact(j).toDouble
                        vFact(j) = New Expression(1.0)
                        If bIsDiv(j) Then
                            factWithOutVarResp /= New Expression(db)
                        Else
                            factWithOutVarResp *= New Expression(db)
                        End If
                        GoTo sigJ
                    ElseIf vFact(j).sign = -1 Then
                        factWithOutVarResp *= New Expression(-1.0)
                        vFact(j) = vFact(j).opChgSgn
                    End If
                    Dim vVar(-1) As String
                    vFact(j).getAllVars(vVar)
                    If vVar IsNot Nothing AndAlso _
                    vVar.Length AndAlso Array.IndexOf(vVar, IRespVar) > -1 Then
                        If bIsDiv(j) Then
                            vSummands(i) /= vFact(j)
                        Else
                            vSummands(i) *= vFact(j)
                        End If
                    Else
                        If bIsDiv(j) Then
                            factWithOutVarResp /= vFact(j)
                        Else
                            factWithOutVarResp *= vFact(j)
                        End If
                    End If
sigJ:
                Next
                eSum = rup.ReduceUsingPolynomials(vSummands(i)) '=  eSum.reduceFactors(False)
                'Dim eIndep As Expression = Nothing
                'If origAntideriv Is Nothing Then
                '    eSum.getDepAndIndep(IRespVar, eSum, eIndep)
                '    vSummands(i) = eSum
                'End If
                vExprParams(0) = eSum
                Dim Nsin As Int64 = 0
                Dim Mcos As Int64 = 0
                Dim Msec As Int64 = 0
                Dim Ntan As Int64 = 0
                Dim arg As Expression = Nothing
                Dim mult As Double = 0.0
                'Dim bIsSinCos As Boolean = False
                'Dim bIsSecTan As Boolean = False
                'If vExprParams(0).Is_sinN_cosM(IRespVar, Nsin, Mcos, arg, mult) Then
                '    bIsSinCos = True
                'End If
                'If vExprParams(0).Is_secN_tanM(IRespVar, Nsec, Mtan, arg, mult) Then
                '    bIsSecTan = True
                'End If
                curExpr = Nothing
                If eSum.IsPolynomial Then
                    eSum.pA = eSum.getPolynomial()
                    If eSum.pA.PolyResto Is Nothing OrElse _
                    (eSum.pA.PolyResto.getDegree = 0 AndAlso _
                    eSum.pA.PolyDivisor.cf.Length = 1) Then
                        Dim polyResto As Polynomial = eSum.pA.PolyResto
                        Dim polyDivisor As Polynomial = eSum.pA.PolyDivisor
                        If polyResto IsNot Nothing AndAlso _
                        polyResto.getDegree = 0 AndAlso _
                        polyDivisor.cf.Length = 1 AndAlso _
                        polyDivisor.var.Length = 1 AndAlso _
                        polyDivisor.var(0) = IRespVar Then
                            Dim exp As Int64 = -polyDivisor.exp(0)(0) + 1
                            If exp <> 0 Then
                                Dim Pa As Polynomial = _
                                    Polynomial.GetPolynomial(IRespVar)
                                Pa ^= New Polynomial(exp)
                                Pa *= polyResto
                                Pa /= New Complex(exp)
                                curExpr = New Expression(Pa)
                            End If
                        Else
                            curExpr = New Expression(eSum.pA.opIntegral(IRespVar))
                        End If
                    End If
                    If curExpr Is Nothing AndAlso vExprParams(0).Is_secN_tanM(IRespVar, Msec, Ntan, arg, mult) Then
                        ' expression's form is not sin(x)^ N * cos(x)^-N * cos(x)^-M
                    ElseIf curExpr Is Nothing AndAlso _
                    Not vSummands(i).InmediateIntegrals( _
                        cfg, IRespVar, respVarID, vars, curExpr, cur, cnt) Then
                        curExpr = Expression.opIntegralDefOrPolinFraction( _
                            cfg, respVarID, vExprParams, cur, vars, cnt)
                    End If
                End If
                If curExpr Is Nothing AndAlso vExprParams(0).Is_secN_tanM(IRespVar, Msec, Ntan, arg, mult) Then
                    ' expression's form is not sin(x)^ N * cos(x)^-N * cos(x)^-M
                ElseIf curExpr Is Nothing AndAlso _
                    Not vSummands(i).InmediateIntegrals( _
                    cfg, IRespVar, respVarID, vars, curExpr, cur, cnt) Then
                    'curExpr = Expression.opIntegralDefOrPolinFraction( _
                    '   cfg, respVarID, vExprParams, cur, vars)
                End If
                vExprParams(0) = vSummands(i)
                If curExpr Is Nothing Then
                    If eSum.integralSecNtanM(cfg, IRespVar, vExprParams, cur, vars, curExpr, _
                            origAntideriv, bFoundOrigAntideriv) Then
                    ElseIf eSum.integralSinNCosM(cfg, IRespVar, vExprParams, cur, vars, curExpr, True) Then
                    ElseIf vSummands(i).InmediateIntegrals(cfg, IRespVar, respVarID, vars, curExpr, cur, cnt) Then
                    ElseIf vSummands(i).integralTrigonometricRemaining(cfg, IRespVar, cur, vars, curExpr, cnt) Then
                    ElseIf vSummands(i).IntegrationByParts(cfg, IRespVar, vExprParams, cur, vars, curExpr, _
                           origAntideriv, bFoundOrigAntideriv, vIndex, sDetail, cnt) Then
                    End If
                End If
                If curExpr IsNot Nothing Then
                    factWithOutVarResp = rup.ReduceUsingPolynomials(factWithOutVarResp)
                    curExpr *= factWithOutVarResp
                    curExpr = rup.ReduceUsingPolynomials(curExpr)
                    'If eIndep IsNot Nothing Then
                    '    curExpr = eIndep * curExpr
                    'End If
                    If ret Is Nothing Then
                        ret = New Expression(0.0)
                    End If
                    If Not bIsMinus(i) Then
                        ret += curExpr
                        'ret = exprOp("+", ret, curExpr)
                    Else
                        ret -= curExpr
                        'ret = exprOp("-", ret, curExpr)
                    End If
                Else
                    ret = Nothing
                    Exit For
                End If
                If origAntideriv IsNot Nothing Then
                    ReDim origAntideriv(origAntideriv.Length - 1)
                End If
            Next
            If ret Is Nothing Then
                vExprParams(0) = Me
                'Me.IntegrationByParts(cfg, IRespVar, vExprParams, cur, vars, ret, _
                '      origAntideriv, bFoundOrigAntideriv, vIndex)
            Else
            End If
            If cfg.bDetail Then
                cfg.oDetail.ClearDivisions()
            End If
        Catch ex As Exception
            If InStr(ex.ToString, "n/a") Then
                Throw ex
            End If
            Throw New Exception(msg8.num(13)) ' n/a 
        End Try
        'If sign = -1 Then
        '    ret = -ret
        'End If
        'Dim rup As New ReduceExprWithPolynomials
        'Return rup.ReduceUsingPolynomials(ret)
        Return ret
    End Function

    Public Shared Function opIntegralDefOrPolinFraction( _
                           ByVal cfg As Config, _
                           ByVal respVarID As Int64, _
                           ByVal vExprParams() As Expression, _
                           ByVal cur As currentMatch, _
                           ByVal vars As VarsAndFns, _
                           Optional cnt As Int64 = 0) As Expression
        Dim ret As Expression = Nothing
        'Dim oDetail As Detall = cfg.oDetail
        Dim bDetail As Boolean = cfg.bDetail
        Try
            '          Case "integral", "i", "∫", "integrate"
            cnt += 1
            Dim nParams As Int64 = vExprParams.Length
            Dim bHayLRP As Boolean = True
            cfg.bDetail = False
            'cfg.oDetail = Nothing
            Dim a As Complex = Nothing
            Dim b As Complex = Nothing
            Dim eA As Expression = Nothing
            Dim eB As Expression = Nothing
            If nParams = 3 Then
                'If Not vExprParams(0).IsComplex Then
                '    ' arguments not valid
                '    Throw New Exception(msg8.num(9))
                'ElseIf Not vExprParams(1).IsComplex Then
                '    ' arguments not valid
                '    Throw New Exception(msg8.num(9))
                'End If
                eA = vExprParams(0)
                If eA.IsReal Then
                    a = vExprParams(0).getPolynomial.cf(0)
                End If
                eB = vExprParams(1)
                If eB.IsReal Then
                    b = vExprParams(1).getPolynomial.cf(0)
                End If
                'a = vExprParams(0).getPolynomial.cf(0)
                'b = vExprParams(1).getPolynomial.cf(0)
            ElseIf nParams = 2 Then
                ' Missing an argument
                Throw New Exception(msg8.num(14))
            End If
            Dim rvArg As Expression = Nothing
            If nParams = 1 Then
                rvArg = vExprParams(0)
            ElseIf nParams = 3 Then
                rvArg = vExprParams(2)
            End If
            Dim IRespVar As String = ""
            If respVarID > -1 Then
                IRespVar = vars.getVarNameByID(respVarID)
            End If
            If IRespVar = "" AndAlso cur.m.Groups("IResp").Success Then
                IRespVar = cur.m.Groups("IResp").ToString
            End If
            Dim varId As Int64 = vars.getVarIDByName(IRespVar, False)
            If varId = -1 Then
                varId = 0
                Dim custFn As customFn = Nothing
                Do While varId + 1 < vars.getNamesList.Length AndAlso _
                    vars.IsCustomFn(vars.getVarNameByID(varId), custFn)
                    varId += 1
                Loop
                If IRespVar = "" AndAlso varId > -1 Then
                    IRespVar = vars.getNamesList(varId)
                End If
            End If
            Dim msg As String = String.Empty
            If IRespVar <> "" Then
                vars.AddVar(IRespVar, Nothing, msg)
                varId = vars.getVarIDByName(IRespVar)
            End If
            If rvArg.IsPolynomial Then
                Dim Pa As Polynomial = New Polynomial(rvArg.getPolynomial)
                Dim vAll() As String = Pa.varAll
                Pa.setVars(vAll)
                Dim resto As Polynomial = Nothing
                If Pa.PolyResto IsNot Nothing Then
                    resto = New Polynomial(Pa.PolyResto)
                    If Array.IndexOf(resto.varAll, IRespVar) > -1 Then
                        resto = Nothing
                    Else
                        Pa.PolyResto = New Polynomial(1.0)
                        Pa.opReduceCommonExponents()
                        Pa.tryReducePolyResto()
                    End If
                End If
                If Pa.varlen = 1 Then
                    Dim entP As New Polynomial(Pa)
                    entP.PolyResto = Nothing
                    entP.PolyDivisor = Nothing
                    If Not (entP.isReal OrElse _
                    entP.ToDouble = 0.0) Then
                        ret = New Expression(entP.opIntegral(IRespVar))
                        Pa.cf = New Complex() {Complex.zero}
                        ReDim Pa.exp(0), Pa.exp(0)(0)
                        Dim var(-1) As String
                        Pa.setVars(var)
                    Else
                        ret = New Expression(0.0)
                    End If
                    If rvArg.getPolynomial.PolyResto IsNot Nothing Then
                        'cfg.oDetail = oDetail
                        cfg.bDetail = bDetail
                        If resto IsNot Nothing Then
                            Dim eResto As New Expression(resto)
                            Dim eFract As Expression = Pa.opIntegralRationalFractions(cfg, IRespVar)
                            ret += eResto * eFract
                            'ret += New Expression(resto) * Pa.opIntegralRationalFractions(cfg, IRespVar)
                        Else
                            ret += Pa.opIntegralRationalFractions(cfg, IRespVar)
                        End If
                        Exit Try
                    End If
                ElseIf Pa.PolyResto Is Nothing AndAlso Pa.getDegree > -1 Then
                    ret = _
                    New Expression( _
                    rvArg.getPolynomial.opIntegral(IRespVar))
                    If nParams = 3 Then
                        Dim oVars2 As New VarsAndFns(vars)
                        oVars2.setValue(varId, New ExprMatrix(eB))
                        Dim exprInPtB As Expression = _
                            ret.evalExprToExpr(oVars2)
                        oVars2.setValue(varId, New ExprMatrix(eA))
                        Dim exprInPtA As Expression = _
                            ret.evalExprToExpr(oVars2)
                        If bDetail Then
                            cfg.oDetail.Add( _
                                cur.toStrCurMatch("", False) + _
                                " = " + ret.ToStringExpr(cfg) + _
                                "&#124;<sub>" + eA.ToString + "</sub>" + _
                                "<sup>" + eB.ToString + "</sup>")
                            cfg.oDetail.Add(" = " + _
                                 ret.ToStringExpr(cfg) + _
                                "&#124;<sub>" + IRespVar + "=" + eB.ToString + "</sub>" + _
                                " - " + ret.ToStringExpr(cfg) + _
                                "&#124;<sub>" + IRespVar + "=" + eA.ToString + "</sub>")
                            cfg.oDetail.Add(" = " + exprInPtB.ToStringExpr(cfg) + " - " + _
                                        exprInPtA.ToStringExpr(cfg))
                        End If
                        ret = exprInPtB - exprInPtA
                        Exit Try
                    End If
                    Exit Try
                ElseIf Pa.PolyDivisor.var.Length > 1 Then
                    ' polynomial PolyDivisor has more than 1 variable:
                    msg = String.Format(msg8.num(69), _
                        Pa.PolyDivisor, Join(Pa.PolyDivisor.var, ", "))
                    Throw New Exception(msg)
                Else
                    Throw New Exception(msg8.num(13)) ' n/a
                End If
                For i As Int64 = 0 To ret.pA.var.Length - 1
                    vars.AddVar(ret.pA.var(i), Nothing, msg)
                Next
                varId = vars.getVarIDByName(IRespVar, False)
                If nParams = 3 Then
                    Dim oVars2 As New VarsAndFns(vars)
                    If oVars2.getNamesList.Length Then
                        oVars2.setValue(varId, New ExprMatrix(eA))
                    End If
                    Dim expr1 As Expression = ret.evalExprToExpr(oVars2)
                    If oVars2.getNamesList.Length Then
                        oVars2.setValue(varId, New ExprMatrix(eB))
                    End If
                    Dim expr2 As Expression = ret.evalExprToExpr(oVars2)
                    If bDetail Then
                        cfg.oDetail.Add( _
                            cur.toStrCurMatch("", False) + _
                            " = " + ret.ToStringExpr(cfg) + _
                            "&#124;<sub>" + eA.ToString + "</sub>" + _
                            "<sup>" + eB.ToString + "</sup>")
                        cfg.oDetail.Add(" = " + _
                             ret.ToStringExpr(cfg) + _
                            "&#124;<sub>" + IRespVar + "=" + eB.ToString + "</sub>" + _
                            " - " + ret.ToStringExpr(cfg) + _
                            "&#124;<sub>" + IRespVar + "=" + eA.ToString + "</sub>")
                        cfg.oDetail.Add(" = " + expr2.ToStringExpr(cfg) + " - " + _
                                    expr1.ToStringExpr(cfg))
                    End If
                    ret = expr2 - expr1
                End If
            ElseIf nParams <> 3 Then
                Throw New Exception( _
                msg8.num(13)) ' n/a
            Else

                ' first try antiderivative:
                Dim oVars2 As New VarsAndFns(vars)
                Dim antiD As Expression = Nothing

                Dim Msec As Int64 = 0
                Dim Ntan As Int64 = 0
                Dim mult As Double
                Dim arg As Expression = Nothing
                If vExprParams(0).Is_secN_tanM(IRespVar, Msec, Ntan, arg, mult) Then
                    ' expression's form is sin(x)^ N * cos(x)^-N * cos(x)^-M
                ElseIf rvArg.InmediateIntegrals(cfg, IRespVar, varId, vars, ret, cur, cnt:=cnt) Then
                    oVars2.setValue(varId, New ExprMatrix(eB))
                    Dim exprInPtB As Expression = _
                        ret.evalExprToExpr(oVars2)
                    oVars2.setValue(varId, New ExprMatrix(eA))
                    Dim exprInPtA As Expression = _
                        ret.evalExprToExpr(oVars2)
                    ret = exprInPtB - exprInPtA
                    Exit Try
                Else
                    'Dim tOut As Long = cfg.timeOutms
                    'Dim cfg1 As New Config
                    'cfg1.timeOutms = 500
                    ret = rvArg.opAntiDerivative( _
                                     cfg, varId, vExprParams, cur, vars, IRespVar, Nothing, Nothing, Nothing)
                    If ret IsNot Nothing Then
                        oVars2.setValue(varId, New ExprMatrix(eB))
                        Dim exprInPtB As Expression = _
                            ret.evalExprToExpr(oVars2)
                        oVars2.setValue(varId, New ExprMatrix(eA))
                        Dim exprInPtA As Expression = _
                            ret.evalExprToExpr(oVars2)
                        ret = exprInPtB - exprInPtA
                        Exit Try
                    End If
                End If

                ret = rvArg
                If nParams <> 3 Then
                    Throw New Exception( _
                    msg8.num(13)) ' n/a
                Else
                    If a Is Nothing Then
                        ' arguments not valid
                        Throw New Exception(msg8.num(9))
                    ElseIf b Is Nothing Then
                        ' arguments not valid
                        Throw New Exception(msg8.num(9))
                    End If
                End If
                ' definite integral,
                ' apply Simpson's rule:
                ' Area, apróx.= (b-a)*(Y0 +4Y1 +2Y2 + 4Y3 + 2Y4 +...+4Yn-1 +Yn)/(3*N)
                Dim N As Int64 = 1000
                Dim Area As Expression = New Expression(b - a)
                Dim multBy As New Expression(0.0)
                Dim m As New Complex(a)
                Dim aRe As Double = a.pRe.ToDouble
                Dim bRe As Double = b.pRe.ToDouble
                Dim t As Double = (bRe - aRe) / N
                Dim four As New Expression(4.0)
                Dim two As New Expression(2.0)
                If oVars2.getNamesList.Length Then
                    oVars2.setValue(varId, New ExprMatrix(a))
                    multBy += ret.evalExprToExpr(oVars2)
                    If multBy.IsComplex = False Then
                        If IRespVar = "" Then
                            Throw New Exception(msg8.num(30)) ' missing var.resp.to which integrate
                        Else
                            Throw New Exception(msg8.num(31)) ' not a definite integral
                        End If
                    End If
                    For i = 1 To N - 1
                        m = New Complex(aRe + i * t)
                        oVars2.setValue(varId, New ExprMatrix(m))
                        If i Mod 2 Then
                            multBy += four * ret.evalExprToExpr(oVars2)
                        Else
                            multBy += two * ret.evalExprToExpr(oVars2)
                        End If
                    Next
                    oVars2.setValue(varId, New ExprMatrix(b))
                    multBy += ret.evalExprToExpr(oVars2)
                    Area *= multBy / New Expression(3 * N)
                Else
                    ' there are no variables:
                    multBy += ret.evalExprToExpr(oVars2)
                    For i = 1 To N
                        m = New Complex(aRe + i * t)
                        If i Mod 2 Then
                            multBy += four * ret.evalExprToExpr(oVars2)
                        Else
                            multBy += two * ret.evalExprToExpr(oVars2)
                        End If
                    Next
                    multBy += ret.evalExprToExpr(oVars2)
                    Area *= multBy / New Expression(3 * N)
                End If
                ret = Area
            End If
        Catch ex As Exception
            Throw ex
        Finally
            'cfg.oDetail = oDetail
            cfg.bDetail = bDetail
            If bDetail Then
                cfg.oDetail.ClearDivisions()
            End If
        End Try
        Return ret
    End Function


    Public Function getAllVars(ByRef vAllVars() As String) As Boolean
        Dim bRet As Boolean = False
        Try
            If vAllVars Is Nothing Then
                ReDim vAllVars(-1)
            End If
            Dim iv As Int32 = vAllVars.Length
            If pA IsNot Nothing Then
                Dim vVarsPa() As String = pA.varAll
                If vVarsPa.Length Then
                    For i As Int32 = 0 To vVarsPa.Length - 1
                        If Array.IndexOf(vAllVars, vVarsPa(i), 0, iv) = -1 Then
                            ReDim Preserve vAllVars(iv)
                            vAllVars(iv) = vVarsPa(i)
                            iv += 1
                        End If
                    Next
                End If
            ElseIf mOp IsNot Nothing AndAlso _
            (mOp.Groups("var2").Success OrElse _
            mOp.Groups("var").Success) Then
                If Array.IndexOf(vAllVars, mOp.ToString, 0, iv) = -1 Then
                    ReDim Preserve vAllVars(iv)
                    vAllVars(iv) = mOp.ToString
                End If
            ElseIf Args.Length Then
                Dim vVarsArg() As String = Nothing
                Args(0).getAllVars(vVarsArg)
                If vVarsArg.Length Then
                    ReDim Preserve vAllVars(iv + vVarsArg.Length - 1)
                    Array.Copy(vVarsArg, 0, vAllVars, iv, vVarsArg.Length)
                    iv = vAllVars.Length
                End If
                If Args.Length > 1 Then
                    Args(1).getAllVars(vVarsArg)
                    If vVarsArg.Length Then
                        ReDim Preserve vAllVars(iv + vVarsArg.Length - 1)
                        Array.Copy(vVarsArg, 0, vAllVars, iv, vVarsArg.Length)
                        iv = vAllVars.Length
                    End If
                End If
            End If
            If vAllVars.Length Then
                If iv > 1 Then
                    Array.Sort(vAllVars)
                    Dim vV(0) As String
                    iv = 0
                    vV(0) = vAllVars(0)
                    For i = 1 To vAllVars.Length - 1
                        If vAllVars(i) <> vV(iv) Then
                            iv += 1
                            ReDim Preserve vV(iv)
                            vV(iv) = vAllVars(i)
                        End If
                    Next
                    vAllVars = vV
                End If
                bRet = True
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return bRet
    End Function
    Public Function getVarAndNoVarFactors(IRespVar As String, _
                                     ByRef vVarFacExpr() As Expression, _
                                     ByRef vNoVarFacExpr() As Expression, _
                                     Optional iNivel As Int64 = -1) As Boolean
        Dim bRet As Boolean = False
        Try
            ' on Output:
            ' vVarFacExpr() = factors containing variables
            ' vNoVarFacExpr() = factors with no vars.
            If vVarFacExpr Is Nothing Then
                ReDim vVarFacExpr(-1), vNoVarFacExpr(-1)
            End If
            Dim iVF As Int64 = vVarFacExpr.Length
            Dim iNV As Int64 = vNoVarFacExpr.Length
            iNivel += 1
            If pA IsNot Nothing Then
                Dim varsPa() As String = pA.varAll()
                If pA.getDegree AndAlso _
                Array.IndexOf(varsPa, IRespVar) > -1 Then
                    If iNivel = 0 Then
                        ReDim vVarFacExpr(iVF)
                        vVarFacExpr(iVF) = New Expression(pA)
                    End If
                    bRet = True
                Else
                    If iNivel = 0 Then
                        ReDim vNoVarFacExpr(iNV)
                        vNoVarFacExpr(iNV) = New Expression(pA)
                    End If
                    bRet = False
                End If
            ElseIf mOp.Groups("num").Success Then
                bRet = False
            ElseIf mOp.Groups("var2").Success _
            OrElse mOp.Groups("var").Success Then
                If mOp.ToString = IRespVar Then
                    If iNivel = 0 Then
                        ReDim Preserve vVarFacExpr(iVF)
                        vVarFacExpr(iVF) = New Expression( _
                            Polynomial.GetPolynomial(IRespVar))
                    End If
                    bRet = True
                Else
                    If iNivel = 0 Then
                        ReDim Preserve vNoVarFacExpr(iNV)
                        vNoVarFacExpr(iNV) = New Expression( _
                            Polynomial.GetPolynomial(IRespVar))
                    End If
                    bRet = False
                End If
            ElseIf mOp.Groups("fn").Success Then
                If Args(0).getVarAndNoVarFactors(IRespVar, _
                                                vVarFacExpr, _
                                                vNoVarFacExpr) Then
                    If iNivel = 0 Then
                        ReDim Preserve vVarFacExpr(iVF)
                        vVarFacExpr(iVF) = Args(0)
                    End If
                    bRet = True
                Else
                    If iNivel = 0 Then
                        ReDim Preserve vNoVarFacExpr(iNV)
                        vNoVarFacExpr(iNV) = Args(0)
                    End If
                    bRet = False
                End If
                'ElseIf mOp.Groups("custFn").Success Then
            Else
                Dim bArgNotValid As Boolean = False
                Select Case mOp.ToString
                    Case "-", "+"
                        If Args(0).getVarAndNoVarFactors(IRespVar, _
                                vVarFacExpr, _
                                vNoVarFacExpr) AndAlso _
                            Args(1).getVarAndNoVarFactors(IRespVar, _
                                vVarFacExpr, _
                                vNoVarFacExpr) Then
                            If iNivel = 0 Then
                                'ReDim Preserve vVarFacExpr(iVF)
                                'vVarFacExpr(iVF) = Args(0)
                                'ReDim Preserve vVarFacExpr(iVF)
                                'vVarFacExpr(iVF) = Args(1)
                            End If
                            bRet = True
                        Else
                            If iNivel = 0 Then
                                'ReDim Preserve vNoVarFacExpr(iNV)
                                'vNoVarFacExpr(iNV) = Args(0)
                                'ReDim Preserve vNoVarFacExpr(iNV)
                                'vNoVarFacExpr(iNV) = Args(1)
                            End If
                            bRet = False
                        End If
                    Case "*", "/", "^"
                        bRet = False
                        If Args(0).getVarAndNoVarFactors(IRespVar, _
                                vVarFacExpr, _
                                vNoVarFacExpr) Then
                            If iNivel = 0 Then
                                ReDim Preserve vVarFacExpr(iVF)
                                vVarFacExpr(iVF) = Args(0)
                            End If
                            bRet = True
                        Else
                            If iNivel = 0 Then
                                ReDim Preserve vNoVarFacExpr(iNV)
                                vNoVarFacExpr(iNV) = Args(0)
                            End If
                        End If
                        If Args(1).getVarAndNoVarFactors(IRespVar, _
                                vVarFacExpr, _
                                vNoVarFacExpr) Then
                            If iNivel = 0 Then
                                ReDim Preserve vVarFacExpr(iVF)
                                vVarFacExpr(iVF) = Args(1)
                            End If
                            bRet = True
                        Else
                            If iNivel = 0 Then
                                ReDim Preserve vNoVarFacExpr(iNV)
                                vNoVarFacExpr(iNV) = Args(1)
                            End If
                        End If
                End Select
            End If
        Catch ex As Exception
            Throw ex
        Finally
            iNivel -= 1
        End Try
        Return bRet
    End Function

#End Region


End Class
