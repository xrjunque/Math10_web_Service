
Partial Public Class Expression
#Region "Inmediate integrals"
    Public Function InmediateIntegrals(ByVal cfg As Config, _
                                       ByVal IRespVar As String, _
                                       ByVal varId As Int64, _
                                       ByVal vars As VarsAndFns, _
                                       ByRef retExpr As Expression, _
                                       ByVal cur As currentMatch, _
                                       Optional ByRef cnt As Int64 = 0) As Boolean
        Dim ret As Expression = Nothing
        Dim mult As Double = 1.0
        Dim bVarIdentified As Boolean = True
        Dim sVarIdentified As String = ""
        Dim eIndepMult As Expression = Nothing
        Dim eIndepMult2 As Expression = Nothing
        Dim factWithOutVarResp As Expression
        Dim bDetail As Boolean = cfg.bDetail
        Try
            cnt += 1
            cfg.bDetail = False
            factWithOutVarResp = New Expression(1)
            Dim bIsDiv2() As Boolean = Nothing
            factWithOutVarResp = New Expression(CType(Me.sign, Double))
            Dim vFact() As Expression = Me.exprToFactors(bIsDiv2)
            Dim eFact As New Expression(1.0)
            For j As Int64 = 0 To vFact.Length - 1
                If vFact(j).IsReal Then
                    Dim db As Double = vFact(j).toDouble
                    vFact(j) = New Expression(1.0)
                    If bIsDiv2(j) Then
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
                    If bIsDiv2(j) Then
                        eFact /= vFact(j)
                    Else
                        eFact *= vFact(j)
                    End If
                Else
                    If bIsDiv2(j) Then
                        factWithOutVarResp /= vFact(j)
                    Else
                        factWithOutVarResp *= vFact(j)
                    End If
                End If
sigJ:
            Next
            CopyExprToMe(eFact)

            Dim cmp As New Expression(Me)
            sign = cmp.sign
            If sign = -1 Then
                cmp.sign *= -1
            End If
            Dim polyRespVar As Polynomial = _
                Polynomial.GetPolynomial(IRespVar)
            Dim varsArg() As String = Nothing
            Me.getAllVars(varsArg)
            If varsArg.Length = 0 OrElse Array.IndexOf(varsArg, IRespVar) = -1 Then
                ' constant
                ret = Me * New Expression(polyRespVar)
                Exit Try
            End If

            Dim x As New Expression(polyRespVar)
            Dim respVarExpr As New Expression(polyRespVar)

            Dim zero As New Expression(0.0)
            Dim one As New Expression(1.0)
            Dim two As New Expression(2.0)
            Dim onemnx2 As New Expression(New Polynomial(1.0) _
                                - polyRespVar * polyRespVar)
            Dim invsqr1mnx2 As Expression = New Expression(1.0) _
                                    / onemnx2 ^ New Expression(0.5)
            Dim oneplusx2 As New Expression(New Polynomial(1.0) _
                                + polyRespVar * polyRespVar)
            Dim invOnePlusx2 As Expression = New Expression( _
                        New Polynomial(1.0)) / oneplusx2
            Dim sqrx2mn1 As Expression = _
                (-onemnx2) ^ New Expression(0.5)
            Dim invxtimesSqrx2Mn1 As Expression = _
                New Expression(1.0) / _
                (New Expression(polyRespVar) * sqrx2mn1)
            Dim exp As Expression = Expression.AddFnAndArg0("exp", respVarExpr)
            Dim sin As Expression = Expression.AddFnAndArg0("sin", respVarExpr)
            Dim cos As Expression = Expression.AddFnAndArg0("cos", respVarExpr)
            Dim sec As Expression = Expression.AddFnAndArg0("sec", respVarExpr)
            Dim sec2 As Expression = exprOp("^", sec, two)
            Dim csc As Expression = Expression.AddFnAndArg0("csc", respVarExpr)
            Dim csc2 As Expression = exprOp("^", csc, two)
            Dim tan As Expression = Expression.AddFnAndArg0("tan", respVarExpr)
            Dim sectan As Expression = exprOp("*", sec, tan)
            Dim cot As Expression = Expression.AddFnAndArg0("cot", respVarExpr)
            Dim csccot As Expression = exprOp("*", csc, cot)

            Dim fn As Expression = Nothing
            Dim oVars As New VarsAndFns(cfg)
            Dim arg As Expression = Nothing
            Dim arg2 As Expression = Nothing
            Dim Darg As Expression = Nothing
            Dim msg As String = ""
            oVars.AddVar(IRespVar, Nothing, msg)

            ' try to extract from 'me' object the argument
            ' contained in exp() function, if there is such function in 'me'
            ' (extract args(0) from exp() function in 'me'):
            If extractFromPath(New String() {"/", "1"}, arg2) AndAlso _
            arg2.getMatchStr = "exp" Then
                cmp = Expression.AddFnAndArg0("exp", -arg2.Args(0))
                cmp *= Me.Args(0)
                CopyExprToMe(cmp)
            End If
            If extractFromPath(New String() {"exp", "0"}, arg) Then
                ' Here, on the output, arg=f(x) and 
                ' eIndepMult=func(other vars.<> x):
                arg.getDepAndIndep(IRespVar, arg, eIndepMult)
                ' there was an exp() function and 'arg' contains
                ' the argument. Obtain the derivative:
                Darg = arg.opDeriv(IRespVar)
                If eIndepMult IsNot Nothing Then
                    ' again, arg contains function's argument 
                    arg *= eIndepMult
                    eIndepMult = one / eIndepMult
                End If
                ' set varId=0 ('x') by the argument found (arg):
                oVars.setValue(0, New ExprMatrix(arg))
                ' replace exp(x) by exp(arg) into 'u':
                fn = exp.evalExprToExpr(oVars)
                If cmp.isEqualTo(Darg * fn, mult, "*") Then
                    ' ∫Du(x)*exp(u(x))*dx = exp(u(x)), i.e.:
                    '=∫Dx(arg)*exp(arg)*dx = exp(arg)
                    ret = fn
                    ' if there was a multiplying factor then
                    ' mult <> 1.0 and, at the end of this method,
                    ' ret is multiplied by mult
                    Exit Try
                End If
            End If
            If (extractFromPath(New String() {"*", "0"}, arg) AndAlso _
            extractFromPath(New String() {"*", "1"}, arg2)) Then
                Dim bPossible As Boolean = True
                Dim dbExp As Double = 1.0
                Dim f As Expression = arg
                If arg.getMatchStr = "^" AndAlso arg.getArgs(1).IsReal Then
                    dbExp = arg.getArgs(1).toDouble
                    f = arg.getArgs(0)
                ElseIf arg.getMatchStr.Length Then
                    bPossible = False
                End If
                If bPossible Then
                    Dim Df As Expression = f.opDeriv(IRespVar)
                    If Df.isEqualTo(arg2, mult) Then
                        ' ∫ (f^exp * mult * Df )dx = (1/mult/(exp+1))* f^(exp+1)
                        ret = f ^ New Expression(dbExp + 1.0)
                        mult = 1.0 / mult / (dbExp + 1.0)
                        Exit Try
                    End If
                End If
            End If
            If extractFromPath(New String() {"*", "1"}, arg) AndAlso _
            extractFromPath(New String() {"*", "0"}, arg2) Then
                Dim bPossible As Boolean = True
                Dim dbExp As Double = 1.0
                Dim f As Expression = arg
                If arg.getMatchStr = "^" AndAlso arg.getArgs(1).IsReal Then
                    dbExp = arg.getArgs(1).toDouble
                    f = arg.getArgs(0)
                ElseIf arg.getMatchStr.Length Then
                    bPossible = False
                End If
                If bPossible Then
                    Dim Df As Expression = f.opDeriv(IRespVar)
                    If Df.isEqualTo(arg2, mult) Then
                        ' ∫ (f^exp * mult * Df )dx = (1/mult/(exp+1))* f^(exp+1)
                        ret = f ^ New Expression(dbExp + 1.0)
                        mult = 1.0 / mult / (dbExp + 1.0)
                        Exit Try
                    End If
                End If
            End If
            ' extract args(1) from 'me', this is, eventualy,
            ' the denominator from 'me', if mOp="/":
            If extractFromPath(New String() {"/", "1"}, arg2) Then
                arg = New Expression(Me.getArgs(0))
                arg2 = New Expression(Me.getArgs(1))
                ' Here, on the output, arg=f(x) and 
                ' eIndepMult=func(other vars.<> x):
                arg2.getDepAndIndep(IRespVar, arg, eIndepMult)
                Dim Darg2 As Expression = arg2.opDeriv(IRespVar)
                If eIndepMult IsNot Nothing Then
                    arg2 *= eIndepMult
                    eIndepMult = one / eIndepMult
                End If
                oVars.setValue(0, New ExprMatrix(arg2))
                If cmp.isEqualTo(Darg2 / arg2, mult, "*") Then
                    ' ∫Du/u = ln(abs((u)) 
                    ret = Expression.AddFnAndArg0("abs", respVarExpr)
                    ret = Expression.AddFnAndArg0("ln", ret)
                    ret = ret.evalExprToExpr(oVars)
                    Exit Try
                ElseIf arg2.mOp IsNot Nothing AndAlso _
                arg2.mOp.ToString = "^" AndAlso _
                arg2.Args.Length > 1 AndAlso _
                arg2.Args(1).IsReal Then
                    ' arg/arg2 =  D(den) / (den^exp) ??
                    Dim den As Expression = arg2.Args(0)
                    Dim Dden As Expression = den.opDeriv(IRespVar)
                    If extractFromPath(New String() {"/", "0"}, arg) AndAlso _
                    Dden.isEqualTo(arg, mult) Then
                        ' ∫Du/u^exp = ∫Du*u^-exp = u^(-exp+1)/(exp+1)
                        ' = den^(exp+1)/(exp+1)
                        Dim pRe As Rational = -arg2.Args(1).pA.An.pRe
                        Dim pRePlusOne As Rational = pRe + New Rational(1.0)
                        Dim cjoRePlusOne As New Complex(pRePlusOne, New Rational(0.0))
                        ret = den ^ New Expression(cjoRePlusOne)
                        mult *= pRePlusOne.ToDouble
                        Exit Try
                    End If
                End If
            End If
            If extractFromPath(New String() {"^", "0"}, arg) AndAlso _
            extractFromPath(New String() {"^", "1"}, arg2) Then
                If arg.isEqualTo(respVarExpr) AndAlso _
                arg2.IsReal AndAlso _
                cmp.isEqualTo(arg ^ arg2, mult, "*") Then
                    ' ∫x^a = 1/(a-1) x^(a+1)
                    Dim db As Double = arg2.pA.cf(0).pRe.ToDouble
                    ret = exprOp("^", arg, New Expression(db + 1))
                    mult *= 1 / (db + 1)
                    Exit Try
                End If
            End If
            If extractFromPath(New String() {"/", "0"}, arg) AndAlso _
            extractFromPath(New String() {"/", "1"}, arg2) Then
                Dim arg3 As Expression = Nothing
                Dim arg4 As Expression = Nothing
                If arg2.extractFromPath(New String() {"^", "0"}, arg3) AndAlso _
                arg2.extractFromPath(New String() {"^", "1"}, arg4) Then
                    If arg3.isEqualTo(respVarExpr) AndAlso _
                    arg4.IsReal AndAlso _
                    cmp.isEqualTo(one / (arg3 ^ arg4), mult, "*") Then
                        ' ∫1/(x^a) = 1/(-a+1) x^(-a+1)
                        Dim db As Double = arg4.pA.cf(0).pRe.ToDouble
                        ret = exprOp("^", arg3, New Expression(-db + 1))
                        mult *= 1 / (-db + 1)
                        Exit Try
                    End If
                End If

            End If

            If extractFromPath(New String() {"csc", "0"}, arg) AndAlso _
            extractFromPath(New String() {"cot", "0"}, arg2) AndAlso _
            arg.isEqualTo(arg2) Then
                oVars.setValue(0, New ExprMatrix(arg))
                fn = (csc * cot).evalExprToExpr(oVars)
                Darg = arg.opDeriv(IRespVar)
                If cmp.isEqualTo(Darg * fn, mult, "*") Then
                    ' ∫csc(x)*cot(x) = -csc(x)
                    ret = -Expression.AddFnAndArg0("csc", respVarExpr)
                    ret = ret.evalExprToExpr(oVars)
                    Exit Try
                End If
            End If
            If extractFromPath(New String() {"sec", "0"}, arg) AndAlso _
            extractFromPath(New String() {"tan", "0"}, arg2) AndAlso _
            arg.isEqualTo(arg2) Then
                oVars.setValue(0, New ExprMatrix(arg))
                fn = (sec * tan).evalExprToExpr(oVars)
                Darg = arg.opDeriv(IRespVar)
                If cmp.isEqualTo(Darg * fn, mult, "*") Then
                    ' ∫sec(x)*tan(x) = sec(x)
                    ret = Expression.AddFnAndArg0("sec", respVarExpr)
                    ret = ret.evalExprToExpr(oVars)
                    Exit Try
                End If
            End If
            If extractFromPath(New String() {"sin", "0"}, arg) Then
                ' again, arg contains function's argument 

                ' On the output, arg=f(x) and 
                ' eIndepMult=func(other vars.<> x):
                arg.getDepAndIndep(IRespVar, arg, eIndepMult)
                Darg = arg.opDeriv(IRespVar)
                If eIndepMult IsNot Nothing Then
                    arg *= eIndepMult
                    eIndepMult = one / eIndepMult
                End If
                oVars.setValue(0, New ExprMatrix(arg))
                fn = sin.evalExprToExpr(oVars)
                If cmp.isEqualTo(Darg * fn, mult, "*") Then
                    ' ∫Du*sin(u) = -cos(u)
                    ret = -cos.evalExprToExpr(oVars)
                    Exit Try
                ElseIf cmp.isEqualTo(Darg * fn * arg) Then
                    ' ∫x*sin(x) = sin(x) - x*cos(x)
                    ret = sin - arg * cos
                    ret = ret.evalExprToExpr(oVars)
                    Exit Try
                End If
            End If
            If extractFromPath(New String() {"cos", "0"}, arg) Then
                ' On the output, arg=f(x) and 
                ' eIndepMult=func(other vars.<> x):
                arg.getDepAndIndep(IRespVar, arg, eIndepMult)
                Darg = arg.opDeriv(IRespVar)
                ' again, arg contains function's argument 
                If eIndepMult IsNot Nothing Then
                    arg *= eIndepMult
                    eIndepMult = one / eIndepMult
                End If
                oVars.setValue(0, New ExprMatrix(arg))
                fn = cos.evalExprToExpr(oVars)
                If cmp.isEqualTo(Darg * fn, mult, "*") Then
                    ' ∫cos(u)*Du = sin(u)
                    ret = sin.evalExprToExpr(oVars)
                    Exit Try
                End If
            End If
            If extractFromPath(New String() {"tan", "0"}, arg) Then
                ' On the output, arg=f(x) and 
                ' eIndepMult=func(other vars.<> x):
                arg.getDepAndIndep(IRespVar, arg, eIndepMult)
                Darg = arg.opDeriv(IRespVar)
                ' again, arg contains function's argument 
                If eIndepMult IsNot Nothing Then
                    arg *= eIndepMult
                    eIndepMult = one / eIndepMult
                End If
                oVars.setValue(0, New ExprMatrix(arg))
                fn = tan.evalExprToExpr(oVars)
                If cmp.isEqualTo(Darg * fn, mult, "*") Then
                    ' ∫tan(x) = -ln(abs(cos(x)))
                    ret = cos
                    ret = Expression.AddFnAndArg0("abs", ret)
                    ret = -Expression.AddFnAndArg0("ln", ret)
                    ret = ret.evalExprToExpr(oVars)
                    Exit Try
                End If
            End If
            If extractFromPath(New String() {"cot", "0"}, arg) Then

                ' On the output, arg=f(x) and 
                ' eIndepMult=func(other vars.<> x):
                arg.getDepAndIndep(IRespVar, arg, eIndepMult)
                Darg = arg.opDeriv(IRespVar)
                If eIndepMult IsNot Nothing Then
                    ' again, arg contains function sin()'s inside argument 
                    arg *= eIndepMult
                    eIndepMult = one / eIndepMult
                End If
                oVars.setValue(0, New ExprMatrix(arg))
                fn = cot.evalExprToExpr(oVars)
                If cmp.isEqualTo(Darg * fn, mult, "*") Then
                    ' ∫cot(x) = ln(abs((sin(x)))
                    ret = sin
                    ret = Expression.AddFnAndArg0("abs", ret)
                    ret = Expression.AddFnAndArg0("ln", ret)
                    ret = ret.evalExprToExpr(oVars)
                    Exit Try
                End If
            End If
            If extractFromPath(New String() {"^", "0", "sec", "0"}, arg) Then

                ' On the output, arg=f(x) and 
                ' eIndepMult=func(other vars.<> x):
                arg.getDepAndIndep(IRespVar, arg, eIndepMult)
                Darg = arg.opDeriv(IRespVar)
                If eIndepMult IsNot Nothing Then
                    ' again, arg contains function's argument 
                    arg *= eIndepMult
                    eIndepMult = one / eIndepMult
                End If
                oVars.setValue(0, New ExprMatrix(arg))
                fn = (sec * sec).evalExprToExpr(oVars)
                If cmp.isEqualTo(Darg * fn, mult, "*") Then
                    ' ∫sec(x)^2 = tan(x)
                    ret = tan
                    ret = ret.evalExprToExpr(oVars)
                    Exit Try
                End If
            End If
            If extractFromPath(New String() {"sec", "0"}, arg) OrElse _
            extractFromPath(New String() {"/", "1", "cos", "0"}, arg) Then
                ' On the output, arg=f(x) and 
                ' eIndepMult=func(other vars.<> x):
                arg.getDepAndIndep(IRespVar, arg, eIndepMult)
                Darg = arg.opDeriv(IRespVar)
                If eIndepMult IsNot Nothing Then
                    ' again, arg contains function's argument 
                    arg *= eIndepMult
                    eIndepMult = one / eIndepMult
                End If
                oVars.setValue(0, New ExprMatrix(arg))
                fn = sec.evalExprToExpr(oVars)
                If cmp.isEqualTo(Darg * fn, mult, "*") OrElse _
                cmp.isEqualTo(Darg / cos.evalExprToExpr(oVars), mult, "*") Then
                    ' ∫sec(x) = ∫1/cos(x) = ln(abs((sec(x)+tan(x)))
                    ret = sec + tan
                    ret = Expression.AddFnAndArg0("abs", ret)
                    ret = Expression.AddFnAndArg0("ln", ret)
                    ret = ret.evalExprToExpr(oVars)
                    Exit Try
                End If
            End If
            If extractFromPath(New String() {"^", "0", "csc", "0"}, arg) Then
                ' On the output, arg=f(x) and 
                ' eIndepMult=func(other vars.<> x):
                arg.getDepAndIndep(IRespVar, arg, eIndepMult)
                Darg = arg.opDeriv(IRespVar)
                If eIndepMult IsNot Nothing Then
                    ' again, arg contains function's argument 
                    arg *= eIndepMult
                    eIndepMult = one / eIndepMult
                End If
                oVars.setValue(0, New ExprMatrix(arg))
                fn = (csc * csc).evalExprToExpr(oVars)
                If cmp.isEqualTo(Darg * fn, mult, "*") Then
                    ' ∫csc(x)^2 = -cot(x)
                    ret = -cot
                    ret = ret.evalExprToExpr(oVars)
                    Exit Try
                End If
            End If
            If extractFromPath(New String() {"/", "1", "sin", "0"}, arg) Then
                ' On the output, arg=f(x) and 
                ' eIndepMult=func(other vars.<> x):
                arg.getDepAndIndep(IRespVar, arg, eIndepMult)
                Darg = arg.opDeriv(IRespVar)
                If eIndepMult IsNot Nothing Then
                    ' again, arg contains function's argument 
                    arg *= eIndepMult
                    eIndepMult = one / eIndepMult
                End If
                fn = Expression.AddFnAndArg0("sin", arg)
                fn = one / fn
                If cmp.isEqualTo(Darg * fn, mult, "*") Then
                    ' ∫csc(x) = ∫1/sin(x) = -ln(abs((csc(x)-cot(x)))
                    ret = csc - cot
                    ret = Expression.AddFnAndArg0("abs", ret)
                    ret = Expression.AddFnAndArg0("ln", ret)
                    ret = ret.evalExprToExpr(oVars)
                    Exit Try
                End If
            End If
            If IsPolynomial() AndAlso pA.PolyResto IsNot Nothing AndAlso _
            pA.getDegree = 0 AndAlso pA.cf(0).esCero Then
                Dim DpolyDiv As Polynomial = _
                    pA.PolyDivisor.opDerivative(IRespVar)
                Dim cjoMult As Complex = Nothing
                If DpolyDiv.IsEqual(pA.PolyResto, cjoMult) Then
                    ' ∫du/u = ln(u)
                    ret = Expression.AddFnAndArg0("ln", _
                        New Expression(pA.PolyDivisor))
                    ret *= New Expression(cjoMult)
                    Exit Try
                End If
                Dim integOf_Darg As Polynomial = pA.PolyResto.opIntegral(IRespVar)
                mult = integOf_Darg.An.pRe.ToDouble
                integOf_Darg = Polynomial.opNormalize(integOf_Darg)
                Dim integ2 As Polynomial = integOf_Darg * integOf_Darg
                arg = New Expression(integOf_Darg)
                Dim ratio As Polynomial = pA.PolyDivisor / (integ2 + New Polynomial(1.0))
                If ratio.isReal AndAlso _
                ratio.cf(0).pRe.ToDouble = 1.0 Then
                    ' ∫du*1/(1+u2) = atan(u)
                    ret = Expression.AddFnAndArg0("atan", respVarExpr)
                    oVars.setValue(0, New ExprMatrix(arg))
                    ret = ret.evalExprToExpr(oVars)
                    Exit Try
                End If
            End If
            If extractFromPath(New String() {"/", "0"}, Darg) Then
                Try
                    Dim bIsDiv(-1) As Boolean
                    arg = Darg.opAntiDerivative(cfg, varId, _
                            New Expression() {Darg}, cur, vars, IRespVar, Nothing, Nothing, Nothing, cnt:=cnt)
                    If arg IsNot Nothing Then
                        Dim vFac() As Expression = arg.exprToFactors(bIsDiv)
                        If vFac(0).IsPolynomial Then
                            mult = vFac(0).pA.An.pRe.ToDouble
                            If mult AndAlso mult <> 1.0 Then
                                vFac(0).pA /= New Polynomial(mult)
                                arg = factorsToExpr(vFac, bIsDiv)
                                Darg = arg.opDeriv(IRespVar)
                            End If
                        End If
                        Dim sqrx2mnOne As Expression = Expression.AddFnAndArg0("sqr", arg * arg - one)
                        Dim mult2 As Double
                        If Not arg.isEqualTo(zero) AndAlso _
                        cmp.isEqualTo(Darg / (arg * sqrx2mnOne), mult2, "*") Then
                            ' ∫1/(x*sqr(x2-1)) = asec(x)
                            ret = Expression.AddFnAndArg0("asec", respVarExpr)
                            oVars.setValue(0, New ExprMatrix(arg))
                            ret = ret.evalExprToExpr(oVars)
                            Exit Try
                        End If
                    End If
                Catch ex As Exception
                    If cnt > maxCntRecursive Then
                        Throw ex
                    End If
                End Try
            End If
            If extractFromPath(New String() {"ln", "0"}, arg) Then
                oVars.setValue(0, New ExprMatrix(arg))
                fn = Expression.AddFnAndArg0("ln", arg)
                Darg = arg.opDeriv(IRespVar)
                If cmp.isEqualTo(Darg * fn, mult, "*") Then
                    ' ∫ln(x) = x*ln(x) - x
                    ret = arg * fn - arg
                    Exit Try
                End If
            End If
            If extractFromPath(New String() {"sin", "0"}, arg) Then
                oVars.setValue(0, New ExprMatrix(arg))
                fn = Expression.AddFnAndArg0("sin", arg)
                Darg = arg.opDeriv(IRespVar)
                If cmp.isEqualTo(Darg * arg * fn, mult, "*") Then
                    ' ∫x*sin(x) = sin(x) - x*cos(x)
                    ret = fn - arg * Expression.AddFnAndArg0("cos", arg)
                    Exit Try
                End If
            End If
            If extractFromPath(New String() {"cos", "0"}, arg) Then
                oVars.setValue(0, New ExprMatrix(arg))
                fn = Expression.AddFnAndArg0("cos", arg)
                Darg = arg.opDeriv(IRespVar)
                If cmp.isEqualTo(Darg * arg * fn, mult, "*") Then
                    ' ∫x*cos(x) = cos(x) + x*sin(x)
                    ret = fn + arg * Expression.AddFnAndArg0("sin", arg)
                    Exit Try
                End If
            End If
            If extractFromPath(New String() {"sin", "0"}, arg) AndAlso _
            extractFromPath(New String() {"exp", "0"}, arg2) AndAlso _
            arg.isEqualTo(arg2) Then
                oVars.setValue(0, New ExprMatrix(arg))
                fn = Expression.AddFnAndArg0("sin", arg)
                Dim fn2 As Expression = Expression.AddFnAndArg0("exp", arg)
                Darg = arg.opDeriv(IRespVar)
                If cmp.isEqualTo(Darg * fn2 * fn, mult, "*") Then
                    ' ∫exp(x)*sin(x) = 0.5 * exp(x) *(sin(x) - cos(x))
                    mult /= 2.0
                    ret = fn2 * (fn - Expression.AddFnAndArg0("cos", arg))
                    Exit Try
                End If
            End If
            If extractFromPath(New String() {"sec", "0"}, arg) AndAlso _
            extractFromPath(New String() {"tan", "0"}, arg2) AndAlso _
            arg.isEqualTo(arg2) Then
                oVars.setValue(0, New ExprMatrix(arg))
                fn = Expression.AddFnAndArg0("sin", arg)
                Dim fn2 As Expression = Expression.AddFnAndArg0("exp", arg)
                Darg = arg.opDeriv(IRespVar)
                If cmp.isEqualTo(Darg * fn2 * fn, mult, "*") Then
                    ' ∫sec(x)*tan(x) = 1/cos(x)
                    ret = Expression.AddFnAndArg0("cos", arg)
                    Exit Try
                End If
            End If
            If extractFromPath(New String() {"sinh", "0"}, arg) Then
                ' On the output, arg=f(x) and 
                ' eIndepMult=func(other vars.<> x):
                arg.getDepAndIndep(IRespVar, arg, eIndepMult)
                Darg = arg.opDeriv(IRespVar)
                If eIndepMult IsNot Nothing Then
                    ' again, arg contains function's argument 
                    arg *= eIndepMult
                    eIndepMult = one / eIndepMult
                End If

                oVars.setValue(0, New ExprMatrix(arg))
                fn = Expression.AddFnAndArg0("sinh", arg)
                If cmp.isEqualTo(Darg * fn, mult, "*") Then
                    ' ∫sinh(x) = cosh(x)
                    ret = Expression.AddFnAndArg0("cosh", arg)
                    Exit Try
                End If
            End If
            If extractFromPath(New String() {"cosh", "0"}, arg) Then
                ' On the output, arg=f(x) and 
                ' eIndepMult=func(other vars.<> x):
                arg.getDepAndIndep(IRespVar, arg, eIndepMult)
                Darg = arg.opDeriv(IRespVar)
                If eIndepMult IsNot Nothing Then
                    ' again, arg contains function's argument 
                    arg *= eIndepMult
                    eIndepMult = one / eIndepMult
                End If

                oVars.setValue(0, New ExprMatrix(arg))
                fn = Expression.AddFnAndArg0("cosh", arg)

                If cmp.isEqualTo(Darg * fn, mult, "*") Then
                    ' ∫cosh(x) = sinh(x)
                    ret = Expression.AddFnAndArg0("sinh", arg)
                    Exit Try
                End If
            End If
            If extractFromPath(New String() {"sech", "0"}, arg) Then
                ' On the output, arg=f(x) and 
                ' eIndepMult=func(other vars.<> x):
                arg.getDepAndIndep(IRespVar, arg, eIndepMult)
                Darg = arg.opDeriv(IRespVar)
                If eIndepMult IsNot Nothing Then
                    ' again, arg contains function's argument 
                    arg *= eIndepMult
                    eIndepMult = one / eIndepMult
                End If

                oVars.setValue(0, New ExprMatrix(arg))
                fn = Expression.AddFnAndArg0("sech", arg)

                If cmp.isEqualTo(Darg * fn * fn, mult, "*") Then
                    ' ∫sech(x)^2 = tanh(x)
                    ret = Expression.AddFnAndArg0("tanh", arg)
                    Exit Try
                End If
            End If
            If extractFromPath(New String() {"csch", "0"}, arg) Then
                ' On the output, arg=f(x) and 
                ' eIndepMult=func(other vars.<> x):
                arg.getDepAndIndep(IRespVar, arg, eIndepMult)
                Darg = arg.opDeriv(IRespVar)
                If eIndepMult IsNot Nothing Then
                    ' again, arg contains function's argument 
                    arg *= eIndepMult
                    eIndepMult = one / eIndepMult
                End If

                oVars.setValue(0, New ExprMatrix(arg))
                fn = Expression.AddFnAndArg0("csch", arg)

                If cmp.isEqualTo(Darg * fn * fn, mult, "*") Then
                    ' ∫csch(x)^2 = -coth(x)
                    mult *= -1
                    ret = Expression.AddFnAndArg0("coth", arg)
                    Exit Try
                End If
            End If
            If extractFromPath(New String() {"sech", "0"}, arg) AndAlso _
            extractFromPath(New String() {"tanh", "0"}, arg2) AndAlso _
            arg.isEqualTo(arg2) Then
                oVars.setValue(0, New ExprMatrix(arg))
                fn = Expression.AddFnAndArg0("sech", arg)
                Dim fn2 As Expression = Expression.AddFnAndArg0("tanh", arg)
                Darg = arg.opDeriv(IRespVar)
                If cmp.isEqualTo(Darg * fn2 * fn, mult, "*") Then
                    ' ∫sech(x)*tanh(x) = -sech(x)
                    mult *= -1.0
                    ret = Expression.AddFnAndArg0("sech", arg)
                    Exit Try
                End If
            End If
            If extractFromPath(New String() {"csch", "0"}, arg) AndAlso _
            extractFromPath(New String() {"coth", "0"}, arg2) AndAlso _
            arg.isEqualTo(arg2) Then
                oVars.setValue(0, New ExprMatrix(arg))
                fn = Expression.AddFnAndArg0("csch", arg)
                Dim fn2 As Expression = Expression.AddFnAndArg0("coth", arg)
                Darg = arg.opDeriv(IRespVar)
                If cmp.isEqualTo(Darg * fn2 * fn, mult, "*") Then
                    ' ∫csch(x)*coth(x) = -csch(x)
                    mult *= -1.0
                    ret = Expression.AddFnAndArg0("csch", arg)
                    Exit Try
                End If
            End If
            If extractFromPath(New String() {"/", "0"}, Darg) Then
                Try
                    Dim bIsDiv(-1) As Boolean
                    arg = Darg.opAntiDerivative(cfg, varId, _
                            New Expression() {Darg}, cur, vars, IRespVar, Nothing, Nothing, Nothing, cnt:=cnt)
                    If arg IsNot Nothing Then
                        Dim vFac() As Expression = arg.exprToFactors(bIsDiv)
                        If vFac(0).IsPolynomial Then
                            mult = vFac(0).pA.An.pRe.ToDouble
                            If mult AndAlso mult <> 1.0 Then
                                vFac(0).pA /= New Polynomial(mult)
                                arg = factorsToExpr(vFac, bIsDiv)
                                Darg = arg.opDeriv(IRespVar)
                            End If
                        End If
                        Dim sqrx2plusOne As Expression = Expression.AddFnAndArg0("sqr", arg * arg + one)
                        Dim mult2 As Double
                        If cmp.isEqualTo(Darg / sqrx2plusOne, mult2, "*") Then
                            ' ∫1/sqr(1+x2) = asinh(x)
                            ret = Expression.AddFnAndArg0("asinh", arg)
                            Exit Try
                        End If
                    End If
                Catch ex As Exception
                    If cnt > maxCntRecursive Then
                        Throw ex
                    End If
                End Try
            End If
            If extractFromPath(New String() {"/", "0"}, Darg) Then
                Try
                    Dim bIsDiv(-1) As Boolean
                    arg = Darg.opAntiDerivative(cfg, varId, _
                            New Expression() {Darg}, cur, vars, IRespVar, Nothing, Nothing, Nothing, cnt:=cnt)
                    If arg IsNot Nothing Then
                        Dim vFac() As Expression = arg.exprToFactors(bIsDiv)
                        If vFac(0).IsPolynomial Then
                            mult = vFac(0).pA.An.pRe.ToDouble
                            If mult AndAlso mult <> 1.0 Then
                                vFac(0).pA /= New Polynomial(mult)
                                arg = factorsToExpr(vFac, bIsDiv)
                                Darg = arg.opDeriv(IRespVar)
                            End If
                        End If
                        Dim sqrx2mnOne As Expression = Expression.AddFnAndArg0("sqr", arg * arg - one)
                        Dim mult2 As Double
                        If cmp.isEqualTo(Darg / sqrx2mnOne, mult2, "*") Then
                            ' ∫1/sqr(x2-1) = acosh(x)
                            ret = Expression.AddFnAndArg0("acosh", arg)
                            Exit Try
                        End If
                    End If
                Catch ex As Exception
                    If cnt > maxCntRecursive Then
                        Throw ex
                    End If
                End Try
            End If
            If extractFromPath(New String() {"/", "0"}, arg) AndAlso _
            extractFromPath(New String() {"/", "1"}, arg2) Then
                Dim bIsDiv(-1) As Boolean
                arg = (-(arg2 - one)) ^ New Expression(0.5)
                arg = arg.reduceFactors(False)
                Dim vFac() As Expression = arg.exprToFactors(bIsDiv)
                'Dim mult2 As Double
                If Darg.isEqualTo(arg.opDeriv(IRespVar), mult, "*") Then
                    ' ∫1/(1-x2) = atanh(x) , if x2<1 
                    ret = Expression.AddFnAndArg0("atanh", arg)
                    Exit Try
                End If
            End If
            If extractFromPath(New String() {"/", "0"}, Darg) Then
                Try
                    Dim bIsDiv(-1) As Boolean
                    arg = Darg.opAntiDerivative(cfg, varId, _
                            New Expression() {Darg}, cur, vars, IRespVar, Nothing, Nothing, Nothing, cnt:=cnt)
                    If arg IsNot Nothing Then
                        Dim vFac() As Expression = arg.exprToFactors(bIsDiv)
                        If vFac(0).IsPolynomial Then
                            mult = vFac(0).pA.An.pRe.ToDouble
                            If mult AndAlso mult <> 1.0 Then
                                vFac(0).pA /= New Polynomial(mult)
                                arg = factorsToExpr(vFac, bIsDiv)
                                Darg = arg.opDeriv(IRespVar)
                            End If
                        End If
                        Dim sqrOnemnx2 As Expression = Expression.AddFnAndArg0("sqr", one - arg * arg)
                        Dim mult2 As Double
                        If Not arg.isEqualTo(zero) AndAlso _
                        Not arg.isEqualTo(zero) AndAlso _
                        cmp.isEqualTo(Darg / (arg * sqrOnemnx2), mult2, "*") Then
                            ' ∫1/(x*sqr(1-x2)) = -asech(x), 0<x<1
                            mult *= -1
                            ret = Expression.AddFnAndArg0("asec", respVarExpr)
                            oVars.setValue(0, New ExprMatrix(arg))
                            ret = ret.evalExprToExpr(oVars)
                            Exit Try
                        End If
                    End If
                Catch ex As Exception
                    If cnt > maxCntRecursive Then
                        Throw ex
                    End If
                End Try
            End If
            If extractFromPath(New String() {"/", "0"}, Darg) Then
                Try
                    Dim bIsDiv(-1) As Boolean
                    arg = Darg.opAntiDerivative(cfg, varId, _
                            New Expression() {Darg}, cur, vars, IRespVar, Nothing, Nothing, Nothing, cnt:=cnt)
                    If arg IsNot Nothing Then
                        Dim vFac() As Expression = arg.exprToFactors(bIsDiv)
                        If vFac(0).IsPolynomial Then
                            mult = vFac(0).pA.An.pRe.ToDouble
                            If mult AndAlso mult <> 1.0 Then
                                vFac(0).pA /= New Polynomial(mult)
                                arg = factorsToExpr(vFac, bIsDiv)
                                Darg = arg.opDeriv(IRespVar)
                            End If
                        End If
                        Dim sqrx2plusOne As Expression = Expression.AddFnAndArg0("sqr", arg * arg + one)
                        Dim mult2 As Double
                        If Not arg.isEqualTo(zero) AndAlso _
                        cmp.isEqualTo(Darg / ( _
                        Expression.AddFnAndArg0("abs", arg) * sqrx2plusOne), mult2, "*") Then
                            ' ∫1/(|x|*sqr(1+x2)) = -acsch(x), x <> 0
                            mult *= -1
                            ret = Expression.AddFnAndArg0("acsch", respVarExpr)
                            oVars.setValue(0, New ExprMatrix(arg))
                            ret = ret.evalExprToExpr(oVars)
                            Exit Try
                        End If
                    End If
                Catch ex As Exception
                    If cnt > maxCntRecursive Then
                        Throw ex
                    End If
                End Try
            End If
            If extractFromPath(New String() {"^", "1"}, arg) AndAlso _
            extractFromPath(New String() {"^", "0"}, arg2) Then
                If arg2.IsReal Then
                    mult = Math.Log(arg2.pA.cf(0).pRe.ToDouble)
                    oVars.setValue(0, New ExprMatrix(arg))
                    fn = arg2 ^ arg
                    Darg = arg.opDeriv(IRespVar)
                    Dim mult2 As Double
                    If cmp.isEqualTo(Darg * fn, mult2, "*") Then
                        ' -∫a^x*ln(a) = a^x, x,a>0
                        ret = fn
                        mult = mult2 / mult
                        Exit Try
                    End If
                End If
            End If
        Catch ex As Exception
            Throw New Exception(msg8.num(13)) ' n/a 
        End Try
        If ret IsNot Nothing Then
            If eIndepMult IsNot Nothing Then
                ret *= eIndepMult
            End If
            mult *= sign
            If mult = 1 Then
            ElseIf mult = -1 Then
                ret = -ret
            Else
                ret = New Expression(mult) * ret
            End If
            retExpr = factWithOutVarResp * ret
            cfg.bDetail = bDetail
            Return True
        End If
        cfg.bDetail = bDetail
        Return False
    End Function
#End Region
End Class
