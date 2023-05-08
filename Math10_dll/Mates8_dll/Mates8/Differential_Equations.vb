Imports System.Text.RegularExpressions

Public Class Differential_Equations
    Public Sub New()
        MyBase.New()
    End Sub

    Dim cfg As Config
    Dim eq, A, B, Y, N, M, Nx, Mx, Ny, My1, Q, Adiv, Bdiv, Subst As Expression
    Dim nQ As Double
    Dim sVar As String
    Dim sDiffRespVar As String
    Public tipo As DiffEqType
    Public nCall As Int64

    Public Shared Function tryParseDifEq(equation As Expression, Optional nCall As Int64 = 0) As Differential_Equations

        ' From 'equation' try to obtain what kind of diff. equation is it.

        Dim ret As New Differential_Equations
        Dim rup As New ReduceExprUsingPolynomials()
        Try
            nCall += 1
            ret.eq = New Expression(equation)
            ret.cfg = equation.cfg
            Dim vResult(0) As Expression
            Dim A As New Expression(0.0)
            Dim B As New Expression(0.0)
            Dim Y As New Expression(0.0)
            Dim i, j As Int64
            Dim vIsDiv(-1) As Boolean
            Dim vFact(-1) As Expression
            Dim DiffTerm As Expression = Nothing

            Dim Expr1 As Expression = Nothing
            Dim CommExpr As Expression = Nothing
            Dim commStr As String = ""

            Dim vSum() As Expression = ret.eq.splitIntoTerms

            ' Try to assure no dividing factors
            Dim vMult(vSum.Length - 1) As Expression
            For i = 0 To vSum.Length - 1
                vFact = vSum(i).exprToFactors(vIsDiv, True)
                Dim bReDo As Boolean = False
                For j = 0 To vFact.Length - 1
                    If vIsDiv(j) Then
                        If vMult(i) Is Nothing Then
                            vMult(i) = New Expression(vFact(j))
                        Else
                            vMult(i) *= vFact(j)
                        End If
                        bReDo = True
                        vFact(j) = Nothing
                    End If
                Next
                If bReDo Then
                    vSum(i) = Expression.factorsToExpr(vFact, vIsDiv)
                End If
            Next
            For i = 0 To vMult.Length - 1
                If vMult(i) IsNot Nothing Then
                    For j = 0 To vSum.Length - 1
                        If i <> j Then
                            vSum(j) *= vMult(i)
                            vSum(j) = rup.ReduceUsingPolynomials(vSum(j))
                        End If
                    Next
                End If
            Next
            Dim vIsMn(vSum.Length - 1) As Boolean
            ret.eq = Expression.summandsToExpr(vSum, vIsMn)
            equation = ret.eq




            'If ret.eq.tryToIsolateToExpression("", vResult, vResult(0), DiffTerm) Then
            ' ret.eq = DiffTerm - vResult(0)
            ' End If
            vSum = ret.eq.splitIntoTerms
            ' Obtener el término A en Dx(y), ret.sVar y ret.sDiffRespVar:
            Dim vDiff(-1), ivd As Int64
            For i = 0 To vSum.Length - 1
                vFact = vSum(i).exprToFactors(vIsDiv, True)
                For j = 0 To vFact.Length - 1
                    If vFact(j).getMatchStr = "diff" Then
                        Dim sgn As Int64 = vFact(j).sign
                        ret.sDiffRespVar = vFact(j).getArgs(1).getPolynomial.var1(0)
                        ret.sVar = vFact(j).getArgs(0).getPolynomial.var1(0)
                        If sgn = 1 Then
                            vFact(j) = New Expression(1.0)
                        Else
                            vFact(j) = New Expression(-1.0)
                        End If
                        vSum(i) = Expression.factorsToExpr(vFact, vIsDiv)
                        ReDim Preserve vDiff(ivd)
                        vDiff(ivd) = i : ivd += 1
                        Exit For
                    End If
                Next
                If j < vFact.Length Then
                    A += vSum(i)
                End If
            Next
            ' Averiguar todas las potencias de "y":
            Dim maxYdegree As Int64 = 0, ivY As Int64 = 0
            Dim minYdegree As Int64 = 0
            For i = 0 To vSum.Length - 1
                vFact = vSum(i).exprToFactors(vIsDiv, True)
                For j = 0 To vFact.Length - 1
                    If vFact(j).IsPolynomial Then
                        Dim Pa As Polynomial = vFact(j).getPolynomial
                        Dim degree As Int64 = Pa.getMaxExponentOfVar(ret.sVar)
                        If Pa.PolyResto IsNot Nothing Then
                            degree = Pa.PolyResto.getMaxExponentOfVar(ret.sVar)
                            If degree = 0 Then
                                degree = -Pa.PolyDivisor.getMaxExponentOfVar(ret.sVar)
                            End If
                        End If
                        If degree <> 0 Then
                            If degree <> 1 Then ivY += 1
                            If degree > maxYdegree Then
                                maxYdegree = degree
                            ElseIf degree < minYdegree Then
                                minYdegree = degree
                            End If
                        End If
                    End If
                Next
            Next
            If ivY > 1 Then
                ' Hay (al menos) 2 términos y^n, y^m con n,m <> 1.
                ' Ahora conseguiremos 1 término y^1 y -si es ec.de Bernoulli-
                ' un término en y con potencia <> 1, y^m donde m<>1:
                Dim yP1 As Polynomial = Polynomial.GetPolynomial(ret.sVar)
                If maxYdegree Then
                    yP1 ^= New Polynomial(-maxYdegree + 1)
                Else
                    yP1 ^= New Polynomial(-minYdegree - 2)
                End If
                Dim exprYp1 As New Expression(yP1)
                A *= exprYp1
                For i = 0 To vSum.Length - 1
                    vSum(i) *= exprYp1
                Next
            End If

            Dim yP As Polynomial = Polynomial.GetPolynomial(ret.sVar)
            Dim yExpr As New Expression(yP)
            For i = 0 To vSum.Length - 1
                If Array.IndexOf(vDiff, i) = -1 Then
                    vFact = vSum(i).exprToFactors(vIsDiv, True)
                    For j = 0 To vFact.Length - 1
                        Dim mult As Complex = Nothing
                        If vFact(j).isEqualTo(yExpr) Then
                            Exit For
                        End If
                    Next
                    If j < vFact.Length Then
                        Y += vSum(i)
                    ElseIf Array.IndexOf(vDiff, i) = -1 Then
                        B += vSum(i)
                    End If
                End If
            Next

            ' 
            Dim Adiv As New Expression(1.0)
            If A.getCommonFactor(ret.cfg, CommExpr, Expr1, commStr) Then
            Else
                Dim invA As Expression = New Expression(1.0) / A
                invA = rup.ReduceUsingPolynomials(invA)
                If invA.getCommonFactor(ret.cfg, CommExpr, Expr1, commStr) Then
                    CommExpr = New Expression(1.0) / CommExpr
                    Expr1 = New Expression(1.0) / Expr1
                Else
                    CommExpr = New Expression(1.0)
                    Expr1 = New Expression(A)
                End If
            End If
            ReDim vIsDiv(-1)
            vFact = CommExpr.exprToFactors(vIsDiv, True)
            Dim k As Int64 = 0
            Dim vvar(-1) As String
            For j = 0 To vFact.Length - 1
                ReDim vvar(-1)
                If vFact(j).getAllVars(vvar) AndAlso _
                     Array.IndexOf(vvar, ret.sDiffRespVar) > -1 Then
                    If Not vIsDiv(j) Then
                        Adiv *= vFact(j)
                    Else
                        Adiv /= vFact(j)
                    End If
                    vFact(j) = New Expression(1.0)
                Else
                    k += 1
                End If
            Next
            CommExpr = Expression.factorsToExpr(vFact, vIsDiv)
            ReDim vIsDiv(-1)

            vFact = Expr1.exprToFactors(vIsDiv, True)
            k = 0
            For j = 0 To vFact.Length - 1
                ReDim vvar(-1)
                If vFact(j).getAllVars(vvar) AndAlso _
                     Array.IndexOf(vvar, ret.sDiffRespVar) > -1 Then
                    If Not vIsDiv(j) Then
                        Adiv *= vFact(j)
                    Else
                        Adiv /= vFact(j)
                    End If
                    vFact(j) = New Expression(1.0)
                Else
                    k += 1
                End If
            Next
            Expr1 = Expression.factorsToExpr(vFact, vIsDiv)





            Dim ExprY As Expression = Nothing
            Dim CommExprY As Expression = Nothing
            Dim Ydiv As New Expression(1.0)

            If Not Y.IsReal Then
                If Y.getCommonFactor(ret.cfg, CommExprY, ExprY) Then
                Else
                    Dim invY As Expression = New Expression(1.0) / Y
                    invY = rup.ReduceUsingPolynomials(invY)
                    If invY.getCommonFactor(ret.cfg, CommExpr, Expr1) Then
                        CommExprY = New Expression(1.0) / CommExprY
                        ExprY = New Expression(1.0) / ExprY
                    Else
                        CommExprY = New Expression(1.0)
                        ExprY = New Expression(Y)
                    End If
                End If
                ReDim vIsDiv(-1)
                vFact = CommExprY.exprToFactors(vIsDiv, True)
                k = 0
                For j = 0 To vFact.Length - 1
                    Dim mult As Complex = Nothing
                    ReDim vvar(-1)
                    vFact(j).getAllVars(vvar)
                    If vFact(j).isEqualTo(yExpr) AndAlso _
                    Array.IndexOf(vvar, ret.sDiffRespVar) > -1 Then
                        If Not vIsDiv(j) Then
                            Ydiv *= vFact(j)
                        Else
                            Ydiv /= vFact(j)
                        End If
                        vFact(j) = New Expression(1.0)
                    Else
                        k += 1
                    End If
                Next
                CommExprY = Expression.factorsToExpr(vFact, vIsDiv)
                ReDim vIsDiv(-1)

                vFact = ExprY.exprToFactors(vIsDiv, True)
                k = 0
                For j = 0 To vFact.Length - 1
                    Dim mult As Complex = Nothing
                    ReDim vvar(-1)
                    vFact(j).getAllVars(vvar)
                    If vFact(j).isEqualTo(yExpr) AndAlso _
                    Array.IndexOf(vvar, ret.sDiffRespVar) > -1 Then
                        If Not vIsDiv(j) Then
                            Ydiv *= vFact(j)
                        Else
                            Ydiv /= vFact(j)
                        End If
                        vFact(j) = New Expression(1.0)
                    Else
                        k += 1
                    End If
                Next
                ExprY = Expression.factorsToExpr(vFact, vIsDiv)
            End If


            Dim Expr2 As Expression = Nothing
            Dim CommExpr2 As Expression = Nothing
            Dim commStr2 As String = ""

            Dim Bdiv As New Expression(1.0)
            If B.getCommonFactor(ret.cfg, CommExpr2, Expr2) Then
            ElseIf Not (B.IsReal AndAlso B.toDouble = 0.0) Then
                Dim invB As Expression = New Expression(1.0) / B
                invB = rup.ReduceUsingPolynomials(invB)
                If invB.getCommonFactor(ret.cfg, CommExpr2, Expr2) Then
                    CommExpr2 = New Expression(1.0) / CommExpr2
                    Expr2 = New Expression(1.0) / Expr2
                Else
                    CommExpr2 = New Expression(1.0)
                    Expr2 = New Expression(B)
                End If
            Else
                CommExpr2 = New Expression(1.0)
                Expr2 = New Expression(B)
            End If
            ReDim vIsDiv(-1)

            vFact = CommExpr2.exprToFactors(vIsDiv, True)


            k = 0
            For j = 0 To vFact.Length - 1
                ReDim vvar(-1)
                If vFact(j).getAllVars(vvar) AndAlso _
                     Array.IndexOf(vvar, ret.sVar) > -1 Then
                    If Not vIsDiv(j) Then
                        Bdiv *= vFact(j)
                    Else
                        Bdiv /= vFact(j)
                    End If
                    vFact(j) = New Expression(1.0)
                Else
                    k += 1
                End If
            Next
            CommExpr2 = Expression.factorsToExpr(vFact, vIsDiv)
            ReDim vIsDiv(-1)


            vFact = Expr2.exprToFactors(vIsDiv, True)

            k = 0
            For j = 0 To vFact.Length - 1
                ReDim vvar(-1)
                If vFact(j).getAllVars(vvar) AndAlso _
                     Array.IndexOf(vvar, ret.sVar) > -1 Then
                    If Not vIsDiv(j) Then
                        Bdiv *= vFact(j)
                    Else
                        Bdiv /= vFact(j)
                    End If
                    vFact(j) = New Expression(1.0)
                Else
                    k += 1
                End If
            Next
            Expr2 = Expression.factorsToExpr(vFact, vIsDiv)

            Dim vQ(-1), iQ As Int64
            If Not B.IsReal AndAlso Not Y.IsReal Then
                vSum = B.splitIntoTerms
                For i = 0 To vSum.Length - 1
                    If Array.IndexOf(vDiff, i) = -1 Then
                        vFact = vSum(i).exprToFactors(vIsDiv, True)
                        For j = 0 To vFact.Length - 1
                            If vFact(j).IsPolynomial Then
                                Dim Pa As Polynomial = vFact(j).getPolynomial
                                Dim vvar1() As String = Pa.varAll
                                Dim pos As Int64 = Array.IndexOf(vvar1, ret.sVar)
                                If pos > -1 Then
                                    Dim degree As Int64 = 0
                                    If Pa.exp(0) IsNot Nothing AndAlso _
                                    Pa.exp(0).Length > pos Then
                                        degree = Pa.exp(0)(pos)
                                        If degree = 0 AndAlso Pa.PolyResto IsNot Nothing Then
                                            vvar1 = Pa.PolyResto.varAll
                                            pos = Array.IndexOf(vvar1, ret.sVar)
                                            If pos > -1 Then degree = Pa.PolyResto.exp(0)(pos)
                                            If pos = -1 OrElse degree = 0 OrElse _
                                            Not (Pa.exp(0) IsNot Nothing AndAlso _
                                            Pa.exp(0).Length > pos) Then
                                                vvar1 = Pa.PolyDivisor.varAll
                                                pos = Array.IndexOf(vvar1, ret.sVar)
                                                degree = -Pa.PolyDivisor.exp(0)(pos)
                                            End If
                                        End If
                                        If degree <> 0 AndAlso ret.nQ AndAlso _
                                        ret.nQ <> degree Then
                                            Exit Try
                                        ElseIf degree > 1 OrElse degree < 0 Then
                                            If vIsDiv(j) Then degree = -degree
                                            ret.nQ = degree
                                            ReDim Preserve vQ(iQ)
                                            ' mark position in vQ()
                                            vQ(iQ) = i
                                            iQ += 1
                                        End If
                                    End If
                                End If
                            ElseIf vFact(j).getMatchStr = "^" AndAlso _
                            vFact(j).getArgs(0).IsPolynomial AndAlso vFact(j).getArgs(1).IsReal Then
                                Dim Pa As Polynomial = vFact(j).getArgs(0).getPolynomial
                                Dim pos As Int64 = Array.IndexOf(Pa.varAll, ret.sVar)
                                If pos > -1 AndAlso Pa.cf.Length = 1 _
                                AndAlso Pa.varAll.Length = 1 Then
                                    Dim degree As Double = vFact(j).getArgs(1).toDouble
                                    ret.nQ = degree
                                    ReDim Preserve vQ(iQ)
                                    vQ(iQ) = i
                                    iQ += 1
                                End If
                            End If
                        Next
                    End If
                Next
                ret.Q = New Expression(0.0)
                If iQ Then
                    For i = 0 To iQ - 1
                        ret.Q += vSum(vQ(i))
                        vSum(vQ(i)) = New Expression(0.0)
                    Next
                    Dim bIsMn(vSum.Length - 1) As Boolean
                    B = Expression.summandsToExpr(vSum, bIsMn)
                End If

            End If
            Dim M As New Expression(A)
            Dim N As New Expression(B + Y)
            Dim Mx As Expression = M.opDeriv(ret.sDiffRespVar)
            Mx = rup.ReduceUsingPolynomials(Mx)
            Dim Ny As Expression = N.opDeriv(ret.sVar)
            Ny = rup.ReduceUsingPolynomials(Ny)
            If Not Mx.IsReal AndAlso iQ = 0 Then
                Dim subs As Expression = Mx - Ny
                subs = rup.ReduceUsingPolynomials(subs)
                Dim My1 As Expression = M.opDeriv(ret.sVar)
                Dim Nx As Expression = N.opDeriv(ret.sDiffRespVar)
                Dim subs1 As Expression = My1 - Nx
                subs1 = rup.ReduceUsingPolynomials(subs1)
                If subs.IsReal AndAlso subs.toDouble = 0.0 Then
                    ret.tipo = DiffEqType.fstOrderExact
                    ret.A = A : ret.B = B : ret.Y = Y
                    ret.Mx = Mx : ret.Ny = Ny
                    ret.My1 = Nothing
                    ret.Nx = Nothing
                    ret.M = M : ret.N = N
                    Exit Try
                ElseIf subs1.IsReal AndAlso subs1.toDouble = 0.0 Then
                    ret.tipo = DiffEqType.fstOrderExact
                    ret.A = A : ret.B = B : ret.Y = Y
                    ret.My1 = My1 : ret.Nx = Nx
                    ret.Mx = Nothing
                    ret.Ny = Nothing
                    ret.M = M : ret.N = N
                    Exit Try
                End If
            End If
            If iQ = 0 Then
                If CommExprY IsNot Nothing Then
                    Y = CommExprY * ExprY / (Adiv * Bdiv * yExpr)
                    Y = rup.ReduceUsingPolynomials(Y)
                    ret.tipo = DiffEqType.fstOrderSeparable
                    ReDim vvar(-1)
                    B.getAllVars(vvar)
                    If vvar.Length > 1 OrElse _
                    (vvar.Length = 1 AndAlso _
                     Array.IndexOf(vvar, ret.sVar) > -1) Then
                        ret.tipo = DiffEqType.NA
                    Else
                        ret.tipo = DiffEqType.fstOrderlinear
                    End If
                Else
                    ret.tipo = DiffEqType.fstOrderSeparable
                    Y = New Expression(0.0)
                End If
                Adiv *= Ydiv
                Bdiv *= Ydiv
                B = CommExpr2 * Expr2 / Adiv
                A = CommExpr * Expr1 / Bdiv
                A = rup.ReduceUsingPolynomials(A)
                If ret.tipo = DiffEqType.fstOrderlinear Then
                    B /= A : Y /= A
                End If
                B = rup.ReduceUsingPolynomials(B)
                Y = rup.ReduceUsingPolynomials(Y)
                ReDim vvar(-1)
                A.getAllVars(vvar)
                If vvar.Length > 1 OrElse _
                (vvar.Length = 1 AndAlso _
                 Array.IndexOf(vvar, ret.sDiffRespVar) > 1) Then
                    If nCall > 1 Then
                        ret.tipo = DiffEqType.NA
                        Exit Try
                    End If
                    Dim dif As Int64 = maxYdegree - minYdegree
                    Dim eY As New Expression(Polynomial.GetPolynomial(ret.sDiffRespVar))
                    eY ^= New Expression(-dif)
                    vSum = equation.splitIntoTerms
                    For i = 0 To vSum.Length - 1
                        vSum(i) *= eY
                        vSum(i) = rup.ReduceUsingPolynomials(vSum(i))
                    Next
                    ReDim vIsMn(vSum.Length - 1)
                    Dim nwEq As New Expression(Expression.summandsToExpr(vSum, vIsMn))
                    Dim nwVar As New VarsAndFns(ret.cfg)
                    Dim xP As Polynomial = Polynomial.GetPolynomial(ret.sDiffRespVar)
                    Dim nwYP As Polynomial = Polynomial.GetPolynomial("_" + ret.sVar)
                    nwVar.AddVar(ret.sVar, New Expression(xP * nwYP))
                    nwEq = nwEq.evalExprToExpr(nwVar)
                    nwEq = rup.ReduceUsingPolynomials(nwEq)
                    Dim dEq As Differential_Equations = _
                        Differential_Equations.tryParseDifEq(nwEq, 1)
                    If dEq.tipo <> DiffEqType.NA Then
                        ret.Subst = nwEq
                        ret.tipo = DiffEqType.fstOrderSubstitution
                        Exit Try
                    End If
                    ret.tipo = DiffEqType.NA
                End If
                ReDim vvar(-1)
                B.getAllVars(vvar)
                If vvar.Length > 1 OrElse _
                (vvar.Length = 1 AndAlso _
                 Array.IndexOf(vvar, ret.sVar) > -1) Then
                    ret.tipo = DiffEqType.NA
                End If
                If ret.tipo <> DiffEqType.NA Then
                    ret.A = A
                    ret.B = -B
                    ret.Y = Y
                Else
                    ret.A = Nothing
                    ret.B = Nothing
                    ret.Y = Nothing
                End If
            Else
                ret.tipo = DiffEqType.fstOrderBernoulli
                ret.A = A
                ret.B = B
                ret.Y = Y / New Expression(Polynomial.GetPolynomial(ret.sVar))
                ret.Adiv = Adiv
                ret.Bdiv = Bdiv
            End If

        Catch ex As Exception
            Throw ex
        End Try
        Return ret
    End Function
    Public Function resolveSeparable(ByRef responseType As retMtx.diffResponse) As VarsAndFns
        Dim retFns As New VarsAndFns(eq.cfg)
        Try
            Dim ret As Expression = Nothing
            If tipo <> DiffEqType.fstOrderSeparable Then
                Exit Try
            End If

            responseType = retMtx.diffResponse.varValue


            Dim rup As New ReduceExprUsingPolynomials
            Dim vExprParams(1) As Expression
            Dim oVar As New VarsAndFns(eq.cfg)
            oVar.AddVar(sVar, Nothing)
            Dim ut As Expression = A.opAntiDerivative(eq.cfg, _
               0, vExprParams, eq.cfg.cur, oVar, sVar, _
               Nothing, Nothing, Nothing, Nothing)
            If ut Is Nothing Then
                Throw New Exception(msg8.num(13))
            End If
            oVar = New VarsAndFns(eq.cfg)
            oVar.AddVar(sDiffRespVar, Nothing)
            Dim xt As Expression = B.opAntiDerivative(eq.cfg, _
               0, vExprParams, eq.cfg.cur, oVar, sDiffRespVar, _
               Nothing, Nothing, Nothing, Nothing)
            If xt Is Nothing Then
                Throw New Exception(msg8.num(13))
            End If
            xt = rup.ReduceUsingPolynomials(xt)
            ret = ut - xt
            Dim mg As MathGlobal8 = eq.cfg.mathGlobal
            Dim sCnt As String = mg.integralCnst
            Dim cnt As New Expression( _
                Polynomial.GetPolynomial(sCnt))
            ret -= cnt
            Dim bImplicit As Boolean = True
            Dim vResult(-1) As Expression
            If ret.tryToIsolateToExpression(sVar, vResult) Then
                bImplicit = False
            Else
                ReDim vResult(0)
                vResult(0) = ret
            End If
            For i = 0 To vResult.Length - 1
                ret = -vResult(i)
                Dim initConditions As VarsAndFns = eq.cfg.cur.vars
                If initConditions IsNot Nothing Then
                    Dim e1 As String = String.Empty
                    Dim id0 As Int64 = 0
                    For Each e1 In initConditions.getNamesList
                        If sVar + "(" = Left(e1, Len(sDiffRespVar) + 1) Then
                            Exit For
                        End If
                        id0 += 1
                    Next
                    e1 = Trim(e1)
                    If Len(e1) AndAlso id0 < initConditions.getNamesList.Length Then
                        Dim m As Match = _
                            Regex.Match(e1, "\([^\)]*\)")
                        Regex.Match(e1, "")
                        Dim dbl As Double
                        If m.Success Then
                            Double.TryParse(Mid(m.ToString, 2, m.Length - 2), dbl)
                        Else
                            If bImplicit Then
                                responseType = retMtx.diffResponse.implicit
                                retFns.AddVar(sVar, ret)
                                retFns.AddVar("_" + sVar, New Expression(0.0))
                            Else
                                retFns.AddVar(sVar, ret)
                            End If
                            Exit Try
                        End If
                        Dim varSOE As New VarsAndFns(eq.cfg)
                        varSOE.AddVar(sDiffRespVar, New Expression(dbl))
                        varSOE.AddVar(sVar, initConditions.getValueByID(0).getExpr(0, 0))
                        Dim evEq As New Expression(ret)
                        Dim varK As New VarsAndFns(eq.cfg)
                        If bImplicit Then
                            evEq = evEq.evalExprToExpr(varSOE)
                        Else
                            evEq -= New Expression(Polynomial.GetPolynomial(sVar))
                            evEq = evEq.evalExprToExpr(varSOE)
                        End If
                        evEq = rup.ReduceUsingPolynomials(evEq)
                        evEq.IsEquation = True
                        Dim vR2(-1) As Expression
                        If evEq.tryToIsolateToExpression(sCnt, vR2) Then
                            If Not vR2(0).IsReal Then
                                GoTo sig
                            End If
                            ret = rup.ReduceUsingPolynomials(-ret)
                            Dim visdiv(-1) As Boolean
                            Dim vF() As Expression = ret.exprToFactors(visdiv, True)
                            If Not bImplicit Then
                                retFns.AddVar(sVar, ret)
                                retFns.AddVar(sCnt, vR2(0))
                            Else
                                responseType = retMtx.diffResponse.implicit
                                ' When implicit 2 entries for each equation
                                ' or expression:
                                retFns.AddVar(sVar, ret) ' svar is dummy, just 'ret' is considered
                                retFns.AddVar("_" + sVar, New Expression(0.0)) ' equal to zero (ret=0)

                                retFns.AddVar(sCnt, cnt) ' only cnt considered: 
                                retFns.AddVar("_" + sCnt, vR2(0)) ' cnt=vR2(0)
                            End If
                            Exit Try
                        End If
                        Dim soe As New SystemOfEquations( _
                             Nothing, New ExprMatrix(evEq), varSOE, eq.cfg.cur, eq.cfg)
                        If soe.resolveSysOfEqs(eq.cfg) Then
                            varK.AddVar(sCnt, New Expression(soe.resultValues(0)))
                            ret = ret.evalExprToExpr(varK)
                            ret = rup.ReduceUsingPolynomials(ret)
                            Dim vR(-1) As Expression
                            If ret.tryToIsolateToExpression(sVar, vR) Then
                                retFns.AddVar(sVar, vR(0))
                                retFns.AddVar(sCnt, New Expression(soe.resultValues(0)))
                            Else
                                If bImplicit Then
                                    evEq = New Expression(ret)
                                    If evEq.tryToIsolateToExpression(sVar, vR2) Then
                                        bImplicit = False
                                        ret = vR2(0)
                                    End If
                                End If
                                If bImplicit Then
                                    responseType = retMtx.diffResponse.implicit
                                    retFns.AddVar(sVar, ret)
                                    retFns.AddVar("_" + sVar, New Expression(0.0))
                                    retFns.AddVar(sCnt, cnt)
                                    retFns.AddVar("_" + sCnt, New Expression(soe.resultValues(0)))
                                Else
                                    retFns.AddVar(sVar, ret)
                                    retFns.AddVar(sCnt, New Expression(soe.resultValues(0)))
                                End If
                            End If
                            Exit Try
                        ElseIf i = vResult.Length - 1 Then
                            ret = rup.ReduceUsingPolynomials(ret)
                            evEq = rup.ReduceUsingPolynomials(evEq)
                            responseType = retMtx.diffResponse.implicit
                            retFns.AddVar(sVar, ret)
                            retFns.AddVar("_" + sVar, New Expression(0.0))
                            retFns.AddVar(sCnt, evEq)
                            retFns.AddVar("_" + sCnt, New Expression(0.0))
                        End If
                    Else
                        ret = rup.ReduceUsingPolynomials(ret)
                        responseType = retMtx.diffResponse.implicit
                        retFns.AddVar(sVar, ret)
                        retFns.AddVar("_" + sVar, New Expression(Polynomial.GetPolynomial(sVar)))
                    End If
                End If
sig:
            Next

        Catch ex As Exception
            Throw ex
        End Try
        Return retFns
    End Function
    Public Function resolveFstOrderLinear(ByRef responseType As retMtx.diffResponse) As VarsAndFns
        Dim retFns As New VarsAndFns(eq.cfg)
        Try
            Dim ret As Expression = Nothing
            If tipo <> DiffEqType.fstOrderlinear Then
                Exit Try
            End If
            Dim rup As New ReduceExprUsingPolynomials
            ' A * Dt(y) + Y(t)*y = B(t)
            Dim i0(2) As Int64
            responseType = retMtx.diffResponse.varValue
            Dim vExprParams() As Expression = {Y}
            Dim oVar As New VarsAndFns(eq.cfg)
            oVar.AddVar(sDiffRespVar, Nothing)
            Dim ut As Expression = Y.opAntiDerivative(eq.cfg, _
               0, vExprParams, eq.cfg.cur, oVar, sDiffRespVar, _
               Nothing, Nothing, Nothing, Nothing)
            If ut Is Nothing Then
                Throw New Exception(msg8.num(13))
            End If
            Dim vIsMn(-1) As Boolean
            Dim vSum() As Expression = ut.exprToSummands(vIsMn)
            Dim mg As MathGlobal8 = eq.cfg.mathGlobal
            Dim sCnt As String = mg.integralCnst
            Dim cnt As New Expression( _
                Polynomial.GetPolynomial(sCnt))
            ut = Expression.summandsToExpr(vSum, vIsMn)
            ut = Expression.AddFnAndArg0("exp", ut)
            Dim μ As New Expression(ut)
            μ = rup.ReduceUsingPolynomials(μ)

            ' 3) Then y(t)=(∫μ(t)g(t)dt+C)/μ(t) (eq.#9, page 10)

            vExprParams = New Expression() {μ}
            ut = (B * μ).opAntiDerivative(eq.cfg, _
               0, vExprParams, eq.cfg.cur, oVar, sDiffRespVar, _
            Nothing, Nothing, Nothing, Nothing)
            If ut Is Nothing Then
                Throw New Exception(msg8.num(13))
            End If
            'ut *= gt
            ut = rup.ReduceUsingPolynomials(ut)
            'ut = ut.opAntiDerivative(eq.cfg, _
            '    0, vExprParams, eq.cfg.cur, oVar, sVar, _
            'Nothing, Nothing, Nothing, Nothing)
            ut += New Expression(cnt)
            ret = ut / μ
            ret = rup.ReduceUsingPolynomials(ret)
            Dim initConditions As VarsAndFns = eq.cfg.cur.vars
            If initConditions IsNot Nothing Then
                Dim e1 As String = String.Empty
                Dim id0 As Int64 = 0
                For Each e1 In initConditions.getNamesList
                    If sVar + "(" = Left(e1, Len(sDiffRespVar) + 1) Then
                        Exit For
                    End If
                    id0 += 1
                Next
                e1 = Trim(e1)
                If Len(e1) AndAlso id0 < initConditions.getNamesList.Length Then
                    Dim m As Match = _
                            Regex.Match(e1, "\([^\)]*\)")
                    Regex.Match(e1, "")
                    Dim dbl As Double
                    Dim result As Expression = Nothing
                    If m.Success Then
                        Double.TryParse(Mid(m.ToString, 2, m.Length - 2), dbl)
                    End If
                    If ret.IsPolynomial Then
                        Dim rP As New Polynomial(ret.getPolynomial)
                        If rP.tryToIsolateToPolynomial(sDiffRespVar, result) Then
                            ret = result
                        End If
                    Else
                        result = New Expression(ret)
                    End If
                    Dim varSOE As New VarsAndFns(eq.cfg)
                    varSOE.AddVar(sDiffRespVar, New Expression(dbl))
                    Dim id As Int64 = initConditions.getVarIDByName(sVar)
                    Dim evEq As New Expression(ret)
                    Dim varK As New VarsAndFns(eq.cfg)
                    If id0 < initConditions.getNamesList.Length Then
                        varSOE.AddVar(sVar, initConditions.getValueByID(id0).getExpr(0, 0))
                        evEq -= initConditions.getValueByID(id0).getExpr(0, 0)
                        evEq = evEq.evalExprToExpr(varSOE)
                        Dim vR(-1) As Expression
                        If evEq.tryToIsolateToExpression(sCnt, vR) Then
                            result = vR(0)
                            varSOE.AddVar(sCnt, result)
                        End If
                        varK.AddVar(sCnt, result)
                        ret = ret.evalExprToExpr(varK)
                        Dim varSOE2 As New VarsAndFns(eq.cfg)
                        varSOE2.AddVar(sCnt, Nothing)
                        Dim soe As New SystemOfEquations( _
                             Nothing, New ExprMatrix(evEq), varSOE2, eq.cfg.cur, eq.cfg)
                        If soe.resolveSysOfEqs(eq.cfg) Then
                            If soe.resultValues.Length Then
                                varSOE2.setValue(0, New ExprMatrix(soe.resultValues(0)))
                            End If
                        End If
                    Else
                        ret = rup.ReduceUsingPolynomials(ret)
                        responseType = retMtx.diffResponse.implicit
                        retFns.AddVar(sVar, ret)
                        retFns.AddVar("_" + sVar, New Expression(Polynomial.GetPolynomial(sVar)))
                    End If
                    retFns.AddVar(sVar, ret)
                    If id0 < initConditions.getNamesList.Length Then
                        retFns.AddVar(sCnt, result)
                    End If
                Else
                    ret = rup.ReduceUsingPolynomials(ret)
                    responseType = retMtx.diffResponse.implicit
                    retFns.AddVar("_" + sVar, New Expression(Polynomial.GetPolynomial(sVar)))
                    retFns.AddVar(sVar, ret)
                End If
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return retFns
    End Function
    Public Function resolveExact(ByRef responseType As retMtx.diffResponse) As VarsAndFns
        Dim retFns As New VarsAndFns(eq.cfg)
        Try

            Dim ret As Expression = Nothing
            If tipo <> DiffEqType.fstOrderExact Then
                Exit Try
            End If

            responseType = retMtx.diffResponse.varValue


            Dim rup As New ReduceExprUsingPolynomials
            Dim M As Expression = Me.M
            Dim N As Expression = Me.N

            Dim Mx As Expression = Me.Mx
            Dim Ny As Expression = Me.Ny
            Dim My1 As Expression = Me.My1
            Dim Nx As Expression = Me.Nx


            Dim vExprParams(1) As Expression
            Dim oVar As New VarsAndFns(eq.cfg)
            oVar.AddVar(sVar, Nothing)

            ' Ψ=∫Mdx=f(x,y)+h(y)=∫Ndy
            Dim Ψ As Expression = Nothing
            If Mx Is Nothing Then
                Ψ = M.opAntiDerivative(eq.cfg,
               0, vExprParams, eq.cfg.cur, oVar, sVar,
               Nothing, Nothing, Nothing, Nothing)
            Else
                Ψ = N.opAntiDerivative(eq.cfg,
               0, vExprParams, eq.cfg.cur, oVar, sDiffRespVar,
               Nothing, Nothing, Nothing, Nothing)
            End If
            If Ψ Is Nothing Then
                Throw New Exception(msg8.num(13))
            End If
            Ψ = rup.ReduceUsingPolynomials(Ψ)
            oVar = New VarsAndFns(eq.cfg)
            oVar.AddVar(sDiffRespVar, Nothing)
            ' Ψy=N=Dy(f)+h'(y) =>...
            Dim DΨ As Expression = Nothing
            Dim Dh As Expression = Nothing
            Dim h As Expression = Nothing
            If Mx Is Nothing Then
                DΨ = Ψ.opDeriv(sDiffRespVar)
                Dh = N - rup.ReduceUsingPolynomials(DΨ)
                h = Dh.opAntiDerivative(eq.cfg,
                   0, vExprParams, eq.cfg.cur, oVar, sDiffRespVar,
                   Nothing, Nothing, Nothing, Nothing)
            Else
                DΨ = Ψ.opDeriv(sVar)
                Dh = M - rup.ReduceUsingPolynomials(DΨ)
                ' ... h(y)=∫N*dy=∫Dy(f)*dy y surgirá una K
                h = Dh.opAntiDerivative(eq.cfg,
                    0, vExprParams, eq.cfg.cur, oVar, sVar,
                    Nothing, Nothing, Nothing, Nothing)
            End If
            Dim mg As MathGlobal8 = eq.cfg.mathGlobal
            Dim sCnt As String = mg.integralCnst
            Dim cnt As New Expression( _
                Polynomial.GetPolynomial(sCnt))
            ret = Ψ + h + cnt ' h - cnt
            Dim bImplicit As Boolean = True
            Dim vResult(-1) As Expression
            If ret.tryToIsolateToExpression(sVar, vResult) Then
                bImplicit = False
            Else
                ReDim vResult(0)
                vResult(0) = ret
            End If
            For i = 0 To vResult.Length - 1
                ret = vResult(i)
                Dim initConditions As VarsAndFns = eq.cfg.cur.vars
                If initConditions IsNot Nothing Then
                    Dim e1 As String = String.Empty
                    Dim id0 As Int64 = 0
                    For Each e1 In initConditions.getNamesList
                        If sVar + "(" = Left(e1, Len(sDiffRespVar) + 1) Then
                            Exit For
                        End If
                        id0 += 1
                    Next
                    e1 = Trim(e1)
                    If Len(e1) AndAlso id0 < initConditions.getNamesList.Length Then
                        Dim m1 As Match = _
                            Regex.Match(e1, "\([^\)]*\)")
                        Dim dbl As Double
                        If m1.Success Then
                            Double.TryParse(Mid(m1.ToString, 2, m1.Length - 2), dbl)
                        Else
                            'If bImplicit Then
                            '    responseType = retMtx.response.implicit
                            '    retFns.AddVar(sVar, ret)
                            '    retFns.AddVar("_" + sVar, New Expression(0.0))
                            'Else
                            '    retFns.AddVar(sVar, ret)
                            'End If
                            Exit For
                        End If
                        Dim varSOE As New VarsAndFns(eq.cfg)
                        varSOE.AddVar(sDiffRespVar, New Expression(dbl))
                        varSOE.AddVar(sVar, initConditions.getValueByID(0).getExpr(0, 0))
                        Dim evEq As New Expression(ret)
                        Dim varK As New VarsAndFns(eq.cfg)
                        If bImplicit Then
                            evEq = evEq.evalExprToExpr(varSOE)
                        Else
                            evEq -= New Expression(Polynomial.GetPolynomial(sVar))
                            evEq = evEq.evalExprToExpr(varSOE)
                        End If
                        evEq = rup.ReduceUsingPolynomials(evEq)
                        evEq.IsEquation = True
                        Dim vR2(-1) As Expression
                        If evEq.tryToIsolateToExpression(sCnt, vR2) Then
                            Dim im As Double = Math.Abs(vR2(0).getPolynomial.cf(0).pIm.ToDouble)
                            If Not vR2(0).IsReal AndAlso im > 10 ^ -6 Then
                                GoTo sig
                            End If
                            'If Not bImplicit Then
                            '    retFns.AddVar(sVar, ret)
                            '    retFns.AddVar(sCnt, vR2(0))
                            'Else
                            '    responseType = retMtx.response.implicit
                            '    ' When implicit 2 entries for each equation
                            '    ' or expression:
                            '    retFns.AddVar(sVar, ret) ' svar is dummy, just 'ret' is considered
                            '    retFns.AddVar("_" + sVar, New Expression(0.0)) ' equal to zero (ret=0)

                            '    retFns.AddVar(sCnt, cnt) ' only cnt considered: 
                            '    retFns.AddVar("_" + sCnt, vR2(0)) ' cnt=vR2(0)
                            'End If
                            responseType = retMtx.diffResponse.varValue
                            varK.AddVar(sCnt, New Expression(vR2(0)))
                            Ψ = Ψ.evalExprToExpr(varK)
                            ret = ret.evalExprToExpr(varK)
                            ret = rup.ReduceUsingPolynomials(ret)
                            retFns.AddVar(sVar, ret)
                            retFns.AddVar(sCnt, vR2(0))

                            Exit Try
                        End If
                        Dim soe As New SystemOfEquations( _
                             Nothing, New ExprMatrix(evEq), varSOE, eq.cfg.cur, eq.cfg)
                        If soe.resolveSysOfEqs(eq.cfg) Then
                            varK.AddVar(sCnt, New Expression(soe.resultValues(0)))
                            Ψ = Ψ.evalExprToExpr(varK)
                            ret = ret.evalExprToExpr(varK)
                            ret = rup.ReduceUsingPolynomials(ret)
                            Dim vR(-1) As Expression
                            If ret.tryToIsolateToExpression(sVar, vR) Then
                                retFns.AddVar(sVar, vR(0))
                                retFns.AddVar(sCnt, New Expression(soe.resultValues(0)))
                            Else
                                If bImplicit Then
                                    evEq = New Expression(ret)
                                    If evEq.tryToIsolateToExpression(sVar, vR2) Then
                                        bImplicit = False
                                        ret = vR2(0)
                                    End If
                                End If
                                'If bImplicit Then
                                '    responseType = retMtx.response.implicit
                                '    retFns.AddVar(sVar, ret)
                                '    retFns.AddVar("_" + sVar, New Expression(0.0))
                                '    retFns.AddVar(sCnt, cnt)
                                '    retFns.AddVar("_" + sCnt, New Expression(soe.resultValues(0)))
                                'Else
                                '    retFns.AddVar(sVar, ret)
                                '    retFns.AddVar(sCnt, New Expression(soe.resultValues(0)))
                                'End If
                            End If
                            Exit For
                        ElseIf i = vResult.Length - 1 Then
                            ret = rup.ReduceUsingPolynomials(ret)
                            evEq = rup.ReduceUsingPolynomials(evEq)
                            responseType = retMtx.diffResponse.implicit
                            retFns.AddVar(sVar, ret)
                            retFns.AddVar("_" + sVar, New Expression(0.0))
                            retFns.AddVar(sCnt, evEq)
                            retFns.AddVar("_" + sCnt, New Expression(0.0))
                        End If
                    Else
                        ret = rup.ReduceUsingPolynomials(ret)
                        responseType = retMtx.diffResponse.implicit
                        retFns.AddVar(sVar, ret)
                        retFns.AddVar("_" + sVar, New Expression(Polynomial.GetPolynomial(sVar)))
                    End If
                End If
sig:
            Next
            Dim sCnt2 As String = mg.integralCnst
            Dim cnt2 As New Expression( _
                Polynomial.GetPolynomial(sCnt2))
            ret += Ψ + cnt2
            If ret.tryToIsolateToExpression(sVar, vResult) Then
                bImplicit = False
            Else
                ReDim vResult(0)
                vResult(0) = ret
            End If
            For i = 0 To vResult.Length - 1
                ret = rup.ReduceUsingPolynomials(vResult(i))
                Dim initConditions As VarsAndFns = eq.cfg.cur.vars
                If initConditions IsNot Nothing Then
                    Dim e1 As String = String.Empty
                    Dim id0 As Int64 = 0
                    For Each e1 In initConditions.getNamesList
                        If sVar + "(" = Left(e1, Len(sDiffRespVar) + 1) Then
                            Exit For
                        End If
                        id0 += 1
                    Next
                    e1 = Trim(e1)
                    If Len(e1) AndAlso id0 < initConditions.getNamesList.Length Then
                        Dim m1 As Match = _
                            Regex.Match(e1, "\([^\)]*\)")
                        Dim dbl As Double
                        If m1.Success Then
                            Double.TryParse(Mid(m1.ToString, 2, m1.Length - 2), dbl)
                        Else
                            If bImplicit Then
                                responseType = retMtx.diffResponse.implicit
                                retFns.AddVar(sVar, ret)
                                retFns.AddVar("_" + sVar, New Expression(0.0))
                            Else
                                retFns.AddVar(sVar, ret)
                            End If
                            Exit Try
                        End If
                        Dim varSOE As New VarsAndFns(eq.cfg)
                        varSOE.AddVar(sDiffRespVar, New Expression(dbl))
                        varSOE.AddVar(sVar, initConditions.getValueByID(0).getExpr(0, 0))
                        Dim evEq As New Expression(ret)
                        Dim varK As New VarsAndFns(eq.cfg)
                        If bImplicit Then
                            evEq = evEq.evalExprToExpr(varSOE)
                        Else
                            evEq -= New Expression(Polynomial.GetPolynomial(sVar))
                            evEq = evEq.evalExprToExpr(varSOE)
                        End If
                        evEq = rup.ReduceUsingPolynomials(evEq)
                        evEq.IsEquation = True
                        Dim vR2(-1) As Expression
                        If evEq.tryToIsolateToExpression(sCnt2, vR2) Then
                            If Not vR2(0).IsReal Then
                                GoTo sig1
                            End If
                            If Not bImplicit Then
                                retFns.AddVar(sVar, ret)
                                retFns.AddVar(sCnt2, vR2(0))
                            Else
                                responseType = retMtx.diffResponse.implicit
                                ' When implicit 2 entries for each equation
                                ' or expression:
                                retFns.AddVar(sVar, ret) ' svar is dummy, just 'ret' is considered
                                retFns.AddVar("_" + sVar, New Expression(0.0)) ' equal to zero (ret=0)

                                retFns.AddVar(sCnt2, cnt2) ' only cnt considered: 
                                retFns.AddVar("_" + sCnt2, vR2(0)) ' cnt=vR2(0)
                            End If
                            Exit For
                        End If
                        Dim soe As New SystemOfEquations( _
                             Nothing, New ExprMatrix(evEq), varSOE, eq.cfg.cur, eq.cfg)
                        If soe.resolveSysOfEqs(eq.cfg) Then
                            varK.AddVar(sCnt2, New Expression(soe.resultValues(0)))
                            ret = ret.evalExprToExpr(varK)
                            ret = rup.ReduceUsingPolynomials(ret)
                            Dim vR(-1) As Expression
                            If ret.tryToIsolateToExpression(sVar, vR) Then
                                retFns.AddVar(sVar, vR(0))
                                retFns.AddVar(sCnt2, New Expression(soe.resultValues(0)))
                            Else
                                If bImplicit Then
                                    evEq = New Expression(ret)
                                    If evEq.tryToIsolateToExpression(sVar, vR2) Then
                                        bImplicit = False
                                        ret = vR2(0)
                                    End If
                                End If
                                If bImplicit Then
                                    responseType = retMtx.diffResponse.implicit
                                    retFns.AddVar(sVar, ret)
                                    retFns.AddVar("_" + sVar, New Expression(0.0))
                                    retFns.AddVar(sCnt2, cnt2)
                                    retFns.AddVar("_" + sCnt2, New Expression(soe.resultValues(0)))
                                Else
                                    retFns.AddVar(sVar, ret)
                                    retFns.AddVar(sCnt2, New Expression(soe.resultValues(0)))
                                End If
                            End If
                            Exit Try
                        ElseIf i = vResult.Length - 1 Then
                            ret = rup.ReduceUsingPolynomials(ret)
                            evEq = rup.ReduceUsingPolynomials(evEq)
                            responseType = retMtx.diffResponse.implicit
                            retFns.AddVar(sVar, ret)
                            retFns.AddVar("_" + sVar, New Expression(0.0))
                            retFns.AddVar(sCnt2, evEq)
                            retFns.AddVar("_" + sCnt2, New Expression(0.0))
                        End If
                    End If
                End If
sig1:
            Next

        Catch ex As Exception
            Throw ex
        End Try
        Return retFns
    End Function
    Public Function resolveBernoulli(ByRef responseType As retMtx.diffResponse) As VarsAndFns
        Dim retFns As New VarsAndFns(eq.cfg)
        Try

            Dim rup As New ReduceExprUsingPolynomials
            Dim ret As Expression = Nothing
            If tipo <> DiffEqType.fstOrderBernoulli Then
                Exit Try
            End If
            '  y' + p(x)*y = q(x)*y^n
            '  
            '  When execution is here data should be:
            '  A(x)*y' - Y(x)*y - Q(x)*y^n (1)
            '  a) Divide (1) by A(x) : y' + Y/A * y - Q/A * y^n (2)
            '   
            '  b) substition by v(x)=y^(1-n)  and differentiating by x (chain rule):
            '    v' = (1-n)*y^(-n)*y' so rewritting (2):
            '  (1/(1-n))* v' + B/A * v = Q/A
            ' i.e. a linear diff. equation.

            ' a)
            Dim varP As Polynomial = Polynomial.GetPolynomial(sVar)
            Q /= A
            Y /= A
            A = New Expression(1.0 / (1 - nQ))
            B = -Q / A / New Expression(varP) ^ New Expression(nQ)
            B = rup.ReduceUsingPolynomials(B)
            Dim sgn As Int64 = 1
            If A.IsReal AndAlso A.toDouble < 0 Then
                sgn = -1
            End If
            Y = Y / A
            Y = rup.ReduceUsingPolynomials(Y)
            A = New Expression(1.0)
            ' b)
            tipo = DiffEqType.fstOrderlinear
            Dim r As retMtx.diffResponse = Nothing
            Dim initConditions As VarsAndFns = eq.cfg.cur.vars
            If initConditions IsNot Nothing Then
                Dim e1 As String = String.Empty
                Dim id0 As Int64 = 0
                For Each e1 In initConditions.getNamesList
                    If sVar + "(" = Left(e1, Len(sDiffRespVar) + 1) Then
                        Exit For
                    End If
                    id0 += 1
                Next
                e1 = Trim(e1)
                If Len(e1) AndAlso id0 < initConditions.getNamesList.Length Then
                    Dim initExpr As New Expression(eq.cfg.cur.vars.getValueByID(id0))
                    initExpr ^= New Expression(1 - nQ) ' v=y^(1-n)
                    eq.cfg.cur.vars.setValue(id0, New ExprMatrix(initExpr))
                Else
                    Throw New Exception(msg8.num(13))
                End If
            End If
            Dim chVar As VarsAndFns = resolveFstOrderLinear(r)
            Dim sV As String = "__v"
            Dim chExpr As New Expression(varP)
            chExpr ^= New Expression(1 / (1 - nQ))
            Dim exprV As Expression = Nothing
            If r = retMtx.diffResponse.varValue Then
                Dim f1 As New VarsAndFns(cfg)
                If sgn = -1 Then
                    f1.AddVar(sVar, New Expression(-Polynomial.GetPolynomial(sV)))
                Else
                    f1.AddVar(sVar, New Expression(Polynomial.GetPolynomial(sV)))
                End If
                f1.AddVar(chVar.getNamesList(1), chVar.getValueByID(1).getExpr(0, 0))
                Dim exprV1 As Expression = New Expression( _
                    varP) - chVar.getValueByID(0).getExpr(0, 0)
                exprV = exprV1.evalExprToExpr(f1)
            ElseIf responseType = retMtx.diffResponse.implicit Then
                exprV = chVar.getValueByID(0).getExpr(0, 0)
                'chFn.AddVar(chVar.getNamesList(1), chVar.getValueByID(1).getExpr(1, 0))
            End If
            Dim chFn As New VarsAndFns(cfg)
            Dim chExpr2 As New Expression(varP)
            chExpr2 ^= New Expression(1 - nQ)
            chFn.AddVar(sV, chExpr2) ' __v  = y^(1-n)
            ret = exprV.evalExprToExpr(chFn)
            ret = rup.ReduceUsingPolynomials(ret)
            Dim vResult(-1) As Expression

            If ret.tryToIsolateToExpression(sVar, vResult) Then
                responseType = retMtx.diffResponse.varValue
                retFns.AddVar(sVar, vResult(0))
            Else
                responseType = retMtx.diffResponse.implicit
                retFns.AddVar(sVar, ret)
                retFns.AddVar("_" + sVar, New Expression(0.0))
            End If


            'Dim expr As Expression = chExpr.evalExprToExpr(chFn)
            'Dim vResult(-1) As Expression
            'If expr.tryToIsolateToExpression(sVar, vResult) Then
            '    retFns.AddVar(sVar, vResult(0))
            'Else
            '    retFns.AddVar(sVar, expr)
            '    retFns.AddVar("_" + sVar, New Expression(0))
            'End If


        Catch ex As Exception
            Throw ex
        End Try
        Return retFns
    End Function
    Public Function resolveSubstitution(ByRef responseType As retMtx.diffResponse) As VarsAndFns
        Dim retFns As New VarsAndFns(eq.cfg)
        Try
            Dim initConditions As New VarsAndFns(eq.cfg.cur.vars)
            If tipo <> DiffEqType.fstOrderSubstitution Then
                Exit Try
            End If
            Dim rMtx As New retMtx
            rMtx.vars = Differential_Equations.resolveSolvable(Subst, rMtx)
            If rMtx.diffType = retMtx.diffResponse.varValue Then
                If rMtx.vars.getNamesList.Length = 0 Then
                    Throw New Exception(msg8.num(13))
                End If
                Dim eqIn_y As Expression = rMtx.vars.getValueByID(0).getExpr(0, 0)
                Dim sK As String = rMtx.vars.getVarNameByID(0)
                If rMtx.vars.getNamesList.Length > 1 Then
                    Dim eK As Expression = rMtx.vars.getValueByID(1).getExpr(0, 0)
                    Dim chF As New VarsAndFns(cfg)
                    chF.AddVar(sK, eK)
                    eqIn_y = eqIn_y.evalExprToExpr(chF)
                End If
                ' x/y = _y => y = _y / x 
                responseType = retMtx.diffResponse.varValue
                'Dim pX As Polynomial = Polynomial.GetPolynomial(sDiffRespVar)
                'retFns.AddVar(sVar, eqIn_y / New Expression(px))



                Dim sCnt As String = "_K1"
                Dim rup As New ReduceExprUsingPolynomials
                Dim bImplicit As Boolean = True
                Dim vResult(-1) As Expression
                Dim ret As New Expression(eqIn_y)
                ret = New Expression(Polynomial.GetPolynomial(sVar)) - ret
                Dim evEq As New Expression(eqIn_y)

                If ret.tryToIsolateToExpression(sVar, vResult) Then
                    bImplicit = False
                Else
                    ReDim vResult(0)
                    vResult(0) = ret
                End If
                For i = 0 To vResult.Length - 1
                    ret = vResult(i)
                    'Dim initConditions As VarsAndFns = eq.cfg.cur.vars
                    If initConditions IsNot Nothing Then
                        Dim e1 As String = String.Empty
                        Dim id0 As Int64 = 0
                        For Each e1 In initConditions.getNamesList
                            If sVar + "(" = Left(e1, Len(sDiffRespVar) + 1) Then
                                Exit For
                            End If
                            id0 += 1
                        Next
                        e1 = Trim(e1)
                        If Len(e1) AndAlso id0 < initConditions.getNamesList.Length Then
                            Dim m1 As Match = _
                                Regex.Match(e1, "\([^\)]*\)")
                            Dim dbl As Double
                            If m1.Success Then
                                Double.TryParse(Mid(m1.ToString, 2, m1.Length - 2), dbl)
                            Else
                                Exit For
                            End If
                            Dim varSOE As New VarsAndFns(eq.cfg)
                            varSOE.AddVar(sDiffRespVar, New Expression(dbl))
                            varSOE.AddVar(sVar, initConditions.getValueByID(0).getExpr(0, 0))
                            'Dim evEq As New Expression(ret)
                            Dim varK As New VarsAndFns(eq.cfg)
                            If bImplicit Then
                                evEq = evEq.evalExprToExpr(varSOE)
                            Else
                                evEq -= New Expression(Polynomial.GetPolynomial(sVar))
                                evEq = evEq.evalExprToExpr(varSOE)
                            End If
                            evEq = rup.ReduceUsingPolynomials(evEq)
                            evEq.IsEquation = True
                            Dim vR2(-1) As Expression
                            Dim soe As New SystemOfEquations( _
                                 Nothing, New ExprMatrix(evEq), varSOE, eq.cfg.cur, eq.cfg)
                            Try
                                Dim t As New System.Threading.Thread(AddressOf soe.resolveSysOfEqs)
                                t.Start(eq.cfg)
                                If Not t.Join(1500) Then
                                    varK.AddVar(sCnt, New Expression(soe.resultValues(0)))
                                    ret = ret.evalExprToExpr(varK)
                                    ret = rup.ReduceUsingPolynomials(ret)
                                    Dim vR(-1) As Expression
                                    If ret.tryToIsolateToExpression(sVar, vR) Then
                                        retFns.AddVar(sVar, vR(0))
                                        retFns.AddVar(sCnt, New Expression(soe.resultValues(0)))
                                    Else
                                        If bImplicit Then
                                            evEq = New Expression(ret)
                                            If evEq.tryToIsolateToExpression(sVar, vR2) Then
                                                bImplicit = False
                                                ret = vR2(0)
                                            End If
                                        End If
                                    End If
                                    Exit Try
                                End If
                            Catch ex3 As Exception

                            End Try
                            If i = vResult.Length - 1 Then
                                ret = New Expression(Polynomial.GetPolynomial(sVar)) - ret
                                ret = rup.ReduceUsingPolynomials(ret)
                                evEq = rup.ReduceUsingPolynomials(evEq)
                                responseType = retMtx.diffResponse.implicit
                                retFns.AddVar(sVar, ret)
                                retFns.AddVar("_" + sVar, New Expression(0.0))
                                retFns.AddVar(sCnt, evEq)
                                retFns.AddVar("_" + sCnt, New Expression(0.0))
                            End If
                        End If
                    End If
                Next




            Else
                Dim eqIn_y As Expression = rMtx.vars.getValueByID(0).getExpr(0, 0)
                Dim _y As String = rMtx.vars.getVarNameByID(0)
                Dim chfn As New VarsAndFns(cfg)
                Dim pY As Polynomial = Polynomial.GetPolynomial(sVar)
                Dim pX As Polynomial = Polynomial.GetPolynomial(sDiffRespVar)
                chfn.AddVar(_y, New Expression(pX / pY))
                eqIn_y = eqIn_y.evalExprToExpr(chfn)
                Dim vR(-1) As Expression, alt As Expression = Nothing



                Dim sCnt As String = "_K1"
                Dim rup As New ReduceExprUsingPolynomials
                Dim bImplicit As Boolean = True
                Dim vResult(-1) As Expression
                Dim ret As New Expression(eqIn_y)
                ret = New Expression(Polynomial.GetPolynomial(sVar)) - ret
                Dim evEq As New Expression(eqIn_y)

                If ret.tryToIsolateToExpression(sVar, vResult) Then
                    bImplicit = False
                Else
                    ReDim vResult(0)
                    vResult(0) = ret
                End If
                For i = 0 To vResult.Length - 1
                    ret = vResult(i)
                    'Dim initConditions As VarsAndFns = eq.cfg.cur.vars
                    If initConditions IsNot Nothing Then
                        Dim e1 As String = String.Empty
                        Dim id0 As Int64 = 0
                        For Each e1 In initConditions.getNamesList
                            If sVar + "(" = Left(e1, Len(sDiffRespVar) + 1) Then
                                Exit For
                            End If
                            id0 += 1
                        Next
                        e1 = Trim(e1)
                        If Len(e1) AndAlso id0 < initConditions.getNamesList.Length Then
                            Dim m1 As Match = _
                                Regex.Match(e1, "\([^\)]*\)")
                            Dim dbl As Double
                            If m1.Success Then
                                Double.TryParse(Mid(m1.ToString, 2, m1.Length - 2), dbl)
                            Else
                                Exit For
                            End If
                            Dim varSOE As New VarsAndFns(eq.cfg)
                            varSOE.AddVar(sDiffRespVar, New Expression(dbl))
                            varSOE.AddVar(sVar, initConditions.getValueByID(0).getExpr(0, 0))
                            'Dim evEq As New Expression(ret)
                            Dim varK As New VarsAndFns(eq.cfg)
                            If bImplicit Then
                                evEq = evEq.evalExprToExpr(varSOE)
                            Else
                                evEq -= New Expression(Polynomial.GetPolynomial(sVar))
                                evEq = evEq.evalExprToExpr(varSOE)
                            End If
                            evEq = rup.ReduceUsingPolynomials(evEq)
                            evEq.IsEquation = True
                            Dim vR2(-1) As Expression
                            Dim soe As New SystemOfEquations( _
                                 Nothing, New ExprMatrix(evEq), varSOE, eq.cfg.cur, eq.cfg)
                            Try
                                Dim t As New System.Threading.Thread(AddressOf soe.resolveSysOfEqs)
                                t.Start(eq.cfg)
                                If Not t.Join(1500) Then
                                    varK.AddVar(sCnt, New Expression(soe.resultValues(0)))
                                    ret = ret.evalExprToExpr(varK)
                                    ret = rup.ReduceUsingPolynomials(ret)
                                    Dim vR3(-1) As Expression
                                    If ret.tryToIsolateToExpression(sVar, vR3) Then
                                        retFns.AddVar(sVar, vR3(0))
                                        retFns.AddVar(sCnt, New Expression(soe.resultValues(0)))
                                    Else
                                        If bImplicit Then
                                            evEq = New Expression(ret)
                                            If evEq.tryToIsolateToExpression(sVar, vR2) Then
                                                bImplicit = False
                                                ret = vR2(0)
                                            End If
                                        End If
                                    End If
                                    Exit Try
                                End If
                            Catch ex3 As Exception

                            End Try
                            If i = vResult.Length - 1 Then
                                ret = New Expression(Polynomial.GetPolynomial(sVar)) - ret
                                ret = rup.ReduceUsingPolynomials(ret)
                                evEq = rup.ReduceUsingPolynomials(evEq)
                                responseType = retMtx.diffResponse.implicit
                                retFns.AddVar(sVar, ret)
                                retFns.AddVar("_" + sVar, New Expression(0.0))
                                retFns.AddVar(sCnt, evEq)
                                retFns.AddVar("_" + sCnt, New Expression(0.0))
                            End If
                        End If
                    End If
                Next


            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return retFns
    End Function
    Public Shared Function resolveEulers(ByVal vExpr() As Expression, _
                            initialConditions As VarsAndFns) As Complex
        Dim ret As Complex = Nothing
        Dim fe As Differential_Equations
        Try
            Dim i As Int64
            Dim cfg As Config = vExpr(0).cfg
            Dim bDetail As Boolean = cfg.bDetail
            cfg.bDetail = False
            fe = Differential_Equations.tryParseDifEq(vExpr(2))
            Dim eq As New Expression(fe.eq)
            Dim bIsMn(-1) As Boolean
            Dim vSum() As Expression = eq.exprToSummands(bIsMn)
            For i = 0 To vSum.Length - 1
                If vSum(i).getMatchStr = "diff" Then
                    vSum(i) = Nothing
                End If
            Next
            eq = -Expression.summandsToExpr(vSum, bIsMn)
            Dim h As Double = vExpr(0).toDouble
            Dim N As Double = vExpr(1).toDouble
            Dim t0 As Complex = Complex.zero
            Dim y0 As Complex = Complex.zero
            If initialConditions Is Nothing Then
            Else
                Dim e1 As String = String.Empty
                Dim id0 As Int64 = 0
                For Each e1 In initialConditions.getNamesList
                    If fe.sVar + "(" = Left(e1, Len(fe.sDiffRespVar) + 1) Then
                        Exit For
                    End If
                    id0 += 1
                Next
                e1 = Trim(e1)
                If Len(e1) AndAlso id0 < initialConditions.getNamesList.Length Then
                    Dim m1 As Match = _
                        Regex.Match(e1, "\([^\)]*\)")
                    If m1.Success Then
                        Complex.TryParseComplex(cfg.sImg, Mid(m1.ToString, 2, m1.Length - 2), t0)
                        't0 = Complex.parse(cfg.sImg, Mid(m1.ToString, 2, m1.Length - 2))
                        y0 = initialConditions.getValueByID(id0).toComplex
                    Else
                        Throw New Exception(msg8.num(13))
                    End If
                End If
                Dim oVars As New VarsAndFns(cfg)
                oVars.AddVar(fe.sDiffRespVar, New Expression(t0))
                oVars.AddVar(fe.sVar, New Expression(y0))
                For i = 1 To Math.Floor(N)
                    Dim m As Complex = eq.evalExprToExpr(oVars).toComplex
                    Dim y1 As Complex = y0 + h * m
                    Dim t1 As Complex = t0 + h
                    If bDetail Then
                        cfg.oDetail.AddAlways("n=" + i.ToString + _
                            "  t=" + t1.toStringComplex(cfg) + _
                            "  y=" + y1.toStringComplex(cfg))

                    End If
                    t0 = t1
                    y0 = y1
                    oVars.setValue(0, New ExprMatrix(t0))
                    oVars.setValue(1, New ExprMatrix(y0))
                Next
                ret = y0
            End If
            cfg.bDetail = bDetail
        Catch ex As Exception
            Throw ex
        End Try
        Return ret
    End Function
    Public Shared Function resolveSolvable(expression As Expression, _
                    Optional ByRef rM As retMtx = Nothing) As VarsAndFns
        Dim fe As Differential_Equations
        Dim vars As VarsAndFns = Nothing
        Dim rMtx As retMtx
        Try
            rMtx = New retMtx

            fe = Differential_Equations.tryParseDifEq(expression)
            If fe.tipo = _
                Differential_Equations.DiffEqType.fstOrderlinear Then
                vars = fe.resolveFstOrderLinear(rMtx.diffType)
            ElseIf fe.tipo = _
            Differential_Equations.DiffEqType.fstOrderSeparable Then
                vars = fe.resolveSeparable(rMtx.diffType)
            ElseIf fe.tipo = Differential_Equations.DiffEqType.fstOrderExact Then
                vars = fe.resolveExact(rMtx.diffType)
            ElseIf fe.tipo = Differential_Equations.DiffEqType.fstOrderBernoulli Then
                vars = fe.resolveBernoulli(rMtx.diffType)
            ElseIf fe.tipo = DiffEqType.fstOrderSubstitution Then
                vars = fe.resolveSubstitution(rMtx.diffType)
            ElseIf fe.tipo = Differential_Equations.DiffEqType.NA Then
                Throw New Exception(msg8.num(13))
            End If
            If rM IsNot Nothing Then
                rM.diffType = rMtx.diffType
                rM.vars = vars
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return vars
    End Function
    Enum DiffEqType As Int64
        NA = 0
        fstOrderlinear
        fstOrderSeparable
        fstOrderExact
        fstOrderBernoulli
        fstOrderSubstitution
        fstOrderEulers
    End Enum
End Class
