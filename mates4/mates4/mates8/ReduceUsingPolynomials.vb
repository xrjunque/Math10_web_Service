Imports System.Text.RegularExpressions
Imports System.Text

Public Class ReduceExprUsingPolynomials
    Public Sub New()
        oVars = New VarsAndFns(shCfg)
    End Sub

    Shared shCfg As Config = Config.cfg
    Dim oVars As New VarsAndFns(shCfg)
    Dim expr As Expression

    Public Sub New(oVars As VarsAndFns)
        Me.oVars = oVars
        If Me.oVars Is Nothing Then
            Me.oVars = New VarsAndFns(shCfg)
        End If
    End Sub
    Public Function ReduceUsingPolynomials(exprToReduce As Expression) As Expression
        Dim eC As Expression = Nothing
        Dim bDetail As Boolean
        Dim bIsEquation As Boolean
        Try
            If exprToReduce.cfg IsNot Nothing Then
                bDetail = exprToReduce.cfg.bDetail
                exprToReduce.cfg.bDetail = False
            End If
            bIsEquation = exprToReduce.IsEquation
            With exprToReduce
                Dim cfg As Config = exprToReduce.cfg
                shCfg = cfg
                If .IsPolynomial() Then
                    Dim pA As Polynomial = .getPolynomial
                    Return New Expression(pA)
                End If
                If .cfg IsNot Nothing AndAlso .cfg.isTimeout Then
                    Throw New TimeoutException
                End If
                Dim bMn(-1) As Boolean
                Dim nFactors As Int64 = exprToReduce.exprToSummands(bMn).Length
                Dim auxR As RUPaux = RUPaux.exprToRUPaux(exprToReduce, oVars)
                Dim nFactAuxR As Int64 = auxR.exprToSummands(bMn).Length
                If nFactAuxR > nFactors + 1 Then
                    Return exprToReduce
                End If
                If auxR.IsPolynomial Then
                    auxR.getPolynomial.opReduceCommonExponents()
                End If
                Dim oV As New VarsAndFns(cfg)
                eC = auxR.evalExprToExpr(oV)
                eC = eC.evalExprToExpr(oVars) ' restore original expressions
                If False Then
                    Dim bIsMinus() As Boolean = Nothing
                    Dim vSum() As Expression = eC.exprToSummands(bIsMinus)
                    Dim i0(vSum.Length - 1) As Int64
                    Dim i1(vSum.Length - 1) As Int64
                    For i = 0 To vSum.Length - 1
                        If vSum(i).IsIntegrationConst Then
                            i0(i) = i + 1 : i1(i) = i + 1
                        Else
                            i0(i) = -2 * vSum.Length + i
                            i1(i) = -2 * vSum.Length + i
                        End If
                        Dim bIsDiv() As Boolean = Nothing
                        Dim vFactA() As Expression = vSum(i).exprToFactors(bIsDiv)
                        Dim sgn As Int64 = 1
                        For j As Int64 = 0 To vFactA.Length - 1
                            If vFactA(j).sign = -1 Then
                                sgn *= -1
                                vFactA(j) = vFactA(j).opChgSgn
                            End If
                        Next
                        If sgn = -1 Then
                            'vFactA(0) = vFactA(0).opChgSgn
                            bIsMinus(i) = Not bIsMinus(i)
                        End If
                        vSum(i) = Expression.factorsToExpr(vFactA, bIsDiv)

                    Next
                    Array.Sort(i0, vSum)
                    Array.Sort(i1, bIsMinus)
                    eC = New Expression(vSum(0))
                    If bIsMinus(0) Then
                        eC = eC.opChgSgn
                    End If
                    For i = 1 To vSum.Length - 1
                        If Not bIsMinus(i) Then
                            eC = Expression.exprOp("+", eC, vSum(i))
                        Else
                            eC = Expression.exprOp("-", eC, vSum(i))
                        End If
                    Next
                End If
            End With
        Catch ex As Exception
            Throw ex
        Finally
            If exprToReduce.cfg IsNot Nothing Then
                exprToReduce.cfg.bDetail = bDetail
            End If
            If eC IsNot Nothing Then
                eC.IsEquation = bIsEquation
            End If
        End Try
        Return eC
    End Function

End Class
Class RUPaux
    Inherits Expression
    Dim shCfg As Config
    Dim oVars As VarsAndFns
    'Public Sub New()
    '    MyBase.New(0.0)
    'End Sub
    Public Sub New(expr As Expression, ByRef oVars As VarsAndFns)
        MyBase.New(expr)
        Me.oVars = oVars
        Me.cfg = oVars.cfg
    End Sub
    Public Shared Function exprToRUPaux(expr As Expression, ByRef oVars As VarsAndFns) As RUPaux
        Dim ret As RUPaux = Nothing
        Try
            If expr.IsPolynomial Then
                If expr.sign = 1 Then
                    ret = New RUPaux( _
                        New Expression(expr.getPolynomial), oVars)
                Else
                    ret = New RUPaux( _
                        New Expression(-expr.getPolynomial), oVars)
                End If
            ElseIf expr.getMatch.Groups("fn").Success Then
                Dim sA As String = expr.getArgs(0).getMatchStr
                Dim sB As String = expr.getMatchStr
                Dim fn2 As String = LCase(sB)
                Dim iFn = Array.IndexOf(MathGlobal8.vFn, sA)
                If iFn > -1 AndAlso _
                MathGlobal8.vInvFn(iFn) = fn2 Then
                    Dim sgn As Int64 = expr.sign
                    If expr.getArgs(0).sign = 1 Then
                        expr = New Expression(expr.getArgs(0).getArgs(0))
                    Else
                        expr = New Expression(1) / expr.getArgs(0).getArgs(0)
                    End If
                    If sgn = -1 Then
                        expr = expr.opChgSgn
                    End If
                    ret = New RUPaux(expr, oVars)
                    Exit Try
                End If
                'expr.getArgs(0).cfg = expr.cfg 
                Dim arg As RUPaux = exprToRUPaux(expr.getArgs(0), oVars)
                Dim sFn As New StringBuilder( _
                    "__" + expr.getMatchStr + "_" + _
                    Replace(arg.ToStringExpr(oVars.cfg), " ", ""))
                sFn = New StringBuilder(Replace(sFn.ToString, "√", "sqr"))
                sFn = New StringBuilder(Replace(sFn.ToString, ".", "dot"))
                sFn = New StringBuilder(Replace(sFn.ToString, "(", "LP"))
                sFn = New StringBuilder(Replace(sFn.ToString, ")", "RP"))
                If sFn.Length > 12 AndAlso Mid(sFn.ToString, 1, 12) = "__logten_10^" Then
                    Dim sign As Int64 = expr.sign * expr.getArgs(0).sign
                    If sign = -1 Then
                        ret = New RUPaux(-expr.getArgs(0).getArgs(1), oVars)
                    Else
                        ret = New RUPaux(expr.getArgs(0).getArgs(1), oVars)
                    End If
                    Exit Try
                End If
                If sFn.Length > 11 AndAlso Mid(sFn.ToString, 1, 11) = "__logtwo_2^" Then
                    Dim sign As Int64 = expr.sign * expr.getArgs(0).sign
                    If sign = -1 Then
                        ret = New RUPaux(-expr.getArgs(0).getArgs(1), oVars)
                    Else
                        ret = New RUPaux(expr.getArgs(0).getArgs(1), oVars)
                    End If
                    Exit Try
                End If
                If Mid(sFn.ToString, 1, 6) = "__sqr_" AndAlso expr.getArgs(0).IsPolynomial Then
                    Dim baseP As Polynomial = expr.getArgs(0).getPolynomial
                    Dim vVarbase() As String = baseP.varAll
                    If vVarbase.Length = 1 AndAlso baseP.cf.Length = 1 AndAlso baseP.PolyResto Is Nothing Then
                        Dim pos As Int64 = Array.IndexOf(baseP.var, vVarbase(0))
                        Dim exp As Double = baseP.exp(0)(pos) * 0.5
                        If expr.sign = -1 Then
                            ret = New RUPaux(
                                -New Expression(
                                baseP.cf(0) ^ Complex.oneHalf *
                                Polynomial.GetPolynomial(vVarbase(0))) ^
                                New Expression(exp), oVars)
                        Else
                            ret = New RUPaux(
                                New Expression(
                                baseP.cf(0) ^ Complex.oneHalf *
                                Polynomial.GetPolynomial(vVarbase(0))) ^
                                New Expression(exp), oVars)
                        End If
                        Exit Try
                    End If
                End If

                Dim m As Match = Regex.Match(sFn.ToString, MathGlobal8.sOp)
                Static vOp() As String = {"-", "+", "*", "/", "^", "!", "%"}
                Static vOpNom() As String = {"mn", "plus", "mult", "div", "pow", "fact", "mod"}
                Do While m.Success
                    Dim nOp As Int64 = Array.IndexOf(vOp, m.ToString)
                    Dim e1 As String = Replace(sFn.ToString, m.ToString, "_" + vOpNom(nOp) + "_")
                    sFn = New StringBuilder(e1)
                    m = Regex.Match(sFn.ToString, MathGlobal8.sOp)
                Loop
                Dim Pa As Polynomial = Polynomial.GetPolynomial(sFn.ToString)
                Dim ID As Int64 = oVars.getVarIDByName(sFn.ToString, False)
                If ID = -1 Then
                    oVars.AddVar(sFn.ToString, Nothing)
                    ID = oVars.getVarIDByName(sFn.ToString, False)
                    oVars.setValue(ID, New ExprMatrix(expr)) ' sign is taken into account
                    ret = New RUPaux(New Expression(Pa), oVars)
                Else
                    Dim mult As Complex = Nothing
                    Dim varVal As Expression = oVars.getValueByID(ID).getExpr(0, 0)
                    If expr.isEqualTo(varVal, mult) Then
                        ret = New RUPaux(New Expression(mult * Pa), oVars)
                    Else
                        ret = New RUPaux(varVal, oVars)
                        ' Throw New Exception(msg8.num(13)) ' n/a
                    End If
                End If
            ElseIf expr.getArgs.Length = 1 Then
                If expr.getMatchStr = "!" Then
                    Dim ovar1 As New VarsAndFns(expr.cfg)
                    ret = New RUPaux(expr, ovar1)
                    Exit Try
                ElseIf expr.sign = 1 Then
                    ret = exprToRUPaux(expr.getArgs(0), oVars)
                Else
                    ret = exprToRUPaux(-expr.getArgs(0), oVars)
                End If
            ElseIf expr.getArgs.Length = 2 Then
                Dim arg0 As RUPaux = exprToRUPaux(expr.getArgs(0), oVars)
                Dim arg1 As RUPaux = exprToRUPaux(expr.getArgs(1), oVars)
                'arg0.cfg = shCfg : arg1.cfg = shCfg
                Dim sOp As String = expr.getMatchStr
                Dim mult As Complex = Nothing
                Select Case sOp
                    Case "+", "-"
                        Dim bIsMnA() As Boolean = Nothing
                        Dim vSumA() As Expression =
                            CType(arg0, Expression).exprToSummands(bIsMnA)
                        Dim bIsMnB() As Boolean = Nothing
                        Dim vSumB() As Expression =
                            CType(arg1, Expression).exprToSummands(bIsMnB)
                        If sOp = "-" Then
                            For i As Int64 = 0 To bIsMnB.Length - 1
                                bIsMnB(i) = Not bIsMnB(i)
                            Next
                        End If
                        Dim ret1 As New Expression(0.0)
                        'ret1.cfg = shCfg
                        For i As Int64 = 0 To vSumA.Length - 1
                            'vSumA(i).cfg = shCfg
                            Dim rAI As New RUPaux(vSumA(i), oVars)
                            Dim bIsCjoA As Boolean = vSumA(i).IsComplex
                            For j As Int64 = 0 To vSumB.Length - 1
                                If vSumB(j) IsNot Nothing AndAlso
                                bIsCjoA AndAlso vSumB(j).IsComplex Then
                                    Dim A As Complex = Nothing
                                    If Not bIsMnA(i) Then
                                        A = vSumA(i).toComplex
                                    Else
                                        A = -vSumA(i).toComplex
                                    End If
                                    Dim B As Complex = Nothing
                                    If Not bIsMnB(j) Then
                                        B = vSumB(j).toComplex
                                    Else
                                        B = -vSumB(j).toComplex
                                    End If
                                    If sOp = "+" Then
                                        ret1 += New Expression(A + B)
                                    Else
                                        ret1 += New Expression(A - B)
                                    End If
                                    vSumA(i) = Nothing
                                    vSumB(j) = Nothing
                                    Exit For
                                ElseIf vSumB(j) IsNot Nothing Then
                                    'vSumB(j).cfg = shCfg
                                    Dim rBI As New RUPaux(vSumB(j), oVars)
                                    If rAI.isEqualTo(rBI, mult) Then
                                        Dim one As RUPaux = Nothing
                                        If Not bIsMnA(i) Then
                                            one = New RUPaux(
                                                New Expression(New Complex(
                                                 1.0)), oVars)
                                        Else
                                            one = New RUPaux(
                                                New Expression(New Complex(
                                                 -1.0)), oVars)
                                        End If
                                        Dim mr As RUPaux = Nothing
                                        If Not bIsMnB(j) Then
                                            mr = New RUPaux(
                                                New Expression(New Complex(
                                                1.0 / mult)), oVars)
                                        Else
                                            mr = New RUPaux(
                                                New Expression(New Complex(
                                               1.0 / -mult)), oVars)
                                        End If
                                        ret1 += (one + mr) * rAI
                                        vSumA(i) = Nothing
                                        vSumB(j) = Nothing
                                        Exit For
                                    End If
                                End If
                            Next
                        Next
                        For i = 0 To vSumA.Length - 1
                            If vSumA(i) IsNot Nothing Then
                                If Not bIsMnA(i) Then
                                    ret1 += vSumA(i)
                                Else
                                    ret1 -= vSumA(i)
                                End If
                            End If
                        Next
                        For i = 0 To vSumB.Length - 1
                            If vSumB(i) IsNot Nothing Then
                                If Not bIsMnB(i) Then
                                    ret1 += vSumB(i)
                                Else
                                    ret1 -= vSumB(i)
                                End If
                            End If
                        Next
                        If ret1.IsPolynomial Then
                            ret1.getPolynomial.opReduceCommonExponents()
                        End If
                        ret = New RUPaux(ret1, oVars)
                    Case "*"
                        Dim bIsMnA() As Boolean = Nothing
                        Dim vSumA() As Expression =
                            CType(arg0, Expression).exprToSummands(bIsMnA)
                        Dim bIsMnB() As Boolean = Nothing
                        Dim vSumB() As Expression =
                            CType(arg1, Expression).exprToSummands(bIsMnB)
                        ret = New RUPaux(New Expression(0.0), oVars)
                        'ret.cfg = shCfg
                        For i As Int64 = 0 To vSumA.Length - 1
                            'vSumA(i).cfg = shCfg
                            Dim rAI As New RUPaux(vSumA(i), oVars)
                            For j As Int64 = 0 To vSumB.Length - 1
                                'vSumB(j).cfg = shCfg
                                Dim rBI As New RUPaux(vSumB(j), oVars)
                                If bIsMnA(i) = bIsMnB(j) Then
                                    ret += rAI * rBI
                                Else
                                    ret -= rAI * rBI
                                End If
                            Next
                        Next

                    Case "/"
                        If arg0.isEqualTo(arg1, mult) Then
                            ret = New RUPaux(New Expression(1 / mult), oVars)
                        Else
                            Dim bIsMnA() As Boolean = Nothing
                            Dim vSumA() As Expression =
                                CType(arg0, Expression).exprToSummands(bIsMnA)
                            Dim bIsMnB() As Boolean = Nothing
                            ret = New RUPaux(New Expression(0.0), oVars)
                            'ret.cfg = shCfg
                            For i As Int64 = 0 To vSumA.Length - 1
                                'vSumA(i).cfg = shCfg
                                Dim rAI As New RUPaux(vSumA(i), oVars)
                                If Not bIsMnA(i) Then
                                    ret += rAI / arg1
                                Else
                                    ret -= rAI / arg1
                                End If
                            Next
                        End If
                    Case "^"
                        Dim A As New Expression(expr.getArgs(0))
                        Dim B As New Expression(expr.getArgs(1))
                        If A.getMatchStr = "sqr" Then A = New Expression(0.5)
                        If B.getMatchStr = "sqr" Then B = New Expression(0.5)
                        Dim AIsPoly As Boolean = A.IsPolynomial
                        If AIsPoly AndAlso B.IsPolynomial Then
                            If expr.getArgs(0).sign = -1 Then A = A.opChgSgn
                            If expr.getArgs(1).sign = -1 Then B = B.opChgSgn
                            A *= B
                            If A.IsReal Then
                                Dim db As Double = A.toDouble
                                If db = 1.0 Then
                                    ret = New RUPaux(expr.getArgs(0).getArgs(0), oVars)
                                    Exit Select
                                ElseIf db = -1 Then
                                    ret = New RUPaux(New Expression(1.0) / expr.getArgs(0).getArgs(0), oVars)
                                    Exit Select
                                End If
                            End If
                        ElseIf AIsPoly AndAlso A.IsReal Then
                            Dim db As Double = A.toDouble
                            If db = 2.0 AndAlso B.getMatchStr = "logtwo" Then
                                ret = New RUPaux(expr.getArgs(1).getArgs(0), oVars)
                                Exit Select
                            ElseIf db = 10.0 AndAlso B.getMatchStr = "logten" Then
                                ret = New RUPaux(expr.getArgs(1).getArgs(0), oVars)
                                Exit Select
                            End If
                        End If
                        ret = arg0 ^ arg1
                    Case Else
                        ret = New RUPaux(expr, oVars)
                End Select

                If expr.sign = -1 Then
                    ret = New RUPaux(ret.opChgSgn, oVars)
                End If
            End If
        Catch ex As Exception
            Throw ex
        End Try
        'ret.cfg = shCfg
        Return ret
    End Function
    Public Overloads Shared Operator -(ByVal eA As RUPaux, ByVal eB As RUPaux) As RUPaux
        Return New RUPaux(CType(eA, Expression) - CType(eB, Expression), eA.oVars)
    End Operator
    Public Overloads Shared Operator +(ByVal eA As RUPaux, ByVal eB As RUPaux) As RUPaux
        Return New RUPaux(CType(eA, Expression) + CType(eB, Expression), eA.oVars)
    End Operator
    Public Overloads Shared Operator *(ByVal eA As RUPaux, ByVal eB As RUPaux) As RUPaux
        Dim eC As RUPaux = Nothing
        Try
            If eA.IsPolynomial AndAlso eB.IsPolynomial Then
                eC = New RUPaux(CType(eA, Expression) * CType(eB, Expression), eA.oVars)
                Exit Try
            End If
            Dim mult As Complex = Nothing
            If eA.isEqualTo(eB, mult) Then
                ' eA * m * eA
                Dim bIsDiv() As Boolean = Nothing
                Dim vFactA() As Expression = eA.exprToFactors(bIsDiv)
                Dim vFA2(-1) As Expression, bDiv2(-1) As Boolean, ia2 As Int64 = 0
                Dim cjo As Complex = 1.0 / mult
                For i As Int64 = 0 To vFactA.Length - 1
                    If vFactA(i).IsComplex Then ' 'if' añadido el ' 2014/10/25
                        cjo *= vFactA(i).toComplex ^ New Complex(2.0)
                    ElseIf vFactA(i).IsPolynomial Then
                        'vFactA(i) = New Expression( _
                        '   vFactA(i).getPolynomial ^ New Polynomial(2.0))
                        ReDim Preserve vFA2(ia2), bDiv2(ia2) ' 2014/10/25
                        vFA2(ia2) = New Expression( _
                            vFactA(i).getPolynomial ^ New Polynomial(2.0))
                        bDiv2(ia2) = bIsDiv(i)
                        ia2 += 1
                    Else
                        'Dim fA As New RUPaux(vFactA(i), eA.oVars)
                        'fA ^= New RUPaux(New Expression(2.0), eA.oVars)
                        'vFactA(i) = CType(fA, Expression)
                        ReDim Preserve vFA2(ia2), bDiv2(ia2) ' 2014/10/25
                        Dim fA As New RUPaux(vFactA(i), eA.oVars)
                        fA ^= New RUPaux(New Expression(2.0), eA.oVars)
                        vFA2(ia2) = CType(fA, Expression)
                        bDiv2(ia2) = bIsDiv(i)
                        ia2 += 1
                    End If
                Next
                'eC = New RUPaux(Expression.factorsToExpr( _
                '                vFactA, bIsDiv), eA.oVars)
                If ia2 = 0 Then
                    eC = New RUPaux(New Expression(cjo), eA.oVars) ' 2014/10/25
                Else
                    eC = New RUPaux(New Expression(cjo) * _
                            Expression.factorsToExpr( _
                            vFA2, bDiv2), eA.oVars) ' 2014/10/25
                End If
                Exit Try
            End If
            If eA.isEqualTo(New Expression(1.0) / eB, mult) Then
                ' eA /( m * eA)
                eC = New RUPaux(New Expression(1.0 / mult), eA.oVars)
                Exit Try
            End If
            If True Then
                Dim bIsMn() As Boolean = Nothing
                Dim eA2 As New Expression(eA)
                Dim eB2 As New Expression(eB)
                Dim sgn As Int64 = eA2.sign * eB2.sign
                Dim vSumm() As Expression = (eA2 * eB2).exprToSummands(bIsMn)
                For i As Int64 = 0 To vSumm.Length - 1
                    Dim bIsDiv() As Boolean = Nothing
                    Dim vFactA() As Expression = vSumm(i).exprToFactors(bIsDiv)
                    For j As Int64 = 0 To vFactA.Length - 1
                        If vFactA(j).IsPolynomial Then
                            Dim bIsDivPoly() As Boolean = Nothing
                            Dim vFactPoly() As Expression = Nothing
                            If vFactA(j).getPolynomial.split_1OnlyTermToExpr( _
                                vFactPoly, bIsDivPoly) Then
                                If bIsDiv(j) Then
                                    For k As Int64 = 0 To vFactPoly.Length - 1
                                        bIsDivPoly(k) = Not bIsDivPoly(k)
                                    Next
                                End If
                                Dim i1 As Int64 = vFactA.Length
                                Dim i2 As Int64 = vFactPoly.Length
                                ReDim Preserve vFactA(i1 + i2 - 1), _
                                    bIsDiv(i1 + i2 - 1)
                                Array.Copy(vFactPoly, 0, vFactA, i1, i2)
                                Array.Copy(bIsDivPoly, 0, bIsDiv, i1, i2)
                                vFactA(j) = Nothing
                            End If
                        End If
                    Next
                    For j As Int64 = 0 To vFactA.Length - 1
                        If vFactA(j) IsNot Nothing Then
                            For k As Int64 = j + 1 To vFactA.Length - 1
                                If j <> k AndAlso
                                vFactA(k) IsNot Nothing Then
                                    Dim bIsDivJ As Int64 = bIsDiv(j)
                                    If vFactA(j).isEqualTo(vFactA(k), mult) Then
                                        If Not bIsDiv(j) Then
                                            If Not bIsDiv(k) Then
                                                ' A * mA
                                                If vFactA(j).IsPolynomial Then
                                                    vFactA(j) = New Expression(
                                                        vFactA(j).getPolynomial ^
                                                        New Polynomial(2.0))
                                                Else
                                                    Dim fA As New RUPaux(vFactA(j), eA.oVars)
                                                    fA ^= New RUPaux(New Expression(2.0), eA.oVars)
                                                    vFactA(j) = CType(fA, Expression)
                                                End If
                                            Else
                                                ' A / (mA) = 1/m
                                                vFactA(j) = New Expression(1.0 / mult)
                                            End If
                                        Else
                                            If Not bIsDiv(k) Then
                                                ' (1/A) * mA = m
                                                vFactA(j) = New Expression(1.0)
                                            Else
                                                ' (1/A) / (mA) 
                                                vFactA(j) = vFactA(j) ^
                                                    New Expression(2.0)
                                            End If
                                        End If
                                        vFactA(k) = Nothing
                                        Exit For
                                    ElseIf vFactA(j).getMatchStr = "^" OrElse
                                    vFactA(k).getMatchStr = "^" Then
                                        Dim cffA As New Complex(1.0)
                                        Dim cffb As New Complex(1.0)
                                        Dim expA As New Expression(1.0)
                                        Dim expB As New Expression(1.0)
                                        Dim baseA As New Expression(vFactA(j))
                                        Dim baseB As New Expression(vFactA(k))
                                        If vFactA(j).getMatchStr = "^" Then
                                            baseA = vFactA(j).getArgs(0)
                                            expA = New Expression(vFactA(j).getArgs(1))
                                            If bIsDivJ Then
                                                expA = -expA
                                                bIsDivJ = False
                                            End If
                                        ElseIf vFactA(j).IsPolynomial Then
                                            Dim pA As Polynomial = New Polynomial(
                                                                  vFactA(j).getPolynomial)
                                            If pA.PolyResto IsNot Nothing Then
                                                GoTo sigK
                                            End If
                                            Dim terms() As Polynomial = pA.splitIntoTerms
                                            If terms.Length > 1 Then
                                                GoTo sigK
                                            End If
                                            If pA.var.Length <> 1 Then GoTo sigK
                                            If pA.exp.Length AndAlso
                                            pA.exp(0) IsNot Nothing Then
                                                expA = New Expression(pA.exp(0)(0))
                                                If bIsDivJ Then
                                                    expA = -expA
                                                    bIsDivJ = False
                                                End If
                                                cffA = pA.An
                                                baseA = New Expression(
                                                    Polynomial.GetPolynomial(pA.var(0)))
                                            End If
                                        End If
                                        If vFactA(k).getMatchStr = "^" Then
                                            baseB = vFactA(k).getArgs(0)
                                            expB = New Expression(vFactA(k).getArgs(1))
                                        ElseIf vFactA(k).IsPolynomial Then
                                            Dim pB As Polynomial = New Polynomial(
                                                                  vFactA(k).getPolynomial)
                                            If pB.PolyResto IsNot Nothing Then
                                                GoTo sigK
                                            End If
                                            Dim terms() As Polynomial = pB.splitIntoTerms
                                            If terms.Length > 1 Then
                                                GoTo sigK
                                            End If
                                            If pB.var.Length <> 1 Then GoTo sigK
                                            If pB.exp.Length AndAlso
                                            pB.exp(0) IsNot Nothing Then
                                                expB = New Expression(pB.exp(0)(0))
                                                cffb = pB.An
                                                baseB = New Expression(
                                                  Polynomial.GetPolynomial(pB.var(0)))
                                            End If
                                        End If
                                        If baseA.isEqualTo(baseB, mult) AndAlso
                                        mult.IsReal AndAlso mult.pRe.ToDouble = 1.0 Then
                                            bIsDiv(j) = bIsDivJ
                                            If Not bIsDiv(j) Then
                                                If Not bIsDiv(k) Then
                                                    ' (cffa*A)^ExpA * (mA)^ExpB
                                                    ' =(cffa)^ExpA* m^ExpB * A^(expA+expB)
                                                    vFactA(j) = New Expression(cffA * cffb) '^ expA
                                                    vFactA(j) *= New Expression(mult) '^ expB
                                                    vFactA(j) *= Expression.exprOp(
                                                                   "^", baseA, expA + expB)
                                                Else
                                                    ' (cffa*A)^ExpA /[ (mA)^ExpB]
                                                    ' = cffa^ExpA* m^-expB * A^(expA-expB)
                                                    vFactA(j) = New Expression(cffA / cffb) '^ expA
                                                    vFactA(j) *= New Expression(mult) '^ -expB
                                                    vFactA(j) *= Expression.exprOp(
                                                                   "^", baseA, expA - expB)
                                                End If
                                            Else
                                                If Not bIsDiv(k) Then
                                                    '(1/(cffa*A)^ExpA) * (mA)^ExpB
                                                    ' = cffa^-ExpA * m^ExpB * A^(-expA+expB)
                                                    vFactA(j) = New Expression(cffb / cffA) ' ^ -expA
                                                    vFactA(j) *= New Expression(mult) '^ expB
                                                    vFactA(j) *= Expression.exprOp(
                                                                   "^", baseA, -expA + expB)
                                                Else
                                                    ' (1/(cffa*A)^ExpA) /[ (mA)^ExpB]
                                                    ' = cffa^-ExpA * m^-expB * A^(-expA-expB)
                                                    vFactA(j) = New Expression(cffA * cffb) '^ -expA
                                                    vFactA(j) *= New Expression(mult) '^ -expB
                                                    vFactA(j) *= Expression.exprOp(
                                                                   "^", baseA, -expA - expB)
                                                End If
                                            End If
                                            vFactA(j) = New Expression(cffA) * vFactA(j)
                                            vFactA(k) = Nothing
                                            Exit For
                                        End If
                                        'If Not cffA.IsReal OrElse _
                                        'cffA.toDouble <> 1.0 Then
                                        '    vFactA(j) = exprOp("*", cffA, vFactA(j))
                                        'End If
                                    ElseIf vFactA(j).getMatchStr = "!" AndAlso
                                    vFactA(k).getMatchStr = "!" Then
                                        Dim pA As Polynomial = Nothing
                                        Dim pB As Polynomial = Nothing
                                        If vFactA(j).getArgs(0).IsPolynomial AndAlso
                                        vFactA(k).getArgs(0).IsPolynomial Then
                                            pA = vFactA(j).getArgs(0).getPolynomial
                                            pB = vFactA(k).getArgs(0).getPolynomial
                                            Dim pC As Polynomial = pA - pB
                                            If pC.isReal Then
                                                Dim db As Double = pC.ToDouble
                                                If db = Math.Floor(db) Then
                                                    If db >= 0 Then
                                                        pC = New Polynomial(pA)
                                                        For i1 As Int64 = 1 To db - 1
                                                            pC *= (pA - New Polynomial(i1))
                                                        Next
                                                        vFactA(j) = New Expression(pC)
                                                    Else ' db<0
                                                        pC = New Polynomial(pB)
                                                        For i1 As Int64 = 1 To -db - 1
                                                            pC *= (pB - New Polynomial(i1))
                                                        Next
                                                        vFactA(j) =
                                                            exprOp("/", New Expression(1.0),
                                                            New Expression(pC))
                                                    End If
                                                End If
                                            End If
                                            vFactA(k) = Nothing
                                        End If
                                    End If
                                End If
sigK:
                            Next
                        End If
                    Next
                    Dim bIsDiv2(-1) As Boolean
                    Dim vFact2(-1) As Expression
                    Dim iv As Int64 = 0
                    For j As Int64 = 0 To vFactA.Length - 1
                        If vFactA(j) IsNot Nothing Then
                            ReDim Preserve bIsDiv2(iv), vFact2(iv)
                            vFact2(iv) = vFactA(j)
                            bIsDiv2(iv) = bIsDiv(j)
                            iv += 1
                        End If
                    Next
                    vSumm(i) = Expression.factorsToExpr(vFact2, bIsDiv2)
                    If sgn = -1 Then
                        'vSumm(i).sign *= -1
                    End If
                Next
                eC = New RUPaux(Expression.summandsToExpr(vSumm, bIsMn), eA.oVars)
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return eC
    End Operator
    Public Overloads Shared Operator /(ByVal eA As RUPaux, ByVal eB As RUPaux) As RUPaux
        Dim eC As RUPaux = Nothing
        Try
            If eA.IsPolynomial AndAlso eB.IsPolynomial Then
                eC = New RUPaux(CType(eA, Expression) / CType(eB, Expression), eA.oVars)
                Exit Try
            End If
            Dim mult As Complex = Nothing
            If eA.isEqualTo(eB, mult) Then
                ' eA /( (1/m) * eA )
                eC = New RUPaux(New Expression(mult), eA.oVars) ' 2014/10/25
                Exit Try

                'Dim bIsDiv() As Boolean = Nothing
                'Dim vFactA() As Expression = eA.exprToFactors(bIsDiv)
                'For i As Int64 = 0 To vFactA.Length - 1
                '    bIsDiv(i) = Not bIsDiv(i)
                '    If vFactA(i).IsPolynomial Then
                '        vFactA(i) = New Expression( _
                '            vFactA(i).getPolynomial ^ New Polynomial(2.0))
                '    Else
                '        Dim fA As New RUPaux(vFactA(i), eA.oVars)
                '        fA ^= New RUPaux(New Expression(2.0), eA.oVars)
                '        vFactA(i) = CType(fA, Expression)
                '    End If
                'Next
                'eC = New RUPaux(Expression.factorsToExpr( _
                '                vFactA, bIsDiv), eA.oVars)
                'Exit Try

            End If
            If eA.isEqualTo(New Expression(1.0) / eB, mult) Then
                ' eA *( m * eA)
                eC = New RUPaux(New Expression(mult), eA.oVars)
                Exit Try
            End If
            If True Then
                Dim bIsMn() As Boolean = Nothing
                Dim eA2 As New Expression(eA)
                Dim eB2 As New Expression(eB)
                Dim vSumm() As Expression = (eA2 / eB2).exprToSummands(bIsMn)
                For i As Int64 = 0 To vSumm.Length - 1
                    Dim bIsDiv() As Boolean = Nothing
                    Dim vFactA() As Expression = vSumm(i).exprToFactors(bIsDiv)
                    For j As Int64 = 0 To vFactA.Length - 1
                        If vFactA(j).IsPolynomial Then
                            Dim bIsDivPoly() As Boolean = Nothing
                            Dim vFactPoly() As Expression = Nothing
                            If vFactA(j).getPolynomial.split_1OnlyTermToExpr( _
                                vFactPoly, bIsDivPoly) Then
                                If bIsDiv(j) Then
                                    For k As Int64 = 0 To vFactPoly.Length - 1
                                        bIsDivPoly(k) = Not bIsDivPoly(k)
                                    Next
                                End If
                                Dim i1 As Int64 = vFactA.Length
                                Dim i2 As Int64 = vFactPoly.Length
                                ReDim Preserve vFactA(i1 + i2 - 1), _
                                    bIsDiv(i1 + i2 - 1)
                                Array.Copy(vFactPoly, 0, vFactA, i1, i2)
                                Array.Copy(bIsDivPoly, 0, bIsDiv, i1, i2)
                                vFactA(j) = Nothing
                            End If
                        End If
                    Next
                    For j As Int64 = 0 To vFactA.Length - 1

                        If vFactA(j) IsNot Nothing Then
                            For k As Int64 = j + 1 To vFactA.Length - 1
                                If j <> k AndAlso _
                                vFactA(k) IsNot Nothing Then
                                    If vFactA(j).isEqualTo(vFactA(k), mult) Then
                                        If Not bIsDiv(j) Then
                                            If Not bIsDiv(k) Then
                                                ' A * mA
                                                If vFactA(j).IsPolynomial Then
                                                    vFactA(j) = New Expression( _
                                                        vFactA(j).getPolynomial ^ _
                                                        New Polynomial(2.0))
                                                Else
                                                    Dim fA As New RUPaux(vFactA(j), eA.oVars)
                                                    fA ^= New RUPaux(New Expression(2.0), eA.oVars)
                                                    vFactA(j) = CType(fA, Expression)
                                                End If
                                            Else
                                                ' A / (mA) = 1/m
                                                vFactA(j) = New Expression(1.0 / mult)
                                            End If
                                        Else
                                            If Not bIsDiv(k) Then
                                                ' (1/A) * mA = m
                                                vFactA(j) = New Expression(1.0)
                                            Else
                                                ' (1/A) / (mA) 
                                                vFactA(j) = vFactA(k) ^ _
                                                    New Expression(-2.0)
                                            End If
                                        End If
                                        vFactA(k) = Nothing
                                        Exit For
                                    ElseIf vFactA(j).getMatchStr = "^" OrElse _
                                    vFactA(k).getMatchStr = "^" Then
                                        'Dim cffA As New Expression(1.0)
                                        'Dim cffb As New Expression(1.0)
                                        Dim bIsDivJ As Boolean = bIsDiv(j)
                                        Dim expA As New Expression(1.0)
                                        Dim expB As New Expression(1.0)
                                        Dim baseA As New Expression(vFactA(j))
                                        Dim baseB As New Expression(vFactA(k))
                                        If vFactA(j).getMatchStr = "^" Then
                                            baseA = vFactA(j).getArgs(0)
                                            expA = New Expression(vFactA(j).getArgs(1))
                                            If Not bIsDivJ Then
                                                expA = -expA
                                                bIsDivJ = True
                                            End If
                                        ElseIf vFactA(j).IsPolynomial Then
                                            Dim pA As Polynomial = New Polynomial( _
                                                                  vFactA(j).getPolynomial)
                                            If pA.PolyResto IsNot Nothing Then
                                                GoTo sigK
                                            End If
                                            Dim terms() As Polynomial = pA.splitIntoTerms
                                            If terms.Length > 1 Then
                                                GoTo sigK
                                            End If
                                            If pA.var.Length <> 1 Then GoTo sigK
                                            If pA.exp.Length AndAlso _
                                            pA.exp(0) IsNot Nothing Then
                                                expA = New Expression(pA.exp(0)(0))
                                                If Not bIsDivJ Then
                                                    expA = -expA
                                                    bIsDivJ = True
                                                End If
                                                'cffA = New Expression(pA.An)
                                                baseA = New Expression(pA.An) * _
                                                    New Expression( _
                                                    Polynomial.GetPolynomial(pA.var(0)))
                                            End If
                                        End If
                                        If vFactA(k).getMatchStr = "^" Then
                                            baseB = vFactA(k).getArgs(0)
                                            expB = New Expression(vFactA(k).getArgs(1))
                                        ElseIf vFactA(k).IsPolynomial Then
                                            Dim pB As Polynomial = New Polynomial(
                                                                  vFactA(k).getPolynomial)
                                            If pB.PolyResto IsNot Nothing Then
                                                GoTo sigK
                                            End If
                                            Dim terms() As Polynomial = pB.splitIntoTerms
                                            If terms.Length > 1 Then
                                                GoTo sigK
                                            End If
                                            If pB.var.Length <> 1 Then GoTo sigK
                                            If pB.exp.Length AndAlso
                                            pB.exp(0) IsNot Nothing Then
                                                expB = New Expression(pB.exp(0)(0))
                                                'cffb = New Expression(pB.An)
                                                baseB = New Expression(pB.An) *
                                                    New Expression(
                                                  Polynomial.GetPolynomial(pB.var(0)))
                                            End If
                                        End If
                                        If baseA.isEqualTo(baseB, mult) AndAlso
                                        mult.IsReal Then
                                            bIsDiv(j) = bIsDivJ
                                            If Not bIsDiv(j) Then
                                                If Not bIsDiv(k) Then
                                                    ' (A)^ExpA * (mA)^ExpB
                                                    ' = m^ExpB * A^(expA+expB)
                                                    'vFactA(j) = cffA * cffb
                                                    vFactA(j) = New Expression(mult) '^ expB
                                                    vFactA(j) *= Expression.exprOp(
                                                                   "^", baseA, expA + expB)
                                                Else
                                                    ' (A)^ExpA /[ (mA)^ExpB]
                                                    ' =  m^-expB * A^(expA-expB)
                                                    'vFactA(j) = cffA / cffb
                                                    vFactA(j) = New Expression(mult) ' ^ -expB
                                                    vFactA(j) *= Expression.exprOp(
                                                                   "^", baseA, expA - expB)
                                                End If
                                            Else
                                                If Not bIsDiv(k) Then
                                                    '(1/(A)^ExpA) * (mA)^ExpB
                                                    ' = cffa^-ExpA * m^ExpB * A^(-expA+expB)
                                                    'vFactA(j) = cffb / cffA
                                                    vFactA(k) = New Expression(1.0 / mult) '^ expB
                                                    vFactA(k) *= Expression.exprOp(
                                                                   "^", baseA, -expA + expB)
                                                    vFactA(j) = vFactA(k)
                                                    bIsDiv(j) = False
                                                Else
                                                    ' (1/(cffa*A)^ExpA) /[ (mA)^ExpB]
                                                    ' = cffa^-ExpA * m^-expB * A^(-expA-expB)
                                                    'vFactA(j) =New Expression (1.0)/( cffA * cffb)
                                                    vFactA(j) = New Expression(mult) ' ^ -expB
                                                    vFactA(j) *= Expression.exprOp(
                                                                   "^", baseA, -expA - expB)
                                                End If
                                            End If
                                            vFactA(k) = Nothing
                                            Exit For
                                        End If
                                        'If Not cffA.IsReal OrElse _
                                        'cffA.toDouble <> 1.0 Then
                                        '    vFactA(j) = exprOp("*", cffA, vFactA(j))
                                        'End If
                                    End If
                                End If
sigK:
                            Next
                        End If
                    Next
                    Dim bIsDiv2(-1) As Boolean
                    Dim vFact2(-1) As Expression
                    Dim iv As Int64 = 0
                    For j As Int64 = 0 To vFactA.Length - 1
                        If vFactA(j) IsNot Nothing Then
                            ReDim Preserve bIsDiv2(iv), vFact2(iv)
                            vFact2(iv) = vFactA(j)
                            bIsDiv2(iv) = bIsDiv(j)
                            iv += 1
                        End If
                    Next
                    vSumm(i) = Expression.factorsToExpr(vFact2, bIsDiv2)
                Next
                eC = New RUPaux(Expression.summandsToExpr(vSumm, bIsMn), eA.oVars)
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return eC
    End Operator
    Public Overloads Shared Operator ^(ByVal eA As RUPaux, ByVal eB As RUPaux) As RUPaux
        Dim eC As RUPaux = Nothing
        Try
            If eB.IsReal AndAlso eA.getMatchStr = "^" Then
                eC = New RUPaux(eA, eA.oVars)
                eC.getArgs(1) *= eB
                Exit Try
            End If
            If eB.IsReal Then
                Dim db As Double = eB.toDouble
                Dim bIsDiv() As Boolean = Nothing
                Dim vFactA() As Expression = Nothing
                If eA.IsPolynomial Then
                    vFactA = New Expression() {eA}
                    bIsDiv = New Boolean() {False}
                Else
                    vFactA = eA.exprToFactors(bIsDiv)
                End If
                Dim i As Int64 = 0
                Dim bTrat As Boolean = True
                Do
                    Dim expA As New Expression(1.0)
                    'vFactA(i).cfg = shCfg
                    Dim baseA As New Expression(vFactA(i))
                    If vFactA(i).getMatchStr = "^" Then
                        baseA = vFactA(i).getArgs(0)
                        expA = New Expression(vFactA(i).getArgs(1))
                        expA *= eB
                        If expA.IsReal AndAlso expA.toDouble = 1.0 Then
                            vFactA(i) = baseA
                        Else
                            vFactA(i) = Expression.exprOp("^", baseA, expA)
                        End If
                    ElseIf vFactA(i).IsPolynomial Then
                        Dim pA As Polynomial = New Polynomial( _
                                              vFactA(i).getPolynomial)
                        Dim cA As Complex = New Complex(pA.An)
                        If pA.PolyResto IsNot Nothing Then
                            Dim p As New Polynomial(pA)
                            p.PolyResto = Nothing
                            p.PolyDivisor = Nothing
                            If Not p.isReal OrElse _
                            Not p.ToDouble = 0 Then
                                GoTo sinTrat
                            End If
                            Dim num As New Polynomial(pA.PolyResto)
                            Dim div As New Polynomial(pA.PolyDivisor)
                            cA = num.An / div.An
                            num.cf(0) = New Complex(1.0)
                            div.cf(0) = New Complex(1.0)
                            vFactA(i) = New Expression(num)
                            ' append 'div' to the factors array vFact:
                            Dim j As Int64 = vFactA.Length
                            ReDim Preserve vFactA(j), bIsDiv(j)
                            vFactA(j) = New Expression(div)
                            bIsDiv(j) = Not bIsDiv(i)
                            ' continue
                            pA = num
                            vFactA(i) = New Expression(num)
                        Else
                            pA.cf(0) = New Complex(1.0)
                        End If
                        Dim terms() As Polynomial = pA.splitIntoTerms
                        If terms.Length > 1 Then
                            GoTo sinTrat
                        End If
                        If pA.var.Length > 1 Then GoTo sinTrat
                        If pA.var.Length = 0 Then
                            expA = New Expression(1.0)
                            baseA = New Expression(pA.An)
                        Else
                            If pA.exp.Length AndAlso _
                            pA.exp(0) IsNot Nothing Then
                                expA = New Expression(pA.exp(0)(0))
                                baseA = New Expression(pA.An) * _
                                    New Expression( _
                                    Polynomial.GetPolynomial(pA.var(0)))
                            End If
                        End If
                        expA *= eB
                        If expA.IsReal AndAlso expA.toDouble = 1.0 Then
                            vFactA(i) = baseA
                        Else
                            vFactA(i) = Expression.exprOp("^", baseA, expA)
                        End If
                        If cA.IsReal AndAlso _
                        cA.pRe.ToDouble < 0 AndAlso _
                        Not eA.IsComplex Then
                            bTrat = False
                        ElseIf Not cA.IsReal OrElse _
                        Not cA.pRe.ToDouble = 1.0 Then
                            vFactA(i) = New Expression( _
                                cA ^ eB.getPolynomial.ToComplex) * vFactA(i)
                        End If
                    Else
                        bTrat = False
                    End If
                    i += 1
                Loop While bTrat AndAlso i < vFactA.Length
                If bTrat Then
                    eC = New RUPaux(Expression.factorsToExpr(vFactA, bIsDiv), eA.oVars)
                    Exit Try
                End If
            End If
sinTrat:
            eC = New RUPaux(Expression.exprOp("^", eA, eB), eA.oVars)
        Catch ex As Exception
            Dim s1 As String = ex.ToString

        End Try
        Return eC
    End Operator
End Class
