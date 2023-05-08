Imports System.IO
Imports System.Text.RegularExpressions
Imports System.Text

Public Class Functions
    Public Shared Function partialFractionDecomposition(pA As Polynomial, _
                    ByRef vNum() As Polynomial, _
                    ByRef vDen() As Polynomial, _
                    ByRef vDenExponent() As Int64, _
                    cfg As Config, _
                    ByRef sErrMsg As String) As Boolean
        Dim bRet As Boolean = False
        Dim bDetail As Boolean = cfg.bDetail
        Try
            cfg.bDetail = False
            ' From Pa = P / Q (degree(Q) < degreee( P )
            '  Q=(x-a1)^b1*(x-a2)^b2*...*(x^2-s1)^c1*(x^2-s2)^c2*...*(x-r1)^t1*(x-r2)^t2
            '  a1,a2,a3,... real roots; s1,s2,s3,.. conjugate pairs of roots
            '  and r1,r2,r3,... complex roots Re(r1)<>0 , Im(r2)<>0
            ' we want to arrive to (partial fraction decomposition):
            ' P/Q = _A1_1/(x-a1) + _A1_2/(x-a1)^2 + ...+ _A1_b1/(x-a1)^b1 +  _A2_1/(x-a2) + _A2_2/(x-a2)^2 +...
            '     + (_B1_1a*x+_B1_1b)/(x^2-s1) + (_B1_2a*x+_B1_2b)/(x^2-s1)^2+...+ (_B1_1a*x+_B1_1b)/(x^2-s1)^c2+ 
            '       (_B2_1a*x+_B2_1b)/(x^2-s2) + (_B2_2a*x+_B2_2b)/(x^2-s2)^2 + ...
            '     + _C1_1/(x-r1) + _C1_2/(x-r1)^2 + ...+ _A1_t1/(x-r1)^t1 +  _A2_1/(x-r2) + _A2_2/(x-r2)^2 +...
            ' Verify pA is just a fraction:
            Dim vPt(-1) As Complex, iPt As Int64 = 0
            Dim p As New Polynomial(pA)

            p.PolyResto = Nothing
            p.PolyDivisor = Nothing
            If Not p.isReal OrElse _
            Not p.ToDouble = 0.0 Then
                sErrMsg = msg8.num(68) ' n/a
                Exit Try
            End If
            If pA.PolyResto Is Nothing Then
                sErrMsg = msg8.num(13) ' n/a
                Exit Try
            End If
            Dim var() As String = pA.varAll
            If var.Length <> 1 Then
                sErrMsg = String.Format(msg8.num(69), pA.ToString, Join(var, " ")) ' more than 1 var
                Exit Try
            End If

            Dim X As Polynomial = Polynomial.GetPolynomial(var(0))
            Dim oVar As New VarsAndFns(cfg)

            ' Find pA's denominator roots:
            'Dim Va As New Vector(pA.PolyDivisor)
            Dim vRoots() As Complex = Polynomial.opRoots(pA.PolyDivisor).cjo
            Dim vR(-1) As Complex
            Dim i, j As Int64

            If cfg.bRounding Then
                For i = 0 To vRoots.Length - 1
                    Dim re As Double = vRoots(i).pRe.ToDouble
                    Dim im As Double = vRoots(i).pIm.ToDouble
                    Dim reRnd As Double = Math.Round(re, 3)
                    Dim imRnd As Double = Math.Round(im, 3)
                    If reRnd AndAlso imRnd = 0 Then
                        vRoots(i) = New Complex(re)
                    End If
                    If reRnd = 0 AndAlso imRnd Then
                        vRoots(i) = New Complex(0.0, im)
                    End If
                Next
            End If

            ' Position, conjugate root pairs, adjacent:
            For i = 0 To vRoots.Length - 1
                If Not vRoots(i).pIm.IsZero Then
                    For j = i + 2 To vRoots.Length - 1
                        ' Check for a conjugate:
                        If (vRoots(i) - _
                        Complex.opConjugate(vRoots(j))).IsZero Then
                            ' found conjugate at index j,
                            ' swap with root at i+1:
                            Dim aux As Complex = vRoots(i + 1)
                            vRoots(i + 1) = vRoots(j)
                            vRoots(j) = aux
                            ' now, adjacent vRoots(i) and vRoots(i+1)
                            ' is a conjugate pair.
                            Exit For
                        End If
                    Next
                End If
            Next

            ' Split the roots into 3 types:
            ' a) real roots
            ' b) conjugate root pairs
            ' c) the rest (imaginary roots without a conjugate)
            ' There may be none, one or more of each type.
            ' If any root is repeated twice or more (multiplicity)
            ' it will be accounted just once and the multiplicity
            ' accounted in arrays vMultiRR, vMultiConj, vMultiNotConj.
            Dim vRealRoots(-1) As Complex, vMultiRR(-1), iR As Int64
            Dim vConjugate(-1) As Complex, vMultiConj(-1), iC As Int64
            Dim vNotConjugate(-1) As Complex, vMultiNotConj(-1), iNC As Int64
            For i = 0 To vRoots.Length - 1
                ReDim Preserve vPt(iPt)
                vPt(iPt) = New Complex(vRoots(i))
                iPt += 1
                If vRoots(i).pIm.IsZero Then
                    ' vRoots(i) is a real root (imaginary part=0)
                    ReDim Preserve vRealRoots(iR), vMultiRR(iR)
                    vRealRoots(iR) = vRoots(i)
                    vMultiRR(iR) = 1
                    ' because roots are sorted, eventual repeated
                    ' real roots are in sequence
                    Do While i + 1 < vRoots.Length AndAlso _
                    (vRoots(i) - vRoots(i + 1)).IsZero
                        ' roots at i and i+1 positions are equal 
                        ' (repeated):
                        vMultiRR(iR) += 1 ' incr. multiplicity
                        i += 1
                    Loop
                    iR += 1
                ElseIf i + 1 < vRoots.Length AndAlso _
                (vRoots(i) - Complex.opConjugate(vRoots(i + 1))).IsZero Then
                    ' roots at i and i+1 positions are conjugate ?
                    ReDim Preserve vConjugate(iC), vMultiConj(iC)
                    vConjugate(iC) = vRoots(i)
                    Do
                        vMultiConj(iC) += 1
                        i += 2
                    Loop While i + 1 < vRoots.Length AndAlso _
                    (vRoots(i) - Complex.opConjugate(vRoots(i + 1))).IsZero AndAlso _
                    ((vConjugate(iC) - vRoots(i)).IsZero OrElse _
                    (vConjugate(iC) + vRoots(i)).IsZero)
                    iC += 1
                    i -= 1
                Else
                    ReDim Preserve vNotConjugate(iNC), vMultiNotConj(iNC)
                    vNotConjugate(iNC) = vRoots(i)
                    Do
                        vMultiNotConj(iNC) += 1
                        i += 1
                    Loop While i + 1 < vRoots.Length AndAlso _
                    (vRoots(i - 1) - vRoots(i)).IsZero
                    iNC += 1
                End If
            Next

            ' Find the polynomial numerators and their respective
            ' denominators:
            ReDim vNum(-1), vDen(-1), vDenExponent(-1)
            Dim iND As Int64 = 0
            ' a) polyn. num. & den. for real roots:
            For i = 0 To vRealRoots.Length - 1
                For j = 1 To vMultiRR(i)
                    ' name numerator as _A1 + _A2 + ... + _B1 +...
                    ' The letter stands for the current num./den. and the
                    ' number for the multiplicity
                    Dim nomVar As String = String.Format("_A{0}_{1}", i + 1, j)
                    oVar.AddVar(nomVar, Nothing)
                    ReDim Preserve vNum(iND), vDen(iND), vDenExponent(iND)
                    vNum(iND) = Polynomial.GetPolynomial(nomVar)
                    vDen(iND) = X - New Polynomial(vRealRoots(i))
                    vDenExponent(iND) = j
                    iND += 1
                Next
            Next
            ' b) polyn. num. & den. for conjugate root pairs:
            For i = 0 To vConjugate.Length - 1
                For j = 1 To vMultiConj(i)
                    ReDim Preserve vNum(iND), vDen(iND), vDenExponent(iND)
                    Dim nomVar As String = String.Format("_B{0}_{1}a", i + 1, j)
                    oVar.AddVar(nomVar, Nothing)
                    Dim nomVar_b As String = String.Format("_B{0}_{1}b", i + 1, j)
                    oVar.AddVar(nomVar_b, Nothing)
                    ' numerator will have the form: A1*X + A1b (degree=1)
                    vNum(iND) = Polynomial.GetPolynomial(nomVar) * X + _
                        Polynomial.GetPolynomial(nomVar_b)
                    Dim a As Double = vConjugate(i).pRe.ToDouble
                    Dim b As Double = vConjugate(i).pIm.ToDouble
                    ' (x-(a+b*i))*(x-(a-b*i)) = x^2 -2a*x +(a2+b2)
                    vDen(iND) = X * X - _
                            New Polynomial(2 * a) * X + _
                            New Polynomial(a * a + b * b)
                    vDenExponent(iND) = j
                    iND += 1
                Next
            Next
            ' c) polyn. num. & den. for complex roots Im<>0:
            For i = 0 To vNotConjugate.Length - 1
                For j = 1 To vMultiNotConj(i)
                    ReDim Preserve vNum(iND), vDen(iND), vDenExponent(iND)
                    Dim nomVar As String = String.Format("_C{0}_{1}", i + 1, j)
                    oVar.AddVar(nomVar, Nothing)
                    vNum(iND) = Polynomial.GetPolynomial(nomVar)
                    vDen(iND) = X - New Polynomial(vNotConjugate(i))
                    vDenExponent(iND) = j
                    iND += 1
                Next
            Next

            ' Now, if degree(Q) = q, we have to establish a system
            ' of q equations and q unknown variables.
            Dim q As Int64 = oVar.getNamesList.Length
            ' set a matrix of q rows and 1 column:
            Dim eMtx As New ExprMatrix(cfg, q, 1)

            ' First, evaluate vNum() and vDen() in the
            ' points defined by the roots. If there is any
            ' root having multiplicity > 1, then we will need
            ' to consider more points besides the roots.
            j = 0
            Dim rnd As New Random(1)
            ReDim Preserve vPt(q - 1)
            Dim nIter As Int64 = 0
            Do
                i = iPt
                Do While i < q ' not enough points ?
                    ' Avoid as possible duplicated points. Otherwise,
                    ' equations won't be independant and the 
                    ' Sys.Of Eq. resolution method will fail:
                    vPt(i) = (1.0 + vPt(j)) * rnd.NextDouble * (j + 1)
                    j += (j + 1) Mod iPt
                    i += 1
                Loop

                ' eMtx is an exprMatrix instance and each
                ' entry contains an expression (eMtx.getExpr(row,0)).
                ' Later, in soe.resolveSysOfEqs(), 
                ' each entry is equated to zero to
                ' solve the system.
                ' If P / Q = vNum(0)/vDen(0) + vNum(1)/vDen(1) + ...
                ' then: 
                ' [1]  P - vNum(0) * (Q/vDen(0)) + vNum(1) * (Q/vDen(1)) + ... = 0 
                ' eMtx's Row 0 will be the left side of [1] evaluated at point vPt(0)
                ' eMtx's Row 1 will be the left side of [1] evaluated at point vPt(1)
                ' and so on.
                ' Incorporate the equations into eMtx:
                Dim oVarX As New VarsAndFns(cfg)
                oVarX.AddVar(var(0), Nothing)
                Dim pEq As Polynomial = New Polynomial(-pA.PolyResto)
                For i = 0 To vDen.Length - 1
                    Dim div As Polynomial = pA.PolyDivisor / _
                        vDen(i) ^ New Polynomial(vDenExponent(i))
                    ' Division should be exact, trim possible
                    ' division error:
                    div.PolyResto = Nothing
                    div.PolyDivisor = Nothing

                    pEq += vNum(i) * div
                Next
                ' Evaluate each row i at point vPt(i):
                For i = 0 To q - 1
                    oVarX.setValue(0, New ExprMatrix(vPt(i)))
                    eMtx.getExpr(i, 0) = pEq.evalMultiCjoToExpr(oVarX)
                Next
                If bDetail Then
                    cfg.oDetail.Add(pA.toStringPoly(cfg) + "= ")
                    Dim e1 As String = String.Empty
                    For i = 0 To vNum.Length - 1
                        Dim div As Polynomial = vNum(i) / vDen(i)
                        If Len(e1) Then e1 += " +"
                        e1 += div.toStringPoly(cfg)
                        If vDenExponent(i) > 1 Then
                            e1 += "^" + vDenExponent(i).ToString
                        End If

                    Next
                    cfg.oDetail.Add("= " + e1)
                End If

                ' Resolve the system:
                'Dim cfg1 As New Config
                Dim soe As New SystemOfEquations(Nothing, eMtx, oVar, Nothing, cfg)
                If Not soe.resolveSysOfEqs(cfg) Then
                    sErrMsg = soe.sErrMsg
                Else
                    If bDetail Then
                        cfg.oDetail.AddAlways(" => ")
                        soe.ToStringInfo(cfg)
                        For i = 0 To soe.sSolutionVars.Length - 1
                            cfg.oDetail.Add(soe.sSolutionVars(i)(0) + " = " + _
                                            soe.sSolutionVars(i)(1))
                        Next

                    End If
                    If cfg.bRounding Then
                        For i = 0 To soe.resultValues.Length - 1
                            Dim Re As Double = soe.resultValues(i).pRe.ToDouble
                            Dim Im As Double = soe.resultValues(i).pIm.ToDouble
                            If Math.Abs(Re) < 10 ^ -6 Then
                                Re = Math.Round(Re)
                            End If
                            If Math.Abs(Im) < 10 ^ -6 Then
                                Im = Math.Round(Im)
                            End If
                            soe.resultValues(i) = New Complex(Re, Im)
                        Next
                    End If

                    ' Set, in oVar, the variables _A1... _B1... _C1...
                    ' to the values just obtained in soe.resultValues:
                    Dim vVar() As String = oVar.getNamesList
                    For i = 0 To vVar.Length - 1
                        Dim pos As Int64 = Array.IndexOf( _
                            soe.resultVars, vVar(i))
                        oVar.setValue(i, New ExprMatrix(soe.resultValues(pos)))
                    Next
                    ' Replace and evaluate vNum(). Variables _A1...
                    ' will be replaced by the values present in oVar and
                    ' evaluated:
                    For i = 0 To vNum.Length - 1
                        vNum(i) = vNum(i).evalMultiCjoToPolynomial(oVar)
                    Next
                    Exit Do
                End If
                ' Unfortunately and most probably, the set of equations
                ' are linearly dependant: try, till 5 attempts, with new points.
                nIter += 1
            Loop While nIter < 5
            If nIter < 5 Then
                bRet = True
            End If
            If bDetail Then
                Dim e1 As String = String.Empty
                For i = 0 To vNum.Length - 1
                    Dim div As Polynomial = vNum(i) / vDen(i)
                    If Len(e1) Then e1 += " +"
                    e1 += div.toStringPoly(cfg)
                    If vDenExponent(i) > 1 Then
                        e1 += "^" + vDenExponent(i).ToString
                    End If
                Next
                cfg.oDetail.Add("= " + e1)
                cfg.oDetail.ClearDivisions()
            End If

        Catch ex As Exception
            sErrMsg = msg8.num(13) ' n/a
        Finally
            cfg.bDetail = bDetail
        End Try
        Return bRet
    End Function

    Public Shared Function busquedaDicotomica(ByVal ini As Double, ByVal fin As Double, ByVal fn As Polynomial) As Complex()
        Dim a As Complex = Nothing, fa As New Complex(0.0)
        Dim fb As New Complex(0.0), b As Complex = Nothing
        Dim fM As New Complex(0.0), M As Complex = Nothing
        Dim nVueltas As Int64 = 0
        If ini > fin Then
            a = New Complex(fin, 0) : b = New Complex(ini, 0)
        Else
            a = New Complex(ini, 0) : b = New Complex(fin, 0)
        End If
        Try
            'If Not fn.tryEvalReDecimal(a.pRe.aDouble, fa.pRe.aDouble) Then
            fa.pRe = fn.evalPrecis(a.pRe)
            'End If
            'If Not fn.tryEvalReDecimal(b.pRe.aDecimal, fb.pRe.aDecimal) Then
            fb.pRe = fn.evalPrecis(b.pRe)
            'End If
        Catch ex As Exception
            Return Nothing
        End Try
        Dim sA As Int64 = fa.pRe.sgn
        Dim sB As Int64 = fb.pRe.sgn
        Dim sM As Int64
        If sA = 0 Then
            Dim retA() As Complex = {a, fa}
            Return retA
        ElseIf sB = 0 Then
            Dim retB() As Complex = {b, fb}
            Return retB
        End If
        If ini = fin Then Return Nothing
        'If Math.Sign(Math.Round(fa.Re, 10)) = Math.Sign(Math.Round(fb.Re, 10)) Then Return Nothing
        If sA = sB Then Return Nothing
        Dim iQuitDecimal As Int64 = 0
        Do
            M = (a + b) / 2
            'If iQuitDecimal > 40 Then
            fM.pRe = fn.evalPrecis(M.pRe)
            'ElseIf Not fn.tryEvalReDecimal(M.pRe.aDecimal, fM.pRe.aDecimal) Then
            '   fM.pRe = fn.evalPrecis(M.pRe)
            '  iQuitDecimal += 1
            'End If
            sM = fM.pRe.sgn
            If sA = sM Then
                If (a - M).esCero Then 'a.pRe.aDouble = M.pRe.aDouble Then
                    Exit Do
                End If
                a = M : fa = fM
            ElseIf sB = sM Then
                If (b - M).esCero Then ' b.pRe.aDouble = M.pRe.aDouble Then
                    Exit Do
                End If
                b = M : fb = fM
            Else
                Exit Do
            End If
            nVueltas += 1
        Loop While nVueltas < 2000

        'If fM.opModulo > 10 ^ -3 Then
        '    If Not Math.Abs((M.opModulo - fM.opModulo) / fM.opModulo) > 10 ^ 10 Then
        '        Return Nothing
        '    End If
        'End If
        If fb.opModulo < fM.opModulo Then
            M = b : fM = fb
        ElseIf fa.opModulo < fM.opModulo Then
            M = a : fM = fa
        End If
salida:
        M = New Complex(M.pRe.ToDecimal)
        Dim ret() As Complex = {M, fM}
        Return ret
    End Function
    Public Shared Function busquedaDicotomicaDbl(ByVal ini As Double, ByVal fin As Double, ByVal fn As Polynomial) As Complex()
        Dim a, b, fa, fb, M, fM As Double
        Dim nVueltas As Int64 = 0
        Try
            If ini > fin Then
                a = fin : b = ini
            Else
                a = ini : b = fin
            End If
            Try
                'fa = fn.evalRe(a)
                'fb = fn.evalRe(b)
                fa = fn.evalPrecis(New Rational(a)).ToDouble
                fb = fn.evalPrecis(New Rational(b)).ToDouble
            Catch ex As Exception
                Return Nothing
            End Try
            Dim sA As Int64 = Math.Sign(fa)
            Dim sB As Int64 = Math.Sign(fb)
            Dim sM As Int64
            If sA = 0 Then
                Dim retA() As Complex = { _
                    New Complex(a), New Complex(fa)}
                Return retA
            ElseIf sB = 0 Then
                Dim retB() As Complex = { _
                    New Complex(b), New Complex(fb)}
                Return retB
            End If
            If ini = fin Then Return Nothing
            If sA = sB Then Return Nothing
            'Dim iQuitDecimal As Int64 = 0
            Do
                M = (a + b) / 2
                'fM = fn.evalRe(M)
                fM = fn.evalPrecis(New Rational(M)).ToDouble
                sM = Math.Sign(fM)
                If sA = sM Then
                    If a = M Then Exit Do
                    a = M : fa = fM
                ElseIf sB = sM Then
                    If b = M Then Exit Do
                    b = M : fb = fM
                Else
                    Exit Do
                End If
                'nVueltas += 1
            Loop While a < b

            If Math.Abs(fb) < Math.Abs(fM) Then
                M = b : fM = fb
            ElseIf Math.Abs(fa) < Math.Abs(fM) Then
                M = a : fM = fa
            End If
        Catch ex As Exception
            Return Nothing
        End Try
        Dim ret() As Complex = { _
            New Complex(M), New Complex(fM)}
        Return ret
    End Function
    Public Shared Function busquedaDicotomicaDbl_Expression(ByVal ini As Double, ByVal fin As Double, ByVal fn As Expression) As Complex()
        Dim a, b, fa, fb, M, fM As Double
        Dim nVueltas As Int64 = 0
        Try
            If ini > fin Then
                a = fin : b = ini
            Else
                a = ini : b = fin
            End If
            Try
                'fa = fn.evalRe(a)
                'fb = fn.evalRe(b)
                fa = fn.evalExpression(New Complex(a)).pRe.ToDouble
                fb = fn.evalExpression(New Complex(b)).pRe.ToDouble
            Catch ex As Exception
                Return Nothing
            End Try
            Dim sA As Int64 = Math.Sign(fa)
            Dim sB As Int64 = Math.Sign(fb)
            Dim sM As Int64
            If sA = 0 Then
                Dim retA() As Complex = {
                    New Complex(a), New Complex(fa)}
                Return retA
            ElseIf sB = 0 Then
                Dim retB() As Complex = {
                    New Complex(b), New Complex(fb)}
                Return retB
            End If
            If ini = fin Then Return Nothing
            If sA = sB Then Return Nothing
            'Dim iQuitDecimal As Int64 = 0
            Do
                M = (a + b) / 2
                'fM = fn.evalRe(M)
                fM = fn.evalExpression(New Complex(M)).pRe.ToDouble
                sM = Math.Sign(fM)
                If sA = sM Then
                    If a = M Then Exit Do
                    a = M : fa = fM
                ElseIf sB = sM Then
                    If b = M Then Exit Do
                    b = M : fb = fM
                Else
                    Exit Do
                End If
                'nVueltas += 1
            Loop While a < b

            If Math.Abs(fb) < Math.Abs(fM) Then
                M = b : fM = fb
            ElseIf Math.Abs(fa) < Math.Abs(fM) Then
                M = a : fM = fa
            End If
        Catch ex As Exception
            Return Nothing
        End Try
        Dim ret() As Complex = {
            New Complex(M), New Complex(fM)}
        Return ret
    End Function

    Public Shared Function busquedaDicotomicaPr(ByVal ini As Double, ByVal fin As Double, ByVal fn As Polynomial) As Complex()
        Dim a, b, fa, fb, M, fM As Rational
        Dim nVueltas As Int64 = 0
        Dim mult As Double = 10 ^ 15
        ini = Math.Round(ini * 10 ^ 10) / 10 ^ 10
        fin = Math.Round(fin * 10 ^ 10) / 10 ^ 10
        If ini > fin Then
            a = New Rational(fin)
            b = New Rational(ini, mult)
        Else
            a = New Rational(ini)
            b = New Rational(fin)
        End If
        Try
            fa = fn.evalPrecis(a)
            fb = fn.evalPrecis(b)
        Catch ex As Exception
            Return Nothing
        End Try
        Dim sA As Int64 = fa.sgn
        Dim sB As Int64 = fb.sgn
        Dim sM As Int64
        If sA = 0 Then
            Dim retA(1) As Complex
            retA(0) = New Complex(a.ToDouble)
            retA(1) = New Complex(fa.ToDouble)
            Return retA
        ElseIf sB = 0 Then
            Dim retB(1) As Complex
            retB(0) = New Complex(b.ToDouble)
            retB(1) = New Complex(fb.ToDouble)
            Return retB
        End If
        If ini = fin Then Return Nothing
        If sA = sB Then Return Nothing
        Dim iQuitDecimal As Int64 = 0
        Do
            M = (a + b) / 2
            fM = fn.evalPrecis(M)
            sM = fM.sgn
            If fM.IsZero Then
                Exit Do
            End If
            If sA = sM Then
                'If (a - M).IsZero Then
                '    Exit Do
                'End If
                a = M : fa = fM
            ElseIf sB = sM Then
                'If (b - M).IsZero Then
                '    Exit Do
                'End If
                b = M : fb = fM
            Else
                Exit Do
            End If
            nVueltas += 1
        Loop While nVueltas < 2000

        If Math.Abs(fb.ToDouble) < Math.Abs(fM.ToDouble) Then
            M = b : fM = fb
        ElseIf Math.Abs(fa.ToDouble) < Math.Abs(fM.ToDouble) Then
            M = a : fM = fa
        End If
salida:
        Dim ret() As Complex = { _
            New Complex(0), New Complex(0)}
        ret(0).pRe = New Rational(M)
        ret(1).pRe = New Rational(fM)
        Return ret
    End Function
    Public Shared Function busquedaMinimoEnIntervalo(ByVal ini As Double, _
                                                     ByVal fin As Double, _
                                                     ByVal fn As Polynomial, _
                                                     ByVal numIntervalos As Int64) As Complex()
        Dim fMin As Double = Double.MaxValue
        Dim xMin As Double
        Try
            If ini < fin Then
                Dim aux As Double = ini
                ini = fin : fin = aux
            End If
            xMin = ini : fMin = Math.Abs(fn.evalRe(ini))
            If numIntervalos < 2 Then
                Exit Try
            End If
            For x As Double = ini To fin Step (fin - ini) / numIntervalos
                Dim fCur As Double = Math.Abs(fn.evalRe(x))
                If fCur < fMin Then
                    xMin = x
                    fCur = fMin
                End If
            Next
        Catch ex As Exception
            Throw ex
        End Try
        Return New Complex() { _
            New Complex(xMin), _
            New Complex(fMin) _
            }
    End Function
    'Public Shared Function Inverse_Matrix_GaussPivotTotal_poly_BAD(
    '                        ByVal Ma As Matrix, ByRef sErrMsg As String,
    '                        Optional bOnlyTriangularize As Boolean = False) As Matrix
    '    Dim i1, i, j As Int64 ', M, N As Int64

    '    Dim dsp As Boolean = False
    '    Dim iIni As Int64 = 0
    '    Dim zero As New Complex(0, 0)
    '    Dim uno As New Complex(1, 0)
    '    Dim polyZero As New Polynomial(zero)
    '    Dim polyUno As New Polynomial(uno)
    '    Dim Mc As New Matrix(Ma)
    '    Dim retGC As New retGaussElim_Mtx
    '    Dim nRows As Int64 = Ma.vVect.Length
    '    Dim nCols As Int64 = Ma.vVect(0).vPoly.Length
    '    Dim B As New Matrix
    '    Dim nomVars(nRows - 1) As Int64
    '    Try
    '        'Array.Copy(nomVar, nomVars, nomVar.Length)
    '        Dim rows(nRows - 1), cols(nCols - 1) As Int64
    '        ReDim B.vVect(nRows - 1)
    '        For i = 0 To rows.Length - 1
    '            B.vVect(i) = New Vector
    '            If False Then ' rows.Length > 1 Then
    '                Mc.vVect(i) = Vector.opConjugate(Ma.vVect(i))
    '            Else
    '                Mc.vVect(i) = New Vector(Ma.vVect(i))
    '            End If
    '            ReDim B.vVect(i).vPoly(nCols - 1)
    '            For j = 0 To nCols - 1
    '                B.vVect(i).vPoly(j) = polyZero
    '                If j = i Then
    '                    B.vVect(i).vPoly(j) = polyUno
    '                End If
    '            Next
    '            rows(i) = i : cols(i) = i
    '        Next

    '        ' Conseguir ceros debajo de la diagonal:
    '        For iCol As Int64 = 0 To nCols - 2
    '            i1 = iCol
    '            Dim M, N As Int64
    '            M = i1 : N = i1
    '            Dim norm As Double = Mc.vVect(M).vPoly(cols(N)).cf(0).opNorm
    '            For i = i1 + 1 To nRows - 1
    '                For j = i1 To i1 ' B.Length - 1
    '                    If norm < Mc.vVect(rows(i)).vPoly(cols(j)).cf(0).opNorm Then
    '                        M = i : N = j
    '                    End If
    '                Next
    '            Next
    '            If (M <> i1 OrElse N <> i1) Then
    '                Dim ijt As Int64 = rows(M)
    '                rows(M) = rows(i1)
    '                rows(i1) = ijt

    '                ijt = cols(N)
    '                cols(N) = cols(i1)
    '                cols(i1) = ijt
    '            End If
    '            Dim polyCol As New Polynomial(Mc.vVect(rows(iCol)).vPoly(cols(iCol)))
    '            For i = iCol + 1 To nCols - 1
    '                Dim polyI As New Polynomial(Mc.vVect(rows(i)).vPoly(cols(iCol)))
    '                'Trace.WriteLine(polyI.toStringPoly(New Config))
    '                If Not (polyI.isComplex AndAlso polyI.cf(0).esCero) Then
    '                    Dim gcd As Polynomial = Nothing
    '                    If polyCol.getDegree > 1 AndAlso polyI.getDegree > 1 Then
    '                        gcd = Polynomial.opGcd(polyCol, polyI)
    '                        If gcd IsNot Nothing AndAlso
    '                        gcd.getDegree > 0 Then
    '                        Else
    '                            gcd = Nothing
    '                        End If
    '                    End If
    '                    Dim vICol As Vector = Mc.vVect(rows(iCol)) * polyI
    '                    'Trace.WriteLine(vICol.toStrVect(New Config))
    '                    Dim vI As Vector = polyCol * Mc.vVect(rows(i))
    '                    'Trace.WriteLine(vI.toStrVect(New Config))
    '                    Mc.vVect(rows(i)) = vI - vICol
    '                    'Trace.WriteLine(Replace(Mc.toStringMtx(New Config), vbCrLf, "|"))
    '                    vICol = B.vVect(rows(iCol)) * polyI
    '                    'Trace.WriteLine(vICol.toStrVect(New Config))
    '                    vI = polyCol * B.vVect(rows(i))
    '                    'Trace.WriteLine(vI.toStrVect(New Config))
    '                    B.vVect(rows(i)) = vI - vICol
    '                    If gcd IsNot Nothing Then
    '                        For k1 = rows(i) To rows(i)
    '                            For k2 As Int64 = 0 To nCols - 1
    '                                B.vVect(rows(i)).vPoly(k2) /= gcd
    '                                Mc.vVect(rows(i)).vPoly(k2) /= gcd
    '                            Next
    '                        Next
    '                    End If
    '                End If
    '            Next
    '        Next
    '        If bOnlyTriangularize Then
    '            ' If last param = True, return
    '            ' value will be an equivalent lower  
    '            ' triangular matrix:
    '            Return Mc
    '        End If
    '        ' Conseguir ceros encima de la diagonal:

    '        For iCol = nCols - 1 To 1 Step -1
    '            Dim polyCol As New Polynomial(Mc.vVect(rows(iCol)).vPoly(cols(iCol)))
    '            For i = iCol - 1 To 0 Step -1
    '                Dim polyI As New Polynomial(Mc.vVect(rows(i)).vPoly(cols(iCol)))
    '                If Not (polyI.isComplex AndAlso polyI.cf(0).esCero) Then
    '                    Dim gcd As Polynomial = Nothing
    '                    If polyCol.getDegree > 1 AndAlso polyI.getDegree > 1 Then
    '                        gcd = Polynomial.opGcd(polyCol, polyI)
    '                        If gcd IsNot Nothing AndAlso
    '                        gcd.getDegree > 0 Then
    '                        Else
    '                            gcd = Nothing
    '                        End If
    '                    End If
    '                    Dim vICol As Vector = Mc.vVect(rows(iCol)) * polyI
    '                    Dim vI As Vector = polyCol * Mc.vVect(rows(i))
    '                    Mc.vVect(rows(i)) = vI - vICol
    '                    vICol = B.vVect(rows(iCol)) * polyI
    '                    vI = polyCol * B.vVect(rows(i))
    '                    B.vVect(rows(i)) = vI - vICol
    '                    If gcd IsNot Nothing Then
    '                        For k1 = rows(i) To rows(i)
    '                            For k2 As Int64 = 0 To nCols - 1
    '                                B.vVect(rows(i)).vPoly(k2) /= gcd
    '                                Mc.vVect(rows(i)).vPoly(k2) /= gcd
    '                            Next
    '                        Next
    '                    End If
    '                End If
    '            Next
    '        Next
    '        ' Conseguir unos en la diagonal:
    '        For i = 0 To nRows - 1
    '            Dim div As Polynomial = Mc.vVect(rows(i)).vPoly(cols(i))
    '            If div.isReal AndAlso
    '            div.ToDouble = 0 Then
    '                sErrMsg = msg8.num(51)
    '                Return Nothing
    '            End If
    '            'Dim polyDiag As Polynomial = polyUno / _
    '            'Mc.vVect(rows(i)).vPoly(cols(i))
    '            Dim polyDiag As Polynomial = polyUno / div
    '            B.vVect(rows(i)) *= polyDiag
    '            'Mc.vVect(i) *= polyDiag
    '        Next

    '        retGC.mtx = New Matrix(B)
    '        For i = 0 To nRows - 1
    '            'B.vVect(rows(i)) = Vector.opConjugate(B.vVect(rows(i)))
    '            For j = 0 To nCols - 1
    '                retGC.mtx.vVect(i).vPoly(j) =
    '                    New Polynomial(B.vVect(rows(i)).vPoly(cols(j)))
    '            Next
    '        Next
    '    Catch ex As Exception
    '        Throw New Exception("Inverse matrix: n/a")
    '    End Try
    '    Return retGC.mtx
    'End Function




    Public Shared Function checkParentheses(ByVal e0 As String, ByVal bThrowErr As Boolean, ByRef sErr As String) As Boolean
        Dim depth As Int64 = 0
        Dim ret As Boolean = False
        Dim i As Int64
        Static re As New Regex("\(\s*\)")
        Try
            Dim e1 As New StringBuilder(e0.Length)
            Dim e10 As String = Regex.Replace(e0, "\[|\{", "(")
            e10 = Regex.Replace(e10, "\]|\}", ")")
            Dim patron As String = "[^\(\)]"
            e10 = Regex.Replace(e10, patron, " ")
            e1.Append(e10)

            Do
                Dim m As MatchCollection = re.Matches(e1.ToString)
                If m.Count = 0 Then Exit Do
                For i = 0 To m.Count - 1
                    e1.Chars(m.Item(i).Index) = " " ' left parenth. "(" -->" "
                    e1.Chars(m.Item(i).Index + m.Item(i).Length - 1) = " " ' ")" --> " "
                Next
            Loop
            Dim e5 As String = e1.ToString()
            Dim pos As Int64 = InStr(e5, "(")
            Dim pos2 As Int64 = 0
            pos2 = -InStr(e5, ")")
            If pos = 0 OrElse (-pos2 > 0 AndAlso -pos2 < pos) Then
                pos = Math.Abs(pos2)
            End If
            If pos Then
                sErr = ""
                If pos > 1 Then
                    sErr = Mid(e0, 1, pos - 1)
                End If
                sErr = Mid(e0, pos, 1)
                If pos < e0.Length Then
                    sErr += Mid(e0, pos + 1)
                End If
                If pos = -pos2 Then pos = pos2
                If pos > 0 Then
                    sErr = msg8.msg(1023)
                    If bThrowErr Then
                        Throw New Exception(msg8.msg(1023))
                    End If
                ElseIf pos < 0 Then
                    sErr = msg8.msg(1024)
                    If bThrowErr Then
                        Throw New Exception(msg8.msg(1024))
                    End If
                End If
            Else
                ret = True ' Parentheses Ok
            End If
        Catch ex As Exception
            Throw New Exception(ex.Message)
        End Try
        Return ret
    End Function
    Public Shared Function gcd(ByVal vectA As Vector) As Int64
        Try
            Dim Re1 As Double = vectA.vPoly(0).cf(0).pRe.ToDouble
            Dim Re2 As Double = vectA.vPoly(1).cf(0).pRe.ToDouble
            If Re1 = 0 Then
                Return Re2
            ElseIf Re2 = 0 Then
                Return Re1
            End If
            Re1 = Math.Abs(Re1)
            Re2 = Math.Abs(Re2)
            Dim larger As Int64 = Math.Max(Re1, Re2)
            Dim smaller As Int64 = Math.Min(Re1, Re2)
            Do
                Dim remainder As Int64 = larger Mod smaller
                If remainder = 0 Then
                    Return smaller
                End If
                smaller = remainder
            Loop
        Catch ex As Exception

        End Try
        Return 0
    End Function


    Public Shared Function ImagRoots_Newton_Raphson(ByVal pa As Polynomial, ByVal cfg As Config) As Complex()
        Dim root(-1) As Complex, ir As Int64 = 0
        Try
            Dim ts As New TimeSpan(Now.Ticks)
            Dim degree = pa.getDegree
            If pa.hasComplexCoeff Then
                Exit Try
            ElseIf degree < 3 Then
                If degree = 0 Then
                    Exit Try
                ElseIf degree = 2 Then
                    Dim a1, b1, c1 As Complex
                    a1 = pa.cf(0)
                    If pa.cf.Length = 2 Then
                        c1 = pa.cf(1)
                        ReDim root(1)
                        'root(0) = -(-c1 / a1) ^ 0.5 ' -(-a * c) ^ 0.5 / a
                        'root(1) = (-c1 / a1) ^ 0.5 ' (-a * c) ^ 0.5 / a
                        root(0) = -(-c1 / a1) ^ Complex.oneHalf  ' 2013/08/09
                        root(1) = (-c1 / a1) ^ Complex.oneHalf  '  2013/08/09
                        Exit Try
                    Else
                        b1 = pa.cf(1)
                        c1 = pa.cf(2)
                        ReDim root(1)
                        'root(0) = (-b1 + (b1 * b1 - 4 * a1 * c1) ^ 0.5) / (2.0 * a1)
                        'root(1) = (-b1 - (b1 * b1 - 4 * a1 * c1) ^ 0.5) / (2.0 * a1)
                        root(0) = (-b1 + (b1 * b1 - 4 * a1 * c1) ^ Complex.oneHalf) / (2.0 * a1) '  2013/08/09
                        root(1) = (-b1 - (b1 * b1 - 4 * a1 * c1) ^ Complex.oneHalf) / (2.0 * a1) '  2013/08/09
                        Exit Try
                    End If
                End If
            End If
            If degree Mod 2 Then
                Exit Try
            End If
            Dim maxVueltas As Int64 = 200
            Do
                Dim Der As Polynomial = pa.opDerivative(pa.var(0))
                Dim x0 As New Complex(0.9, -0.5)
                If Der.evalCjo(x0).IsZero Then
                    x0 = New Complex(0.5, -0.9)
                End If
                Dim nVueltas As Int64 = 0
                Dim dif As Complex = Nothing
                Dim errMax As Double = pa.cf(0).opNorm / 10 ^ (pa.getDegree ^ 2)
                Dim x1 As Complex
                Do
                    Dim den As Complex = Der.evalCjo(x0)
                    If Not den.IsZero Then
                        x1 = x0 - pa.evalCjo(x0) / Der.evalCjo(x0)
                        dif = x1 - x0
                        x0 = x1
                    Else
                        GoTo err
                    End If
                    nVueltas += 1
                    Dim ts2 As New TimeSpan(Now.Ticks - ts.Ticks)
                    'If ts2.TotalMilliseconds > 2 * cfg.timeOutms / 3 Then
                    If cfg IsNot Nothing AndAlso cfg.isTimeout Then
                        nVueltas = maxVueltas
                    End If
                Loop While nVueltas < maxVueltas AndAlso dif.opNorm > 0
                ReDim Preserve root(ir + 1)
                root(ir) = New Complex(x0)
                x0.pIm.opChgSgn()
                root(ir + 1) = New Complex(x0)
                ir += 2
                degree -= 2
                If True OrElse degree Then
                    'Trace.WriteLine(pa.toStringPoly)
                    Dim c() As Double = New Double() { _
                        1.0, -(2 * x0.pRe).ToDecimal, _
                        (x0.pRe * x0.pRe + _
                        x0.pIm * x0.pIm).ToDecimal}
                    Dim polyC As Polynomial = _
                        Polynomial.GetPolyomial(pa.var(0), c)
                    pa /= polyC
                    pa.PolyResto = Nothing
                    pa.PolyDivisor = Nothing
                    'Dim pa2 As Polynomial = pa * polyC
                    'Trace.WriteLine(pa2.toStringPoly)
                End If
                Dim ts3 As New TimeSpan(Now.Ticks - ts.Ticks)
                'If ts3.TotalMilliseconds >= 2.5 * CDbl(cfg.timeOutms) / 3.0 Then
                If cfg IsNot Nothing AndAlso cfg.isTimeout Then
                    Exit Do
                End If
            Loop While degree > 0
        Catch ex As Exception
            GoTo err
        End Try
        Return root

err:    ' something went wrong:

        ReDim root(-1)
        Return root
    End Function


    Public Shared Function ImagRoots_Newton_RaphsonBAD(ByVal pa As Polynomial, ByVal cfg As Config) As Complex()
        Dim root(-1) As Complex, ir As Int64 = 0
        Try
            Dim ts As New TimeSpan(Now.Ticks)
            Dim degree = pa.getDegree
            If pa.hasComplexCoeff Then
                Exit Try
            ElseIf degree < 3 Then
                If degree = 0 Then
                    Exit Try
                ElseIf degree = 2 Then
                    Dim a1, b1, c1 As Complex
                    a1 = pa.cf(0)
                    If pa.cf.Length = 2 Then
                        c1 = pa.cf(1)
                        ReDim root(1)
                        'root(0) = -(-c1 / a1) ^ 0.5 ' -(-a * c) ^ 0.5 / a
                        'root(1) = (-c1 / a1) ^ 0.5 ' (-a * c) ^ 0.5 / a
                        root(0) = -(-c1 / a1) ^ Complex.oneHalf  ' 2013/08/09
                        root(1) = (-c1 / a1) ^ Complex.oneHalf  '  2013/08/09
                        Exit Try
                    Else
                        b1 = pa.cf(1)
                        c1 = pa.cf(2)
                        ReDim root(1)
                        'root(0) = (-b1 + (b1 * b1 - 4 * a1 * c1) ^ 0.5) / (2.0 * a1)
                        'root(1) = (-b1 - (b1 * b1 - 4 * a1 * c1) ^ 0.5) / (2.0 * a1)
                        root(0) = (-b1 + (b1 * b1 - 4 * a1 * c1) ^ Complex.oneHalf) / (2.0 * a1) '  2013/08/09
                        root(1) = (-b1 - (b1 * b1 - 4 * a1 * c1) ^ Complex.oneHalf) / (2.0 * a1) '  2013/08/09
                        Exit Try
                    End If
                End If
            End If
            If degree Mod 2 Then
                Exit Try
            End If
            Dim maxVueltas As Int64 = 1000
            Dim X As Polynomial = Polynomial.GetPolynomial(pa.var(0))
            Dim D As Polynomial = X * X + X + New Polynomial(1.0) ' eP.ret.curExpr.getPolynomial
            If cfg.isTimeoutFraction(0.7) Then
                Exit Try
            End If
            Dim P(degree) As Complex
            Dim k As Int64 = 0
            For i = 0 To degree
                Dim exp As Int64 = 0
                If k < pa.exp.Length Then
                    exp = pa.exp(k)(0)
                    Do While exp < degree - i AndAlso _
                    i < degree ' Pa.cff(degree)<>0 
                        P(i) = Complex.zero
                        i += 1
                    Loop
                End If
                P(i) = New Complex(pa.cf(k))
                k += 1
            Next
            Dim rnd As New Random
            Do
                'If cfg.isTimeoutFraction(0.7) Then
                '    Exit Try
                'End If
                Dim Der As Polynomial = pa.opDerivative(pa.var(0))
                Dim x0 As New Complex(0.9, -0.5)
                If Der.evalCjo(x0).IsZero Then
                    x0 = New Complex(0.5, -0.9)
                End If
                Dim nVueltas As Int64 = 0
                Dim dif As New Complex(Double.MaxValue)
                Dim errMax As Double = pa.cf(0).opNorm / 10 ^ (pa.getDegree ^ 2)
                Dim x1 As Complex
                Do
                    Dim den As Complex = Der.evalCjo(x0)
                    If Not den.IsZero Then
                        x1 = x0 - pa.evalCjo(x0) / Der.evalCjo(x0)
                        dif = x1 - x0
                        x0 = x1
                    ElseIf nVueltas < 10 Then
                        x0 = New Complex(rnd.NextDouble, -rnd.NextDouble)
                    Else
                        GoTo err
                    End If
                    nVueltas += 1
                    Dim ts2 As New TimeSpan(Now.Ticks - ts.Ticks)
                    'If ts2.TotalMilliseconds > 2 * cfg.timeOutms / 3 Then
                    'If cfg IsNot Nothing AndAlso cfg.isTimeout Then
                    '    nVueltas = maxVueltas
                    'End If
                Loop While nVueltas < maxVueltas AndAlso dif.opNorm > 0
                ReDim Preserve root(ir + 1)
                root(ir) = New Complex(x0)
                x0.pIm.opChgSgn()
                root(ir + 1) = New Complex(x0)
                ir += 2
                degree -= 2
                If True OrElse degree Then
                    'Trace.WriteLine(pa.toStringPoly)
                    'Dim c() As Double = New Double() { _
                    '    1.0, -(2 * x0.pRe).ToDecimal, _
                    '    (x0.pRe * x0.pRe + _
                    '    x0.pIm * x0.pIm).ToDecimal}
                    D.cf(1).pRe = -2.0 * x0.pRe
                    D.cf(2) = New Complex(x0.opNorm)
                    'Dim polyC As Polynomial = _
                    '    Polynomial.GetPolyomial(pa.var(0), c)
                    'pa /= polyC
                    'pa /= D
                    Dim Q(degree) As Complex
                    Dim D1(2) As Complex
                    ReDim pa.exp(degree)
                    For i As Int64 = 0 To degree
                        Q(i) = P(i)
                        For j = 0 To 2
                            D1(j) = D.cf(j) * Q(i)
                            P(i + j) -= D1(j)
                        Next
                        pa.exp(i) = New Int64() {degree - i}
                    Next
                    Array.Copy(Q, P, degree)
                    pa.cf = Q
                    pa.PolyResto = Nothing
                    pa.PolyDivisor = Nothing
                    'Dim pa2 As Polynomial = pa * polyC
                    'Trace.WriteLine(pa2.toStringPoly)
                End If
                'Dim ts3 As New TimeSpan(Now.Ticks - ts.Ticks)
                ''If ts3.TotalMilliseconds >= 2.5 * CDbl(cfg.timeOutms) / 3.0 Then
                'If cfg IsNot Nothing AndAlso cfg.isTimeout Then
                '    Exit Do
                'End If
            Loop While degree > 0
        Catch ex As Exception
            GoTo err
        End Try
        Return root

err:    ' something went wrong:

        ReDim root(-1)
        Return root
    End Function


    Public Shared Function busquedaDicotomicaComplejo(ByVal ini As Complex, ByVal fin As Complex, ByVal fn As Polynomial) As Complex()
        Dim a As Complex = Nothing, fa As New Complex(0.0)
        Dim fb As New Complex(0.0), b As Complex = Nothing
        Dim fM As New Complex(0.0), M As Complex = Nothing
        Dim nVueltas As Int64 = 0
        a = New Complex(ini) : b = New Complex(fin)
        Try
            fa = fn.evalCjo(a)
            fb = fn.evalCjo(b)
        Catch ex As Exception
            Return Nothing
        End Try
        Dim sA As Double = fa.opModulo
        Dim sB As Double = fb.opModulo
        Dim sM As Double
        If sA = 0 Then
            Dim retA() As Complex = {a, fa}
            Return retA
        ElseIf sB = 0 Then
            Dim retB() As Complex = {b, fb}
            Return retB
        End If
        Dim iQuitDecimal As Int64 = 0
        Dim sMin As Double = Double.MaxValue
        Dim ptMin As Complex = Nothing
        Dim p As Rational = Nothing
        Dim one As New Rational(1.0)
        Dim twoK As Double = 200
        Do
            p = New Rational(nVueltas / twoK)
            M = New Complex(New Rational((a.pRe + b.pRe) / New Rational(2.0)), New Rational(p * a.pIm + (one - p) * b.pIm))
            fM = fn.evalCjo(M)
            sM = fM.opModulo
            If sM < sMin Then
                sMin = sM : ptMin = New Complex(M)
                Dim nVueltas1 As Double = 0
                Dim twoH As Double = 2000
                Do
                    p = New Rational(nVueltas1 / twoH)
                    M = New Complex(New Rational(p * a.pRe + (one - p) * b.pRe), New Rational(ptMin.pIm))
                    fM = fn.evalCjo(M)
                    sM = fM.opModulo
                    If sM < sMin Then
                        sMin = sM : ptMin = New Complex(M)
                    End If
                    'If sM = 0 OrElse (a - b).esCero Then Exit Do
                    'If sM < sA Then
                    '    a = M : sA = sM
                    'Else
                    '    b = M : sB = sM
                    'End If
                    nVueltas1 += 1
                Loop While nVueltas1 < twoH
            End If
            'If sM = 0 OrElse (a - b).esCero Then Exit Do
            'If sM < sA Then
            '    a = M : sA = sM
            'Else
            '    b = M : sB = sM
            'End If
            nVueltas += 1
        Loop While nVueltas < twoK

        If sMin <> 0 Then
            Return Nothing
        End If

        Dim ret() As Complex = {ptMin, Complex.zero}
        Return ret
    End Function
    Shared Function HeapsPermByInterchange(n As Int64, _
                    ByRef signature() As Int64, _
                    Optional bSave As Boolean = False, _
                    Optional ByRef filePath As String = "") As Int64()()

        ' See http://en.wikipedia.org/wiki/Heap's_algorithm
        ' and also http://comjnl.oxfordjournals.org/content/6/3/293.full.pdf

        Static vTable()() As Int64 = { _
            New Int64() {1, 2, 3}, _
            New Int64() {3, 1, 3, 1}, _
            New Int64() {3, 4, 3, 2, 3}, _
            New Int64() {5, 3, 1, 5, 3, 1}, _
            New Int64() {5, 2, 7, 2, 1, 2, 3}, _
            New Int64() {7, 1, 5, 5, 3, 3, 7, 1}, _
            New Int64() {7, 8, 1, 6, 5, 4, 9, 2, 3}, _
            New Int64() {9, 7, 5, 3, 1, 9, 7, 5, 3, 1}, _
            New Int64() {9, 6, 3, 10, 9, 4, 3, 8, 9, 2, 3}}
        Dim ret()() As Int64 = Nothing
        Dim fs As FileStream = Nothing
        Dim sw As StreamWriter = Nothing
        Try
            Dim vIndex(n) As Int64
            Dim c As Int64 = 0
            Dim aux, iAux, viAux(n) As Int64
            Dim i As Int64
            Dim fact As Int64 = n
            For i = n - 1 To 2 Step -1
                fact *= i
            Next
            Dim nDigits As Int64 = Math.Floor(Math.Log10(fact)) + 1
            ReDim ret(fact - 1), signature(fact - 1)
            Dim cI(n - 1) As Int64
            For i = 0 To fact - 1
                If i < n Then cI(i) = i
                ReDim ret(i)(n - 1)
            Next
            If bSave Then
                filePath = Microsoft.VisualBasic.CurDir _
                    + "\perm" + n.ToString + ".txt"
                fs = New FileStream(filePath, FileMode.Create)
                sw = New StreamWriter(fs)
            End If
            Dim iOld As Int64
            Do
                signature(c) = 1
                For i = cI.Length - 1 To 0 Step -1
                    For j As Int64 = i - 1 To 0 Step -1
                        If cI(i) < cI(j) Then
                            signature(c) *= -1
                        End If
                    Next
                    If bSave Then
                        sw.Write(signature(i) + " ")
                        sw.Write(cI(i).ToString + " ")
                    End If
                    ret(c)(i) = cI(i)
                Next
                If bSave Then
                    sw.WriteLine("")
                End If
                c += 1
                i = 1
                Do
                    vIndex(i) = (vIndex(i) + 1) Mod (i + 1)
                    If vIndex(i) Then Exit Do
                    i += 1
                    If i >= n Then Exit Try
                Loop
                iOld = i
                If i > 2 Then
                    iAux = viAux(i)
                    iAux = vTable(i - 3)(iAux) - 1
                    aux = cI(iAux)
                    cI(iAux) = cI(i) : cI(i) = aux
                    viAux(i) = (viAux(i) + 1) Mod i
                Else
                    aux = cI(0)
                    cI(0) = cI(i) : cI(i) = aux
                End If
            Loop
        Catch ex As Exception
            Throw ex
        Finally
            If sw IsNot Nothing Then
                sw.Close()
            End If
            If fs IsNot Nothing Then
                fs.Close()
            End If
        End Try
        Return ret
    End Function
    Public Shared Function Inverse_Matrix_GaussPartial_poly( _
                    ByVal Ma As Matrix, ByRef sErrMsg As String, _
                    Optional bOnlyTriangularize As Boolean = False, _
                    Optional ByRef oVars As VarsAndFns = Nothing) As Matrix
        Dim Mc As Matrix = Nothing
        Try
            Mc = New Matrix(Ma)
            Dim i, j, k As Int64
            Dim nRows As Int64 = Mc.vVect.Length
            Dim nCols As Int64 = Mc.vVect(0).vPoly.Length - 1
            Dim vCols(nCols - 1) As Int64
            Dim nRowsDone As Int64 = 0
            Dim max As Double = 0.0
            Dim imax As Int64 = -1
            Dim jmax As Int64 = -1
            For j = 0 To vCols.Length - 1
                vCols(j) = j
            Next
            For k = 0 To nCols - 1
                max = 0.0
                For i = nRowsDone To nRows - 1
                    For j = nRowsDone To nCols - 1
                        If Mc.vVect(i).vPoly(j).isComplex Then
                            Dim db As Double = Mc.vVect(i).vPoly(j).cf(0).opNorm
                            If db > max OrElse _
                            (db = max AndAlso j = k) Then
                                max = db
                                imax = i
                                jmax = j
                            End If
                        End If
                    Next
                Next
                If imax <> nRowsDone Then
                    ' intercambiar la fila con el pivot con
                    ' la primera fila sin tratar:
                    Dim v As New Vector(Mc.vVect(imax))
                    Mc.vVect(imax) = New Vector(Mc.vVect(nRowsDone))
                    Mc.vVect(nRowsDone) = v
                End If
                'If jmax <> k Then
                '    i = vCols(k)
                '    vCols(k) = jmax
                '    vCols(jmax) = i
                '    ' intercambiar la columna con el pivot con
                '    ' la primera columna sin tratar:
                '    For i = nRowsDone To nRows - 1
                '        Dim Pa As New Polynomial(Mc.vVect(i).vPoly(jmax))
                '        Mc.vVect(i).vPoly(jmax) = Mc.vVect(i).vPoly(k)
                '        Mc.vVect(i).vPoly(k) = Pa
                '    Next
                'End If
                Dim pivot As Polynomial = Mc.vVect(k).vPoly(k)
                If pivot.isReal AndAlso pivot.ToDouble = 0.0 Then
                    Exit For
                End If
                Dim wj As New Vector()
                ReDim wj.vPoly(nRows - 1)
                For i = 0 To nRows - 1
                    wj.vPoly(i) = New Polynomial(Mc.vVect(i).vPoly(k))
                Next
                For i = k + 1 To nRows - 1
                    Dim mu As New Polynomial(wj.vPoly(i))
                    If Not (mu.isReal AndAlso mu.ToDouble = 0.0) Then
                        For j = k To nCols
                            Mc.vVect(i).vPoly(j) = -Mc.vVect(k).vPoly(j) * mu _
                                + Mc.vVect(i).vPoly(j) * pivot
                        Next
                    End If
                Next
                nRowsDone += 1
                If nRowsDone >= nRows Then
                    Exit For
                End If
            Next
            ' Conseguir unos en la diagonal:
            For i = 0 To nRows - 1
                Dim diag As Polynomial = Mc.vVect(i).vPoly(i)
                If Not (diag.isReal AndAlso diag.ToDouble = 0.0) Then
                    Mc.vVect(i).vPoly(i) = New Polynomial(1.0)
                    For j = i + 1 To nCols
                        Mc.vVect(i).vPoly(j) /= diag
                    Next
                End If
            Next
            'For i = nRows - 1 To 1 Step -1
            '    Dim mu As New Polynomial(Mc.vVect(i - 1).vPoly(i))
            '    For k = 1 To i
            '        For j = i To nCols
            '            Mc.vVect(i - k).vPoly(j) -= _
            '                Mc.vVect(i).vPoly(j) * mu
            '        Next
            '    Next
            'Next
            'For j = 0 To vCols.Length - 1
            '    If vCols(j) <> j Then
            '        k = vCols(j)
            '        For i = 0 To nRows - 1
            '            Dim Pa As New Polynomial(Mc.vVect(i).vPoly(j))
            '            Mc.vVect(i).vPoly(j) = Mc.vVect(i).vPoly(k)
            '            Mc.vVect(i).vPoly(k) = Pa
            '        Next
            '        vCols(k) = k
            '    End If
            'Next

            ' ordenar por el número de ceros:
            Dim leftZeros(nRows - 1) As Int64
            For i = 0 To nRows - 1
                Dim bFound As Boolean = False
                For j = 0 To nCols
                    Dim Pa As Polynomial = Mc.vVect(i).vPoly(j)
                    If Not (Pa.isReal AndAlso Pa.ToDouble = 0.0) Then
                        bFound = True
                    Else
                        If Not bFound Then
                            leftZeros(i) += 100
                        Else
                            leftZeros(i) += 1
                        End If
                    End If
                Next
            Next
            Array.Sort(leftZeros, Mc.vVect)

            If oVars IsNot Nothing Then
                For i = nRows - 1 To 0 Step -1
                    j = Math.Floor(leftZeros(i) / 100)
                    If j < nCols Then
                        'Dim Pa As Polynomial = Mc.vVect(i).vPoly(j)
                        'If Not (Pa.isReal AndAlso Pa.ToDouble = 0.0) Then
                        '    Pa = Mc.vVect(i).vPoly(nCols) / Pa
                        '    oVars.setValue(j, New ExprMatrix(Pa))
                        '    For k = i - 1 To 0 Step -1
                        '        Mc.vVect(k).vPoly(j) *= Pa
                        '    Next
                        'End If
                    Else
                        Dim Pa As Polynomial = Mc.vVect(i).vPoly(nCols)
                        If Pa.isReal AndAlso Pa.ToDouble <> 0.0 Then
                            Mc = Nothing
                            sErrMsg = msg8.num(74)
                            Exit Try
                        End If
                    End If
                Next
                Dim sVars() As String = oVars.getNamesList
                For i = nRows - 1 To 0 Step -1
                    k = 0

                    Dim iFound As Int64 = -1
                    Do
                        Dim Pa As Polynomial = Mc.vVect(i).vPoly(k)
                        If Not (Pa.isReal AndAlso Pa.ToDouble = 0.0) Then
                            iFound = k
                            Exit Do
                        End If
                        k += 1
                    Loop While k < nCols
                    k += 1
                    If iFound > -1 Then
                        Dim Indep As New Polynomial(0.0)
                        Indep = New Polynomial(Mc.vVect(i).vPoly(nCols))
                        Do While k < nCols
                            Dim Pa As Polynomial = Mc.vVect(i).vPoly(k)
                            If Not (Pa.isReal AndAlso Pa.ToDouble = 0.0) Then
                                Indep -= Pa * Polynomial.GetPolynomial(sVars(k))
                            End If
                            k += 1
                        Loop
                        Dim Pb As Polynomial = Mc.vVect(i).vPoly(iFound)
                        If Not (Pb.isReal AndAlso Pb.ToDouble = 0.0) Then
                            oVars.setValue(iFound, New ExprMatrix(Indep / Pb))
                        Else
                            oVars.setValue(iFound, New ExprMatrix(Indep))
                        End If
                    End If
                Next
            End If
            Dim nlambda As Int64 = 1
            Dim sLambda As String = "_λ"
            For i = 0 To oVars.length - 1
                If oVars.getValueByID(i) Is Nothing Then
                    Dim pLambda As Polynomial = Polynomial.GetPolynomial(sLambda + nlambda.ToString)
                    nlambda += 1
                    oVars.setValue(i, New ExprMatrix(pLambda))
                    For j = 0 To oVars.length - 1
                        If j <> i Then
                            Dim eM As ExprMatrix = oVars.getValueByID(j)
                            If eM IsNot Nothing Then
                                Dim expr As Expression = eM.getExpr(0, 0)
                                expr = expr.evalExprToExpr(oVars)
                                oVars.setValue(j, New ExprMatrix(expr))
                            End If
                        End If
                    Next
                End If
            Next
            For j = 0 To oVars.length - 1
                Dim eM As ExprMatrix = oVars.getValueByID(j)
                If eM IsNot Nothing Then
                    Dim expr As Expression = eM.getExpr(0, 0)
                    expr = expr.evalExprToExpr(oVars)
                    oVars.setValue(j, New ExprMatrix(expr))
                End If
            Next
        Catch ex As Exception
            'sErrMsg = ex.ToString
            Mc = Nothing
            Throw ex
        End Try
        Return Mc
    End Function
    Public Shared Function LCM_GCD(a() As Double, Optional cfg As Config = Nothing, Optional ByRef sDetail As String = "") As Double()
        Dim dbRet(1) As Double
        Try
            ' Finds Least Common Multiple (LCM) and
            ' Greatest Common Divisor (GCD) of a series of 
            ' numbers present in a()

            ' Return values:
            ' element dbRet(0)<--GCD , dbRet(1)<--- LCM
            Dim cols As Int64 = a.Length
            Dim vExp(cols - 1)() As Int64, iv As Int64 = 0
            For i As Int64 = 0 To cols - 1
                Dim curDb As Double = a(i)
                If curDb < 1.0 Then
                    Throw New Exception(msg8.num(9)) ' Argument(s) not valid.
                End If
                For j As Int64 = 0 To MathGlobal8.vPrime.Length - 1
                    Dim prime As Double = MathGlobal8.vPrime(j)
                    Do While Math.Floor(curDb / prime) = curDb / prime
                        curDb /= prime
                        If j > iv Then
                            iv = j
                        End If
                        ReDim Preserve vExp(i)(iv)
                        vExp(i)(j) += 1
                        If curDb = 1.0 Then Exit For
                    Loop
                Next
            Next
            Dim vMax(iv) As Int64
            Dim vMin(iv) As Int64
            For j As Int64 = 0 To iv
                vMin(j) = Int64.MaxValue
                For i As Int64 = 0 To cols - 1
                    ReDim Preserve vExp(i)(iv)
                    If vExp(i)(j) > vMax(j) Then
                        vMax(j) = vExp(i)(j)
                    End If
                    If vExp(i)(j) < vMin(j) Then
                        vMin(j) = vExp(i)(j)
                    End If
                Next
            Next
            Dim lcm As Double = 1
            Dim gcd As Double = 1
            If cfg IsNot Nothing AndAlso cfg.bDetail Then
                'Dim ni As Globalization.NumberFormatInfo = MathGlobal8.us.NumberFormat.Clone
                For i As Int64 = 0 To cols - 1
                    cfg.oDetail.AddAlways(a(i).ToString + " = ")
                    Dim e1 As String = String.Empty
                    For j = 0 To iv
                        If vExp(i)(j) Then
                            If Len(e1) Then
                                e1 += " * "
                            End If
                            If vExp(i)(j) = 1 Then
                                e1 += MathGlobal8.vPrime(j).ToString
                            Else
                                e1 += MathGlobal8.vPrime(j).ToString + "^" + vExp(i)(j).ToString
                            End If
                        End If
                    Next
                    cfg.oDetail.AddAlways(e1)
                Next
            End If
            For j = 0 To iv
                If vMax(j) Then
                    lcm *= MathGlobal8.vPrime(j) ^ vMax(j)
                End If
                If vMin(j) Then
                    gcd *= MathGlobal8.vPrime(j) ^ vMin(j)
                End If
            Next
            dbRet(0) = gcd
            dbRet(1) = lcm
        Catch ex As Exception
            Throw ex
        End Try
        Return dbRet
    End Function
End Class
Public Class retGaussElim_Mtx
    Public mtx As Matrix
    Public exprMtx As ExprMatrix
    Public var(-1) As Int64
End Class
