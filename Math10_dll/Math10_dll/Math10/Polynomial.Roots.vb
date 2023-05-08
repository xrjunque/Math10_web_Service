Imports System.Windows.Input

Partial Public Class Polynomial
    Public Shared bOnlyRationalRoots As Boolean = False
    Public Function RootsNumerator() As Complex()
        Roots()
        Return pRootsNum
    End Function

    Public Sub Roots()
        Dim bDetail As Boolean = G10.detail
        Try
            G10.detail = False
            'Dim poly As Polynomial = Polynomial.CopyFrom(Me)
            replaceConstants()
            Dim vVar() As String = Me.GetVars
            If vVar.Length > 1 Then
                Dim svar As String = ""
                For i = 0 To vVar.Length - 1
                    If i Then svar += ", "
                    svar += vVar(i)
                Next
                Throw New Exception(String.Format(Msg10.Num(105), svar))
            ElseIf vVar.Length <> 1 Then
                Throw New ArgumentOutOfRangeException
            End If
            SortTerms()
            If resto(0).f.Count AndAlso resto(0).f(0).exp Then
                pRootsNum = RootsListOfTerms(resto, vVar(0))
            End If
            If divisor(0).f.Count AndAlso divisor(0).f(0).exp Then
                pRootsDen = RootsListOfTerms(divisor, vVar(0))
            End If
        Catch ex As Exception
            Throw
        Finally
            G10.detail = bDetail
        End Try
    End Sub
    Public Function RootsListOfTerms(lst As List(Of Term), sVar As String) As Complex()
        Dim rootsG(-1), root As Complex, iv, i As Int32
        Try
            Dim lstT As New List(Of Term)
            lstT.AddRange(lst)
            Dim leadingT As Term = lstT(0) ' poly.LeadingTerm
            If leadingT.f.Count = 0 OrElse leadingT.f(0).exp = 0 Then
                Return rootsG
            End If
            Dim n As Int32 = Term.Degree(leadingT)
            If n < 1 OrElse n > 500 Then
                Throw New ArgumentOutOfRangeException
            End If
            Dim lowerT As Term = lstT(lstT.Count - 1) ' poly.LowerTerm
            Dim ltDegree As Int32 = Term.Degree(lowerT)
            iv += ltDegree
            If iv Then
                ReDim rootsG(iv - 1)
                For i = 0 To iv - 1
                    rootsG(i) = Complex.zero
                Next
                'poly /= New Polynomial(vVar(0), iv)
                For i = 0 To lstT.Count - 1
                    If lstT(i).f.Count Then lstT(i).f(0).exp -= iv
                Next
                n -= iv
            End If
            Do While n
                If n = 1 Then
                    Dim t() As Term = lstT.ToArray
                    root = -lst(1).cf / lst(0).cf
                    ReDim Preserve rootsG(rootsG.Length)
                    rootsG(rootsG.Length - 1) = root
                    Exit Try
                ElseIf n = 2 Then
                    Dim t() As Term = lstT.ToArray
                    Dim a, b, c As Complex
                    a = t(0).cf
                    If t.Length = 2 Then
                        ' a*x^2 + c = 0
                        c = t(1).cf
                        root = -(-c / a) ^ Complex.oneHalf
                        ReDim Preserve rootsG(rootsG.Length)
                        rootsG(rootsG.Length - 1) = root
                        root = (-c / a) ^ Complex.oneHalf
                        ReDim Preserve rootsG(rootsG.Length)
                        rootsG(rootsG.Length - 1) = root
                        Exit Try
                    Else
                        ' a*x^2 + b*x + c = 0
                        b = t(1).cf
                        c = t(2).cf
                        root = (-b + (b * b - 4 * a * c) ^ Complex.oneHalf) / (2.0 * a)
                        i = rootsG.Length
                        ReDim Preserve rootsG(rootsG.Length)
                        rootsG(rootsG.Length - 1) = root
                        root = (-b - (b * b - 4 * a * c) ^ Complex.oneHalf) / (2.0 * a)
                        ReDim Preserve rootsG(rootsG.Length)
                        rootsG(rootsG.Length - 1) = root
                        Dim bVerySmall As Boolean = False
                        If rootsG(i).opModulo * 10 ^ 6 < rootsG(i + 1).opModulo Then
                            bVerySmall = True
                        End If
                        If bVerySmall Then
                            ' if root1 << root2
                            ' c = root1 * root2
                            ' root1 =  c / root2 
                            rootsG(i) = c / rootsG(i + 1)
                        Else
                            i += 1
                            If rootsG(i).opModulo * 10 ^ 6 < rootsG(i - 1).opModulo Then
                                bVerySmall = True
                            Else
                                bVerySmall = False
                            End If
                            ' if root2 << root1
                            ' c = root1 * root2
                            ' root2 = c / root1
                            If bVerySmall Then
                                rootsG(i) = c / rootsG(i - 1)
                            End If
                        End If
                    End If
                    Exit Try
                End If
                If True OrElse n > 2 Then
                    Dim rr() As Complex = Polynomial.FindRationalRoots(lstT, sVar)
                    If rr.Length Then
                        Do
                            i = rootsG.Length
                            Dim j As Int32 = rr.Length
                            ReDim Preserve rootsG(i + j - 1)
                            Array.Copy(rr, 0, rootsG, i, j)
                            n -= j
                            If n > 0 Then
                                rr = Polynomial.FindRationalRoots(lstT, sVar)
                            End If
                        Loop While n > 0 AndAlso rr.Length
                    End If
                    If n < 1 Then Exit Try
                    If bOnlyRationalRoots Then Exit Try
                    If n > 2 Then
                        Dim DK() As Complex = Polynomial.FindRoots_DuranKernel(lstT)
                        If DK.Length Then
                            i = rootsG.Length
                            Dim j As Int32 = DK.Length
                            ReDim Preserve rootsG(i + j - 1)
                            Array.Copy(DK, 0, rootsG, i, j)
                            n -= j
                        End If
                        If n Then
                            Throw New Exception("n/a")
                        End If
                        Exit Try
                    End If
                End If
            Loop
        Catch ex As Exception
            Throw
        End Try
        Dim sR As New SortRoots
        Array.Sort(rootsG, sR)
        Return rootsG
    End Function
    Public Shared Sub opRuffini(ByRef lstT As List(Of Term), sVar As String, root As Complex)
        ' Divide current polynomial P(x) by the binomial (x-a)
        ' (see Ruffini's rule, for ex. at http://en.wikipedia.org/wiki/Ruffini%27s_rule )
        Dim i As Int64 = 0
        Dim n As Int64 = lstT(0).f(0).exp
        Dim b(n) As Complex
        Try
            'Dim sVar As String = Me.GetVars(0)
            Dim r As Complex = root
            b(n) = New Complex(lstT(0).ToComplex)
            Dim j As Int32 = 1
            For i = n - 1 To 1 Step -1
                If j < lstT.Count AndAlso lstT(j).f.Count AndAlso lstT(j).f(0).exp = i Then
                    b(i) = b(i + 1) * r + lstT(j).cf
                    j += 1
                Else
                    b(i) = b(i + 1) * r
                End If
            Next
            lstT.Clear() '  ReDim cf(n), exp(n)
            j = 0
            For i = n To 1 Step -1
                If Not b(i).IsZero Then
                    lstT.Add(New Term(b(i)))
                    If i > 1 Then
                        lstT(j).f.Add(New Factor(sVar, i - 1))
                    End If
                    j += 1
                End If
            Next
        Catch ex As Exception
            Throw
        End Try
    End Sub

    Public Shared Function FindRationalRoots(ByRef Pa As List(Of Term), sVar As String) As Complex()
        Dim root, rootsG(-1) As Complex
        Try
            Dim i, j As Int32
            Dim p0 As New Polynomial(Pa)
            Dim degree As Int32 = p0.Degree
            Dim possibleRoots As New List(Of Double)
            For i = 1 To 20
                For j = 1 To 20
                    possibleRoots.Add(i / j)
                Next
            Next
            possibleRoots.Sort()
            For i = 0 To possibleRoots.Count - 1
                If i AndAlso possibleRoots(i) = possibleRoots(i - 1) Then
                    Continue For
                End If
                root = New Complex(possibleRoots(i))
                Dim iFound As Int32 = 0
                iFound = 0
                Dim dbR As Double = EvalListOfTerms(p0.resto, root.pRe).ToDouble
                If dbR = 0 Then
                    iFound = 1
                Else
                    dbR = EvalListOfTerms(p0.resto, -root.pRe).ToDouble
                    If dbR = 0 Then
                        iFound = -1
                    End If
                End If
                If iFound Then
                    If iFound = -1 Then
                        root.opChgSgn()
                    End If
                    Dim db As Double = root.ToDouble
                    Dim sgn As Int32 = Math.Sign(db)
                    db = Math.Abs(db)
                    Dim den As Double = 1.0
                    Do While db < 10 ^ 6 AndAlso db <> Math.Floor(db)
                        db *= 10
                        den *= 10
                    Loop
                    Dim comm As Double = Rational.GCD(db, den)
                    db /= comm : den /= comm
                    root = New Complex(New Rational(sgn * db, den), New Rational(0.0))
                    ReDim Preserve rootsG(rootsG.Length)
                    rootsG(rootsG.Length - 1) = New Complex(root)
                    ' 'deflate' Pa:
                    opRuffini(Pa, sVar, root)
                    opRuffini(p0.resto, sVar, root)
                    If rootsG.Length = degree Then Exit For
                    i = -1
                End If
            Next
        Catch ex As Exception
            Throw
        End Try
        Return rootsG
    End Function
    Public Shared Function FindRoots_DuranKernel(ByVal lstT As List(Of Term)) As Complex()
        Dim i, j As Int64
        Dim degree As Int32 = lstT(0).f(0).exp
        Dim p(degree - 1) As Complex
        Try
            ReduceListOfTerms(lstT)
            Dim LT As Term = lstT(0)
            Dim cf As Complex = LT.cf
            For i = 0 To lstT.Count - 1
                lstT(i).cf /= cf
            Next
            'Pa /= New Polynomial(cf)
            'Pa.SortTerms()
            Dim angle As Double = 2 * Math.PI / degree
            Dim start As New Complex(0.4, 0.9)
            For i = 0 To p.Length - 1
                p(i) = New Complex(Math.Cos(angle * i), Math.Sin(angle * i))
                'p(i) = start ^ New Complex(i + 1)
            Next
            Dim divisor As Complex
            For curVueltas As Int32 = 1 To 35
                For i = 0 To p.Length - 1
                    divisor = New Complex(Complex.one)
                    For j = 0 To p.Length - 1
                        If j <> i Then
                            divisor *= (p(i) - p(j))
                            divisor.pRe.reduce(20)
                            divisor.pIm.reduce(20)
                        End If
                    Next
                    Try
                        p(i) -= EvalListOfTermsCplx(lstT, p(i)) / divisor
                        p(i).pRe.reduce(20)
                        p(i).pIm.reduce(20)
                    Catch ex As Exception

                    End Try
                Next
            Next
            For i = 0 To p.Length - 1
                Dim z3 As Complex = p(i)
                Dim x0 As New Complex(New Rational(0.0), New Rational(Math.Round(z3.pIm.ToDouble, 15)))
                If z3.pRe.ToDouble < 10 ^ -15 Then
                    If EvalListOfTermsCplx(lstT, x0).opModulo <= EvalListOfTermsCplx(lstT, z3).opModulo Then
                        p(i) = x0
                    End If
                End If
                If z3.pIm.ToDouble < 10 ^ -15 Then
                    x0 = New Complex(Math.Round(z3.pRe.ToDouble, 15))
                    If EvalListOfTermsCplx(lstT, x0).opModulo <= EvalListOfTermsCplx(lstT, z3).opModulo Then
                        p(i) = x0
                    End If
                End If
                p(i).pRe.reduce()
                p(i).pIm.reduce()
            Next
        Catch ex As Exception
            Throw
        End Try
        Return p
    End Function
    Public Shared Function Factorize(ByRef poly As Polynomial, Decimals As Int32) As Polynomial()
        Dim vPoly(-1) As Polynomial, iv As Int32
        Try
            Dim vVars() As String = poly.GetVars
            If vVars.Length = 0 Then
                vPoly = New Polynomial() {poly}
                Exit Try
            End If
            Dim sVar As String = vVars(0)
            Dim vRoots() As Complex = poly.RootsNumerator
            For i = 0 To vRoots.Length - 1
                Dim p As New Polynomial(sVar, 1)
                If i < vRoots.Length - 1 AndAlso
              Math.Round(vRoots(i).pRe.ToDouble, Decimals) = Math.Round(vRoots(i + 1).pRe.ToDouble, Decimals) AndAlso
              Math.Round(vRoots(i).pIm.ToDouble, Decimals) = Math.Round(-vRoots(i + 1).pIm.ToDouble, Decimals) AndAlso
              Math.Round(vRoots(i).pIm.ToDouble, Decimals) <> 0.0 Then
                    Dim a As Rational = vRoots(i).pRe
                    Dim b As Rational = vRoots(i).pIm
                    ' (x-a+bi)*(x-a-bi)= ... = x^2-2*a*x + a^2+b^2
                    p = New Polynomial(sVar, 2)
                    Dim pax As New Polynomial(sVar, 1)
                    pax *= New Polynomial(New Complex(a * -2, New Rational(0)))
                    p += pax + New Polynomial(New Complex(a * a + b * b, New Rational(0)))
                    i += 1
                Else
                    p -= New Polynomial(vRoots(i))
                    ReDim Preserve vPoly(iv)
                End If
                ReDim Preserve vPoly(iv)
                vPoly(iv) = p
                iv += 1
            Next
        Catch ex As Exception
            Throw
        End Try
        Return vPoly
    End Function
    Shared Function AddRoot(ByRef vCplx() As Complex, x0 As Complex, Optional AddAlways As Boolean = True) As Int32
        Dim j As Int32
        For j = 0 To vCplx.Length - 1
            If (vCplx(j) - x0).opModulo < 10 ^ -15 Then
                Exit For
            End If
        Next
        If AddAlways OrElse j >= vCplx.Length Then
            ReDim Preserve vCplx(vCplx.Length)
            If G10.nDec < 15 Then
                x0 = New Complex(Math.Round(x0.pRe.ToDouble, G10.nDec),
                               Math.Round(x0.pIm.ToDouble, G10.nDec))
            End If
            vCplx(vCplx.Length - 1) = x0
            Return vCplx.Length - 1
        End If
        Return j
    End Function

    'Public Shared Function PartialFractions(numerator As Polynomial, denominator As Polynomial,
    '                                        sortedRootsDenominator() As Complex) As Polynomial()
    '    Dim ret(-1) As Polynomial, ir As Int32 = 0
    '    Try
    '        Dim sVar As String = denominator.GetVars(0)
    '        Dim x As New Polynomial(sVar)
    '        Dim roots(-1) As Complex
    '        Dim vDegreeRoot(-1) As Int32
    '        Dim vHasConjugate(-1) As Boolean
    '        If G10.nDec < 15 Then
    '            For i As Int32 = 0 To sortedRootsDenominator.Length - 1
    '                Dim re As Double = sortedRootsDenominator(i).pRe.ToDouble
    '                Dim im As Double = sortedRootsDenominator(i).pIm.ToDouble
    '                re = Math.Round(re, G10.nDec)
    '                im = Math.Round(im, G10.nDec)
    '                sortedRootsDenominator(i) = New Complex(re, im)
    '            Next
    '        End If
    '        For i As Int32 = 0 To sortedRootsDenominator.Length - 1
    '            If sortedRootsDenominator(i).pIm.IsZero Then
    '                If i < sortedRootsDenominator.Length - 1 AndAlso
    '                        (sortedRootsDenominator(i) - sortedRootsDenominator(i + 1)).IsZero Then
    '                    Dim pos As Int32 = AddRoot(roots, sortedRootsDenominator(i))
    '                    ReDim Preserve vDegreeRoot(roots.Length - 1), vHasConjugate(roots.Length - 1)
    '                    vDegreeRoot(pos) += 1
    '                    i += 1
    '                Else
    '                    AddRoot(roots, sortedRootsDenominator(i))
    '                    ReDim Preserve vDegreeRoot(roots.Length - 1), vHasConjugate(roots.Length - 1)
    '                End If
    '            ElseIf i < sortedRootsDenominator.Length - 1 AndAlso
    '                    (sortedRootsDenominator(i) - Complex.opConjugate(sortedRootsDenominator(i + 1))).IsZero Then
    '                If i < sortedRootsDenominator.Length - 3 AndAlso
    '                    (sortedRootsDenominator(i + 2) - Complex.opConjugate(sortedRootsDenominator(i + 3))).IsZero AndAlso
    '                    (sortedRootsDenominator(i) - Complex.opConjugate(sortedRootsDenominator(i + 2))).IsZero Then
    '                    Dim pos As Int32 = AddRoot(roots, sortedRootsDenominator(i))
    '                    ReDim Preserve vDegreeRoot(roots.Length - 1), vHasConjugate(roots.Length - 1)
    '                    vDegreeRoot(pos) += 1
    '                    vHasConjugate(pos) = True
    '                    i += 2
    '                Else
    '                    Dim pos As Int32 = AddRoot(roots, sortedRootsDenominator(i))
    '                    ReDim Preserve vDegreeRoot(roots.Length - 1), vHasConjugate(roots.Length - 1)
    '                    vHasConjugate(pos) = True
    '                    i += 1
    '                End If
    '            Else
    '                AddRoot(roots, sortedRootsDenominator(i))
    '                ReDim Preserve vDegreeRoot(roots.Length - 1), vHasConjugate(roots.Length - 1)
    '            End If
    '        Next
    '        Dim vs(-1) As Complex
    '        For i As Int32 = 0 To roots.Length - 1
    '            If vDegreeRoot(i) = 0 Then
    '                For n As Int32 = 0 To 1
    '                    ReDim Preserve ret(ir)
    '                    ret(ir) = New Polynomial(0.0)
    '                    ret(ir).resto = New Polynomial(1.0)
    '                    For j = 0 To sortedRootsDenominator.Length - 1
    '                        If Not (roots(i) - sortedRootsDenominator(j)).IsZero Then
    '                            ret(ir).resto *= (x - New Polynomial(sortedRootsDenominator(j)))
    '                        End If
    '                    Next
    '                    Dim resto As New Polynomial(ret(ir).resto)
    '                    Dim num As New Polynomial(numerator)
    '                    Dim cjoNum As Complex = num.EvalCplx(roots(i))
    '                    ReDim Preserve vs(vs.Length)
    '                    vs(vs.Length - 1) = cjoNum
    '                    Dim cjoDen As Complex = ret(ir).resto.EvalCplx(roots(i))
    '                    ret(ir).resto = New Polynomial(cjoNum / cjoDen)
    '                    ret(ir).divisor = x - New Polynomial(roots(i))
    '                    ir += 1
    '                    If Not vHasConjugate(i) Then Exit For
    '                    roots(i) = Complex.opConjugate(roots(i))
    '                Next
    '            Else
    '                For n As Int32 = 0 To 1
    '                    For k As Int32 = vDegreeRoot(i) To 0 Step -1
    '                        ReDim Preserve ret(ir)
    '                        Dim A As New Polynomial(0.0)
    '                        A.resto = New Polynomial(numerator)
    '                        A.divisor = New Polynomial(1.0)
    '                        For j = 0 To sortedRootsDenominator.Length - 1
    '                            If Not (roots(i) - sortedRootsDenominator(j)).IsZero Then
    '                                A.divisor *= (x - New Polynomial(sortedRootsDenominator(j)))
    '                            End If
    '                        Next
    '                        For k1 As Int32 = 1 To vDegreeRoot(i) - k
    '                            A = A.Derivative(sVar)
    '                        Next
    '                        ret(ir) = New Polynomial(0.0)
    '                        ret(ir).resto = New Polynomial(A.EvalCplx(roots(i)))
    '                        ReDim Preserve vs(vs.Length)
    '                        vs(vs.Length - 1) = ret(ir).resto.ToComplex
    '                        ret(ir).divisor = (x - New Polynomial(roots(i))) ^ New Polynomial(k + 1)
    '                        ir += 1
    '                    Next
    '                    If Not vHasConjugate(i) Then Exit For
    '                    roots(i) = Complex.opConjugate(roots(i))
    '                Next
    '            End If
    '        Next
    '        Dim sr As New SortRoots
    '        Array.Sort(vs, ret, sr)
    '    Catch ex As Exception
    '        Throw
    '    End Try
    '    Return ret
    'End Function
    Public Shared Function partialFractionDecomposition(expr As Expression,
                    ByRef vNum() As Polynomial,
                    ByRef vDen() As Polynomial,
                    ByRef vDenExponent() As Int64,
                    ByRef sErrMsg As String) As Boolean
        Dim bRet As Boolean = False
        'Dim bDetail As Boolean = cfg.bDetail
        Dim bMathMl As Boolean = G10.mathml
        Dim bVar As Boolean = G10.var
        Try
            G10.mathml = False
            G10.var = True
            'cfg.bDetail = False
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
            ' Dim p As New Polynomial(pA)

            'p.resto = Nothing
            'p.divisor = Nothing
            'If Not p.IsRational OrElse
            'Not p.ToDouble = 0.0 Then
            '    sErrMsg = Msg8.num(68) ' n/a
            '    Exit Try
            'End If
            If expr.resto Is Nothing Then
                sErrMsg = Msg8.num(13) ' n/a
                Exit Try
            End If
            Dim bRestoIsPolynomial As Boolean = New Expression(expr.resto).IsPolynomial
            If Not bRestoIsPolynomial Then
                Throw New Exception(Msg8.num(13))
            End If
            Dim var() As String = expr.GetVars
            If var.Length <> 1 Then
                sErrMsg = String.Format(Msg8.num(69), expr.ToString, Join(var, " ")) ' more than 1 var
                Exit Try
            End If

            Dim X As New Polynomial(var(0), 1)
            'Dim oVar As New VarsAndFns(cfg)
            Dim oVar As New Dictionary(Of String, Expression)

            ' Find pA's denominator roots:
            'Dim Va As New Vector(pA.PolyDivisor)
            Dim div As New Expression(expr.divisor)
            If Not div.IsPolynomial Then
                Throw New Exception(Msg10.Num(117))
            End If
            Dim resto As New Expression(expr.resto)
            If Not resto.IsPolynomial Then
                Throw New Exception(Msg10.Num(117))
            End If
            Dim pResto As Polynomial = New Polynomial(resto.ToPolynomial)
            Dim paDiv As Polynomial = div.ToPolynomial
            paDiv.Roots()
            Dim vRoots() As Complex = paDiv.pRootsNum  '.pRootsDen  ' Polynomial.opRoots(pA.PolyDivisor).cjo
            Dim vR(-1) As Complex
            Dim i, j As Int64

            If G10.nDec < 15 Then
                For i = 0 To vRoots.Length - 1
                    Dim re As Double = vRoots(i).pRe.ToDouble
                    Dim im As Double = vRoots(i).pIm.ToDouble
                    Dim reRnd As Double = Math.Round(re, G10.nDec)
                    Dim imRnd As Double = Math.Round(im, G10.nDec)
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
                        If (vRoots(i) -
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
                    Do While i + 1 < vRoots.Length AndAlso
                        (vRoots(i) - vRoots(i + 1)).IsZero
                        ' roots at i and i+1 positions are equal 
                        ' (repeated):
                        vMultiRR(iR) += 1 ' incr. multiplicity
                        i += 1
                    Loop
                    iR += 1
                ElseIf i + 1 < vRoots.Length AndAlso
                    (vRoots(i) - Complex.opConjugate(vRoots(i + 1))).IsZero Then
                    ' roots at i and i+1 positions are conjugate ?
                    ReDim Preserve vConjugate(iC), vMultiConj(iC)
                    vConjugate(iC) = vRoots(i)
                    Do
                        vMultiConj(iC) += 1
                        i += 2
                    Loop While i + 1 < vRoots.Length AndAlso
                        (vRoots(i) - Complex.opConjugate(vRoots(i + 1))).IsZero AndAlso
                        ((vConjugate(iC) - vRoots(i)).IsZero OrElse
                        (vConjugate(iC) + vRoots(i)).IsZero)
                    iC += 1
                    i -= 1
                Else
                    ReDim Preserve vNotConjugate(iNC), vMultiNotConj(iNC)
                    vNotConjugate(iNC) = vRoots(i)
                    vMultiNotConj(iNC) = 1
                    Do While i + 1 < vRoots.Length AndAlso
                    (vRoots(i - 1) - vRoots(i)).IsZero
                        vMultiNotConj(iNC) += 1
                        i += 1
                    Loop
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
                    'oVar.AddVar(nomVar, Nothing)
                    oVar.Add(nomVar, Nothing)
                    ReDim Preserve vNum(iND), vDen(iND), vDenExponent(iND)
                    vNum(iND) = New Polynomial(nomVar, 1)
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
                    oVar.Add(nomVar, Nothing)
                    Dim nomVar_b As String = String.Format("_B{0}_{1}b", i + 1, j)
                    oVar.Add(nomVar_b, Nothing)
                    ' numerator will have the form: A1*X + A1b (degree=1)
                    vNum(iND) = New Polynomial(nomVar, 1) * X +
                            New Polynomial(nomVar_b, 1)
                    Dim a As Double = vConjugate(i).pRe.ToDouble
                    Dim b As Double = vConjugate(i).pIm.ToDouble
                    ' (x-(a+b*i))*(x-(a-b*i)) = x^2 -2a*x +(a2+b2)
                    vDen(iND) = X * X -
                                New Polynomial(2 * a) * X +
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
                    oVar.Add(nomVar, Nothing)
                    vNum(iND) = New Polynomial(nomVar, 1)
                    vDen(iND) = X - New Polynomial(vNotConjugate(i))
                    vDenExponent(iND) = j
                    iND += 1
                Next
            Next

            ' Now, if degree(Q) = q, we have to establish a system
            ' of q equations and q unknown variables.
            Dim q As Int64 = oVar.Count
            ' set a matrix of q rows and 1 column:
            Dim eMtx As New ExprMatrix(q, 1)

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
                    vPt(i) = New Complex(Rational.vPrime(i + 1)) * rnd.NextDouble * (j + 1)
                    j += (j + 1) Mod iPt
                    i += 1
                Loop
                Dim oVarX As New Dictionary(Of String, Expression)
                oVarX.Add(var(0), Nothing)
                Dim polyEq As New Polynomial(0.0)
                For i = 0 To vDen.Length - 1
                    Dim div2 As Polynomial = paDiv /
                            vDen(i) ^ New Polynomial(vDenExponent(i))
                    ' Division should be exact, trim possible
                    ' division error:
                    'Dim intPart As New List(Of Term)
                    'div2.Reduce(intPart)
                    'div.resto = Nothing
                    'div2.divisor.Clear()
                    'div2.divisor.Add(New Term(1.0))

                    'pEq += vNum(i) * div
                    polyEq += New Polynomial(vNum(i) * div2)
                    'pResto *= vDen(i)
                Next
                ' Evaluate each row i at point vPt(i):
                ' Establish q equations for q powers of sVar:
                G10.Initialize()
                Dim vsEq(q - 1) As String
                For i = q - 1 To 0 Step -1
                    Dim t As Polynomial
                    Dim xn As New Polynomial(var(0), i)
                    Dim s As String
                    t = pResto.PolynomialFromTermsOfADegreeAndVar(i, var(0))
                    Dim cf As New Complex(0.0)
                    s = "0="
                    If t IsNot Nothing Then
                        If i Then
                            s = (t / xn).ToString + "="
                        Else
                            s = t.ToString + "="
                        End If
                    End If
                    vsEq(i) = s
                    t = polyEq.PolynomialFromTermsOfADegreeAndVar(i, var(0))
                    s = "0"
                    If t IsNot Nothing Then
                        If i Then
                            s = (t / xn).ToString
                        Else
                            s = t.ToString
                        End If
                    End If
                    vsEq(i) += s
                    'eMtx.Item(i, 0) = pOM.Evaluate(oVarX, polyEq.ToString)(0)(0).eMtx.Item(0, 0)
                Next
                Dim pOM As New ParseOneMatrix
                eMtx = pOM.Evaluate(Nothing, Join(vsEq, "|"))(0)(0).eMtx
                ' Resolve the system:
                Dim vVars() As String = oVar.Keys.ToArray
                Try
                    eMtx = ExprMatrix.resolveLinearSystemOfEquations(eMtx, vVars)
                    oVar.Clear()
                    For i = 0 To vVars.Length - 1
                        oVar.Add(vVars(i), eMtx.Item(i, 1))
                    Next
                    For i = 0 To vNum.Length - 1
                        'vNum(i) = vNum(i).evalMultiCjoToPolynomial(oVar)
                        Dim pE As New ParseExpression
                        vNum(i) = pE.Evaluate(oVar, vNum(i).ToString).vExpr(0).ToPolynomial
                    Next
                    bRet = True
                    Exit Do
                Catch ex As Exception

                End Try
                G10.var = bVar

                ' Most probably, the set of equations
                ' are linearly dependant: try, till 5 attempts, with new points.
                nIter += 1
            Loop While nIter < 0
            'If bDetail Then
            '    Dim e1 As String = String.Empty
            '    For i = 0 To vNum.Length - 1
            '        Dim div As Polynomial = vNum(i) / vDen(i)
            '        If Len(e1) Then e1 += " +"
            '        e1 += div.toStringPoly(cfg)
            '        If vDenExponent(i) > 1 Then
            '            e1 += "^" + vDenExponent(i).ToString
            '        End If
            '    Next
            '    cfg.oDetail.Add("= " + e1)
            '    cfg.oDetail.ClearDivisions()
            'End If

        Catch ex As Exception
            sErrMsg = Msg8.num(13) ' n/a
        Finally
            'cfg.bDetail = bDetail
            G10.mathml = bMathMl
            G10.var = bVar
        End Try
        Return bRet
    End Function

End Class

Public Class SortRoots
    Implements IComparer
    Dim n As Int32 = 0
    Public Function Compare(x1 As Object, y1 As Object) As Integer Implements IComparer.Compare
        Dim x As Complex = CType(x1, Complex)
        Dim y As Complex = CType(y1, Complex)
        Try
            n += 1
            If x.pIm.IsZero AndAlso y.pIm.IsZero Then
                Return (x.pRe > y.pRe)
            ElseIf x.pRe.IsZero AndAlso y.pRe.IsZero Then
                Dim absX As Rational = x.pIm.Abs()
                Dim absY As Rational = y.pIm.Abs
                Return (absX > absY)
            End If
            If x.pIm.IsZero Then
                Return -1
            ElseIf y.pIm.IsZero Then
                Return 1
            End If
            If x.pRe.Sign < y.pRe.Sign Then
                Return -1
            ElseIf x.pRe.Sign = y.pRe.Sign Then
                If x.pRe < y.pRe Then
                    Return -1
                ElseIf x.pRe > y.pRe Then
                    Return 1
                ElseIf x.pIm = -y.pIm Then
                    Return 0
                Else
                    Return (x.pIm - y.pIm).Sign
                End If
            ElseIf x.pRe.Sign > y.pRe.Sign Then
                Return 1
            End If
            If x.pRe = y.pRe AndAlso x.pIm = -y.pIm Then
                Return 0
            End If
        Catch ex As Exception
            Throw
        End Try
        Return x.pRe.Sign * Math.Sign(x.opModulo - y.opModulo)
    End Function

End Class
