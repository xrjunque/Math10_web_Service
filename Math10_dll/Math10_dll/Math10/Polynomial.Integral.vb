Partial Public Class Polynomial
#Region "Polynomial Integral"
    Public Function opIntegral(Optional ByVal sVar As String = "") As Polynomial
        Dim polyC As New Polynomial(0.0)
        Try
            Dim pResto As New Polynomial(resto)
            Dim pDivisor As New Polynomial(divisor)
            Dim x As New Term(sVar, 1)
            If pDivisor.IsComplex Then
                Dim cjo As Complex = pDivisor.ToComplex
                For Each t As Term In resto
                    Dim exp As Double = 1.0
                    For Each f As Factor In t.f
                        If f.var.GetType Is GetType(String) AndAlso f.var = sVar Then
                            exp = f.exp + 1
                        End If
                    Next
                    polyC.resto.Add(t * x / New Term(exp))
                Next
                Return polyC / cjo
            End If
        Catch ex As Exception
            Throw
        End Try
        Return Nothing
    End Function
    '    Public Function opIntegralRationalFractions(
    '                    Optional ByVal sVar As String = "") As Expression

    '        Dim vNum(-1), vDen(-1) As Polynomial, vDenExponent(-1) As Int64
    '        Dim sErrMsg As String = String.Empty
    '        Dim retExpr As New Expression(0.0)
    '        Dim bDetail As Boolean = cfg.bDetail
    '        Try
    '            Functions.partialFractionDecomposition(
    '                Me, vNum, vDen, vDenExponent, cfg, sErrMsg)
    '            If Len(sErrMsg) Then
    '                Throw New Exception(sErrMsg)
    '            End If

    '            cfg.bDetail = False

    '            ' Supress any zero:
    '            Dim vNum2(-1), vDen2(-1) As Polynomial, vDenExp2(-1) As Int64
    '            Dim i2 As Int64 = 0
    '            For i = 0 To vNum.Length - 1
    '                If Not (vNum(i).isReal AndAlso vNum(i).ToDouble = 0.0) Then
    '                    ReDim Preserve vNum2(i2), vDen2(i2), vDenExp2(i2)
    '                    vNum2(i2) = vNum(i)
    '                    vDen2(i2) = vDen(i)
    '                    vDenExp2(i2) = vDenExponent(i)
    '                    i2 += 1
    '                End If
    '            Next
    '            vNum = vNum2 : vDen = vDen2 : vDenExponent = vDenExp2

    '            Dim x As Polynomial = Polynomial.GetPolynomial(sVar)
    '            For i = 0 To vNum.Length - 1
    '                Dim m As Int64 = vDenExponent(i)
    '                Dim vTerms() As Polynomial = vDen(i).splitIntoTerms
    '                If vDen(i).Degree = 2 AndAlso vNum(i).Degree = 0 Then
    '                    If cfg.bDetail Then
    '                        cfg.oDetail.AddAlways("∫c/(x2+ax+b) ={ x2+ax+b=x2+ax+a/2 +b-a/2= (x+a/2)^2 +b-a/2 } ")
    '                        cfg.oDetail.AddAlways("=∫c/((x+a/2)^2 +(b-a/2)")
    '                        cfg.oDetail.AddAlways("= { ∫C/(x+A)^2+B = (C/sqrt(B))*atan((x+a)/sqr(b)) }")
    '                        cfg.oDetail.AddAlways("= (c/sqr(b-a/2)) * atan((x+a/2)/sqr(b-a/2)) ")
    '                    End If
    '                    Dim a As Double = 0.0
    '                    Dim b As Double = 0.0
    '                    For j = 0 To vTerms.Length - 1
    '                        If vTerms(j).Degree = 0 Then
    '                            b = vTerms(j).ToDouble
    '                        ElseIf vTerms(j).Degree = 1 Then
    '                            a = vTerms(j).ToDouble
    '                        End If
    '                    Next
    '                    Dim cte As Double = vNum(i).ToDouble / Math.Sqrt(b - a / 2.0)
    '                    retExpr +=
    '                        New Expression(cte) *
    '                        Expression.AddFnAndArg0("atan",
    '                        New Expression(x + New Polynomial(a / 2.0)) _
    '                        / New Expression(Math.Sqrt(b - a / 2.0)))
    '                    If cfg.bDetail Then
    '                        cfg.oDetail.AddAlways(retExpr.ToStringExpr(cfg))
    '                    End If
    '                ElseIf m = 1 Then
    '                    If vNum(i).IsComplex Then
    '                        retExpr +=
    '                            New Expression(vNum(i)) *
    '                            Expression.AddFnAndArg0("ln",
    '                            New Expression(vDen(i)))
    '                    Else
    '                        retExpr +=
    '                            Expression.AddFnAndArg0("ln",
    '                            New Expression(vNum(i) / vDen(i)))
    '                    End If
    '                Else
    '                    If vNum(i).IsComplex Then
    '                        retExpr +=
    '                            New Expression(vNum(i) / New Polynomial(-(m - 1))) *
    '                            Expression.exprOp("^", New Expression(vDen(i)),
    '                            New Expression(-(m - 1)))
    '                    Else
    '                        retExpr +=
    '                            New Expression(New Polynomial(-1.0 / (m - 1))) *
    '                            Expression.exprOp("/", New Expression(vNum(i)),
    '                            Expression.exprOp("^", New Expression(vDen(i)),
    '                            New Expression(-(m - 1))))
    '                    End If
    '                End If
    '            Next
    '            Dim rup As New ReduceExprUsingPolynomials
    '            retExpr = rup.ReduceUsingPolynomials(retExpr)
    '            If bDetail Then
    '                cfg.oDetail.ClearDivisions()
    '            End If
    '        Catch ex As Exception
    '            Throw
    '        Finally
    '            cfg.bDetail = bDetail
    '        End Try
    '        Return retExpr
    '    End Function
    '    Public Function opIntegralRationalFractionsImgLog(
    '                    cfg As Config,
    '                    Optional ByVal sVar As String = "") As Expression
    '        Dim polyC As Polynomial = Nothing
    '        Dim retExpr As Expression = Nothing
    '        Dim bDetail As Boolean = cfg.bDetail
    '        Try

    '            cfg.bDetail = False
    '            If sVar = "" Then
    '                If Me.var.Length Then
    '                    sVar = Me.var(0)
    '                ElseIf Me.Resto IsNot Nothing Then
    '                    If Me.Resto.var.Length Then
    '                        sVar = Me.Resto.var(0)
    '                    Else
    '                        sVar = Me.divisor.var(0)
    '                    End If
    '                End If
    '            End If
    '            Dim R, Q As Polynomial
    '            If Me.resto Is Nothing Then
    '                polyC = opIntegralNoFractions(sVar)
    '                Exit Try
    '            End If
    '            Dim sPreReducing As String = String.Empty
    '            If bDetail Then
    '                sPreReducing = Me.toStringPoly(cfg)
    '            End If
    '            Me.resto.reduce()
    '            Dim sDetail As String = String.Empty
    '            If bDetail Then
    '                Dim sPostReducing As String =
    '                    Me.toStringPoly(cfg)
    '                If sPreReducing <> sPostReducing Then
    '                    sDetail += sPreReducing + " =" + vbCrLf +
    '                        "= " + sPostReducing + vbCrLf
    '                End If
    '            End If
    '            R = New Polynomial(Me.resto)
    '            Q = New Polynomial(Me.divisor)
    '            If R.Degree > 15 OrElse
    '            Q.Degree > 18 Then
    '                Throw New Exception(Msg8.num(13)) ' n/a 
    '            End If
    '            polyC = New Polynomial(Me)
    '            polyC.resto = Nothing
    '            polyC.divisor = Nothing
    '            If Q.An.pRe.ToDouble <> 1.0 Then
    '                R = R / Q.An
    '                Q = Q / Q.An
    '            End If
    '            'Dim Va As New Vector(Q)
    '            Dim rootsMtx As Matrix =
    '                Polynomial.opRoots(Q, False, cfg).mtx
    '            Dim nQ As Int64 = Q.Degree
    '            Dim roots(nQ - 1) As Complex
    '            Dim rMultiplicity(nQ - 1) As Int64
    '            roots(0) = New Complex(rootsMtx.vVect(0).vPoly(0).cf(0))
    '            rMultiplicity(0) = 1
    '            Dim j As Int64 = 0
    '            Dim k As Int64 = 0

    '            ' Set roots' multiplicity in rMultiplicity():
    '            For i As Int64 = 1 To nQ - 1
    '                For k = 0 To j
    '                    If (rootsMtx.vVect(i).vPoly(0).cf(0) - roots(k)).esCero Then
    '                        Exit For
    '                    End If
    '                Next
    '                If k < j + 1 Then
    '                    rMultiplicity(k) += 1
    '                Else
    '                    j += 1
    '                    roots(j) = New Complex(rootsMtx.vVect(i).vPoly(0).cf(0))
    '                    rMultiplicity(j) = 1
    '                End If
    '            Next
    '            ReDim Preserve rMultiplicity(j), roots(j)
    '            Dim sA(nQ - 1) As String
    '            Dim pA(nQ - 1) As Polynomial
    '            Dim eqMtx As New Matrix
    '            Dim eqVars As New VarsAndFns(cfg)
    '            eqMtx.setRowAndCol(nQ - 1, nQ - 1)
    '            k = 0
    '            Dim x As Polynomial = Polynomial.GetPolynomial(sVar)
    '            Dim sMsg As String = ""

    '            ' generate vars' names and add to eqVars object:
    '            ' 1/(xn+...) = _A1/(..) + ...
    '            For i = 0 To roots.Length - 1
    '                For j = 1 To rMultiplicity(i)
    '                    ' _A1 _A2 ... _B1 _B2 ..
    '                    sA(k) = "_" + Chr(&H41 + i) + Chr(&H30 + j)
    '                    pA(k) = Polynomial.GetPolynomial(sA(k))
    '                    eqVars.AddVar(sA(k), Nothing, sMsg)
    '                    k += 1
    '                Next
    '            Next
    '            Dim eqP As New Polynomial(0.0)
    '            k = 0
    '            If bDetail Then
    '                ' if detailed info. required then
    '                ' set the fractions into sDetail:
    '                sDetail += Me.toStringPoly(cfg) + " = "
    '                For i = 0 To roots.Length - 1
    '                    For j = 1 To rMultiplicity(i)
    '                        sDetail += sA(k) + "/(" + sVar
    '                        If roots(i).IsReal Then
    '                            Dim r1 As New Rational(roots(i).pRe)
    '                            If r1.Sgn = -1 Then
    '                                sDetail += "+" + (-r1).toString(cfg) + ")"
    '                            ElseIf r1.Sgn = 0 Then
    '                                sDetail += ")"
    '                            Else
    '                                sDetail += "-" + r1.toString(cfg) + ")"
    '                            End If
    '                        Else
    '                            sDetail += "-" + (-roots(i)).ToStringComplex(cfg) + ")"
    '                            sDetail = Replace(sDetail, "--", "+")
    '                        End If
    '                        If j > 1 Then
    '                            sDetail += "^" + j.ToString
    '                        End If
    '                        If i < roots.Length - 1 OrElse
    '                        j < rMultiplicity(i) Then
    '                            sDetail += " +"
    '                        End If
    '                        k += 1
    '                    Next
    '                Next
    '                cfg.oDetail.AddAlways(sDetail)
    '                sDetail = ""
    '            End If
    '            k = 0

    '            ' Obtain polynomial equations for the system of
    '            ' equations (later on 'soe') to resolve:
    '            For i = 0 To roots.Length - 1

    '                ' equating P/Q = _A1/(x-r1) + _A2/(x-r2)^2 + ...+ _B1/(x-r2) + ...
    '                ' if m1,m2, ...,mn are the multiplicity of each root r1,r2,..,rn
    '                ' operating gives:
    '                ' P = 
    '                '   _A1*(x-r1)^(m1-1)*(x-r2)^m2*...*(x-Rn)^mn 
    '                ' + _A2*r1^(m1-2)* "     "   "    "
    '                ' + ...
    '                ' + _Am1 *1*(x- r2)^m2 * (x- r3)^m3 *... * (x- Rn)^mn
    '                ' + _B1....
    '                '.....
    '                ' + _J1*(x-r1)^m1*(x-r2)^m2* .... * (x-Rj )^ (mj-1) * (x-Rj+1) ^ Mj+1 * ... * (x-Rn)^mn
    '                ' + _J1     "         "              * (x-Rj) ^ (mj-2) * (x-Rj+1) ^ Mj+1 *.....
    '                '....
    '                ' + _J1     "         "              * (x-Rj) ^1         * (x-Rj+1) ^ Mj+1 *.....
    '                ' + _J1*   "          "          ....*      1         * (x-Rj+1) ^ Mj+1

    '                For m = rMultiplicity(i) To 1 Step -1 ' 2013/09/04 intercambiados los limites de asc. a descendente
    '                    Dim pOthers As New Polynomial(pA(k))
    '                    k += 1
    '                    For j = 0 To roots.Length - 1
    '                        If j <> i Then
    '                            For j1 = 1 To rMultiplicity(j)
    '                                pOthers *= (x - New Polynomial(roots(j)))
    '                            Next
    '                        End If
    '                    Next
    '                    If m = 1 Then
    '                        eqP += pOthers
    '                    Else
    '                        Dim pCur As New Polynomial(1.0)
    '                        For i1 As Int64 = 1 To m - 1
    '                            pCur *= (x - New Polynomial(roots(i)))
    '                        Next
    '                        eqP += pCur * pOthers
    '                    End If
    '                Next
    '            Next
    '            ' The above eq. evaluated at Qn points gives a
    '            ' system of Qn eqs. and Qn vars. (_A1, _A2, ..., _B1, ...)
    '            eqP -= R

    '            Dim oVars As New VarsAndFns(cfg)
    '            Dim msg As String = ""
    '            oVars.AddVar(sVar, Nothing, msg)
    '            Dim k2 As Int64 = k
    '            k = 0
    '            For i = 0 To rootsMtx.vVect.Length - 1
    '                Dim curPto As Int64 = 1
    '                If k < roots.Length Then
    '                    oVars.setValue(0, New ExprMatrix(roots(k)))
    '                Else
    '                    oVars.setValue(0, New ExprMatrix(
    '                        roots(roots.Length - 1) + New Complex(curPto)))
    '                    curPto += 1
    '                End If
    'otroPto:
    '                Dim evalP As Polynomial =
    '                    eqP.evalMultiCjoToPolynomial(oVars)
    '                eqMtx.vVect(k) = New Vector(evalP)
    '                For j = 0 To k - 1
    '                    Dim diff As Polynomial =
    '                        eqMtx.vVect(k).vPoly(0) -
    '                        eqMtx.vVect(j).vPoly(0)
    '                    If diff.isReal AndAlso
    '                    diff.cf(0).pRe.ToDouble = 0 Then
    '                        ' el polinimio en curso es una ec. repetida
    '                        ' reintentar calc. f(x) en otro pto. 
    '                        ' para poder obtener un sistema de ecs.
    '                        ' independientes:
    '                        oVars.setValue(0, New ExprMatrix(
    '                            roots(roots.Length - 1) + New Complex(curPto)))
    '                        curPto += 1
    '                        GoTo otroPto
    '                    End If
    '                Next
    '                k += 1
    '            Next
    '            ' if there was any multiplicity k2 will be < nQ:
    '            j = k2
    '            Dim rExtra As New Complex(0.5)
    '            For i = j To nQ - 1
    '                For k1 As Int64 = 0 To roots.Length - 1
    '                    If (rExtra - roots(k1)).esCero Then
    '                        rExtra *= rExtra
    '                    End If
    '                Next
    '                oVars.setValue(0, New ExprMatrix(rExtra))
    '                Dim evalP As Polynomial =
    '                    eqP.evalMultiCjoToPolynomial(oVars)
    '                eqMtx.vVect(k) = New Vector(evalP)
    '                k += 1
    '                rExtra *= rExtra
    '            Next
    '            ' Solve the system
    '            Dim soe As New SystemOfEquations(eqMtx, Nothing, eqVars, Nothing, cfg)
    '            soe.resolveSysOfEqs(cfg)
    '            If bDetail Then
    '                cfg.oDetail.AddAlways(" => ")
    '                soe.ToStringInfo(cfg)
    '                For i = 0 To soe.sSolutionVars.Length - 1
    '                    cfg.oDetail.Add(soe.sSolutionVars(i)(0) + " = " +
    '                                    soe.sSolutionVars(i)(1))
    '                Next
    '            End If

    '            ' Consider particular case integral(1/(x-r)^n) = n * ln(x-r),
    '            ' then there will be at least one var=zero:
    '            For i = 0 To soe.Mresult.vVect.Length - 1
    '                If soe.Mresult.vVect(i).vPoly(0).isReal AndAlso
    '                soe.Mresult.vVect(i).vPoly(0).ToDouble = 0.0 Then
    '                    retExpr = New Expression(rMultiplicity(0)) *
    '                                    Expression.AddFnAndArg0("ln",
    '                                    New Expression(x -
    '                                    New Polynomial(roots(0))))
    '                    Exit Try
    '                End If
    '            Next
    '            If cfg.bRounding Then
    '                For i = 0 To soe.resultValues.Length - 1
    '                    With soe.resultValues(i)
    '                        If Math.Abs(.pRe.ToDouble) < 10 ^ -3 Then
    '                            .pRe = New Rational(0.0)
    '                        End If
    '                        If Math.Abs(.pIm.ToDouble) < 10 ^ -3 Then
    '                            .pIm = New Rational(0.0)
    '                        End If
    '                    End With
    '                Next
    '            End If
    '            If bDetail Then
    '                cfg.oDetail.AddAlways(" => ")
    '                soe.ToStringInfo(cfg)
    '                For i = 0 To soe.sSolutionVars.Length - 1
    '                    cfg.oDetail.Add(soe.sSolutionVars(i)(0) + " = " +
    '                                    soe.sSolutionVars(i)(1))
    '                Next
    '            End If
    '            retExpr = New Expression(0.0)
    '            k = 0
    '            For i As Int64 = 0 To roots.Length - 1
    '                If rMultiplicity(i) = 1 Then
    '                    Dim mult As New Expression(
    '                        soe.resultValues(k))
    '                    If Not mult.IsReal AndAlso
    '                    k < soe.resultValues.Length - 1 AndAlso
    '                    (soe.resultValues(k) + soe.resultValues(k + 1)).IsZero Then
    '                        Dim arg As Expression =
    '                            New Expression(x -
    '                            New Polynomial(-roots(i)))
    '                        arg /= New Expression(x -
    '                            New Polynomial(roots(i)))

    '                        retExpr += -mult * (
    '                            Expression.AddFnAndArg0("ln",
    '                             arg))
    '                        k += 1
    '                        i += 1
    '                    Else
    '                        retExpr += mult * (
    '                            Expression.AddFnAndArg0("ln",
    '                            New Expression(x -
    '                            New Polynomial(roots(i)))))
    '                    End If

    '                    k += 1
    '                Else
    '                    For m As Int64 = 1 To rMultiplicity(i) 'To 1 Step -1
    '                        If m = 1 Then
    '                            Dim mult As New Expression(
    '                                soe.resultValues(k))
    '                            retExpr += mult * (
    '                                Expression.AddFnAndArg0("ln",
    '                                New Expression(x -
    '                                New Polynomial(roots(i)))))
    '                        Else
    '                            ' 2013/09/04:
    '                            ' D(x-a)^-n = -n(x-a)^(-n-1) =>
    '                            ' => (x-a)^-n = integral( -n(x-a)^(-n-1) )
    '                            ' => (-1/n)(x-a)^-n = integral( (x-a)^(-n-1) )
    '                            ' if -m = -n-1 => m = n+1, m-1=n, -n = -m+1
    '                            ' => (-1/(m-1))*(x-a)^(-m+1) = integral( (x-a)^-m )
    '                            ' => (-1/(m-1))/(x-a)^(m-1) = integral( (x-a)^-m )

    '                            Dim mult As New Expression(
    '                                soe.resultValues(k / (m - 1)))
    '                            retExpr -= mult * (
    '                               New Expression(
    '                               New Polynomial(1.0) /
    '                               (x - New Polynomial(roots(i))) ^
    '                               New Polynomial(m - 1)))
    '                        End If
    '                        k += 1
    '                    Next
    '                End If
    '            Next
    '        Catch ex As Exception
    '            throw
    '        Finally
    '            'cfg.oDetail = oDetail
    '            cfg.bDetail = bDetail
    '        End Try
    '        Return retExpr
    '    End Function
#End Region

End Class
