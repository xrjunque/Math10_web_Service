Partial Public Class Polynomial84
    Public Function opIntegral(Optional ByVal sVar As String = "") As Polynomial84
        Try
            'If var.Length < 2 Then
            If PolyResto Is Nothing AndAlso getDegree() > -1 Then
                Return opIntegralNoFractions(sVar)
            Else
                Throw New Exception(
                msg884.num(13)) ' n/a
            End If
            'Else
            'Throw New Exception( _
            'msg884.num(13)) ' n/a
            'End If
        Catch ex As Exception
            Throw New Exception(ex.Message)
        End Try
        Return Nothing
    End Function
    Public Function opIntegralNoFractions(Optional ByVal sVar As String = "") As Polynomial84
        Dim polyC As Polynomial84 = Nothing
        Try
            Dim nVar As Int32 = 0
            If Len(sVar) Then
                nVar = Array.IndexOf(Me.var, sVar)
                If nVar = -1 Then
                    nVar = Me.var.Length
                    ReDim Preserve Me.var1(nVar)
                    Me.var1(nVar) = sVar
                End If
            End If
            polyC = New Polynomial84(Me)
            Dim i As Int32
            For i = 0 To Me.cf.Length - 1
                If nVar >= polyC.exp(i).Length Then
                    ReDim Preserve polyC.exp(i)(nVar)
                Else
                    polyC.cf(i) /= (Me.exp(i)(nVar) + 1)
                End If
                polyC.exp(i)(nVar) += 1
                If polyC.exp(i)(nVar) < 0 Then
                    polyC.exp(i)(nVar) = 0
                    polyC.cf(i) = New Complex84(0.0)
                End If
            Next
            polyC.opReduceCommonExponents()
        Catch ex As Exception
            Throw New Exception(ex.Message)
        End Try
        Return polyC
    End Function
    Public Function opIntegralRationalFractions(
                    cfg As Config84,
                    Optional ByVal sVar As String = "") As Expression
        Dim polyC As Polynomial84 = Nothing
        Dim retExpr As Expression = Nothing
        'Dim oDetail As Detall84 = cfg.oDetail
        Dim bDetail As Boolean = cfg.bDetail
        Try
            'Dim nVar As Int32 = 0
            'cfg.oDetail = Nothing
            cfg.bDetail = False
            If sVar = "" Then
                If Me.var.Length Then
                    sVar = Me.var(0)
                ElseIf Me.PolyResto IsNot Nothing Then
                    If Me.PolyResto.var.Length Then
                        sVar = Me.PolyResto.var(0)
                    Else
                        sVar = Me.PolyDivisor.var(0)
                    End If
                End If
            End If
            'If Len(sVar) Then
            '    nVar = Array.IndexOf(Me.var, sVar)
            '    If nVar = -1 Then
            '        nVar = Me.var.Length
            '        ReDim Preserve Me.var1(nVar)
            '        Me.var1(nVar) = sVar
            '    End If
            'End If
            Dim R, Q As Polynomial84
            If Me.PolyResto Is Nothing Then
                polyC = opIntegralNoFractions(sVar)
                Exit Try
            End If
            R = New Polynomial84(Me.PolyResto)
            Q = New Polynomial84(Me.PolyDivisor)
            If R.getDegree > 15 OrElse
            Q.getDegree > 18 Then
                Throw New Exception(msg884.num(13)) ' n/a 
            End If
            polyC = New Polynomial84(Me)
            polyC.PolyResto = Nothing
            polyC.PolyDivisor = Nothing
            If Q.An.pRe.ToDouble <> 1.0 Then
                R = R / Q.An
                Q = Q / Q.An
            End If
            Dim Va As New Vector(Q)
            Dim rootsMtx As Matrix =
                Polynomial84.opRoots(Va, False, cfg).mtx
            'If cfg.bRounding OrElse _
            ' MathGlobal8.bSaveRounding Then
            '    For i = 0 To rootsMtx.vVect.Length - 1
            '        Dim re As Double = rootsMtx.vVect(i).vPoly(0).cf(0).pRe.ToDouble
            '        Dim im As Double = rootsMtx.vVect(i).vPoly(0).cf(0).pIm.ToDouble
            '        If Math.Abs(re) > 0.5 AndAlso Math.Round(im, 3) = 0 Then
            '            rootsMtx.vVect(i).vPoly(0).cf(0).pIm = New Precis84(0.0)
            '            rootsMtx.vVect(i).vPoly(0).cf(0).pRe = New Precis84(Math.Round(re, 3))
            '        End If
            '        If Math.Abs(im) > 0.5 AndAlso Math.Round(re, 3) = 0 Then
            '            rootsMtx.vVect(i).vPoly(0).cf(0).pRe = New Precis84(0.0)
            '            rootsMtx.vVect(i).vPoly(0).cf(0).pIm = New Precis84(Math.Round(im, 3))
            '        End If
            '    Next
            'End If
            'Dim rootsV As Vector = rootsMtx.vVect(0)
            Dim nQ As Int32 = Q.getDegree
            Dim roots(nQ - 1) As Complex84
            Dim rMultiplicity(nQ - 1) As Int32
            roots(0) = New Complex84(rootsMtx.vVect(0).vPoly(0).cf(0))
            rMultiplicity(0) = 1
            Dim j As Int32 = 0
            Dim k As Int32 = 0

            ' Set roots' multiplicity in rMultiplicity():
            For i As Int32 = 1 To nQ - 1
                For k = 0 To j
                    If (rootsMtx.vVect(i).vPoly(0).cf(0) - roots(k)).esCero Then
                        Exit For
                    End If
                Next
                If k < j + 1 Then
                    rMultiplicity(k) += 1
                Else
                    j += 1
                    roots(j) = New Complex84(rootsMtx.vVect(i).vPoly(0).cf(0))
                    rMultiplicity(j) = 1
                End If
            Next
            ReDim Preserve rMultiplicity(j), roots(j)
            Dim sA(nQ - 1) As String
            Dim pA(nQ - 1) As Polynomial84
            Dim eqMtx As New Matrix
            Dim eqVars As New VarsAndFns(cfg)
            eqMtx.setRowAndCol(nQ - 1, nQ - 1)
            k = 0
            Dim x As Polynomial84 = Polynomial84.GetPolyomial(sVar)
            Dim sMsg As String = ""

            ' generate vars' names and add to eqVars object:
            ' 1/(xn+...) = _A1/(..) + ...
            For i = 0 To roots.Length - 1
                For j = 1 To rMultiplicity(i)
                    ' _A1 _A2 ... _B1 _B2 ..
                    sA(k) = "_" + Chr(&H41 + i) + Chr(&H30 + j)
                    pA(k) = Polynomial84.GetPolyomial(sA(k))
                    eqVars.AddVar(sA(k), Nothing, sMsg)
                    k += 1
                Next
            Next
            Dim eqP As New Polynomial84(0.0)
            k = 0
            Dim sDetail As String = ""
            If bDetail Then
                ' if detailed info. required then
                ' set the fractions into sDetail:
                sDetail += Me.toStringPoly(cfg) + " = "
                For i = 0 To roots.Length - 1
                    For j = 1 To rMultiplicity(i)
                        sDetail += sA(k) + "/(" + sVar
                        If roots(i).pRe.ToDouble < 0 Then
                            sDetail += "+" + roots(i).toStringComplex(cfg) + ")"
                        ElseIf roots(i).pRe.ToDouble = 0 Then
                            sDetail += ")"
                        Else
                            sDetail += "-" + roots(i).toStringComplex(cfg) + ")"
                        End If
                        If j > 1 Then
                            sDetail += "^" + j.ToString
                        End If
                        If i < roots.Length - 1 OrElse
                        j < rMultiplicity(i) Then
                            sDetail += " +"
                        End If
                        k += 1
                    Next
                Next
                cfg.oDetail.AddAlways(sDetail)
                sDetail = ""
            End If
            k = 0

            ' Obtain Polynomial84 equations for the system of
            ' equations (later on 'soe') to resolve:
            For i = 0 To roots.Length - 1

                ' equating P/Q = _A1/(x-r1) + _A2/(x-r2)^2 + ...+ _B1/(x-r2) + ...
                ' if m1,m2, ...,mn are the multiplicity of each root r1,r2,..,rn
                ' operating gives:
                ' P = 
                '   _A1*(x-r1)^(m1-1)*(x-r2)^m2*...*(x-Rn)^mn 
                ' + _A2*r1^(m1-2)* "     "   "    "
                ' + ...
                ' + _Am1 *1*(x- r2)^m2 * (x- r3)^m3 *... * (x- Rn)^mn
                ' + _B1....
                '.....
                ' + _J1*(x-r1)^m1*(x-r2)^m2* .... * (x-Rj )^ (mj-1) * (x-Rj+1) ^ Mj+1 * ... * (x-Rn)^mn
                ' + _J1     "         "              * (x-Rj) ^ (mj-2) * (x-Rj+1) ^ Mj+1 *.....
                '....
                ' + _J1     "         "              * (x-Rj) ^1         * (x-Rj+1) ^ Mj+1 *.....
                ' + _J1*   "          "          ....*      1         * (x-Rj+1) ^ Mj+1

                For m = rMultiplicity(i) To 1 Step -1 ' 2013/09/04 intercambiados los limites de asc. a descendente
                    Dim pOthers As New Polynomial84(pA(k))
                    k += 1
                    For j = 0 To roots.Length - 1
                        If j <> i Then
                            For j1 = 1 To rMultiplicity(j)
                                pOthers *= (x - New Polynomial84(roots(j)))
                            Next
                        End If
                    Next
                    If m = 1 Then
                        eqP += pOthers
                    Else
                        Dim pCur As New Polynomial84(1.0)
                        For i1 As Int32 = 1 To m - 1
                            pCur *= (x - New Polynomial84(roots(i)))
                        Next
                        eqP += pCur * pOthers
                    End If
                Next
            Next
            ' The above eq. evaluated at Qn points gives a
            ' system of Qn eqs. and Qn vars. (_A1, _A2, ..., _B1, ...)
            eqP -= R

            Dim oVars As New VarsAndFns(cfg)
            Dim msg As String = ""
            oVars.AddVar(sVar, Nothing, msg)
            Dim k2 As Int32 = k
            k = 0
            For i = 0 To rootsMtx.vVect.Length - 1
                Dim curPto As Int32 = 1
                If k < roots.Length Then
                    oVars.setValue(0, New exprMatrix84(roots(k)))
                Else
                    oVars.setValue(0, New exprMatrix84(
                        roots(roots.Length - 1) + New Complex84(curPto)))
                    curPto += 1
                End If
otroPto:
                Dim evalP As Polynomial =
                    eqP.evalMultiCjoToPolynomial(oVars)
                eqMtx.vVect(k) = New Vector(evalP)
                For j = 0 To k - 1
                    Dim diff As Polynomial84 =
                        eqMtx.vVect(k).vPoly(0) -
                        eqMtx.vVect(j).vPoly(0)
                    If diff.isReal AndAlso
                    diff.cf(0).pRe.ToDouble = 0 Then
                        ' el polinimio en curso es una ec. repetida
                        ' reintentar calc. f(x) en otro pto. 
                        ' para poder obtener un sistema de ecs.
                        ' independientes:
                        oVars.setValue(0, New exprMatrix84(
                            roots(roots.Length - 1) + New Complex84(curPto)))
                        curPto += 1
                        GoTo otroPto
                    End If
                Next
                k += 1
            Next
            ' if there was any multiplicity k2 will be < nQ:
            j = k2
            Dim rExtra As New Complex84(0.5)
            For i = j To nQ - 1
                For k1 As Int32 = 0 To roots.Length - 1
                    If (rExtra - roots(k1)).esCero Then
                        rExtra *= rExtra
                    End If
                Next
                oVars.setValue(0, New exprMatrix(rExtra))
                Dim evalP As Polynomial =
                    eqP.evalMultiCjoToPolynomial(oVars)
                eqMtx.vVect(k) = New Vector(evalP)
                k += 1
                rExtra *= rExtra
            Next
            ' Solve the system
            Dim soe As New SystemOfEquations(eqMtx, Nothing, eqVars, Nothing, cfg)
            soe.resolveSysOfEqs(cfg, cfg)

            ' Consider particular case integral(1/(x-r)^n) = n * ln(x-r),
            ' then there will be a least a var=zero:
            For i = 0 To soe.Mresult.vVect.Length - 1
                If soe.Mresult.vVect(i).vPoly(0).ToDouble = 0.0 Then
                    retExpr = New Expression(rMultiplicity(0)) *
                                    Expression.AddFnAndArg0("ln",
                                    New Expression(x -
                                    New Polynomial84(roots(0))))
                    Exit Try
                End If
            Next
            If bDetail Then
                cfg.oDetail.AddAlways(" => ")
                soe.ToStringInfo(cfg)
                For i = 0 To soe.sSolutionVars.Length - 1
                    cfg.oDetail.Add(soe.sSolutionVars(i)(0) + " = " +
                                    soe.sSolutionVars(i)(1))
                Next
                ''For i = 0 To soe.Mresult.vVect.Length - 1
                ''    If i >= rMultiplicity.Length OrElse rMultiplicity(i) = 1 Then
                ''        cfg.oDetail.Add(sA(i) + " = " + soe.Mresult.vVect(i).toStrVect(cfg))
                ''    Else
                ''        For m As Int32 = 1 To rMultiplicity(i)
                ''            k = i + rMultiplicity(i) - m
                ''            cfg.oDetail.Add(sA(i + m - 1) + " = " + _
                ''                soe.Mresult.vVect( _
                ''                   k).toStrVect(cfg))
                ''        Next
                ''        i += rMultiplicity(i)
                ''    End If
                ''Next
            End If
            retExpr = New Expression(0.0)
            k = 0
            For i As Int32 = 0 To roots.Length - 1
                If rMultiplicity(i) = 1 Then
                    Dim mult As New Expression(
                        soe.resultValues(k))
                    retExpr += mult * (
                        Expression.AddFnAndArg0("ln",
                        New Expression(x -
                        New Polynomial84(roots(i)))))
                    k += 1
                Else
                    For m As Int32 = 1 To rMultiplicity(i) 'To 1 Step -1
                        If m = 1 Then
                            Dim mult As New Expression(
                                soe.resultValues(k))
                            retExpr += mult * (
                                Expression.AddFnAndArg0("ln",
                                New Expression(x -
                                New Polynomial84(roots(i)))))
                        Else
                            ' 2013/09/04:
                            ' D(x-a)^-n = -n(x-a)^(-n-1) =>
                            ' => (x-a)^-n = integral( -n(x-a)^(-n-1) )
                            ' => (-1/n)(x-a)^-n = integral( (x-a)^(-n-1) )
                            ' if -m = -n-1 => m = n+1, m-1=n, -n = -m+1
                            ' => (-1/(m-1))*(x-a)^(-m+1) = integral( (x-a)^-m )
                            ' => (-1/(m-1))/(x-a)^(m-1) = integral( (x-a)^-m )

                            Dim mult As New Expression(
                                soe.resultValues(k / (m - 1)))
                            retExpr -= mult * (
                               New Expression(
                               New Polynomial84(1.0) /
                               (x - New Polynomial84(roots(i))) ^
                               New Polynomial84(m - 1)))
                        End If
                        k += 1
                    Next
                End If
            Next
        Catch ex As Exception
            Throw New Exception(ex.Message)
        Finally
            'cfg.oDetail = oDetail
            cfg.bDetail = bDetail
        End Try
        Return retExpr
    End Function
End Class
