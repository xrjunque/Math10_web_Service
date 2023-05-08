Partial Public Class Expression

#Region "By parts integral"
    Public Function IntegrationByParts( _
                            ByVal cfg1 As Config, _
                            ByVal IRespVar As String, _
                            ByVal vExprParams() As Expression, _
                            ByVal cur As currentMatch, _
                            ByVal vars As VarsAndFns, _
                            ByRef result As Expression, _
                            ByRef origAntideriv() As Expression, _
                            ByRef bFoundOrigAntideriv() As Boolean, _
                            ByRef vIndex() As Int64, _
                            Optional ByRef sDetail() As String = Nothing, _
                            Optional ByRef cnt As Int64 = 0 _
                            ) As Boolean
        Static enUsoPorPartes As Boolean = False
        cnt += 1
        Dim iln As Int64 = 0
        Dim bRet As Boolean = False
        Dim mult As Double = 1.0
        If enUsoPorPartes Then
            Return False
        End If
        Dim bFound As Boolean = False
        Dim rup As New ReduceExprUsingPolynomials
        Dim factWithOutVarResp As New Expression(1.0)
        Dim bDetail As Boolean = cfg.bDetail
        Dim e1 As String = String.Empty
        Try
            cfg.bDetail = False
            'Dim cfg As New Config ' cfg.bdetail=false => no detail
            cfg = cfg1 ' 2014/06/21

            enUsoPorPartes = True
            ' D( u * v ) =  u' v  +   u  v'
            '∫D( u * v ) = ∫u' v  + ∫ u  v'
            '    u * v   = ∫u' v  + ∫ u  v'
            ' if I = ∫u' v (or I = ∫ u  v')
            ' I = u * v - ∫ u  v' (1), if last integral is known
            ' then we can resolve
            ' (or I = ∫ u  v' => I = u * v - ∫ u'  v , the
            ' same stands for ∫ u'  v: if known, we may
            ' resolve)
            Dim varId As Int64 = vars.getVarIDByName(IRespVar)
            Dim i As Int64
            Dim vIsDiv() As Boolean = Nothing
            Dim commExpr As Expression = Nothing
            Dim vFactors() As Expression = Nothing
            Dim nFactors As Int64 = 0
            Dim sIntegr As String = "_laIntegral"
            Dim eIntegr As Expression = New Expression( _
                    Polynomial.GetPolynomial(sIntegr))
            If bDetail AndAlso sDetail Is Nothing Then
                ReDim Preserve sDetail(-1)
            End If
            If origAntideriv IsNot Nothing Then
                ' I = u * v - ∫ u  v' = u * v ¿¿ - mult*I ??
                Dim mult2 As Double = 1.0
                Dim iln2 As Int64 = origAntideriv.Length - 1
                For iOrig As Int64 = 0 To iln2
                    If origAntideriv(iOrig) IsNot Nothing AndAlso _
                    Me.isEqualTo(origAntideriv(iOrig), mult2, "*") Then
                        bFoundOrigAntideriv(iln2) = True
                        result = New Expression(mult2) * _
                            New Expression( _
                            Polynomial.GetPolynomial(sIntegr))
                        origAntideriv(iln2) = result
                        bRet = True
                        Exit Try
                    End If
                Next
            End If
            Dim Expr As Expression = Nothing
            If getCommonFactor(cfg, commExpr, Expr) Then
                ReDim vFactors(0)
                vFactors(0) = Expr
                Dim bIsDiv() As Boolean = Nothing
                Dim vFactors2() = commExpr.exprToFactors(bIsDiv)
                ReDim Preserve vFactors(vFactors2.Length)
                For i = 0 To vFactors2.Length - 1
                    If bIsDiv(i) Then
                        vFactors(i + 1) = New Expression(1.0) / _
                            vFactors2(i)
                    Else
                        vFactors(i + 1) = vFactors2(i)
                    End If
                Next
            Else
                Dim bIsDiv() As Boolean = Nothing
                vFactors = Me.exprToFactors(bIsDiv)
                'For i = 0 To vFactors.Length - 1
                '    If bIsDiv(i) Then
                '        vFactors(i) = New Expression(1.0) / _
                '            vFactors(i)
                '    End If
                'Next
                Dim vFact2(-1) As Expression, iv As Int64 = 0, bIsDiv2(-1) As Boolean
                For j As Int64 = 0 To vFactors.Length - 1
                    Dim vVar(-1) As String
                    vFactors(j).getAllVars(vVar)
                    If vVar IsNot Nothing AndAlso _
                    vVar.Length AndAlso Array.IndexOf(vVar, IRespVar) > -1 Then
                        ReDim Preserve vFact2(iv), bIsDiv2(iv)
                        If bIsDiv(j) Then
                            vFact2(iv) = New Expression(1.0) / vFactors(j)
                        Else
                            vFact2(iv) = vFactors(j)
                        End If
                        iv += 1
                    Else
                        If bIsDiv(j) Then
                            factWithOutVarResp /= vFactors(j)
                        Else
                            factWithOutVarResp *= vFactors(j)
                        End If
                    End If
                Next
                vFactors = vFact2
                bIsDiv = bIsDiv2
            End If
            If nFactors > 4 Then
                ' limitamos a 4 factores
                Exit Try
            End If
            Dim bResolved As Boolean = False
            For i = 0 To vFactors.Length - 1
                If vFactors(i).IsReal Then
                    mult *= vFactors(i).pA.cf(0).pRe.ToDouble
                End If
            Next

            If origAntideriv Is Nothing Then
                ReDim origAntideriv(0), bFoundOrigAntideriv(0)
            Else
                If bFoundOrigAntideriv IsNot Nothing Then
                    iln = bFoundOrigAntideriv.Length
                End If
                If iln > 5 Then
                    ' limitamos a máximo varios niveles
                    ' de recursividad
                    bRet = False
                    Exit Try
                    'Throw New Exception(msg8.num(13)) ' n/a 
                End If
                ReDim Preserve origAntideriv(iln), _
                    bFoundOrigAntideriv(iln)
            End If
            If vIndex Is Nothing OrElse vIndex.Length = 0 Then
                ReDim Preserve vIndex(0)
                vIndex(0) = 0
            ElseIf iln >= vIndex.Length Then
                ReDim Preserve vIndex(iln)
                vIndex(iln) = 0
            End If
            For i = vIndex(iln) To vFactors.Length - 1
                'If i <= vIndex(iln) Then
                '    i = vIndex(iln) + 1
                'End If
reIntento:
                If i >= vFactors.Length Then
                    Exit For
                End If
                If vFactors(i).IsReal Then
                    i += 1
                    vIndex(iln) = i
                    GoTo reIntento
                Else
                    If Not bResolved Then
                        ' Let's set I= ∫u dv
                        Dim u As Expression = _
                             New Expression(vFactors(i))
                        Dim dv As Expression = Me / u / New Expression(mult)
                        dv = rup.ReduceUsingPolynomials(dv)
                        'dv = rup.ReduceUsingPolynomials(dv)

                        ' then I = u*v - ∫ v du
                        Dim du As Expression = _
                            u.opDeriv(IRespVar)
                        du = rup.ReduceUsingPolynomials(du)
                        Dim v As Expression = Nothing
                        If dv.IsPolynomial Then
                            v = opIntegralDefOrPolinFraction(cfg, varId, _
                                    New Expression() {dv}, cur, vars, cnt)
                        ElseIf Not dv.InmediateIntegrals(cfg, IRespVar, varId, vars, v, cur, cnt:=cnt) Then
                            vIndex(iln) = i
                            GoTo sigI
                        End If


                        Dim vdu As Expression = v * du
                        vdu = rup.ReduceUsingPolynomials(vdu)
                        vExprParams(0) = vdu
                        origAntideriv(iln) = Me
                        'Trace.WriteLine( _
                        '    "iln=" + iln.ToString + _
                        '    " i=" + i.ToString + _
                        '    " me=" + Me.ToStringExpr + _
                        '    " mult=" + mult.ToString + _
                        '    " u=" + u.ToStringExpr)
                        If bDetail Then
                            e1 = String.Empty
                            If vIndex.Length = 1 Then
                                e1 += "I= "
                            End If
                            e1 += "∫ udv = u*v - ∫ v du" + vbCrLf
                            e1 += "{u= " + vFactors(i).ToStringExpr(cfg) + " dv=" + dv.ToStringExpr(cfg) + "}" + vbCrLf
                            e1 += "{du= " + du.ToStringExpr(cfg) + " v=" + v.ToStringExpr(cfg) + "}" + vbCrLf
                            e1 += "= " + (u * v).ToStringExpr(cfg) + "-∫(" + (v * du).ToStringExpr(cfg) + ")" + vbCrLf
                            'e1 += " = " + vFactors(i).ToStringExpr(cfg) + "*" + v.ToStringExpr(cfg) + vbCrLf
                        End If
                        Dim dblInt As Double = 1.0
                        Dim integr_vdu As Expression = Nothing
                        Dim oldiln As Int64 = iln
                        cfg.bDetail = bDetail
                        If bDetail Then
                            ReDim Preserve sDetail(iln)
                            sDetail(iln) = e1
                        End If
                        integr_vdu = vdu.opAntiDerivative( _
                             cfg, varId, vExprParams, cur, vars, IRespVar, _
                             origAntideriv, bFoundOrigAntideriv, vIndex, sDetail)
                        'End If
                        'Trace.WriteLine("uv=" + (u * v).ToStringExpr + _
                        '                " mult=" + mult.ToString + _
                        '                 "   ∫ vdu=" + integr_vdu.ToStringExpr)
                        'If bDetail Then
                        '    e1 += "∫ vdu= ∫" + integr_vdu.ToStringExpr(cfg) + vbCrLf
                        '    e1 = Replace(e1, "_laIntegral", " I")
                        '    ReDim Preserve sDetail(iln)
                        '    sDetail(i) = e1

                        'End If
                        'If bDetail Then
                        '    e1 += "= " + origAntideriv(iln).ToStringExpr(cfg)
                        '    e1 = Replace(e1, "_laIntegral", " I")
                        '    sDetail(vIndex(iln)) = e1
                        'End If
                        vIndex(iln) = i
                        'If integr_vdu IsNot Nothing Then
                        If bFoundOrigAntideriv(iln) Then
                            bFound = True
                            ' I = u * v - mult*I
                            ' I = u*v/(1+mult) 
                            Dim oVar As New VarsAndFns(cfg1)
                            Dim exprIntegr As New Expression( _
                                Polynomial.GetPolynomial(sIntegr))
                            oVar.AddVar(sIntegr, New Expression(1.0))
                            If iln > 0 Then
                                bFoundOrigAntideriv(iln) = False
                                bFoundOrigAntideriv(iln - 1) = True
                                origAntideriv(iln - 1) = _
                                    New Expression(mult) * _
                                    (u * v - integr_vdu)
                                'result = rup.ReduceUsingPolynomials(origAntideriv(iln - 1))
                                If iln = 1 Then
                                    Dim vIsMnS(-1) As Boolean
                                    Dim vS() As Expression = origAntideriv(iln - 1).exprToSummands(vIsMnS)
                                    For ks As Int64 = 0 To vS.Length - 1
                                        Dim subs As Expression = vS(ks) + eIntegr
                                        If subs.IsReal AndAlso subs.toDouble = 0.0 Then
                                            vS(ks) = New Expression(-vExprParams(0))
                                            origAntideriv(iln - 1) = Expression.summandsToExpr(vS, vIsMnS)
                                            Exit For
                                        End If
                                    Next
                                End If
                                result = origAntideriv(iln - 1)
                                iln -= 1
                                ReDim Preserve bFoundOrigAntideriv(iln)
                                bRet = True
                                Exit Try
                            Else
                                origAntideriv(0) = _
                                    New Expression(mult) * _
                                    (u * v - integr_vdu)
                                Dim exprResult As Expression = origAntideriv(0) - exprIntegr
                                'exprResult.cfg.cur = cur
                                Dim vExprR(-1) As Expression

                                ' 2017/05/03:
                                If exprResult.tryToIsolateToExpression( _
                                   sIntegr, vExprR) AndAlso _
                                   Not (vExprR(0).IsReal AndAlso vExprR(0).toDouble = 0.0) Then
                                    result = vExprR(0)
                                    If bDetail Then
                                        For isd As Int64 = 0 To sDetail.Length - 1
                                            cfg.oDetail.AddAlways(sDetail(isd))
                                        Next
                                    End If
                                    bResolved = True
                                    Exit Try
                                Else
                                    bFound = False
                                    bResolved = False
                                    ' resulta I = U*v - .... + I =>
                                    ' I - I = 0 = u*v - ∫ ... =>
                                    ' => la sustitución no es válida y, así,
                                    ' intentaremos con otros factores,

                                    ' el reintento no será con los
                                    ' mismos elementos de vFact() para
                                    ' un valor de iln mayor
                                    ' porque vIndex() contiene
                                    ' los ya probados y
                                    ' continuará con los siguientes.
                                    vIndex(vIndex.Length - 1) += 1
                                    GoTo reIntento
                                End If
                                ' EndOf 2017/05/03

                                'Dim vIsMinusI() As Boolean = Nothing
                                'Dim vSum() As Expression = origAntideriv(0).exprToSummands(vIsMinusI)
                                'Dim vSum2(vSum.Length - 2) As Expression
                                'Dim vMinus2(vSum.Length - 2) As Boolean
                                'Dim k As Int64 = 0
                                'Dim mult2 As Double = 0.0
                                'Dim factWithOutVarResp2 As New Expression(1.0)

                                'For iSum As Int64 = 0 To vSum.Length - 1
                                '    If vSum(i).getMatchStr = "/" Then
                                '        Dim vVarAll() As String = Nothing
                                '        vSum(iSum).getAllVars(vVarAll)
                                '        Dim div As Expression = vSum(iSum).Args(1)
                                '        If Array.IndexOf(vVarAll, sIntegr) > -1 Then
                                '            For j As Int64 = 0 To vSum.Length - 1
                                '                vSum(i) *= div
                                '            Next
                                '            factWithOutVarResp2 /= div
                                '            Exit For
                                '        End If
                                '    End If
                                'Next
                                'For iSum As Int64 = 0 To vSum.Length - 1
                                '    Dim vVarAll() As String = Nothing
                                '    vSum(iSum).getAllVars(vVarAll)
                                '    If vSum(iSum).IsPolynomial AndAlso _
                                '   Array.IndexOf(vVarAll, sIntegr) > -1 Then
                                '        Dim bIsDiv() As Boolean = Nothing
                                '        Dim vFact() As Expression = vSum(iSum).exprToFactors(bIsDiv)
                                '        vSum(iSum) = New Expression(1.0)
                                '        Dim bFound2 As Boolean = False
                                '        For j As Int64 = 0 To vFact.Length - 1
                                '            Dim vVar(-1) As String
                                '            vFact(j).getAllVars(vVar)
                                '            If vVar IsNot Nothing AndAlso _
                                '            vVar.Length AndAlso Array.IndexOf(vVar, IRespVar) > -1 Then
                                '                If bIsDiv(j) Then
                                '                    vSum(iSum) /= vFact(j)
                                '                Else
                                '                    vSum(iSum) *= vFact(j)
                                '                End If
                                '            Else
                                '                bFound2 = True
                                '                If bIsDiv(j) Then
                                '                    factWithOutVarResp2 /= vFact(j)
                                '                Else
                                '                    factWithOutVarResp2 *= vFact(j)
                                '                End If
                                '            End If
                                '        Next
                                '        If bFound2 Then
                                '            factWithOutVarResp2 /= _
                                '                New Expression(Polynomial.GetPolynomial(sIntegr))
                                '        End If
                                '    ElseIf k < vSum2.Length Then
                                '        vSum2(k) = vSum(iSum)
                                '        vMinus2(k) = vIsMinusI(iSum)
                                '        k += 1
                                '    Else
                                '        GoTo sigI
                                '    End If
                                'Next
                                'If factWithOutVarResp2.IsReal Then
                                '    mult2 = factWithOutVarResp2.toDouble
                                'End If
                                'If Math.Round(mult2 * mult, 5) = 1 Then
                                '    ' resulta I = U*v - .... + I =>
                                '    ' I - I = 0 = u*v - ∫ ... =>
                                '    ' => la sustitución no es válida y, así,
                                '    ' intentaremos con otros factores,
                                '    bFound = False
                                '    bResolved = False
                                '    'If iln AndAlso iln < vIndex.Length - 1 Then
                                '    ' el reintento no será con los
                                '    ' mismos elementos de vFact() para
                                '    ' un valor de iln mayor
                                '    ' porque vIndex() contiene
                                '    ' los ya probados y
                                '    ' continuará con los siguientes.
                                '    vIndex(vIndex.Length - 1) += 1
                                '    GoTo reIntento
                                'Else
                                '    If bDetail Then
                                '        For isd As Int64 = 0 To sDetail.Length - 1
                                '            cfg.oDetail.AddAlways(sDetail(isd))
                                '        Next
                                '    End If
                                '    Dim resto As Expression = _
                                '        summandsToExpr(vSum2, vMinus2)
                                '    'result = resto / _
                                '    '    New Expression(1.0 - mult2)
                                '    result = resto / (New Expression(1.0) - factWithOutVarResp2)
                                '    bResolved = True
                                '    Exit Try
                                'End If
                            End If
                        ElseIf integr_vdu Is Nothing Then
                            Exit Try
                        Else
                            result = (u * v - integr_vdu).reduceFactors(False)
                            result = rup.ReduceUsingPolynomials(result)
                            bResolved = True
                            Exit Try
                        End If
                    End If
                End If
                vIndex(iln) = i + 1
sigI:
            Next
            If bResolved Then
                ReDim vExprParams(-1), origAntideriv(-1), bFoundOrigAntideriv(-1)
                cnt = 0
                bRet = True
            End If
        Catch ex As Exception
            cfg.bDetail = bDetail
            Return False ' n/a 
        Finally
            If Not bFound AndAlso _
            bRet AndAlso Math.Abs(mult) <> 1.0 Then
                result = New Expression(mult) * factWithOutVarResp * result
                'result = rup.ReduceUsingPolynomials(result)
            End If
            enUsoPorPartes = False
            cfg.bDetail = bDetail
        End Try
        Return bRet
    End Function

#End Region

End Class
