Imports System.Text.RegularExpressions
Imports System.Text

<Serializable()> _
Partial Public Class Polynomial
    Public Sub New()
        ReDim exp(0)
        exp(0) = New Int64() {0}
    End Sub

    Public cf(0) As Complex
    Public exp(0)() As Int64
    Friend var1(-1) As String
    Public PolyResto As Polynomial = Nothing
    Public PolyDivisor As Polynomial = Nothing
    Public Shared bDoGCD As Boolean = True
    Public roots As retOpRoots
    Friend dbMod As Double
    Dim cfg As Config = Config.cfg
    Public Sub New(ByVal Re As Double)
        cf(0) = New Complex(Re)
        ReDim exp(0)
        exp(0) = New Int64() {0}
    End Sub
    Public Sub New(ByVal Re As Double, ByVal Im As Double)
        cf(0) = New Complex(Re, Im)
        ReDim exp(0)
        exp(0) = New Int64() {0}
    End Sub
    Public Sub New(ByVal cjo As Complex)
        cf(0) = New Complex(cjo)
        ReDim exp(0)
        exp(0) = New Int64() {0}
    End Sub
    Public Sub New(ByVal polyA As Polynomial)
        copyToThis(polyA)
    End Sub

    'Public Shared Function parsePolynomial(ByVal sPoly As String) As Polynomial
    '    Dim retPoly As Polynomial = Nothing
    '    Try
    '        If sPoly Is Nothing OrElse _
    '        Trim(sPoly).Length = 0 Then
    '            Return Nothing
    '        End If
    '        Dim mtxP As New matrixParser
    '        mtxP.bCaseS = Config.cfg.bCaseSensitive
    '        mtxP.parse(sPoly, "")
    '        retPoly = mtxP.ret.exprMtx.getVector(0).vPoly(0)
    '    Catch ex As Exception
    '        Throw ex
    '    End Try
    '    Return retPoly
    'End Function
    'Public Shared Function tryParsePolynomial(cfg As Config, ByVal sPoly As String, ByRef result As Polynomial) As Boolean
    '    Try
    '        Dim expr As Expression = Expression.parseExpression(sPoly)

    '        Return True
    '    Catch ex As Exception
    '        Return False
    '    End Try
    '    Return False
    'End Function
    Public Function tryReducePolyResto(Optional bDetailed As Boolean = False) As Boolean
        Try
            If PolyResto Is Nothing Then
                Return False
            End If
            Dim degR As Int64 = PolyResto.getDegree
            Dim degD As Int64 = PolyDivisor.getDegree
            Dim bDetail As Boolean = cfg.bDetail
            cfg.bDetail = False
            Dim e1 As String = ""
            If bDetailed Then
                e1 += Me.toStringPoly(cfg) + " = "
            End If
            If degR > 0 AndAlso degD > 0 Then
                Dim cjo As Complex = Nothing
                If PolyResto.IsEqual(PolyDivisor, cjo) Then
                    Me.PolyResto = Nothing
                    Me.PolyDivisor = Nothing
                    Dim polyC = Me + New Polynomial(cjo)
                    Me.copyToThis(polyC)
                ElseIf PolyResto.varAll.Length = 1 AndAlso
                PolyDivisor.varAll.Length = 1 AndAlso
                PolyResto.varAll(0) = PolyDivisor.varAll(0) Then

                    Dim gcd As Polynomial =
                        opGcd(PolyResto, PolyDivisor)
                    If gcd IsNot Nothing AndAlso
                    gcd.getDegree > 0 Then
                        PolyResto /= gcd
                        PolyDivisor /= gcd
                        If bDetailed Then
                            Dim entero As New Polynomial(Me)
                            entero.PolyResto = Nothing
                            entero.PolyDivisor = Nothing
                            If Not (entero.isReal AndAlso entero.ToDouble = 0.0) Then
                                e1 += entero.toStringPoly(cfg) + "+"
                            End If
                            e1 += "(" + PolyResto.ToString + ")*(" + gcd.ToString + ")"
                            e1 += "/((" + PolyDivisor.ToString + ")*(" + gcd.ToString + "))"
                            cfg.oDetail.AddAlways(e1)
                        End If
                        degR = PolyResto.getDegree
                        degD = PolyDivisor.getDegree
                    End If
                ElseIf PolyDivisor.var1.Length = 1 Then
                    Dim vroots() As Complex = Polynomial.opRoots(PolyDivisor).cjo
                    Dim resto As New Polynomial(PolyResto)
                    Dim x As Polynomial = Polynomial.GetPolynomial(PolyDivisor.var1(0))
                    Dim bIsDiv As Boolean = False
                    For i As Int64 = 0 To vroots.Length - 1
                        Dim div As Polynomial = resto / (x - New Polynomial(vroots(i)))
                        If div.PolyResto Is Nothing Then
                            resto /= (x - New Polynomial(vroots(i)))
                            vroots(i) = Nothing
                            bIsDiv = True
                        End If
                    Next
                    If bIsDiv Then
                        PolyResto = resto
                        PolyDivisor = New Polynomial(1.0)
                        For i As Int64 = 0 To vroots.Length - 1
                            If vroots(i) IsNot Nothing Then
                                PolyDivisor *= (x - New Polynomial(vroots(i)))
                            End If
                        Next
                    End If
                Else
                    Dim div As Polynomial = PolyResto / PolyDivisor
                    If div.isComplex Then
                        PolyResto = Nothing
                        PolyDivisor = Nothing
                        Dim Pa As Polynomial = Me + div
                        copyToThis(Pa)
                    End If
                End If
            End If

            Me.opReduceCommonExponents()
            cfg.bDetail = bDetail
        Catch ex As Exception

        End Try
        Return False
    End Function
    Public Sub copyToThis(ByVal polyA As Polynomial)
        Dim polyC As New Polynomial
        polyC.cf = polyA.cf
        polyC = CopyFrom(polyA)
        Me.cf = polyC.cf
        If polyC.var1.Length Then
            ReDim Me.var1(polyC.var1.Length - 1)
            Array.Copy(polyC.var1, Me.var1, polyC.var1.Length)
        End If
        Me.exp = polyC.exp
        Me.dbMod = polyC.dbMod
        If polyA.PolyResto IsNot Nothing Then
            Me.PolyResto = CopyFrom(polyA.PolyResto)
            Me.PolyDivisor = CopyFrom(polyA.PolyDivisor)
        Else
            PolyResto = Nothing
            PolyDivisor = Nothing
        End If
        If polyA.roots IsNot Nothing Then
            roots = New retOpRoots(polyA.roots)
        End If
    End Sub
    Public Sub varReduce()
        Try
            Dim iVar As Int64 = 0
            Dim j As Int64
            Do While iVar < var.Length
                Dim bSuprVar As Boolean = True
                For i As Int64 = 0 To cf.Length - 1
                    If exp(i) IsNot Nothing AndAlso
                        iVar < exp(i).Length AndAlso
                        exp(i)(iVar) <> 0 Then
                        bSuprVar = False
                        Exit For
                    End If
                Next
                If bSuprVar Then
                    For i As Int64 = 0 To cf.Length - 1
                        If exp(i) IsNot Nothing Then
                            For j = iVar + 1 To exp(i).Length - 1
                                exp(i)(j - 1) = exp(i)(j)
                            Next
                            ReDim Preserve exp(i)(exp(i).Length - 2)
                        End If
                    Next
                    For j = iVar + 1 To var1.Length - 1
                        var1(j - 1) = var1(j)
                    Next
                    ReDim Preserve var1(var1.Length - 2)
                    iVar -= 1
                End If
                iVar += 1
            Loop
        Catch ex As Exception

        End Try
    End Sub
    Public ReadOnly Property varlen() As Int64
        Get
            Dim varU(-1) As String
            Dim iu As Int32 = 0
            If PolyResto IsNot Nothing Then
                var1 = Me.varAll
                varU = varUnion(PolyResto, PolyDivisor)
                iu = varU.Length
                For i = 0 To var1.Length - 1
                    Dim pos As Int64 = Array.IndexOf(varU, var1(i), 0, iu)
                    If pos = -1 Then
                        ' no está presente: copiar
                        ReDim Preserve varU(iu)
                        varU(iu) = var1(i)
                        iu += 1
                    End If
                Next
            End If
            Return iu
        End Get
    End Property
    Public ReadOnly Property varAll As String()
        Get
            Dim vVar(-1) As String
            Try
                Dim i, j As Int64
                For i = 0 To cf.Length - 1
                    If i < exp.Length Then
                        For j = 0 To exp(i).Length - 1
                            If exp(i)(j) Then
                                Dim pos As Int64 =
                                    Array.IndexOf(vVar, var1(j), 0, vVar.Length)
                                If pos = -1 Then
                                    ReDim Preserve vVar(vVar.Length)
                                    vVar(vVar.Length - 1) = var1(j)
                                End If
                            End If
                        Next
                    End If
                Next
                If PolyResto IsNot Nothing Then
                    Dim varCur() As String = PolyResto.varAll
                    If varCur.Length Then
                        For i = 0 To varCur.Length - 1
                            Dim pos As Int64 =
                                Array.IndexOf(vVar, varCur(i), 0, vVar.Length)
                            If pos = -1 Then
                                ReDim Preserve vVar(vVar.Length)
                                vVar(vVar.Length - 1) = varCur(i)
                            End If
                        Next
                    End If
                    varCur = PolyDivisor.varAll
                    If varCur.Length Then
                        For i = 0 To varCur.Length - 1
                            Dim pos As Int64 =
                                Array.IndexOf(vVar, varCur(i), 0, vVar.Length)
                            If pos = -1 Then
                                ReDim Preserve vVar(vVar.Length)
                                vVar(vVar.Length - 1) = varCur(i)
                            End If
                        Next
                    End If
                End If
            Catch ex As Exception
                Throw ex
            End Try
            Return vVar
        End Get
    End Property
    Public ReadOnly Property An() As Complex
        Get
            'Sort()
            opReduceCommonExponents()
            If cf Is Nothing OrElse cf.Length = 0 Then
                Return Nothing
            End If
            Return cf(0)
        End Get
    End Property
    Public Property dbModuloInOperations() As Double
        Get
            Return dbMod
        End Get
        Set(ByVal value As Double)
            If value <> 0.0 AndAlso value < 2 Then
                Exit Property
            End If
            If value <> dbMod Then
                dbMod = value
                opReduceCommonExponents() ' operates modulo
            End If
        End Set
    End Property
    Public Shared Function GetPolynomial(ByVal sVar As String) As Polynomial
        Dim polyC As New Polynomial
        Try
            polyC.var1 = New String() {sVar}
            polyC.exp = New Int64()() {New Int64() {1}}
            polyC.cf = New Complex() {New Complex(1.0)}
        Catch ex As Exception
            Throw ex
        End Try
        Return polyC
    End Function
    Public Shared Function GetPolyomial(ByVal sVar As String, ByVal dbl() As Double) As Polynomial
        Dim polyC As New Polynomial
        Try
            polyC.var1 = New String() {sVar}
            ReDim polyC.exp(dbl.Length - 1), polyC.cf(dbl.Length - 1)
            For i = 0 To dbl.Length - 1
                polyC.exp(i) = New Int64() {dbl.Length - 1 - i}
                polyC.cf(i) = New Complex(dbl(i))
            Next
        Catch ex As Exception
            Throw ex
        End Try
        Return polyC
    End Function
    Public Shared Function GetPolyomial(ByVal sVar As String, ByVal cf As Complex) As Polynomial
        Dim polyC As New Polynomial
        Try
            polyC.var1 = New String() {sVar}
            polyC.exp = New Int64()() {New Int64() {1}}
            polyC.cf = New Complex() {New Complex(cf)}
        Catch ex As Exception
            Throw ex
        End Try
        Return polyC
    End Function
    Public Function GetPolyFromTerm(ByVal iTerm As Int64,
                                  ByVal setVars1() As String) As Polynomial
        Dim polyC As New Polynomial
        Try
            If iTerm >= Me.exp.Length Then
                Return Nothing
            End If
            Dim bFound As Boolean = False
            ReDim polyC.cf(0), polyC.exp(0), polyC.exp(0)(Me.exp(iTerm).Length - 1)
            If polyC.exp(0).Length Then
                For i As Int64 = 0 To Me.exp(iTerm).Length - 1
                    Dim pos As Int64 = -1
                    If setVars1 Is Nothing Then
                        pos = i
                    Else
                        If i >= var.Length Then
                            pos = -1
                        Else
                            pos = Array.IndexOf(setVars1, var(i))
                        End If
                    End If
                    If pos > -1 AndAlso
                    pos < polyC.exp(0).Length Then
                        polyC.exp(0)(pos) = Me.exp(iTerm)(i)
                        bFound = True
                    End If
                Next
            End If
            If Not bFound Then
                If iTerm < cf.Length Then
                    polyC.cf(0) = New Complex(Me.cf(iTerm))
                    Exit Try
                End If
                Return Nothing
            End If
            If setVars1 Is Nothing Then
                polyC.setVars(Me.var)
            Else
                polyC.setVars(setVars1)
            End If
            polyC.cf(0) = New Complex(Me.cf(iTerm))
        Catch ex As Exception
            Throw ex
        End Try
        Return polyC
    End Function
    Public Function splitIntoTerms() As Polynomial()
        Dim vRet(-1) As Polynomial
        Dim sVar() As String = Nothing
        Try
            Dim i As Int64
            Dim cur As Polynomial = GetPolyFromTerm(i, sVar)
            Do While cur IsNot Nothing
                ReDim Preserve vRet(i)
                vRet(i) = cur
                i += 1
                cur = GetPolyFromTerm(i, sVar)
            Loop
        Catch ex As Exception
            Throw ex
        End Try
        Return vRet
    End Function
    Public Function splitIntoTermsRestoAndDiv(ByRef iPosDiv As Int64) As Polynomial()
        Dim vRet(-1) As Polynomial
        Try
            If PolyResto Is Nothing Then
                Return vRet
            End If
            Dim Pa As New Polynomial(Me)
            Pa.PolyResto = Nothing
            Pa.PolyDivisor = Nothing
            If Not (Pa.isReal AndAlso Pa.ToDouble = 0.0) Then
                Return vRet
            End If
            vRet = PolyResto.splitIntoTerms
            Dim vDiv() As Polynomial = PolyDivisor.splitIntoTerms
            Dim n As Int64 = vRet.Length
            iPosDiv = n ' posición en vRet() donde empiezan los divisores
            ReDim Preserve vRet(n + vDiv.Length - 1)
            Array.Copy(vDiv, 0, vRet, n, vDiv.Length)
        Catch ex As Exception
            Throw ex
        End Try
        Return vRet
    End Function
    Public Function split_1OnlyTermToExpr(ByRef vFact() As Expression,
                                          ByRef bIsDiv() As Boolean) As Boolean
        Try
            If vFact Is Nothing Then
                ReDim vFact(-1)
            End If
            If PolyResto Is Nothing Then
                If cf.Length > 1 Then Return False
                Dim An As New Complex(Me.An)
                Dim iv As Int64 = 0
                If exp Is Nothing OrElse
                exp(0) Is Nothing OrElse
                exp(0).Length = 0 Then
                    Return False
                End If
                For i As Int64 = 0 To exp(0).Length - 1
                    If exp(0)(i) Then
                        ReDim Preserve vFact(iv), bIsDiv(iv)
                        vFact(iv) = New Expression(
                           Polynomial.GetPolynomial(var(i)))
                        If exp(0)(i) > 1 Then
                            vFact(iv) ^= New Expression(exp(0)(i))
                        End If
                        iv += 1
                    End If
                Next
                If iv = 0 Then Return False
                vFact(0) = New Expression(An) * vFact(0)
            Else
                Dim p As New Polynomial(Me)
                p.PolyResto = Nothing
                p.PolyDivisor = Nothing
                If Not p.isReal OrElse
                Not p.ToDouble = 0.0 Then
                    Return False
                End If
                If Not Me.PolyResto.split_1OnlyTermToExpr(
                    vFact, bIsDiv) Then
                    Return False
                End If
                Dim bIsDiv2() As Boolean = Nothing
                Dim vFact2() As Expression = Nothing
                If Not Me.PolyDivisor.split_1OnlyTermToExpr(
                    vFact2, bIsDiv2) Then
                    Return False
                End If
                Dim i1 As Int64 = vFact.Length
                Dim i2 As Int64 = vFact2.Length
                For i As Int64 = 0 To vFact2.Length - 1
                    bIsDiv2(i) = True
                Next
                ReDim Preserve vFact(i1 + i2 - 1), bIsDiv(i1 + i2 - 1)
                Array.Copy(vFact2, 0, vFact, i1, i2)
                Array.Copy(bIsDiv2, 0, bIsDiv, i1, i2)
                vFact(0) = New Expression(1.0 / Me.PolyDivisor.An) * vFact(0)
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return True
    End Function
    Public Shared Function CopyFrom(ByVal polyA As Polynomial) As Polynomial
        Dim polyC As New Polynomial
        Try
            ReDim polyC.cf(polyA.cf.Length - 1), polyC.exp(polyA.exp.Length - 1), polyC.var1(polyA.var1.Length - 1)
            If polyC.cf.Length Then
                Array.Copy(polyA.cf, polyC.cf, polyA.cf.Length)
            End If
            If polyC.exp.Length Then
                Dim i As Int64
                For i = 0 To polyC.exp.Length - 1
                    ReDim Preserve polyC.exp(i)(polyA.exp(i).Length - 1)
                    Array.Copy(polyA.exp(i), polyC.exp(i), polyA.exp(i).Length)
                Next
            End If
            If polyC.var1.Length Then
                Array.Copy(polyA.var1, polyC.var1, polyA.var1.Length)
            End If
            polyC.dbMod = polyA.dbMod
            If polyA.PolyResto IsNot Nothing Then
                polyC.PolyResto = New Polynomial(polyA.PolyResto)
                polyC.PolyDivisor = New Polynomial(polyA.PolyDivisor)
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return polyC
    End Function
    Public Sub opChgSgn()
        Dim i As Int64
        Try
            For i = 0 To cf.Length - 1
                cf(i) = -cf(i)
            Next
        Catch ex As Exception

        End Try
    End Sub
    Public Shared Function opConjugate(ByVal Pa As Polynomial) As Polynomial
        Dim Pc As New Polynomial(Pa)
        Dim i As Int64
        For i = 0 To Pc.cf.Length - 1
            If Pa.cf(i).pIm.ToDouble <> 0 Then
                Pc.cf(i) = New Complex(Pa.cf(i).pRe, -Pa.cf(i).pIm)
                If Pc.cf(i).pIm.ToDouble = Pa.cf(i).pIm.ToDouble Then
                    Throw New Exception("error in conjugate")
                End If
            End If
        Next
        Return Pc
    End Function
    Public Shared Operator +(ByVal polyA As Polynomial, ByVal polyB As Polynomial) As Polynomial
        Return opSumaResta(True, polyA, polyB)
    End Operator
    Public Shared Operator -(ByVal polyA As Polynomial) As Polynomial
        Dim polyC As New Polynomial(polyA)
        Try
            Dim i As Int64
            For i = 0 To polyC.cf.Length - 1
                polyC.cf(i) = -polyC.cf(i)
                If polyA.dbMod <> 0.0 Then
                    polyC.cf(i).pRe = New Rational(polyC.cf(i).pRe.ToDouble Mod polyA.dbMod)
                    Do While polyC.cf(i).pRe.ToDouble < 0.0
                        polyC.cf(i).pRe += polyA.dbMod
                    Loop
                End If
            Next
            If polyA.PolyResto IsNot Nothing Then
                For i = 0 To polyC.PolyResto.cf.Length - 1
                    polyC.PolyResto.cf(i) = -polyC.PolyResto.cf(i)
                    If polyA.PolyResto.dbMod <> 0.0 Then
                        polyC.PolyResto.cf(i).pRe =
                            New Rational(polyC.PolyResto.cf(i).pRe.ToDouble Mod polyA.PolyResto.dbMod)
                        Do While polyC.PolyResto.cf(i).pRe.ToDouble < 0.0
                            polyC.PolyResto.cf(i).pRe += polyA.PolyResto.dbMod
                        Loop
                    End If
                Next
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return polyC
    End Operator
    Public Shared Operator -(ByVal polyA As Polynomial, ByVal polyB As Polynomial) As Polynomial
        Return opSumaResta(False, polyA, polyB)
    End Operator
    Public Shared Function opSumaResta(ByVal bSumar As Boolean, ByVal polyA As Polynomial, ByVal polyB As Polynomial) As Polynomial
        Dim polyC As New Polynomial
        Try
            If polyB.isComplex AndAlso polyB.cf(0).esCero Then
                polyC = New Polynomial(polyA)
                Exit Try
            ElseIf polyA.isComplex AndAlso polyA.cf(0).esCero Then
                If bSumar Then
                    polyC = New Polynomial(polyB)
                Else
                    polyC = New Polynomial(-polyB)
                End If
                Exit Try
            End If
            polyA.opReduceCommonExponents()
            polyB.opReduceCommonExponents()
            Dim varU() As String = varUnion(polyA, polyB)
            polyC = New Polynomial(polyA)
            polyC.PolyResto = Nothing
            polyC.PolyDivisor = Nothing
            ReDim polyC.var1(varU.Length - 1)
            Array.Copy(varU, 0, polyC.var1, 0, varU.Length)
            ReDim Preserve polyC.cf(polyA.cf.Length + polyB.cf.Length - 1)
            Array.Copy(polyB.cf, 0, polyC.cf, polyA.cf.Length, polyB.cf.Length)
            ReDim Preserve polyC.exp(polyA.exp.Length + polyB.exp.Length - 1)

            '
            Dim i, j, k As Int64
            For i = 0 To polyB.cf.Length - 1
                k = polyA.exp.Length + i
                polyC.exp(k) = New Int64() {0}
                For j = 0 To polyB.exp(i).Length - 1
                    Dim expVarJ_enB As Int64 = polyB.exp(i)(j)
                    If expVarJ_enB <> 0 Then
                        Dim posVarJ_dePolyB_en_varU As Int64 =
                            Array.IndexOf(varU, polyB.var1(j), 0, varU.Length)
                        If posVarJ_dePolyB_en_varU >= polyC.exp(k).Length Then
                            ReDim Preserve polyC.exp(k)(posVarJ_dePolyB_en_varU)
                        End If
                        polyC.exp(k)(posVarJ_dePolyB_en_varU) = expVarJ_enB
                    End If
                Next
                If Not bSumar Then
                    polyC.cf(k) = -polyC.cf(k)
                End If
            Next
            polyC.dbModuloInOperations = polyA.dbMod
            polyC.opReduceCommonExponents()

            If polyA.PolyResto Is Nothing Then
                If polyB.PolyResto IsNot Nothing Then
                    If bSumar Then
                        polyC.PolyResto = New Polynomial(polyB.PolyResto)
                    Else
                        polyC.PolyResto = New Polynomial(-polyB.PolyResto)
                    End If
                    polyC.PolyDivisor = New Polynomial(polyB.PolyDivisor)
                End If
            ElseIf polyB.PolyResto Is Nothing Then
                If polyA.PolyResto IsNot Nothing Then
                    polyC.PolyResto = New Polynomial(polyA.PolyResto)
                    polyC.PolyDivisor = New Polynomial(polyA.PolyDivisor)
                End If
            Else
                Dim mult As Complex = Nothing
                ' 2013/08/05 added:
                If polyA.PolyDivisor.IsEqual(polyB.PolyDivisor, mult) Then
                    ' PolyC =(Pa + A/C) +/- (Pb + B/C) =
                    '       =(Pa+Pb) +/- (A+B)/C
                    If bSumar Then
                        polyC.PolyResto = polyA.PolyResto + polyB.PolyResto
                    Else
                        polyC.PolyResto = polyA.PolyResto - polyB.PolyResto
                    End If
                    polyC.PolyDivisor = New Polynomial(polyA.PolyDivisor)
                Else
                    Dim gcd As Polynomial = Nothing ' 2013/10/25
                    Dim polyADiv As New Polynomial(polyA.PolyDivisor)
                    Dim polyBDiv As New Polynomial(polyB.PolyDivisor)
                    If varU.Length = 1 Then
                        gcd = opGcd(polyA.PolyDivisor, polyB.PolyDivisor)
                        If Not gcd.isReal OrElse gcd.ToDouble <> 1.0 Then
                            polyADiv /= gcd
                            polyADiv.PolyResto = Nothing ' force division rounding
                            polyADiv.PolyDivisor = Nothing
                            polyBDiv /= gcd
                            polyBDiv.PolyResto = Nothing ' force division rounding
                            polyBDiv.PolyDivisor = Nothing
                        End If
                    End If
                    Dim numerator As Polynomial = polyA.PolyResto * polyBDiv ' * polyB.PolyDivisor 
                    If bSumar Then
                        numerator += polyB.PolyResto * polyADiv ' * polyA.PolyDivisor 
                    Else
                        numerator -= polyB.PolyResto * polyADiv ' * polyA.PolyDivisor 
                    End If
                    Dim denominator As Polynomial = polyA.PolyDivisor * polyB.PolyDivisor
                    If gcd IsNot Nothing AndAlso
                    (Not gcd.isReal OrElse gcd.ToDouble <> 1.0) Then
                        polyC.PolyResto = numerator
                        polyC.PolyDivisor = denominator / gcd
                        polyC.PolyDivisor.PolyResto = Nothing ' force division rounding
                        polyC.PolyDivisor.PolyDivisor = Nothing
                    Else
                        polyC.PolyResto = numerator
                        polyC.PolyDivisor = denominator
                    End If
                End If
            End If
            If polyC.PolyResto IsNot Nothing Then
                If polyC.isComplex() Then
                    Dim aSumar As New Polynomial(polyC.PolyResto.cf(0) /
                                                 polyC.PolyDivisor.cf(0))
                    polyC.PolyResto = Nothing
                    polyC += aSumar
                End If
            End If
            polyC.opReduceCommonExponents()
        Catch ex As Exception
            Throw ex
        End Try
        Return polyC
    End Function

    Public Shared Operator *(ByVal db As Double, ByVal polyA As Polynomial) As Polynomial
        Return New Polynomial(db) * polyA
    End Operator
    Public Shared Operator *(ByVal polyA As Polynomial, ByVal db As Double) As Polynomial
        Return New Polynomial(db) * polyA
    End Operator
    Public Shared Operator *(ByVal cjo As Complex, ByVal polyA As Polynomial) As Polynomial
        Return New Polynomial(cjo) * polyA
    End Operator
    Public Shared Operator *(ByVal polyA As Polynomial, ByVal cjo As Complex) As Polynomial
        Return New Polynomial(cjo) * polyA
    End Operator
    Public Shared Operator *(ByVal polyA As Polynomial, ByVal polyB As Polynomial) As Polynomial
        Dim polyC As Polynomial
        If (polyA.isReal AndAlso polyA.cf(0).pRe.IsZero) OrElse
        (polyB.isReal AndAlso polyB.cf(0).pRe.IsZero) Then
            Return New Polynomial(0.0)
        End If
        polyA.opReduceCommonExponents()
        polyB.opReduceCommonExponents()
        If (polyA.isComplex AndAlso polyA.cf(0).esCero) OrElse
        (polyB.isComplex AndAlso polyB.cf(0).esCero) Then
            If polyA.PolyResto Is Nothing AndAlso
            polyB.PolyResto Is Nothing Then
                Return New Polynomial(0.0)
            End If
        End If
        If polyA.PolyResto Is Nothing AndAlso polyB.PolyResto Is Nothing Then
            polyC = mult(polyA, polyB)
            Return polyC
        End If
        If polyA.PolyResto Is Nothing Then
            '  A * ( B + c/d) = A*( (B*d+c)/d) =
            ' = (A*B*d + A*c) / d
            Dim B1 As New Polynomial(polyB)
            Dim c1 As New Polynomial(polyB.PolyResto)
            Dim d1 As New Polynomial(polyB.PolyDivisor)
            B1.PolyResto = Nothing
            B1.PolyDivisor = Nothing
            polyC = (mult(mult(polyA, B1), d1) + mult(polyA, c1))
            polyC /= d1
        ElseIf polyB.PolyResto Is Nothing Then
            Dim A1 As New Polynomial(polyA)
            Dim c1 As New Polynomial(polyA.PolyResto)
            Dim d1 As New Polynomial(polyA.PolyDivisor)
            ' (A+c/d)*B = (A*d*B+c*B)/d
            A1.PolyResto = Nothing
            polyC = (mult(mult(polyB, A1), d1) + mult(polyB, c1)) / d1
        Else
            Dim A As New Polynomial(polyA)
            Dim ac As New Polynomial(polyA.PolyResto)
            Dim ad As New Polynomial(polyA.PolyDivisor)
            A.PolyResto = Nothing
            A.PolyDivisor = Nothing
            Dim B As New Polynomial(polyB)
            Dim c As New Polynomial(polyB.PolyResto)
            Dim d As New Polynomial(polyB.PolyDivisor)
            B.PolyResto = Nothing
            B.PolyDivisor = Nothing
            ' (a+ ac/ad) * (B + c/d) = [( a*ad + ac)/ad ] * [(B*d+c)/d ] =

            ' = ( a*ad + ac) * (B*d+c) / (ad * d)
            polyC = mult(mult(A, ad) + ac, mult(B, d) + c) / mult(ad, d)
        End If
        Return polyC
    End Operator
    Shared Function mult(ByVal polyA As Polynomial, ByVal polyB As Polynomial) As Polynomial
        Dim polyC As New Polynomial
        Try
            Dim i, i1, j, j1, k As Int64
            If polyA.isComplex Then
                Dim cjo As Complex = polyA.ToComplex
                If cjo.IsZero Then
                    polyC = New Polynomial(0.0)
                    Exit Try
                End If
                polyC = New Polynomial(polyB)
                For i = 0 To polyC.cf.Length - 1
                    polyC.cf(i) *= cjo
                Next
                If polyC.PolyResto IsNot Nothing Then
                    For i = 0 To polyC.PolyResto.cf.Length - 1
                        polyC.PolyResto.cf(i) *= cjo
                    Next
                End If
                Exit Try
            End If
            If polyB.isReal Then
                Dim cjo As Complex = polyB.ToComplex
                If cjo.IsZero Then
                    polyC = New Polynomial(0.0)
                    Exit Try
                End If
                polyC = New Polynomial(polyA)
                For i = 0 To polyC.cf.Length - 1
                    polyC.cf(i) *= cjo
                Next
                Exit Try
            End If
            polyA = polyA + New Polynomial(0.0)
            polyB = polyB + New Polynomial(0.0)
            If polyA.exp.Length Then
                If polyB.exp.Length Then
                    Dim varU() As String = varUnion(polyA, polyB)
                    ReDim polyC.var1(varU.Length - 1)
                    Array.Copy(varU, polyC.var1, varU.Length)
                    k = polyC.cf.Length
                    ReDim polyC.cf(polyA.exp.Length * polyB.exp.Length - 1)
                    ReDim polyC.exp(polyA.exp.Length * polyB.exp.Length - 1)
                    For i = 0 To polyA.exp.Length - 1
                        For j = 0 To polyB.exp.Length - 1
                            ReDim Preserve polyC.exp(i1)
                            ReDim polyC.exp(i1)(varU.Length - 1)
                            For j1 = 0 To Math.Max(polyA.exp(i).Length - 1, polyB.exp(j).Length - 1)
                                If j1 < polyA.exp(i).Length Then
                                    Dim exp As Int64 = polyA.exp(i)(j1)
                                    If exp Then
                                        Dim posCa As Int64 =
                                            Array.IndexOf(varU, polyA.var1(j1), 0, varU.Length)
                                        polyC.exp(i1)(posCa) += exp
                                    End If
                                End If
                                If j1 < polyB.exp(j).Length Then
                                    Dim exp As Int64 = polyB.exp(j)(j1)
                                    If exp Then
                                        Dim posCb As Int64 =
                                            Array.IndexOf(varU, polyB.var1(j1), 0, varU.Length)
                                        polyC.exp(i1)(posCb) += exp
                                    End If
                                End If
                            Next
                            polyC.cf(i1) = polyA.cf(i) * polyB.cf(j)
                            i1 += 1
                        Next
                        k += polyA.exp.Length
                    Next
                Else
                    ReDim polyC.cf(polyA.cf.Length - 1)
                    ReDim polyC.exp(polyA.exp.Length - 1)
                    ReDim polyC.var1(polyA.var1.Length - 1)
                    Array.Copy(polyA.var1, polyC.var1, polyA.var1.Length)
                    For i = 0 To polyA.cf.Length - 1
                        ReDim polyC.exp(i)(polyA.exp(i).Length - 1)
                        Array.Copy(polyA.exp(i), polyC.exp(i), polyA.exp(i).Length)
                        polyC.cf(i) = polyA.cf(i) * polyB.cf(0)
                    Next
                End If
            ElseIf polyB.exp.Length Then
                ReDim polyC.cf(polyB.cf.Length - 1)
                ReDim polyC.exp(polyB.exp.Length - 1)
                ReDim polyC.var1(polyB.var1.Length - 1)
                Array.Copy(polyB.var1, polyC.var1, polyB.var1.Length)
                For i = 0 To polyB.cf.Length - 1
                    ReDim polyC.exp(i)(polyB.exp(i).Length - 1)
                    Array.Copy(polyB.exp(i), polyC.exp(i), polyB.exp(i).Length)
                    polyC.cf(i) = polyA.cf(0) * polyB.cf(i)
                Next
            Else
                polyC.cf = New Complex() {polyA.cf(0) * polyB.cf(0)}
            End If

            polyC.opReduceCommonExponents()
        Catch ex As Exception
            Throw ex
        End Try
        Return polyC
    End Function
    Public Shared Operator /(ByVal polyA As Polynomial, ByVal cf As Complex) As Polynomial
        Dim polyC As New Polynomial(polyA)
        Try
            If cf.esCero Then
                Throw New Exception(msg8.msg(1005)) ' Division by zero
            End If
            Dim i As Int64
            For i = 0 To polyC.cf.Length - 1
                polyC.cf(i) /= cf
            Next
            If polyA.PolyResto IsNot Nothing Then
                For i = 0 To polyA.PolyResto.cf.Length - 1
                    polyC.PolyResto.cf(i) /= cf
                Next
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return polyC
    End Operator
    Private Function opDegree1var(ByRef cf_N As Complex) As Int64
        Dim n As Int64 = 0
        Try
            Dim i, j As Int64
            For i = 0 To exp.Length - 1
                Dim nCur As Int64 = 0
                For j = 0 To exp(i).Length - 1
                    nCur += exp(i)(j)
                Next
                If nCur > n Then
                    n = nCur
                    cf_N = New Complex(cf(i))
                End If
            Next
        Catch ex As Exception
            Throw ex
        End Try
        Return n
    End Function
    Public Function opDegree(ByRef cf_N As Complex,
                              ByVal varsUnion() As String,
                              ByRef expsDiff() As Int64,
                              Optional ByVal bSortMultiVar As Boolean = False,
                              Optional ByRef iVarMaxExp As Int64 = -1,
                              Optional ByRef sortPoly As polynomialSort = Nothing) As Int64
        Dim n As Int64 = 0
        Try
            If varsUnion Is Nothing Then
                varsUnion = var1
            End If
            If var1.Length = 0 Then
                n = 0
            ElseIf False AndAlso varsUnion.Length = 1 Then

                ' OBSOLETO:

                Dim i, j As Int64
                ' Supposedly opReduceCommonExponents() method
                ' has been called anytime before and there
                ' are no 2 coeff. with equal exponents (exponents are unique)
                Dim vDegrees(exp.Length - 1) As Int64
                For i = 0 To exp.Length - 1
                    Dim nCur As Int64 = 0
                    For j = 0 To exp(i).Length - 1
                        nCur += exp(i)(j)
                    Next
                    If nCur > n Then
                        n = nCur
                        cf_N = New Complex(cf(i))
                    End If
                    vDegrees(i) = -nCur ' - sign so highest exponent will go to vDegrees(0) after array.sort()
                Next
                If vDegrees.Length > 1 Then
                    Dim i2(vDegrees.Length - 1) As Int64
                    Array.Copy(vDegrees, i2, i2.Length)
                    Array.Sort(vDegrees, exp) ' sort from lower to higher exponent: for ex. 1+x^2+x^5+...
                    Array.Sort(i2, cf) ' sort the corresponding coeff. with the same criterium
                End If
            Else 'If varsUnion.Length > 1 Then

                ' This piece of code is for polynomial division (or sorting)
                ' when there is more than 1 variable.
                Dim lnDif As Int64 = exp.Length - cf.Length
                If lnDif > 0 Then
                    Dim lncf As Int64 = cf.Length
                    ReDim Preserve cf(exp.Length - 1)
                    For k As Int64 = lncf To cf.Length - 1
                        cf(k) = Complex.zero
                    Next
                End If
                If exp.Length Then
                    Dim oS As New polynomialSort(exp, var, cf)
                    sortPoly = oS

                    cf_N = oS.cfMaxExp
                    n = oS.degreeMaxVar
                    iVarMaxExp = oS.iMaxVar


                    If expsDiff IsNot Nothing Then
                        ' exponents with max. degree are at exp(0)
                        For j = 0 To exp(0).Length - 1
                            If j < var1.Length Then
                                Dim pos As Int64 = Array.IndexOf(varsUnion, var1(j))
                                If pos > -1 AndAlso j < exp(0).Length Then
                                    expsDiff(pos) = exp(0)(j)
                                End If
                            End If
                        Next
                    End If
                End If
            End If
            If exp.Length = 0 OrElse
            (exp.Length = 1 AndAlso exp(0).Length = 0) Then
                ReDim exp(0), exp(0)(0)
            End If
            If cf_N Is Nothing Then
                If cf.Length Then
                    cf_N = New Complex(cf(0))
                Else
                    cf_N = New Complex(0.0)
                End If
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return n
    End Function

    Public Shared Operator /(ByVal polyA As Polynomial,
                             ByVal polyB As Polynomial) As Polynomial
        Dim polyC As New Polynomial(0.0)
        Try
            polyA.opReduceCommonExponents()
            polyB.opReduceCommonExponents()
            If polyB.isComplex Then
                polyC = New Polynomial(polyA / polyB.cf(0))
                Exit Try
            End If
            If polyA.isComplex AndAlso polyA.cf(0).esCero Then
                Return New Polynomial(0.0)
            End If
            If polyB.isComplex AndAlso polyB.cf(0).pRe.ToDouble = 1.0 AndAlso
                polyB.cf(0).pIm.ToDouble = 0.0 Then
                Return polyA
            End If
            Dim varsAB() As String = varUnion(polyA, polyB)
            If polyA.PolyResto Is Nothing AndAlso
            polyA.cf.Length = 1 AndAlso
            polyB.PolyResto Is Nothing AndAlso
            polyB.cf.Length = 1 Then
                Dim num As Polynomial = New Polynomial(polyA.cf(0) / polyB.cf(0))
                Dim den As Polynomial = New Polynomial(1.0)
                ReDim num.exp(0), num.exp(0)(varsAB.Length - 1)
                ReDim den.exp(0), den.exp(0)(varsAB.Length - 1)
                If varsAB.Length Then
                    ReDim num.var1(varsAB.Length - 1), den.var1(varsAB.Length - 1)
                    Array.Copy(varsAB, num.var1, varsAB.Length)
                    Array.Copy(varsAB, den.var1, varsAB.Length)
                End If
                Dim expAB(varsAB.Length - 1) As Int64
                If polyA.exp IsNot Nothing Then
                    For i As Int64 = 0 To polyA.exp.Length - 1
                        If polyA.exp(i) IsNot Nothing Then
                            For j As Int64 = 0 To polyA.exp(i).Length - 1
                                Dim exp As Int64 = polyA.exp(i)(j)
                                If exp Then
                                    Dim pos As Int64 = Array.IndexOf(varsAB, polyA.var(j))
                                    expAB(pos) += exp
                                End If
                            Next
                        End If
                    Next
                End If
                If polyB.exp IsNot Nothing Then
                    For i As Int64 = 0 To polyB.exp.Length - 1
                        If polyB.exp(i) IsNot Nothing Then
                            For j As Int64 = 0 To polyB.exp(i).Length - 1
                                Dim exp As Int64 = polyB.exp(i)(j)
                                If exp Then
                                    Dim pos As Int64 = Array.IndexOf(varsAB, polyB.var(j))
                                    expAB(pos) -= exp
                                End If
                            Next
                        End If
                    Next
                End If
                Dim PolyResto As New Polynomial(num.cf(0))
                Dim PolyDivisor As New Polynomial(1.0)
                Dim bDivEqualOne As Boolean = True
                Dim bNotCjo As Boolean = False
                For i As Int64 = 0 To expAB.Length - 1
                    If expAB(i) > 0 Then
                        Dim pI As Polynomial = Polynomial.GetPolynomial(varsAB(i))
                        pI.exp(0)(0) = expAB(i)
                        PolyResto *= pI
                        bNotCjo = True
                    ElseIf expAB(i) < 0 Then
                        Dim pI As Polynomial = Polynomial.GetPolynomial(varsAB(i))
                        pI.exp(0)(0) = -expAB(i)
                        PolyDivisor *= pI
                        bDivEqualOne = False
                        bNotCjo = True
                    End If
                Next
                If Not bNotCjo Then
                    polyC = New Polynomial(num.cf(0))
                ElseIf bDivEqualOne Then
                    polyC = PolyResto
                Else
                    polyC = New Polynomial(0.0)
                    polyC.PolyResto = PolyResto
                    polyC.PolyDivisor = PolyDivisor
                End If
                GoTo SinReducir
                'Exit Try
            ElseIf polyB.PolyResto Is Nothing AndAlso
            polyB.cf.Length = 1 AndAlso
            polyA.PolyResto Is Nothing Then
                polyC = New Polynomial(0.0)
                For i As Int64 = 0 To polyA.cf.Length - 1
                    Dim polyTerm As Polynomial = polyA.GetPolyFromTerm(i, Nothing)
                    If polyTerm IsNot Nothing Then
                        polyC += polyTerm / polyB
                    End If
                Next
                If polyA.PolyResto IsNot Nothing Then
                    polyC.PolyResto = New Polynomial(polyA.PolyResto)
                    polyC.PolyDivisor = mult(polyA.PolyDivisor, polyB)
                End If
                polyC.sortVar()
                Exit Try
            End If
            If polyA.PolyResto IsNot Nothing AndAlso polyB.PolyResto IsNot Nothing Then
                Dim a As New Polynomial(polyA)
                a.PolyResto = Nothing
                Dim b As New Polynomial(polyA.PolyResto)
                Dim c As New Polynomial(polyA.PolyDivisor)

                Dim d As New Polynomial(polyB)
                d.PolyResto = Nothing
                d.PolyDivisor = Nothing
                Dim e As New Polynomial(polyB.PolyResto)
                Dim f As New Polynomial(polyB.PolyDivisor)

                ' (a+b/c) / (d+e/f) = ((ac+b)/c) / ((df+e)/f) = f(ac+b)/(c(df+e))
                'polyC = f * (a * c + b) / (c * (d * f + e))
                polyC = New Polynomial(0.0)
                polyC.PolyResto = f * (a * c + b)
                polyC.PolyDivisor = (c * (d * f + e))
                'polyC.opReduceCommonExponents()
                'Return polyC
            ElseIf polyA.PolyResto IsNot Nothing Then
                Dim a As New Polynomial(polyA)
                a.PolyResto = Nothing
                Dim b As New Polynomial(polyA.PolyResto)
                Dim c As New Polynomial(polyA.PolyDivisor)

                Dim d As New Polynomial(polyB)
                d.PolyResto = Nothing
                d.PolyDivisor = Nothing

                ' (a+b/c)/d = (a+b/c)/(d/1) = ((ac+b)/c)/(d/1) = (ac+b)/(cd)
                'polyC = (a * c + b) / (c * d)
                polyC = New Polynomial(0.0)
                polyC.PolyResto = a * c + b
                polyC.PolyDivisor = c * d
                'Return polyC
            ElseIf polyB.PolyResto IsNot Nothing Then
                Dim a As New Polynomial(polyA)

                Dim d As New Polynomial(polyB)
                d.PolyResto = Nothing
                d.PolyDivisor = Nothing
                Dim e As New Polynomial(polyB.PolyResto)
                Dim f As New Polynomial(polyB.PolyDivisor)

                ' a/(d+e/f) = (a/1)/((df+e)/f) = af/(df+e)
                polyC = New Polynomial(0.0)
                polyC.PolyResto = a * f
                polyC.PolyDivisor = d * f + e
                'Return polyC
            Else
                Dim R As New Polynomial(polyA) ' Reminder
                Dim P2 As Polynomial = Nothing

                Dim polyB2 As New Polynomial(polyB)
                Dim Quotient As New Polynomial(0.0)
                Dim cf_Rn As Complex = Nothing
                Dim cf_Bn As Complex = Nothing
                Dim maxVarDegreeR, maxVarDegreeB As Int64, degreeP1 As Int64 = 10000
                If polyA.getDegree < polyB.getDegree Then
                    polyC.PolyResto = New Polynomial(polyA)
                    polyC.PolyDivisor = New Polynomial(polyB)
                    Exit Try
                End If
                If varsAB.Length = 1 Then
                    Dim polyA_An As Complex = New Complex(polyA.An)
                    Dim polyB_Bn As Complex = New Complex(polyB.An)
                    Dim AB_An As Complex = polyA_An / polyB_Bn ' 2010/10/25
                    Dim pA As Polynomial = Polynomial.opNormalize(polyA)
                    Dim pB As Polynomial = Polynomial.opNormalize(polyB)

                    Dim AmnB As Polynomial = pA - pB
                    If AmnB.isReal AndAlso AmnB.ToDouble = 0.0 Then
                        polyC = New Polynomial(AB_An)
                        Exit Try
                    End If
                    Dim curterm As Detall.addDivTerm = Detall.addDivTerm.first
                    Dim nVueltas As Int64 = 0
                    Do
                        maxVarDegreeR = R.opDegree1var(cf_Rn)
                        maxVarDegreeB = polyB2.opDegree1var(cf_Bn)
                        If maxVarDegreeR + 1 <= maxVarDegreeB OrElse
                        maxVarDegreeB * maxVarDegreeR = 0 Then
                            Exit Do
                        End If
                        ' 2) P1 = cf_Rn * x^(num_degree - den_degree)
                        Dim P1 As New Polynomial(cf_Rn / cf_Bn)
                        Dim xN_m As Polynomial = Polynomial.GetPolynomial(polyB2.var1(0))
                        xN_m.exp(0)(0) = maxVarDegreeR - maxVarDegreeB
                        P1 *= xN_m
                        ' 3) Quotient = Quotient + P1
                        Quotient += P1
                        ' 4) P2 = P1 * denominator
                        Dim P1_by_polyB2 As Polynomial = P1 * polyB2
                        Dim sum As Polynomial = Nothing
                        Dim dif As Polynomial = Nothing
                        If P2 IsNot Nothing Then
                            sum = P2 + P1_by_polyB2
                            dif = P2 - P1_by_polyB2
                            If sum.isReal AndAlso sum.ToDouble = 0.0 Then
                                Exit Do
                            ElseIf dif.isReal AndAlso dif.ToDouble = 0.0 Then
                                Exit Do
                            End If
                        End If
                        P2 = P1_by_polyB2



                        Dim RmnP2 As Polynomial = R - P2
                        Dim degRmn2 As Int64 = RmnP2.getDegree
                        Dim degR As Int64 = R.getDegree
                        If degRmn2 > degR OrElse
                        (degRmn2 = degR AndAlso
                        RmnP2.getMaxExponent > R.getMaxExponent) Then
                            Exit Do
                        End If




                        If pA.dbMod <> 0.0 Then
                            P2.dbModuloInOperations = pA.dbMod
                        End If


                        If P2.CoefDblAreZero OrElse
                        P1.getDegree < 1 Then
                            If pA.cfg.bDetail Then
                                pA.cfg.oDetail.addDivision(
                                  pA.cfg, R,
                                    polyB, Quotient, P1, P2, varsAB, Detall.addDivTerm.last)
                            End If
                            R = RmnP2
                            Exit Do
                        End If
                        If pA.cfg.bDetail Then
                            pA.cfg.oDetail.addDivision(
                              pA.cfg, R,
                                polyB, Quotient, P1, P2, varsAB, curterm)
                            curterm = Detall.addDivTerm.other
                        End If
                        R = RmnP2

                        'If P2.CoefDblAreZero OrElse _
                        'P1.getDegree < 1 OrElse _
                        'R.getDegree < P2.getDegree Then
                        '    Exit Do
                        'End If
                        ' 5) R = R - P2
                        'R -= P2
                        nVueltas += 1
                    Loop While nVueltas < 1000
                    polyC = Quotient
                    If R.cf.Length = 0 OrElse (R.cf(0).esCero AndAlso (R.exp.Length = 0 OrElse
                    (R.exp.Length = 1 AndAlso
                    R.exp(0).Length = 1 AndAlso
                    R.exp(0)(0) = 0))) Then
                        polyC.PolyResto = Nothing
                        polyC.PolyDivisor = Nothing
                    Else
                        polyC.PolyResto = R
                        polyC.PolyDivisor = New Polynomial(polyB2)
                        Dim e1 As String = ""
                        If pA.cfg.bDetail AndAlso
                        pA.cfg.bDetail Then
                            e1 = polyC.toStringPoly(pA.cfg)
                        End If
                        polyC.opReduceCommonExponents() ' 20120612 añadido
                        If pA.cfg.bDetail Then
                            Dim e2 As String = polyC.toStringPoly(pA.cfg)
                            If e1 <> e2 Then
                                pA.cfg.oDetail.Add(e1 + " =")
                                pA.cfg.oDetail.Add("= " + e2)
                            End If
                        End If
                    End If
                    Exit Try
                Else
                    Dim Resto As Polynomial = polyA
                    Dim vResto() As Polynomial = Resto.splitIntoTerms
                    Dim maxExponente As Int64 = 0
                    Dim iTerm As Int64 = -1
                    Dim i, j As Int64
                    For i = 0 To vResto.Length - 1
                        Dim exp As Int64 = vResto(i).getMaxExponent
                        If exp > maxExponente Then
                            iTerm = i
                            maxExponente = exp
                        End If
                    Next
                    'Trace.Write(vResto(iTerm).ToString + " " + maxExponente.ToString)
                    For i = 0 To vResto(iTerm).exp.Length - 1
                        If vResto(iTerm).exp(i) IsNot Nothing Then
                            For j = 0 To vResto(iTerm).exp(i).Length - 1
                                If vResto(iTerm).exp(i)(j) = maxExponente Then
                                    Exit For
                                End If
                            Next
                            If j < vResto(iTerm).exp.Length Then
                                Exit For
                            End If
                        End If
                    Next
                    Dim varDiv As String = vResto(iTerm).var(j)
                    'Trace.WriteLine(" " + vResto(iTerm).var(j))

                    Dim Divisor As Polynomial = polyB
                    Dim vDivisor() As Polynomial = Divisor.splitIntoTerms
                    ' Obtener máximo grado de varDiv en vResto:
                    Dim maxGradoDivisor As Int64 = 0
                    Dim iT1 As Int64
                    For iT As Int64 = 0 To vDivisor.Length - 1
                        If vDivisor(iT).exp IsNot Nothing Then
                            j = Array.IndexOf(vDivisor(iT).var, varDiv)
                            If j > -1 Then
                                Dim deg As Int64 = vDivisor(iT).getMaxExponentOfVar(varDiv)
                                ' start 2017/03/24:
                                If deg > maxGradoDivisor Then
                                    maxGradoDivisor = deg
                                    iT1 = iT
                                ElseIf deg = maxGradoDivisor Then
                                    Dim div As Polynomial = vResto(iTerm) / vDivisor(iT)
                                    If div.PolyResto Is Nothing Then
                                        iT1 = iT
                                    End If
                                End If
                                ' End 2017/03/24

                                ' commented 2017/03/24:
                                'For i = 0 To vDivisor(iT).exp.Length - 1
                                '    If vDivisor(iT).exp(i) IsNot Nothing AndAlso _
                                '    j < vDivisor(iT).exp(i).Length Then
                                '        If vDivisor(iT).exp(i)(j) > maxGradoDivisor Then
                                '            maxGradoDivisor = vDivisor(iT).exp(i)(j)
                                '            iT1 = iT
                                '        End If
                                '    End If
                                'Next
                            End If
                        End If
                    Next
                    If maxGradoDivisor = 0 Then
                        polyC.PolyResto = New Polynomial(polyA)
                        polyC.PolyDivisor = New Polynomial(polyB)
                    Else
                        Dim mult As Complex = Nothing
                        Dim QTerm As Polynomial = Nothing
                        Dim Prod As Polynomial = Nothing
                        Dim oldExp As Int64 = 0
                        Dim oldExp2 As Int64 = 0
                        Do While maxExponente > 0
                            QTerm = vResto(iTerm) / vDivisor(iT1)
                            If oldExp2 AndAlso oldExp2 = maxExponente Then
                                Exit Do
                            End If
                            oldExp2 = oldExp
                            oldExp = maxExponente
                            Prod = QTerm * Divisor
                            If QTerm.getDegree = 0 Then
                                If polyA.cfg.bDetail AndAlso QTerm.PolyResto Is Nothing Then
                                    polyA.cfg.oDetail.addDivision(
                                      polyA.cfg, Resto,
                                        polyB, Quotient, QTerm, Prod, varsAB, Detall.addDivTerm.last)
                                End If
                                'Exit Do
                            End If
                            If polyA.cfg.bDetail Then
                                polyA.cfg.oDetail.addDivision(
                                  polyA.cfg, Resto,
                                    polyB, Quotient, QTerm, Prod, varsAB, Detall.addDivTerm.last)
                            End If
                            polyC += QTerm
                            Resto -= Prod
                            vResto = Resto.splitIntoTerms
                            maxExponente = 0
                            For iT As Int64 = 0 To vResto.Length - 1
                                If vResto(iT).exp IsNot Nothing Then
                                    j = Array.IndexOf(vResto(iT).var, varDiv)
                                    If j > -1 Then
                                        For i = 0 To vResto(iT).exp.Length - 1
                                            If vResto(iT).exp(i) IsNot Nothing AndAlso
                                            j < vResto(iT).exp(i).Length Then
                                                If vResto(iT).exp(i)(j) > maxExponente Then
                                                    maxExponente = vResto(iT).exp(i)(j)
                                                    iTerm = iT
                                                End If
                                            End If
                                        Next
                                    End If
                                End If
                            Next
                            If QTerm.getDegree <= 0 Then
                                Exit Do
                            End If
                        Loop
                        polyC.PolyResto = Resto
                        polyC.PolyDivisor = Divisor
                    End If

                End If
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Try
            If polyC.PolyResto IsNot Nothing AndAlso
            polyC.PolyDivisor.cf.Length = 1 AndAlso
            polyC.PolyResto.cf.Length = 1 Then
                polyC.PolyDivisor.opReduceCommonExponents()
                If polyC.PolyDivisor.var.Length = 0 AndAlso
                polyC.PolyResto.var.Length = 0 Then
                    Dim Pr As New Polynomial(
                        polyC.PolyResto.cf(0) / polyC.PolyDivisor.cf(0))
                    polyC.PolyResto = Nothing
                    polyC.PolyDivisor = Nothing
                    polyC += Pr
                End If
            End If
            polyC.opReduceCommonExponents()
        Catch ex As Exception
            Throw ex
        End Try
SinReducir:
        Return polyC
    End Operator

    Public Shared Function opGcd(ByVal pA As Polynomial,
                             ByVal pB As Polynomial,
                             Optional ByVal cfg As Config = Nothing) As Polynomial
        'Dim Va As New Vector(pA)
        'Dim Vb As New Vector(pB)
        Dim polyC As New Polynomial(1.0)
        Try
            If Not bDoGCD OrElse pA.PolyResto IsNot Nothing OrElse
            pB.PolyResto IsNot Nothing Then
                Exit Try
            End If
            Dim varA() As String = pA.varAll
            Dim varB() As String = pB.varAll
            If varA.Length <> 1 OrElse
            varB.Length <> 1 OrElse
            varA(0) <> varB(0) Then
                Exit Try
            End If
            Dim degA As Int64 = pA.getDegree
            Dim degB As Int64 = pB.getDegree
            If degA < 1 OrElse degB < 1 Then
                Exit Try
            End If
            Dim cfAn As Complex = New Complex(pA.An)
            Dim mult As Complex = Nothing
            If pA.IsEqual(pB, mult) Then
                polyC = Polynomial.opNormalize(pA)
                Exit Try
            End If
            pA = Polynomial.opNormalize(pA)
            Dim cfBn As Complex = New Complex(pB.An)
            If Not cfAn.pIm.IsZero Then
                Throw New Exception(String.Format(msg8.num(83), pA.ToString))
            End If
            If Not cfBn.pIm.IsZero Then
                Throw New Exception(String.Format(msg8.num(83), pB.ToString))
            End If
            Dim rootsA() As Complex = opRoots(pA).cjo
            Dim rootsB() As Complex = opRoots(pB).cjo
            Dim i, j As Int64
            Dim x As Polynomial = Polynomial.GetPolynomial(pA.var(0))
            For i = 0 To rootsA.Length - 1
                For j = 0 To rootsB.Length - 1
                    If rootsB(j) IsNot Nothing AndAlso
                    (rootsA(i) - rootsB(j)).IsZero Then
                        polyC *= (x - New Polynomial(rootsB(j)))
                        rootsB(j) = Nothing
                        Exit For
                    End If
                Next
            Next
            Dim A As Double = cfAn.pRe.ToDouble
            Dim B As Double = cfBn.pRe.ToDouble
            If A > 1.0 AndAlso B > 1.0 AndAlso
            A = Math.Floor(A) AndAlso B = Math.Floor(B) Then
                Dim vDbl() As Double = New Double() {A, B}
                polyC *= Functions.LCM_GCD(vDbl)(0)
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return polyC
    End Function

    Shared Function varUnion(ByVal polyA As Polynomial, ByVal polyB As Polynomial) As String()
        Dim varU(-1) As String
        Dim iU As Int32 = polyA.var1.Length
        Try
            If polyA.var.Length + polyB.var.Length = 0 Then
                Return varU
            End If
            Dim i As Int64
            If polyA.var1.Length Then
                ReDim varU(iU - 1)
                Array.Copy(polyA.var1, varU, iU)
                For i = 0 To polyB.var1.Length - 1
                    ' sólo copiar las variables de polyB
                    ' no presentes en polyA:
                    Dim pos As Int64 = Array.IndexOf(varU, polyB.var1(i), 0, iU)
                    If pos = -1 Then
                        ' no está presente: copiar
                        ReDim Preserve varU(iU)
                        varU(iU) = polyB.var1(i)
                        iU += 1
                    End If
                Next
            ElseIf polyB.var1.Length Then
                ReDim varU(polyB.var1.Length - 1)
                Array.Copy(polyB.var1, varU, varU.Length)
            Else
                ReDim varU(-1)
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return varU
    End Function
    Public Shared Function opNormalize(ByVal polyA As Polynomial) As Polynomial
        Dim polyC As New Polynomial(polyA)
        Try
            Dim i As Int64
            Dim cf_N As Complex = Nothing
            polyC.opDegree(cf_N, polyC.var1, Nothing) ' cf_N returns with the coefficient (of the highest monomial)
            If cf_N.IsZero Then
                Return New Polynomial(Complex.zero)
            End If
            If cf_N.pRe.ToDouble <> 1.0 OrElse cf_N.pIm.ToDouble <> 0.0 Then ' if coef=1.0 --> polyC is already normalized
                polyC.cf(0) = New Complex(1.0)
                For i = 1 To polyC.cf.Length - 1
                    polyC.cf(i) /= cf_N
                Next
            End If
            polyC.opReduceCommonExponents()
        Catch ex As Exception
            Throw ex
        End Try
        Return polyC
    End Function
    Public Function opSuprNegExp() As Boolean
        Dim ret As Boolean = False
        Try
            Dim Pa As Polynomial = Me
            If Pa.exp.Length Then
                ' ver si tiene 
                ' exponentes negativos:
                Dim exp1 As Int64 = Pa.exp(Pa.exp.Length - 1)(0)
                If exp1 < 0 Then
                    For j As Int64 = 0 To Pa.exp.Length - 1
                        ' incrementar el grado del polynomio:
                        Pa.exp(j)(0) -= exp1
                        ret = True
                    Next
                End If
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return ret
    End Function

    Public Function CoefDblAreZero() As Boolean
        Try
            For i As Int64 = 0 To Me.cf.Length - 1
                If Me.cf(i).pRe.ToDouble <> 0.0 OrElse
                Me.cf(i).pIm.ToDouble <> 0.0 Then
                    Return False
                End If
            Next
        Catch ex As Exception
            Throw ex
        End Try
        Return True
    End Function

    Public Sub opReduceCommonExponents()
        Try
            Dim i, j As Int64
            'If cfg IsNot Nothing AndAlso _
            '(cfg.isTimeout OrElse exp.Length > 10000) Then
            If cfg IsNot Nothing AndAlso cfg.isTimeout Then
                'Throw New TimeoutException
            End If
            If exp.Length <> cf.Length Then
                ReDim Preserve exp(cf.Length - 1)
            End If
            For i = 0 To exp.Length - 1
                If var1.Length AndAlso
                exp(i).Length <> var1.Length Then
                    ReDim Preserve exp(i)(var1.Length - 1)
                End If
            Next
            If exp.Length > 1 AndAlso var.Length Then
                Dim oS As New polynomialSort(exp, var, cf)
                If oS.minDegree = 0 AndAlso oS.degree = 0 Then
                    ReDim var1(-1)
                End If
            End If
            i = 0
            Dim bIsInfinity As Boolean = False
            Do
Repite:
                Dim bReduce As Boolean = IIf(
                cf.Length AndAlso cf(i).esCero, True, False)
                ' if coeff. is zero, reduce
                If Not bReduce AndAlso i + 1 < exp.Length Then
                    bReduce = True
                    For j = 0 To exp(i).Length - 1
                        Dim expA As Int64 = exp(i)(j)
                        Dim expB As Int64 = 0
                        If j < exp(i + 1).Length Then
                            expB = exp(i + 1)(j)
                        End If
                        If expA <> expB Then
                            bReduce = False : Exit For
                        End If
                    Next
                End If
                If bReduce Then
                    If Not bIsInfinity AndAlso i + 1 < cf.Length Then
                        cf(i) += cf(i + 1)
                        exp(i) = exp(i + 1) ' por si cf(i).esCero()
                        ' (en ese caso quizás exp(i) <> exp(i+1) )
                    End If
                    For j = i + 1 To exp.Length - 2
                        cf(j) = cf(j + 1)
                        exp(j) = exp(j + 1)
                    Next
                    If cf.Length > 1 Then
                        If exp.Length >= cf.Length OrElse
                        exp.Length < cf.Length - 2 Then
                            ReDim Preserve exp(cf.Length - 2)
                        End If
                        ReDim Preserve cf(cf.Length - 2)
                    Else
                        exp(i) = New Int64() {0}
                    End If
                    If i + 1 < exp.Length Then
                        GoTo Repite
                    End If
                End If
                i += 1
            Loop While i < exp.Length
            If Me.dbModuloInOperations Then
                For i = 0 To cf.Length - 1
                    cf(i).pRe = New Rational(cf(i).pRe.ToDouble Mod Me.dbModuloInOperations)
                Next
            End If

            If PolyResto IsNot Nothing Then
                If var.Length = 0 AndAlso
                varAll.Length Then
                    var1 = varAll
                End If
            End If


            If PolyResto IsNot Nothing Then
                If PolyResto.isReal AndAlso
                PolyResto.ToDouble = 0 Then
                    PolyResto = Nothing
                    PolyDivisor = Nothing
                ElseIf PolyDivisor.isComplex AndAlso PolyDivisor.cf(0).pIm.ToDouble = 0.0 AndAlso
                    PolyDivisor.cf(0).pRe.ToDouble = 1.0 Then
                    Dim polyC As New Polynomial(PolyResto)
                    copyToThis(polyC)
                    PolyResto = Nothing : PolyDivisor = Nothing
                End If
            End If
            If PolyResto IsNot Nothing Then
                ' reducir exponentes comunes al numerador
                ' y denominador, e.g. (x3y3+xy2)/(xy) --> x2y2+y
                Dim vVarsNum() As String = PolyResto.varAll
                Dim vVarsDen() As String = PolyDivisor.varAll
                If vVarsNum.Length AndAlso vVarsDen.Length Then
                    Dim vMinExpN(vVarsNum.Length - 1) As Int64
                    For i = 0 To vMinExpN.Length - 1
                        vMinExpN(i) = Int64.MaxValue
                    Next
                    With PolyResto
                        For i = 0 To .exp.Length - 1
                            For j = 0 To .exp(i).Length - 1
                                If j < vMinExpN.Length Then
                                    vMinExpN(j) = Math.Min(
                                        vMinExpN(j), .exp(i)(j))
                                End If
                            Next
                        Next
                    End With
                    Dim vMinExpD(vVarsDen.Length - 1) As Int64
                    For i = 0 To vMinExpD.Length - 1
                        vMinExpD(i) = Int64.MaxValue
                    Next
                    Dim vVarsUnion(-1) As String, iU As Int64 = 0
                    Dim vMinExpUnion(-1) As Int64
                    Dim vPosNum(-1) As Int64
                    Dim vPosDen(-1) As Int64
                    With PolyDivisor
                        For i = 0 To .exp.Length - 1
                            For j = 0 To .exp(i).Length - 1
                                If j < vVarsDen.Length Then
                                    Dim pos As Int64 =
                                        Array.IndexOf(vVarsNum, vVarsDen(j))
                                    If pos > -1 Then
                                        Dim bIncr As Boolean = False
                                        Dim iU2 As Int64 = Array.IndexOf(vPosDen, j)
                                        If iU2 = -1 Then
                                            ReDim Preserve vVarsUnion(iU), vMinExpUnion(iU),
                                                vPosNum(iU), vPosDen(iU)
                                            bIncr = True
                                            iU2 = iU
                                        End If
                                        vMinExpD(j) = Math.Min(
                                                    vMinExpD(j), .exp(i)(j))
                                        vMinExpUnion(iU2) = Math.Min(
                                            vMinExpD(j), vMinExpN(pos))
                                        vPosNum(iU2) = pos
                                        vPosDen(iU2) = j
                                        If bIncr Then
                                            iU += 1
                                        End If
                                    End If
                                End If
                            Next
                        Next
                    End With
                    For iU = 0 To vMinExpUnion.Length - 1
                        If vMinExpUnion(iU) Then
                            Dim pos As Int64 = vPosNum(iU)
                            For i = 0 To PolyResto.exp.Length - 1
                                If pos < PolyResto.exp(i).Length Then
                                    PolyResto.exp(i)(pos) -= vMinExpUnion(iU)
                                End If
                            Next
                            pos = vPosDen(iU)
                            For i = 0 To PolyDivisor.exp.Length - 1
                                If pos < PolyDivisor.exp(i).Length Then
                                    PolyDivisor.exp(i)(pos) -= vMinExpUnion(iU)
                                End If
                            Next
                        End If
                    Next
                End If
            End If
            If False AndAlso PolyResto IsNot Nothing Then
                Dim An As New Complex(PolyDivisor.An)
                If An.IsZero Then
                    PolyResto = Nothing
                    PolyDivisor = Nothing
                Else
                    For i = 0 To PolyResto.cf.Length - 1
                        PolyResto.cf(i) /= An
                    Next
                    For i = 0 To PolyDivisor.cf.Length - 1
                        PolyDivisor.cf(i) /= An
                    Next
                End If
            End If
            If cf.Length = 0 Then
                cf = New Complex() {New Complex(0.0)}
            ElseIf dbMod >= 1 Then
                For i = 0 To cf.Length - 1
                    cf(i).pRe = New Rational(cf(i).pRe.ToDouble Mod dbMod)
                Next
            End If
        Catch ex As Exception
            Throw ex
        End Try
    End Sub
    Public Sub sortVar()
        Dim vvar(var.Length - 1) As String
        If PolyResto IsNot Nothing Then
            PolyResto.sortVar()
            PolyDivisor.sortVar()
            'Exit Sub
        End If
        Try
            If var Is Nothing OrElse vvar.Length < 2 Then
                Exit Try
            End If
            Array.Copy(var, vvar, var.Length)
            Dim i, j As Int64
            Dim vvarCpy(vvar.Length - 1) As String
            Array.Copy(vvar, vvarCpy, vvar.Length)
            Dim vv2(vvar.Length - 1) As String
            For i = 0 To vv2.Length - 1
                vv2(i) = String.Format("{0:0000}{1}", 9999 - vvar(i).Length, vvar(i))
            Next
            Array.Sort(vv2, vvar)
            For i = 0 To vvar.Length - 1
                If vvar(i) <> vvarCpy(i) Then
                    Exit For
                End If
            Next
            If i >= vvar.Length Then
                Exit Try
            End If
            Dim exp2(exp.Length - 1)() As Int64
            For i = 0 To exp.Length - 1
                If exp(i) IsNot Nothing Then
                    ReDim Preserve exp2(i)(exp(i).Length - 1)
                    For j = 0 To exp(i).Length - 1
                        If exp(i)(j) Then
                            Dim sVarOld As String = vvarCpy(j)
                            Dim posNewVar As Int64 = Array.IndexOf(
                                vvar, sVarOld)
                            exp2(i)(posNewVar) = exp(i)(j)
                        End If
                    Next
                End If
            Next
            exp = exp2
            var1 = vvar
        Catch ex As Exception

        End Try
    End Sub
    Public Shared Operator ^(ByVal polyA As Polynomial, ByVal polyB As Polynomial) As Polynomial
        Dim polyC As Polynomial = Nothing
        If polyA.var1.Length + polyB.var1.Length = 0 Then
            If polyA.PolyResto Is Nothing AndAlso
            polyB.PolyResto Is Nothing Then
                polyC = New Polynomial(polyA.cf(0) ^ polyB.cf(0))
                Return polyC
            End If
        End If
        Try
            polyC = New Polynomial(polyA)
            If polyB.isComplex AndAlso
            (polyB.PolyResto Is Nothing OrElse
             polyB.var1.Length = 0) Then
                Dim cf As Complex = polyB.cf(0)
                If cf.pIm.IsZero Then
                    If cf.pRe.ToDouble = Math.Floor(cf.pRe.ToDouble) Then
                        Dim i As Int64
                        Dim expB As Int64 = cf.pRe.ToDouble
                        Dim Pa As Polynomial
                        If expB > 0 Then
                            Pa = New Polynomial(polyA)
                        Else
                            Pa = New Polynomial(1.0) / polyA
                        End If
                        Dim exp As Int64 = Math.Abs(expB)
                        If exp > 0 Then
                            If Pa.isReal AndAlso Pa.ToDouble = 0.0 Then
                                polyC = New Polynomial(0.0) ' 0^exp = 0, if exp>0
                                Exit Try
                            End If
                            Dim log2 As Int64 = Math.Floor(Math.Log10(exp) / Math.Log10(2.0))
                            Dim vPolyC(log2) As Polynomial
                            Dim vExp(log2) As Int64
                            Dim curExp As Int64 = 2
                            vPolyC(0) = New Polynomial(Pa)
                            vExp(0) = 1
                            For i = 1 To log2
                                vPolyC(i) = vPolyC(i - 1) * vPolyC(i - 1)
                                vExp(i) = curExp
                                curExp *= 2
                            Next
                            curExp = vExp(log2)
                            polyC = vPolyC(log2)
                            For i = log2 - 1 To 0 Step -1
                                If curExp + vExp(i) <= exp Then
                                    polyC *= vPolyC(i)
                                    curExp += vExp(i)
                                    If curExp = exp Then
                                        Exit For
                                    End If
                                End If
                            Next
                        ElseIf exp = 0 Then
                            polyC = New Polynomial(1.0) ' x^0 =1 or 0^0=1
                        End If
                    Else
                        Dim var() As String = polyA.varAll
                        If var.Length = 1 AndAlso polyA.cf.Length = 1 AndAlso
                                                polyB.isReal Then
                            polyC = Polynomial.GetPolynomial(var(0))
                            polyC.cf(0) = New Complex(polyA.cf(0))
                            Dim pos As Int64 = Array.IndexOf(polyA.var, var(0))
                            Dim exp As Double = polyA.exp(0)(pos) * polyB.ToDouble
                            If exp = Math.Floor(exp) AndAlso exp > 0 Then
                                polyC.exp(0)(0) = exp
                            Else
                                Throw New Exception(msg8.msg(1009)) ' polynomial exponent non integer: n/a
                            End If
                        Else
                            Throw New Exception(msg8.msg(1009)) ' polynomial exponent non integer: n/a
                        End If
                    End If
                Else
                    Throw New Exception(msg8.msg(1008)) ' polynomial exponent is imaginary: n/a
                End If
            Else
                Throw New Exception(msg8.msg(1010)) ' polynomial exponent is a polynomial: n/a
            End If
            polyC.opReduceCommonExponents()
        Catch ex As Exception
            Throw ex
        End Try
        Return polyC
    End Operator

    Public Function opDerivative(ByVal respVar As String) As Polynomial
        Dim polyC As New Polynomial(Me)
        Try
            Dim i As Int64
            Dim nVar As Int64 = Array.IndexOf(var, respVar)
            If nVar > -1 Then
                For i = 0 To Me.cf.Length - 1
                    If nVar < Me.exp(i).Length Then
                        polyC.cf(i) *= Me.exp(i)(nVar)
                        polyC.exp(i)(nVar) -= 1
                        If polyC.exp(i)(nVar) < 0 Then
                            polyC.exp(i)(nVar) = 0
                            polyC.cf(i) = New Complex(0.0)
                        End If
                    Else
                        polyC.cf(i) = New Complex(0.0)
                    End If
                Next
            Else
                If PolyResto Is Nothing Then
                    Return New Polynomial(0.0)
                End If
            End If
            If polyC.PolyResto IsNot Nothing Then
                Dim exprR As New Expression(polyC.PolyResto)
                Dim exprD As New Expression(polyC.PolyDivisor)
                polyC.PolyResto = Nothing
                polyC.PolyDivisor = Nothing
                Dim f As Expression = exprR
                Dim g As Expression = exprD
                Dim Df As Expression = f.opDeriv(respVar)
                Dim Dg As Expression = g.opDeriv(respVar)
                Dim varG() As String = Nothing
                Dim ret As Expression
                g.getAllVars(varG)
                Dim varF() As String = Nothing
                f.getAllVars(varF)
                If varG Is Nothing OrElse
                varG.Length = 0 OrElse
                Array.IndexOf(varG, respVar) = -1 Then
                    ret = Df / g
                ElseIf varF Is Nothing OrElse
                varF.Length = 0 OrElse
                Array.IndexOf(varF, respVar) = -1 Then
                    ret = -f * Dg / (g * g)
                Else
                    Dim num As Expression = (Df * g - f * Dg)
                    Dim den As Expression = g ^ New Expression(2.0) 'g * g
                    If num.IsPolynomial AndAlso den.IsPolynomial Then
                        Dim pNum As Polynomial = num.getPolynomial
                        Dim pDen As Polynomial = den.getPolynomial
                        Dim gcd As Polynomial =
                            Polynomial.opGcd(pNum, pDen)
                        ret = New Expression((pNum / gcd) / (pDen / gcd))
                    Else
                        ret = num / den
                    End If
                End If
                polyC += ret.getPolynomial
            End If
            polyC.opReduceCommonExponents()
        Catch ex As Exception
            Throw ex
        End Try
        Return polyC
    End Function
    Public Function opDiff(ByVal respVar As String) As Expression
        Dim polyC As New Polynomial(Me)
        Try
            Dim i As Int64
            Dim nVar As Int64 = Array.IndexOf(var, respVar)
            If nVar > -1 Then
                For i = 0 To Me.cf.Length - 1
                    If nVar < Me.exp(i).Length Then
                        polyC.cf(i) *= Me.exp(i)(nVar)
                        polyC.exp(i)(nVar) -= 1
                        If polyC.exp(i)(nVar) < 0 Then
                            polyC.exp(i)(nVar) = 0
                            polyC.cf(i) = New Complex(0.0)
                        End If
                    Else
                        polyC.cf(i) = New Complex(0.0)
                    End If
                Next
            Else
                If PolyResto Is Nothing Then
                    Dim m1 As Match = Regex.Match("diff", MathGlobal8.sFn)
                    Dim ret As New Expression(m1)
                    ret.getArgs = New Expression() {
                        New Expression(Polynomial.GetPolynomial(var1(0))),
                        New Expression(Polynomial.GetPolynomial(respVar))}
                    Return ret
                End If
            End If
            If polyC.PolyResto IsNot Nothing Then
                Dim exprR As New Expression(polyC.PolyResto)
                Dim exprD As New Expression(polyC.PolyDivisor)
                polyC.PolyResto = Nothing
                polyC.PolyDivisor = Nothing
                Dim f As Expression = exprR
                Dim g As Expression = exprD
                Dim Df As Expression = f.opDeriv(respVar)
                Dim Dg As Expression = g.opDeriv(respVar)
                Dim varG() As String = Nothing
                Dim ret As Expression
                g.getAllVars(varG)
                Dim varF() As String = Nothing
                f.getAllVars(varF)
                If varG Is Nothing OrElse
                varG.Length = 0 OrElse
                Array.IndexOf(varG, respVar) = -1 Then
                    ret = Df / g
                ElseIf varF Is Nothing OrElse
                varF.Length = 0 OrElse
                Array.IndexOf(varF, respVar) = -1 Then
                    ret = -f * Dg / (g * g)
                Else
                    Dim num As Expression = (Df * g - f * Dg)
                    Dim den As Expression = g ^ New Expression(2.0) 'g * g
                    If num.IsPolynomial AndAlso den.IsPolynomial Then
                        Dim pNum As Polynomial = num.getPolynomial
                        Dim pDen As Polynomial = den.getPolynomial
                        Dim gcd As Polynomial =
                            Polynomial.opGcd(pNum, pDen)
                        ret = New Expression((pNum / gcd) / (pDen / gcd))
                    Else
                        ret = num / den
                    End If
                End If
                polyC += ret.getPolynomial
            End If
            polyC.opReduceCommonExponents()
        Catch ex As Exception
            Throw ex
        End Try
        Return New Expression(polyC)
    End Function
    Public Shared Function opOrthog(ByVal Ma As Matrix) As Complex
        Dim ret As Complex = Nothing
        Try
            Dim A As Complex = Ma.vVect(0).vPoly(0).cf(0)
            Dim B As Complex = Ma.vVect(0).vPoly(1).cf(0)
            Dim Pproduct As Polynomial = Ma.vVect(0).vPoly(2) * Ma.vVect(0).vPoly(3)
            Dim Pintegral As Polynomial = Pproduct.opIntegral
            Dim pA As Complex = Pintegral.evalCjo(A)
            Dim pB As Complex = Pintegral.evalCjo(B)
            ret = pB - pA
        Catch ex As Exception
            Throw ex
        End Try
        Return ret
    End Function
    Public Function evalMulti(ByVal Xn() As Complex) As Complex
        Dim retCjo As New Complex(0.0)
        Try
            Dim i, j As Int64
            For i = 0 To cf.Length - 1
                Dim cur As New Complex(cf(i))
                If Not cur.esCero AndAlso
                i < exp.Length AndAlso exp(i) IsNot Nothing Then
                    For j = 0 To exp(i).Length - 1
                        If exp(i)(j) Then
                            cur *= Xn(j) ^ New Complex(exp(i)(j))
                        End If
                    Next
                    retCjo += cur
                End If
            Next
            If Me.PolyResto IsNot Nothing Then
                retCjo += PolyResto.evalMulti(Xn) /
                    PolyDivisor.evalMulti(Xn)
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return retCjo
    End Function
    Public Function evalMultiDouble(ByVal Xn() As Double) As Double
        Dim retDbl As Double
        Try
            Dim i, j As Int64
            For i = 0 To cf.Length - 1
                Dim cur As Double = cf(i).pRe.ToDouble
                If cur AndAlso
                i < exp.Length AndAlso exp(i) IsNot Nothing Then
                    For j = 0 To exp(i).Length - 1
                        If exp(i)(j) Then
                            cur *= Xn(j) ^ exp(i)(j)
                        End If
                    Next
                    retDbl += cur
                End If
            Next
            If Me.PolyResto IsNot Nothing Then
                retDbl += PolyResto.evalMultiDouble(Xn) /
                    PolyDivisor.evalMultiDouble(Xn)
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return retDbl
    End Function
    Public Function evalMultiCjoToPolynomial(ByVal oVars As VarsAndFns) As Polynomial
        Dim polyC As New Polynomial(Me)
        Try
            Dim i, j As Int64
            For i = 0 To cf.Length - 1
                Dim cur As New Complex(cf(i))
                If Not cur.esCero AndAlso
                i < exp.Length AndAlso exp(i) IsNot Nothing Then
                    For j = 0 To exp(i).Length - 1
                        If exp(i)(j) Then
                            Dim eM As ExprMatrix =
                                oVars.getValueByName(var(j), False)
                            If eM IsNot Nothing AndAlso eM.IsComplex Then
                                Dim Xnj As Complex = eM.getExpr(0, 0).getPolynomial.cf(0)
                                cur *= Xnj ^ New Complex(exp(i)(j))
                                polyC.exp(i)(j) = 0
                            End If
                        End If
                    Next
                    polyC.cf(i) = cur
                End If
            Next
            If polyC.PolyResto IsNot Nothing Then
                polyC.PolyResto = polyC.PolyResto.evalMultiCjoToPolynomial(oVars)
                polyC.PolyDivisor = polyC.PolyDivisor.evalMultiCjoToPolynomial(oVars)
            End If
            polyC.varReduce()
            polyC.opReduceCommonExponents()
        Catch ex As Exception
            Throw ex
        End Try
        Return polyC
    End Function

    Public Function evalMultiCjoToExpr(ByVal oVars As VarsAndFns) As Expression
        Dim retExpr As Expression = Nothing
        Try
            If Me.isReal Then
                Return New Expression(Me)
            End If
            Dim i, j As Int64
            For i = 0 To cf.Length - 1
                Dim curExpr As New Expression(cf(i))
                For j = 0 To exp(i).Length - 1
                    Dim exp1 As Int64 = exp(i)(j)
                    If exp1 Then
                        Dim mtx As ExprMatrix = oVars.getValueByName(var(j), False)
                        If mtx Is Nothing Then
                            Dim eVar As New Expression(
                                Polynomial.GetPolynomial(var(j)))
                            If exp1 = 1 Then
                                curExpr = Expression.exprOp("*", curExpr,
                                            eVar)
                            Else
                                curExpr = Expression.exprOp("*", curExpr,
                                    Expression.exprOp("^", eVar,
                                    New Expression(exp1)))
                            End If
                        Else
                            Dim bCurIsReal As Boolean = False
                            Dim bMtxIsReal As Boolean = False
                            Dim dbMtx, dbCur As Double
                            If curExpr.IsReal Then
                                bCurIsReal = True : dbCur = curExpr.toDouble
                            End If
                            If mtx.IsReal Then
                                bMtxIsReal = True : dbMtx = mtx.getExpr(0, 0).toDouble
                            End If

                            If exp1 = 1 Then

                                If bCurIsReal AndAlso dbCur = 1.0 Then
                                    curExpr = mtx.getExpr(0, 0)
                                ElseIf bCurIsReal AndAlso dbCur = -1.0 Then
                                    curExpr = -mtx.getExpr(0, 0)
                                ElseIf bMtxIsReal AndAlso dbMtx = 1.0 Then
                                ElseIf mtx.getExpr(0, 0).IsReal AndAlso mtx.toDouble = -1.0 Then
                                    curExpr = -curExpr
                                Else
                                    Dim sign As Int64 = mtx.getExpr(0, 0).sign
                                    If sign = -1 Then
                                        curExpr = Expression.exprOp("*", -curExpr,
                                                                -mtx.getExpr(0, 0))
                                    Else
                                        curExpr = Expression.exprOp("*", curExpr,
                                                                mtx.getExpr(0, 0))
                                    End If
                                End If
                            Else
                                If bCurIsReal AndAlso bMtxIsReal Then
                                    curExpr = New Expression(
                                        New Complex(dbCur) *
                                        (New Complex(dbMtx) ^ New Complex(exp1)))
                                Else
                                    Dim exprExp As Expression =
                                        Expression.exprOp("^",
                                        mtx.getExpr(0, 0),
                                        New Expression(exp1))
                                    If bCurIsReal AndAlso dbCur = 1.0 Then
                                        curExpr = exprExp
                                    ElseIf bCurIsReal AndAlso dbCur = -1.0 Then
                                        curExpr = -exprExp
                                    Else
                                        curExpr = Expression.exprOp("*", curExpr,
                                            exprExp)
                                    End If
                                End If
                            End If
                        End If
                        'curExpr = curExpr.reduceFactors(False)
                    End If
                    If curExpr.IsPolynomial Then
                        curExpr = New Expression(curExpr.getPolynomial)
                    End If
                Next
                If retExpr Is Nothing Then
                    retExpr = curExpr
                Else
                    If retExpr.IsReal AndAlso retExpr.toDouble = 0 Then
                        retExpr = curExpr
                    ElseIf curExpr.IsReal AndAlso curExpr.toDouble = 0 Then
                    Else
                        retExpr = Expression.exprOp("+", retExpr, curExpr)
                    End If
                End If
            Next
            Dim NdivD As Expression = Nothing
            tryReducePolyResto()
            If PolyResto IsNot Nothing Then
                Dim N As Expression = PolyResto.evalMultiCjoToExpr(oVars)
                If N.IsReal AndAlso N.toDouble = 0 Then
                    ' There is nothing to add to 'retExpr'
                Else
                    Dim D As Expression = PolyDivisor.evalMultiCjoToExpr(oVars)
                    If D.IsReal AndAlso D.toDouble = 0.0 Then
                        Throw New DivideByZeroException
                    End If

                    Dim Pa As Polynomial = Nothing
                    Dim polyC As Polynomial = Nothing
                    Dim exprA As Expression = Nothing
                    Dim bAIsPoly As Boolean = False
                    If N.IsPolynomial Then
                        Pa = N.getPolynomial
                        bAIsPoly = True
                        exprA = D
                    ElseIf D.IsPolynomial Then
                        Pa = D.getPolynomial
                        exprA = N
                    End If
                    Dim arg As Expression = Nothing
                    If exprA IsNot Nothing AndAlso
                    exprA.getMatch IsNot Nothing Then
                        If exprA.extractFromPath(
                        New String() {"^", "0"}, arg) AndAlso
                        arg.IsPolynomial Then
                            If arg.IsPolynomial Then
                                Dim mult As Complex = Nothing
                                Dim expPa As Int64 = 1
                                If Pa.cf.Length = 1 AndAlso Pa.varAll.Length = 1 Then
                                    expPa = Pa.exp(0)(0)
                                    Pa = New Polynomial(Pa.cf(0)) *
                                        Polynomial.GetPolynomial(Pa.varAll(0))
                                End If
                                If Pa.IsEqual(arg.getPolynomial, mult) Then
                                    ' x/x^0.5 or x^(3/2)*x, (x2+1)^(n+1) ] ^ (x2+1), ...
                                    Dim exprExp As Expression = Nothing
                                    If exprA.extractFromPath(
                                        New String() {exprA.getMatch.ToString, "1"}, exprExp) Then
                                        If bAIsPoly Then
                                            ' (x-1) / (x-1)^(3/2) = (x-1)^(-3/2 + 1 ), x0.5/x2
                                            exprExp = exprExp.opChgSgn
                                            exprExp += New Expression(expPa)
                                            exprA.getArgs(1) = exprExp
                                        Else
                                            ' (x-1)^(3/2) / (x-1) = (x-1)^(3/2 - 1 )
                                            exprExp += New Expression(-expPa)
                                            exprA.getArgs(1) = exprExp
                                        End If
                                    Else
                                        exprA = Nothing
                                    End If
                                Else
                                    exprA = Nothing
                                End If
                            Else
                                exprA = Nothing
                            End If
                        Else
                            exprA = Nothing
                        End If
                    Else
                        exprA = N
                        If exprA IsNot Nothing AndAlso
                        exprA.getMatch IsNot Nothing Then
                            If exprA.extractFromPath(
                            New String() {"^", "0"}, arg) AndAlso arg.IsPolynomial Then
                                Pa = arg.getPolynomial
                                Dim exprB As Expression = D
                                Dim arg2 As Expression = Nothing
                                If exprB.extractFromPath(
                                New String() {"^", "0"}, arg2) AndAlso
                                arg2.IsPolynomial Then
                                    Dim Pb As Polynomial = arg2.getPolynomial
                                    Dim mult As Complex = Nothing
                                    Dim expPa As Int64 = 1
                                    Dim expPb As Int64 = 1
                                    If Pa.cf.Length = 1 AndAlso Pa.varAll.Length = 1 Then
                                        expPa = Pa.exp(0)(0)
                                        Pa = New Polynomial(Pa.cf(0)) *
                                            Polynomial.GetPolynomial(Pa.varAll(0))
                                    End If
                                    If Pb.cf.Length = 1 AndAlso Pb.varAll.Length = 1 Then
                                        expPb = Pb.exp(0)(0)
                                        Pb = New Polynomial(Pb.cf(0)) *
                                            Polynomial.GetPolynomial(Pb.varAll(0))
                                    End If
                                    If Pa.IsEqual(Pb, mult) Then
                                        ' (1+x)^1.5/(1+x)^0.5 ...
                                        Dim exprExpA As Expression = Nothing
                                        Dim exprExpB As Expression = Nothing
                                        If exprA.extractFromPath(
                                        New String() {exprA.getMatch.ToString, "1"}, exprExpA) AndAlso
                                        exprB.extractFromPath(
                                        New String() {exprB.getMatch.ToString, "1"}, exprExpB) Then
                                            ' (x-1) / (x-1)^(3/2) = (x-1)^(-3/2 + 1 ), x0.5/x2
                                            exprExpA -= exprExpB
                                            exprA.getArgs(1) = exprExpA
                                        Else
                                            exprA = Nothing
                                        End If
                                    Else
                                        exprA = Nothing
                                    End If
                                Else
                                    exprA = Nothing
                                End If
                            Else
                                exprA = Nothing
                            End If
                        Else
                            exprA = Nothing
                        End If
                    End If
                    If exprA IsNot Nothing Then
                        If retExpr.IsReal AndAlso retExpr.toDouble = 0 Then
                            retExpr = exprA
                        Else
                            retExpr = Expression.exprOp("+", retExpr, exprA)
                        End If
                    Else
                        Dim div As Expression = Expression.exprOp("/", N, D)
                        If retExpr.IsReal AndAlso retExpr.toDouble = 0 Then
                            retExpr = div
                        Else
                            retExpr = Expression.exprOp("+", retExpr, div)
                        End If
                    End If

                End If
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return retExpr
    End Function
    Public Function evalMultiDblToPolyn(bValidIndices() As Boolean, ByVal dbl() As Double) As Polynomial
        Dim retExpr As Polynomial = Nothing
        Try
            If Me.isReal Then
                Return New Polynomial(Me)
            End If
            Dim i, j As Int64
            For i = 0 To cf.Length - 1
                Dim curExpr As New Polynomial(cf(i))
                For j = 0 To exp(i).Length - 1
                    Dim exp1 As Int64 = exp(i)(j)
                    If exp1 Then
                        'Dim mtx As ExprMatrix = oVars.getValueByName(var(j), False)
                        If j > bValidIndices.Length OrElse bValidIndices(j) = False Then
                            Dim eVar As New Expression(
                                Polynomial.GetPolynomial(var(j)))
                            If exp1 = 1 Then
                                curExpr *= Polynomial.GetPolynomial(var(j))
                            Else
                                curExpr *= Polynomial.GetPolynomial(var(j)) ^ New Polynomial(exp1)
                            End If
                        Else
                            If exp1 = 1 Then
                                curExpr *= New Polynomial(dbl(j))
                            Else
                                curExpr *= New Polynomial(dbl(j) ^ exp1)
                            End If
                        End If
                    End If
                Next
                If retExpr Is Nothing Then
                    retExpr = curExpr
                Else
                    retExpr += curExpr
                End If
            Next
            If PolyResto IsNot Nothing Then
                Dim Num As Polynomial = PolyResto.evalMultiDblToPolyn(bValidIndices, dbl)
                Dim Den As Polynomial = PolyDivisor.evalMultiDblToPolyn(bValidIndices, dbl)
                Dim div As Polynomial = Num / Den
                retExpr += div
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return retExpr
    End Function
    Public Function evalRe(ByVal x As Double) As Double
        Dim i As Int64
        Dim reN As Double
        Try
            If Me.isReal Then
                Return Me.cf(0).pRe.ToDouble
            End If
            If exp.Length > 1 Then
                If exp(0)(0) < exp(1)(0) Then
                    Array.Reverse(exp)
                    Array.Reverse(cf)
                End If
            End If
            reN = cf(0).pRe.ToDouble
            If exp.Length AndAlso exp(0).Length Then
                Dim exp1 As Int64 = exp(0)(0)
                Dim exp2 As Int64 = exp1
                For i = 1 To cf.Length - 1
                    exp2 = exp(i)(0)
                    Do While exp1 > exp2
                        reN *= x
                        exp1 -= 1
                    Loop
                    reN += cf(i).pRe.ToDouble
                Next
                Do While exp2 > 0
                    reN *= x
                    exp2 -= 1
                Loop
            End If
            If PolyResto IsNot Nothing Then
                reN += PolyResto.evalRe(x) / PolyDivisor.evalRe(x)
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return reN
    End Function
    Public Function tryEvalRe(ByVal x As Double, ByRef result As Double) As Boolean
        Dim i As Int64
        Dim reN As Double
        Dim bRet As Boolean = False
        Try
            If Me.isReal Then
                Return Me.cf(0).pRe.ToDouble
            End If
            If exp.Length > 1 Then
                If exp(0)(0) < exp(1)(0) Then
                    Array.Reverse(exp)
                    Array.Reverse(cf)
                End If
            End If
            reN = cf(0).pRe.ToDouble
            If exp.Length AndAlso exp(0).Length Then
                Dim exp1 As Int64 = exp(0)(0)
                Dim exp2 As Int64 = exp1
                For i = 1 To cf.Length - 1
                    exp2 = exp(i)(0)
                    Do While exp1 > exp2
                        reN *= x
                        exp1 -= 1
                    Loop
                    reN += cf(i).pRe.ToDouble
                Next
                Do While exp2 > 0
                    reN *= x
                    exp2 -= 1
                Loop
            End If
            If PolyResto IsNot Nothing Then
                reN += PolyResto.evalRe(x) / PolyDivisor.evalRe(x)
            End If
            result = reN
            bRet = True
        Catch ex As Exception
            bRet = False
        End Try
        Return bRet
    End Function
    Public Function evalPrecis(ByVal x As Rational) As Rational
        Dim i As Int64
        Dim prN As Rational
        Try
            If exp.Length > 1 Then
                If exp(0)(0) < exp(1)(0) Then
                    Array.Reverse(exp)
                    Array.Reverse(cf)
                End If
            End If
            prN = New Rational(cf(0).pRe)
            Dim exp1 As Int64 = exp(0)(0)
            Dim exp2 As Int64 = exp1
            For i = 1 To cf.Length - 1
                exp2 = exp(i)(0)
                Dim nVueltas As Int64 = 0
                Do While exp1 > exp2
                    prN *= x
                    exp1 -= 1
                    nVueltas += 1
                Loop
                prN += cf(i).pRe
            Next
            Dim nV2 As Int64 = 0
            Do While exp2 > 0
                prN *= x
                exp2 -= 1
                nV2 += 1
                'If nV2 > 50 Then
                '    Throw New Exception(msg8.num(13))
                'End If
            Loop
            If PolyResto IsNot Nothing Then
                prN += PolyResto.evalPrecis(x) / PolyDivisor.evalPrecis(x)
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return prN
    End Function

    Public Function tryEvalReDecimal(ByVal x As Double, ByRef result As Decimal) As Boolean
        Dim i As Int64
        Dim NDec As Decimal
        Dim xDec As Decimal
        Dim LMax As Int64 =
            1 + Math.Floor(Math.Log10(Decimal.MaxValue))
        Try
            xDec = x
            If exp.Length > 1 Then
                If exp(0)(0) < exp(1)(0) Then
                    Array.Reverse(exp)
                    Array.Reverse(cf)
                End If
            End If
            NDec = cf(0).pRe.ToDecimal
            Dim exp1 As Int64 = exp(0)(0)
            Dim exp2 As Int64 = exp1
            For i = 1 To cf.Length - 1
                exp2 = exp(i)(0)
                Do While exp1 > exp2
                    If NDec = 0 OrElse xDec = 0 Then
                        NDec = 0
                    Else
                        Dim L As Int64 = 2 +
                                Math.Log10(Math.Abs(NDec)) +
                                Math.Log10(Math.Abs(xDec))
                        If L >= LMax Then
                            Return False
                        End If
                        NDec *= xDec
                    End If
                    exp1 -= 1
                Loop
                NDec += CDec(cf(i).pRe.ToDecimal)
            Next
            Do While exp2 > 0
                If NDec = 0 OrElse xDec = 0 Then
                    NDec = 0
                Else
                    Dim L As Int64 = 2 +
                            Math.Log10(Math.Abs(NDec)) +
                            Math.Log10(Math.Abs(xDec))
                    If L >= LMax Then
                        Return False
                    End If
                    NDec *= xDec
                End If
                exp2 -= 1
            Loop
            If PolyResto IsNot Nothing Then
                NDec += PolyResto.evalRe(xDec) / PolyDivisor.evalRe(xDec)
            End If
        Catch ex As Exception
            Return False
        End Try
        result = NDec
        Return True
    End Function
    Public Function evalCjo(ByVal x As Complex) As Complex

        ' Evaluate polynomial 'Me' at point 'x:
        Dim i As Int64
        Dim cjoN As Complex
        Try
            Dim Pa As New Polynomial(Me)
            If Pa.exp.Length > 1 Then
                If Pa.exp(0)(0) < Pa.exp(1)(0) Then
                    Array.Reverse(Pa.exp)
                    Array.Reverse(Pa.cf)
                End If
            End If
            cjoN = New Complex(Pa.cf(0))
            If Pa.exp.Length AndAlso Pa.exp(0).Length Then
                Dim exp1 As Int64 = Pa.exp(0)(0)
                Dim exp2 As Int64 = exp1
                For i = 1 To Pa.cf.Length - 1
                    exp2 = Pa.exp(i)(0)
                    Do While exp1 > exp2
                        cjoN *= x
                        exp1 -= 1
                    Loop
                    cjoN += Pa.cf(i)
                Next
                Do While exp2 > 0
                    cjoN *= x
                    exp2 -= 1
                Loop
            End If
            If PolyResto IsNot Nothing Then
                Dim cjoResto As Complex = PolyResto.evalCjo(x)
                Dim cjoDivis As Complex = PolyDivisor.evalCjo(x)
                cjoN += cjoResto / cjoDivis
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return cjoN
    End Function
    Public Function evalCjo_B(ByVal x As Complex) As Complex

        ' Evaluate polynomial 'Me' at point 'x:
        Dim i As Int64
        Dim cjoN As Complex
        Try
            Dim Pa As New Polynomial(Me)
            If Pa.exp.Length > 1 Then
                If Pa.exp(0)(0) < Pa.exp(1)(0) Then
                    Array.Reverse(Pa.exp)
                    Array.Reverse(Pa.cf)
                End If
            End If
            cjoN = New Complex(Pa.cf(0))
            If Pa.exp.Length AndAlso Pa.exp(0).Length Then
                Dim exp1 As Int64 = Pa.exp(0)(0)
                Dim exp2 As Int64 = exp1
                For i = 1 To Pa.cf.Length - 1
                    exp2 = Pa.exp(i)(0)
                    Do While exp1 > exp2
                        cjoN *= x
                        exp1 -= 1
                    Loop
                    If Double.IsInfinity(cjoN.pRe.ToDouble) OrElse
                    Double.IsInfinity(cjoN.pIm.ToDouble) Then
                        Return Nothing
                    End If
                    cjoN += Pa.cf(i)
                Next
                Do While exp2 > 0
                    cjoN *= x
                    exp2 -= 1
                Loop
            End If
            If PolyResto IsNot Nothing Then
                Dim cjoResto As Complex = PolyResto.evalCjo(x)
                Dim cjoDivis As Complex = PolyDivisor.evalCjo(x)
                cjoN += cjoResto / cjoDivis
            End If
        Catch ex As Exception
            cjoN = Nothing
        End Try
        Return cjoN
    End Function

    Public Sub multByPolyDivisor()
        If PolyResto Is Nothing Then
            Return
        End If
        Dim Pc As New Polynomial(Me)
        Pc.PolyResto = Nothing
        Pc.PolyDivisor = Nothing
        Pc = Pc * Me.PolyDivisor + Me.PolyResto
        copyToThis(Pc)
    End Sub
    Public Function hasComplexCoeff() As Boolean
        Dim i As Int64
        For i = 0 To cf.Length - 1
            If cf(i).pIm.ToDouble Then
                Return True
            End If
        Next
        Return False
    End Function
    Public Function isReal() As Boolean
        Try
            If hasComplexCoeff() Then
                Return False
            End If
            If isComplex() AndAlso cf(0).pIm.IsZero Then
                Return True
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return False
    End Function
    Public Function IsOnlyFraction() As Boolean
        If PolyResto IsNot Nothing AndAlso
        (cf.Length = 0 OrElse
         (cf.Length = 1 AndAlso cf(0).IsZero)) Then
            Return True
        End If
        Return False
    End Function
    Public Function isComplex() As Boolean
        Try
            If PolyResto Is Nothing Then
                For i As Int64 = 0 To cf.Length - 1
                    If exp(i) IsNot Nothing Then
                        For j = 0 To exp(i).Length - 1
                            If exp(i)(j) Then
                                Return False
                            End If
                        Next
                    End If
                Next
                Return True
            Else
                If PolyResto.isComplex Then
                    If PolyDivisor.isComplex Then
                        Return True
                    End If
                End If
                Return False
            End If
        Catch ex As Exception

        End Try
        Return False
    End Function
    Public Function IsMultiVar() As Boolean
        If Not isComplex() Then
            If PolyResto Is Nothing Then
                If var1.Length < 2 Then
                    Return False
                Else
                    Return True
                End If
            Else
                Dim unionVar() As String = varUnion(Me, PolyResto)
                If unionVar.Length = 0 Then
                    Return (PolyDivisor.var1.Length > 1)
                End If
                If unionVar.Length > 1 Then
                    Return True
                ElseIf PolyDivisor.var1.Length > 1 OrElse
                    (PolyDivisor.var1.Length = 1 AndAlso
                     Array.IndexOf(unionVar, PolyDivisor.var1(0))) = -1 Then
                    Return True
                End If
            End If
        End If
        Return False
    End Function
    Public Function getDegree() As Int64
        Dim degree As Int64 = -1
        Try
            Dim oS As New polynomialSort(exp, var, cf)
            degree = oS.degree ' Pa.opDegree(Nothing, New String() {""}, New Int64() {0})
        Catch ex As Exception
        End Try
        Return degree
    End Function
    Public Function getMaxExponent(Optional ByRef retVarMaxExponent As String = "") As Int64
        Dim max As Int64 = 0
        Try
            Dim i, j As Int64
            For i = 0 To exp.Length - 1
                If exp(i) IsNot Nothing Then
                    For j = 0 To exp(i).Length - 1
                        If exp(i)(j) > max Then
                            max = exp(i)(j)
                            retVarMaxExponent = var(j)
                        End If
                    Next
                End If
            Next
        Catch ex As Exception
            Throw ex
        End Try
        Return max
    End Function
    Public Function getMaxExponentOfVar(var As String) As Int64
        Dim max As Int64 = 0
        Try
            Dim i As Int64
            For i = 0 To exp.Length - 1
                If exp(i) IsNot Nothing Then
                    Dim pos As Int64 = Array.IndexOf(var1, var)
                    If pos > -1 AndAlso pos < exp(i).Length Then
                        If exp(i)(pos) > max Then
                            max = exp(i)(pos)
                        End If
                    End If
                End If
            Next
        Catch ex As Exception
            Throw ex
        End Try
        Return max
    End Function
    Public ReadOnly Property var() As String()
        Get
            Return var1
        End Get
    End Property
    Public Sub setVars(ByVal newvar() As String)
        ReDim Me.var1(newvar.Length - 1)
        Array.Copy(newvar, Me.var1, newvar.Length)
    End Sub
    Public Sub setVar(ByVal iVar As Int64, ByVal index As Int64)
        Try
            If index < var1.Length Then
                var1(index) = iVar
            End If
        Catch ex As Exception

        End Try
    End Sub
    Private Function Grade_Of_a_coeff(Optional ByVal sum_exp_of_coeff_I As Int64 = -1) As Int64
        Dim dbSum As Int64 = 0
        Dim i As Int64
        Try
            ' add exponents of all variables for the same coefficient
            Dim j As Int64 = IIf(sum_exp_of_coeff_I = -1, cf.Length - 1, sum_exp_of_coeff_I)
            For i = 0 To exp.Length - 1
                dbSum += exp(i)(j)
            Next
        Catch ex As Exception
            Throw ex
        End Try
        Return dbSum
    End Function
    Public Shared Function GradePolyn_of_1var(ByVal Pa As Polynomial) As Int64
        ' Supposedly Pa has been sorted previously
        Return Pa.Grade_Of_a_coeff
    End Function

    Public Shared Function opRoots(ByRef PaOrig As Polynomial,
                Optional ByVal bByPassFirstDegreeCondition As Boolean = False,
                Optional ByVal cfg As Config = Nothing,
                Optional bReverse As Boolean = False) As retOpRoots
        Dim ret1 As New retOpRoots, iRet As Int64 = 0
        Try
            If cfg Is Nothing Then cfg = New Config
            ret1 = opRootsB(PaOrig, bByPassFirstDegreeCondition, cfg, bReverse)
            If ret1.cjo.Length <> PaOrig.getDegree Then
                Dim db() As Double = {1000000.0, 0.000001, 1000000000000.0, 0.000000000001, 1.0E+18, 1.0E-18}
                For i As Int32 = 0 To db.Length - 1
                    Dim yExpr As New Expression(PaOrig)
                    Dim y As Polynomial = Polynomial.GetPolynomial("y")
                    y *= New Polynomial(New Complex(db(i)))
                    Dim oVar As New VarsAndFns(cfg)
                    Dim sVar As String = PaOrig.varAll(0)
                    oVar.AddVar(sVar, New Expression(y))
                    yExpr = yExpr.evalExprToExpr(oVar)
                    Dim yPa As Polynomial = yExpr.getPolynomial
                    Dim ret2 As New retOpRoots
                    ret2 = opRootsB(yPa, bByPassFirstDegreeCondition, cfg, bReverse)
                    If ret2.cjo.Length = PaOrig.getDegree Then
                        ReDim ret1.cjo(ret2.cjo.Length - 1)
                        For j As Int32 = 0 To ret1.cjo.Length - 1
                            ret1.cjo(j) = ret2.cjo(j) * db(i)
                        Next
                        ret1.mtx = New Matrix(0.0)
                        ReDim ret1.mtx.vVect(0)
                        ret1.mtx.vVect(0) = New Vector
                        ReDim ret1.mtx.vVect(0).vPoly(ret1.cjo.Length - 1)
                        For j = 0 To ret2.cjo.Length - 1
                            ret1.mtx.vVect(0).vPoly(j) = New Polynomial(ret2.cjo(j) * db(i))
                        Next
                        Dim vCjoSort()() As Complex = Complex.sortRootsByType(ret1.cjo, cfg)
                        ret1.cjoByType = vCjoSort
                        PaOrig.roots = ret1
                        PaOrig.roots.mtx = Matrix.opTranspose(ret1.mtx)
                        Exit For
                    End If
                Next
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return ret1
    End Function

    Public Shared Function opRootsB(ByRef PaOrig As Polynomial,
                Optional ByVal bByPassFirstDegreeCondition As Boolean = False,
                Optional ByVal cfg As Config = Nothing,
                Optional bReverse As Boolean = False) As retOpRoots

        Dim ret1 As New retOpRoots, iRet As Int64 = 0
        Dim bDetail As Boolean
        Dim bDoGCD As Boolean = Polynomial.bDoGCD
        Dim bRound As Boolean
        Try
            Polynomial.bDoGCD = False
            Dim Pa1 As New Polynomial(PaOrig)
            Dim k As Int64
            Dim i As Int64
            ReDim ret1.cjo(-1)
            Dim sVar As String = Pa1.varAll(0) '= Vc.vPoly(i).var(0)
            If cfg Is Nothing Then
                cfg = Config.cfg
            End If
            bRound = cfg.bRounding
            cfg.bRounding = False

            If Not bByPassFirstDegreeCondition AndAlso
            Pa1.getDegree > MathGlobal8.maxPolyDegree Then
                Return Nothing
            End If
            bDetail = cfg.bDetail
            cfg.bDetail = False
            Dim exp1 As Int64 = 0
            Dim Pc As Polynomial = New Polynomial(Pa1)
            If Pc.PolyResto IsNot Nothing Then
                Dim Presto As New Polynomial(Pc.PolyResto)
                Dim Pdiv As New Polynomial(Pc.PolyDivisor)
                Pc.PolyResto = Nothing
                Pc.PolyDivisor = Nothing
                Pc = Pdiv * Pc + Presto
                Pa1 = Pc
            End If

            Dim Pa As New Polynomial(Pa1)
            If Pa.exp.Length Then
                ' ver si tiene 
                ' exponentes negativos:
                exp1 = Pa.exp(Pa.exp.Length - 1)(0)
                If exp1 < 0 Then
                    For j As Int64 = 0 To Pa.exp.Length - 1
                        ' incrementar el grado del polynomio:
                        Pa.exp(j)(0) -= exp1
                    Next
                End If
            End If
            ' Find real roots for real coeff. polynomial:
            If Pa1.hasComplexCoeff Then
                'Dim soe As New SystemOfEquations(Nothing, New ExprMatrix(Pa1), Nothing, cfg.cur, cfg)
                'Dim cjoRoots(-1) As Complex
                'soe.roots_fn_1var_Newton_Raphson(New Expression(Pa1), cfg, "", cjoRoots, Nothing)
                Dim cjoRoots() As Complex = opRootsOfPoly_Newton(Pa)
                ReDim Preserve ret1.cjo(iRet + cjoRoots.Length - 1)
                Array.Copy(cjoRoots, 0, ret1.cjo, iRet, cjoRoots.Length)
            Else
                Dim realRoots() As Complex

                realRoots = opRealRoots_Junque(
                        sVar,
                        Pa1, New Polynomial(Pa1), Pa1.getDegree) ' find ALL real roots
                'Dim degree As Int64 = Vc.vPoly(i).opDegree(Nothing, Va.vPoly(i).varAll, New Int64() {0})
                Dim degree As Int64 = Pa1.getDegree
                If realRoots.Length > 0 Then
                    Dim vDen As New Vector(realRoots)
                    Dim polyDen As Polynomial = vDen.RootsToPolynomial
                    polyDen.setVars(New String() {sVar})
                    Pa1 /= polyDen
                    Pa1.PolyResto = Nothing
                    Pa1.PolyDivisor = Nothing
                    ReDim ret1.cjo(realRoots.Length - 1)
                    Dim j As Int64
                    For j = 0 To realRoots.Length - 1
                        ret1.cjo(j) = New Complex(realRoots(j))
                    Next
                    iRet = realRoots.Length
                End If
                degree = Pa1.getDegree
                Dim imgRoots(-1) As Complex

                ' try to find imaginary roots of the form x = b*i 
                ' Replacing x = i*y, if there are real roots y=(+/-)r1 for P(y)=0 these
                ' will be purely imaginary x roots = (+/-) i * r1
                If False AndAlso degree Then
                    Dim yExpr As New Expression(Pa1)
                    Dim y As Polynomial = Polynomial.GetPolynomial("y")
                    y *= New Polynomial(New Complex(0, 1))
                    Dim oVar As New VarsAndFns(cfg)
                    oVar.AddVar(sVar, New Expression(y))
                    yExpr = yExpr.evalExprToExpr(oVar)
                    Dim yPa As Polynomial = yExpr.getPolynomial
                    imgRoots = opRealRoots_Junque("y",
                            yPa, New Polynomial(yPa), yPa.getDegree) ' find ALL real roots
                    If imgRoots.Length > 0 Then
                        Dim vCjo()() As Complex = Complex.sortRootsByType(imgRoots, cfg)
                        imgRoots = vCjo(0) ' extract only real roots
                        degree = Pa1.getDegree

                        Dim lenR1 As Int64 = realRoots.Length
                        ReDim Preserve realRoots(lenR1 + imgRoots.Length - 1)
                        'ReDim Preserve ret1.cjo(lenR1 + imgRoots.Length - 1)
                        Dim img(imgRoots.Length - 1) As Complex
                        For j = 0 To imgRoots.Length - 1 Step 2
                            If imgRoots(j).pIm.IsZero Then
                                imgRoots(j) *= -Complex.i
                                img(j) = New Complex(imgRoots(j))
                            Else
                                img(j) = New Complex(imgRoots(j))
                            End If
                            realRoots(lenR1 + j) = New Complex(imgRoots(j))
                            If j + 1 >= img.Length Then
                                Exit For
                            End If
                            img(j + 1) = Complex.opConjugate(img(j))
                            If Math.Sign(imgRoots(j).pRe.ToDouble) =
                                -Math.Sign(imgRoots(j + 1).pRe.ToDouble) Then
                                realRoots(lenR1 + j + 1) = New Complex(imgRoots(j))
                            Else
                                realRoots(lenR1 + j + 1) = New Complex(-imgRoots(j))
                            End If
                        Next
                        Dim vDen As Vector = New Vector(img)
                        Dim polyDen As Polynomial = vDen.RootsToPolynomial
                        For j = 0 To polyDen.cf.Length - 1
                            ' All coefficients should be real. 
                            ' So, round, forcing any imaginary to zero:
                            polyDen.cf(j) = New Complex(polyDen.cf(j).pRe, New Rational(0.0))
                        Next
                        polyDen.setVars(New String() {sVar})
                        Pa1 /= polyDen
                        Pa1.PolyResto = Nothing
                        Pa1.PolyDivisor = Nothing
                        ret1.cjo = realRoots
                        iRet = realRoots.Length
                        degree = Pa1.getDegree ' Vc.vPoly(i).opDegree(Nothing, Vc.vPoly(i).varAll, New Int64() {0})
                    End If
imgPuraNoHallada:
                End If
                'imgRoots = opRootsImg_Junque(Pa)
                'If imgRoots.Length Then
                '    Dim vDen As New Vector(imgRoots)
                '    Dim polyDen As Polynomial = vDen.RootsToPolynomial
                '    polyDen.setVars(New String() {sVar})
                '    Pa1 /= polyDen
                '    Pa1.PolyResto = Nothing
                '    Pa1.PolyDivisor = Nothing
                '    ReDim Preserve ret1.cjo(iRet + imgRoots.Length - 1)
                '    ReDim Preserve ret1.cjo(iRet + imgRoots.Length - 1)
                '    Array.Copy(imgRoots, 0, ret1.cjo, iRet, imgRoots.Length)
                '    iRet = ret1.cjo.Length
                '    degree = Pa1.getDegree
                'End If

                If True OrElse Not bByPassFirstDegreeCondition Then
                    ' Find only imaginary roots:
                    If (True OrElse degree < 40) AndAlso Pa1.cf.Length > 1 Then
                        Dim Pb As Polynomial = Polynomial.opNormalize(Pa1)
                        ''imgRoots = opImgRoots_Prueba(sVar, Vc.vPoly(i), _
                        ''               New Polynomial(Vc.vPoly(i)), Vc.vPoly(i).getDegree)

                        'imgRoots = opRoots_BairstowPrecis(sVar, Pa1,
                        '              New Polynomial(Pa1), Pa1.getDegree) ' 

                        ''imgRoots = opRoots_BairstowDbl(sVar, Vc.vPoly(i), _
                        ''              New Polynomial(Vc.vPoly(i)), Vc.vPoly(i).getDegree) ' 

                        For k = 0 To imgRoots.Length - 1
                            If imgRoots(k).pIm.IsZero Then
                                ' Really is not a Real Root?? ...not enough precision??
                                imgRoots(k) = New Complex(imgRoots(k).pRe, New Rational(1.0E-150))
                                'ReDim imgRoots(-1)
                                'Exit For
                            End If
                        Next
                        If imgRoots.Length = degree Then
                            Pa1 = New Polynomial(0.0)
                            ReDim Preserve ret1.cjo(iRet + imgRoots.Length - 1)
                            Array.Copy(imgRoots, 0, ret1.cjo, iRet, imgRoots.Length)
                            iRet = ret1.cjo.Length
                            degree = 0
                        ElseIf imgRoots.Length > 0 Then
                            Dim vDen As New Vector(imgRoots)
                            Dim polyDen As Polynomial = vDen.RootsToPolynomial
                            polyDen.setVars(New String() {sVar})
                            Pa1 /= polyDen
                            Pa1.PolyResto = Nothing
                            Pa1.PolyDivisor = Nothing
                            ReDim Preserve ret1.cjo(iRet + imgRoots.Length - 1)
                            Array.Copy(imgRoots, 0, ret1.cjo, iRet, imgRoots.Length)
                            iRet = ret1.cjo.Length
                            degree = Pa1.getDegree
                        End If
                    End If
                    'If degree > 0 AndAlso Pa1.cf.Length > 1 Then
                    '    Dim Pb As Polynomial = Polynomial.opNormalize(Pa1)
                    '    imgRoots = Functions.ImagRoots_Newton_Raphson(Pb, cfg)
                    '    If imgRoots.Length > 0 Then
                    '        Dim vDen As New Vector(imgRoots)
                    '        Dim polyDen As Polynomial = vDen.RootsToPolynomial
                    '        polyDen.setVars(New String() {sVar})
                    '        Pa1 /= polyDen
                    '        Pa1.PolyResto = Nothing
                    '        Pa1.PolyDivisor = Nothing
                    '        ReDim Preserve ret1.cjo(iRet + imgRoots.Length - 1)
                    '        Array.Copy(imgRoots, 0, ret1.cjo, iRet, imgRoots.Length)
                    '        iRet = ret1.cjo.Length
                    '        degree = Pa1.getDegree
                    '    End If
                    'End If

                    If degree > 0 AndAlso Pa1.cf.Length > 1 _
                    AndAlso True Then ' degree < MathGlobal8.maxPolyDegree Then
                        ' Find complex roots employing Durand Kernel method:
                        Dim Pb As Polynomial = Polynomial.opNormalize(Pa1)
                        'Dim cjoRoots() As Complex = opRoots_DuranKernel(Pb)
                        'Dim cjoRoots() As Complex = opRoots_DuranKernel(Pb)
                        Dim cjoRoots() As Complex = opRootsOfPoly_Newton(Pa1)
                        iRet = ret1.cjo.Length
                        ReDim Preserve ret1.cjo(iRet + cjoRoots.Length - 1)
                        Array.Copy(cjoRoots, 0, ret1.cjo, iRet, cjoRoots.Length)
                        iRet = ret1.cjo.Length
                        'If cjoRoots.Length < degree Then
                        '    Dim vDen As New Vector(cjoRoots)
                        '    Dim polyDen As Polynomial = vDen.RootsToPolynomial
                        '    polyDen.setVars(New String() {sVar})
                        '    Pb = Pa1 / polyDen
                        '    Pb.PolyResto = Nothing
                        '    Pb.PolyDivisor = Nothing
                        '    iRet = ret1.cjo.Length
                        '    cjoRoots = opRoots_DuranKernel(Pb)
                        '    If cjoRoots.Length Then
                        '        ReDim Preserve ret1.cjo(iRet + cjoRoots.Length - 1)
                        '        Array.Copy(cjoRoots, 0, ret1.cjo, iRet, cjoRoots.Length)
                        '    End If
                        'End If
                    End If
                End If


            End If

            Dim vCjoSort()() As Complex = Complex.sortRootsByType(ret1.cjo, cfg)
            k = 0
            For i = 0 To vCjoSort.Length - 1
                'vCjoSort(i) = Complex.sortRoots(vCjoSort(i))
                If vCjoSort(i) IsNot Nothing Then
                    If bReverse Then
                        Array.Reverse(vCjoSort(i))
                    End If
                    For j As Int64 = 0 To vCjoSort(i).Length - 1
                        ret1.cjo(k) = vCjoSort(i)(j)
                        k += 1
                    Next
                End If
            Next
            ret1.mtx = New Matrix(0.0)
            ReDim ret1.mtx.vVect(0)
            ret1.mtx.vVect(0) = New Vector
            ReDim ret1.mtx.vVect(0).vPoly(ret1.cjo.Length - 1)
            For i = 0 To ret1.cjo.Length - 1
                ret1.mtx.vVect(0).vPoly(i) = New Polynomial(ret1.cjo(i))
            Next
            ret1.cjoByType = vCjoSort
            'Dim Vb As New Vector(ret1.cjo)
            'Vb = Vb.sortRoots
            'For i = 0 To ret1.cjo.Length - 1
            '    ret1.cjo(i) = Vb.vPoly(i).ToComplex
            'Next
            'ret1.mtx.vVect(0) = ret1.mtx.vVect(0).sortRoots
            ret1.mtx = Matrix.opTranspose(ret1.mtx)
            PaOrig.roots = ret1
        Catch ex As Exception
            Throw New Exception(ex.Message)
        Finally
            cfg.bDetail = bDetail
            cfg.bRounding = bRound
            Polynomial.bDoGCD = bDoGCD
        End Try
        Return ret1
    End Function

    Public Shared Function opRootsImg_Junque(Pa As Polynomial) As Complex()
        Dim rootsG(-1) As Complex
        Try
            Dim Porig As New Polynomial(Pa)
            Dim sVar As String = Pa.var(0)
            Dim Pavar As Polynomial = Polynomial.GetPolynomial(sVar)
            Dim paso As Double = Math.PI / 100
            Dim root(0) As Complex
            Dim oVar As New VarsAndFns(Pa.cfg)
            oVar.AddVar(sVar, Nothing)
            Dim bFound As Boolean = False
            Do
                Dim degree As Int64 = Pa.getDegree
                Dim i As Int64
                If degree < 3 Then
                    If degree < 1 Then
                        Exit Try
                    ElseIf degree = 2 Then
                        Dim a, b, c As Complex
                        a = Pa.cf(0)
                        If Pa.cf.Length = 2 Then
                            ' a*x^2 + c = 0
                            c = Pa.cf(1)
                            root(0) = -(-c / a) ^ Complex.oneHalf
                            ReDim Preserve rootsG(rootsG.Length)
                            rootsG(rootsG.Length - 1) = root(0)
                            root(0) = (-c / a) ^ Complex.oneHalf
                            ReDim Preserve rootsG(rootsG.Length)
                            rootsG(rootsG.Length - 1) = root(0)
                            Exit Try
                        Else
                            ' a*x^2 + b*x + c = 0
                            b = Pa.cf(1)
                            c = Pa.cf(2)
                            root(0) = (-b + (b * b - 4 * a * c) ^ Complex.oneHalf) / (2.0 * a)
                            i = rootsG.Length
                            ReDim Preserve rootsG(rootsG.Length)
                            rootsG(rootsG.Length - 1) = root(0)
                            root(0) = (-b - (b * b - 4 * a * c) ^ Complex.oneHalf) / (2.0 * a)
                            ReDim Preserve rootsG(rootsG.Length)
                            rootsG(rootsG.Length - 1) = root(0)
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
                            Exit Try
                        End If
                    Else
                        root(0) = -Pa.cf(1) / Pa.cf(0)
                        ReDim Preserve rootsG(rootsG.Length)
                        rootsG(rootsG.Length - 1) = root(0)
                        Exit Try
                    End If
                End If
                Dim exprPa As New Expression(Pa)
                bFound = False
                Dim d As Double = 1.0
                Dim a1 As Double = 0
                Dim aVal As Double = 0
                oVar.setValue(0, New ExprMatrix(
                            New Complex(Math.Cos(a1), -Math.Sin(a1)) * d))
                Dim expr As Expression = exprPa.evalExprToExpr(oVar)
                Dim roots(0) As Complex
                aVal = expr.toComplex.pIm.ToDouble * 10 ^ 6
                If aVal = 0 Then
                    aVal = expr.toComplex.pRe.ToDouble
                End If


                Dim b1 As Double = Math.PI
                Dim bVal As Double = 0
                oVar.setValue(0, New ExprMatrix(
                            New Complex(Math.Cos(b1), -Math.Sin(b1)) * d))
                expr = exprPa.evalExprToExpr(oVar)

                bVal = expr.toComplex.pIm.ToDouble * 10 ^ 6
                If bVal = 0 Then
                    bVal = expr.toComplex.pRe.ToDouble
                End If

                Dim M As Double = Math.PI / 2
                Dim mVal As Double = 0
                Do
                    M = (a1 + b1) / 2
                    If Math.Abs(M - a1) < 1.0E-100 OrElse
                    Math.Abs(M - b1) < 1.0E-100 Then Exit Do
                    oVar.setValue(0, New ExprMatrix(
                            New Complex(Math.Cos(M), -Math.Sin(M)) * d))
                    expr = exprPa.evalExprToExpr(oVar)
                    'roots = opRoots_BairstowDbl(sVar, PaInNewCoordinates,
                    '              New Polynomial(PaInNewCoordinates), PaInNewCoordinates.getDegree, False)
                    roots(0) = New Complex(d, M)
                    mVal = expr.toComplex.pIm.ToDouble * 10 ^ 6
                    If mVal = 0 Then
                        mVal = expr.toComplex.pRe.ToDouble
                    End If
                    If Math.Abs(mVal) < Math.Abs(aVal) Then
                        If a1 = M Then Exit Do
                        a1 = M
                        aVal = mVal
                    Else
                        If b1 = M Then Exit Do
                        b1 = M
                        bVal = mVal
                    End If
                Loop
                'If  roots IsNot Nothing AndAlso roots.Length AndAlso roots(0).IsReal Then
                If True Then
                    Dim ang As Double = M ' roots(0).pRe.ToDouble * Math.PI
                    root(0) = New Complex(Math.Cos(ang), -Math.Sin(ang))
                    Dim PaVal As Double = Pa.evalCjo(root(0)).opModulo
                    ReDim Preserve rootsG(rootsG.Length)
                    rootsG(rootsG.Length - 1) = root(0)
                    Dim negPaval As Double = Pa.evalCjo(New Complex(root(0).pRe, -root(0).pIm)).opModulo
                    Dim x As Polynomial = Polynomial.GetPolynomial(sVar)
                    x -= New Polynomial(root(0))
                    ' 'deflate' Pa:
                    Pa /= x
                    Pa.PolyResto = Nothing
                    Pa.PolyDivisor = Nothing
                    bFound = True
                    root(0).pIm = -root(0).pIm
                    If negPaval <= PaVal Then
                        ReDim Preserve rootsG(rootsG.Length)
                        rootsG(rootsG.Length - 1) = root(0)
                        x = Polynomial.GetPolynomial(sVar)
                        x -= New Polynomial(root(0))
                        ' 'deflate' Pa:
                        Pa /= x
                        Pa.PolyResto = Nothing
                        Pa.PolyDivisor = Nothing
                    End If
                    degree = Pa.getDegree
                End If
            Loop While bFound
        Catch ex As Exception

        End Try
        Return rootsG
    End Function
    Public Shared Function opRootsOfPoly_Newton(ByVal Pc As Polynomial, Optional cfg As Config = Nothing) As Complex()

        Dim ret(-1) As Complex, nRoots As Int64 = 0
        Dim rootsG(-1) As Complex
        Dim bDoGCD As Boolean = Polynomial.bDoGCD
        Polynomial.bDoGCD = False
        Try
            Dim Pa As New Polynomial(Pc)
            If cfg Is Nothing Then
                cfg = New Config
                cfg.doTimeOut = timeout.whenTimeIsOver
                cfg.timeOutms = 180 * 1000
            End If
            Dim Pcorig As New Polynomial(Pc)
            Pc = Polynomial.opNormalize(Pc)
            Pa = New Polynomial(Pc)
            Dim sVar As String = Pc.var(0)

            Dim i As Int64
            Dim xP As Polynomial = Polynomial.GetPolynomial(sVar)

            Dim oS As New polynomialSort(Pa.exp, Pa.varAll, Pa.cf)
            Dim degree As Int64
            degree = oS.degree
            If degree < 1 Then
                Exit Try
            End If

            Dim root() As Complex = {Nothing, Nothing}
            Dim exp1 As Int64 = 0
            If Pa.exp.Length Then
                ' ver si tiene raíz = 0
                exp1 = Pa.exp(Pa.exp.Length - 1)(0)
                Dim nOrigDeg As Int64 = Pcorig.getDegree
                Dim bOrigDegree As Boolean =
                        IIf(Pa.getDegree = nOrigDeg, True, False)
                Do While exp1 > 0
                    ReDim Preserve rootsG(rootsG.Length)
                    rootsG(rootsG.Length - 1) = New Complex(0.0) ' cero es raíz
                    If rootsG.Length = nOrigDeg Then
                        Exit Try
                    End If
                    nRoots += 1
                    exp1 -= 1
                    For i = 0 To Pa.exp.Length - 1
                        Pa.exp(i)(0) -= 1
                    Next
                Loop
                degree = Pa.getDegree
                If nRoots Then
                    Pcorig /= xP ^ New Polynomial(nRoots)
                End If
            End If

            ' |min| = 0.5*Min[ (|a0/ak|)^(1/k) ]
            Dim min As Double = Double.MaxValue
            Dim max As Double = 0.0
            Dim a0 As Complex = Pa.cf(Pa.cf.Length - 1)
            For i = 0 To Pa.cf.Length - 1
                If Not Pa.cf(i).IsZero Then
                    Dim exp As Int64 = Pa.exp(i)(0)
                    Dim cjo As Complex = a0 / Pa.cf(i)
                    cjo = cjo ^ New Complex(1.0 / exp)
                    Dim norm As Double = cjo.opNorm
                    If norm < min Then min = norm
                End If
                max += Pa.cf(i).opNorm
            Next
            min /= 2.0
            Dim D1 As Polynomial = Pa.opDerivative(sVar)
            Dim D2 As Polynomial = D1.opDerivative(sVar)
            Dim nIter As Int64 = 0
            Dim rnd As New Random(1)
            Dim initialValues() As Complex = {
                    New Complex(min, -min), New Complex(-min, min),
                     New Complex(0.9, -0.5),
                    Complex.minusi, Complex.i, Complex.zero, New Complex(0.5), New Complex(-0.5)
                    }
            Dim ts As New TimeSpan(Now.Ticks)
            Dim bFound As Boolean = False
            Dim nSolutions As Int64 = 0
            Dim rup As New ReduceExprUsingPolynomials
            Dim iVal(40 + initialValues.Length - 1) As Complex
            Array.Copy(initialValues, 0, iVal, 0, initialValues.Length)
            bFound = False
            For i = initialValues.Length To iVal.Length - 1 - 40
                If rnd.Next(0, 2) = 0 Then
                    iVal(i) = New Complex(
                            (-1) ^ rnd.Next(0, 2) *
                            rnd.NextDouble *
                            10 ^ rnd.Next(-2, 2))
                Else
                    iVal(i) = New Complex(
                            (-1) ^ rnd.Next(0, 2) *
                            rnd.NextDouble *
                            10 ^ rnd.Next(-2, 2),
                            (-1) ^ rnd.Next(0, 2) *
                            rnd.NextDouble *
                            10 ^ rnd.Next(-2, 2))
                End If
            Next
            Dim k As Int64 = 1
            For i = iVal.Length - 1 - 39 To iVal.Length - 30
                iVal(i) = New Complex(k)
                k += 1
            Next
            k = -1
            For i = iVal.Length - 1 - 29 To iVal.Length - 20
                iVal(i) = New Complex(k)
                k -= 1
            Next
            k = 0
            For i = iVal.Length - 1 - 20 To iVal.Length - 1
                iVal(i) = New Complex(k * (-1) ^ rnd.Next(0, 2),
                                            k * (-1) ^ rnd.Next(0, 2))
                k += 1
            Next
            Dim iV3(iVal.Length - 1) As Complex
            For i = 0 To iVal.Length - 1
                If Not iVal(i).IsZero Then
                    iV3(i) = Complex.one / iVal(i)
                Else
                    iV3(i) = Complex.zero
                End If
            Next
            i = iVal.Length
            ReDim Preserve iVal(i * 2 - 1)
            For i = 0 To iV3.Length - 1
                iVal(iV3.Length + i) = iV3(i)
            Next
            i = iVal.Length
            ReDim Preserve iVal(i * 2 - 1)
            For i = 0 To iVal.Length / 2 - 1
                iVal(i + iVal.Length / 2) = -iVal(i)
            Next
            Dim t0 As New TimeSpan(Now.Ticks)
            Dim nIntentos As Int64 = 0
            Dim bHallado As Boolean = False
            Dim two As New Complex(2.0)
            Dim numRoots As Int64 = Pa.getDegree
            Do
                bHallado = False
                Dim xk As Complex = Complex.zero
                bFound = False
                Dim Px As New Complex(0)
                Dim rootsBaistow(-1) As Complex
                If Pa.hasComplexCoeff Then
                Else
                    rootsBaistow = Polynomial.opRoots_BairstowDbl(sVar, Pa, Pa, degree)
                    i = iVal.Length
                    ReDim Preserve iVal(i + rootsBaistow.Length - 1)
                    Array.Copy(iVal, 0, iVal, rootsBaistow.Length, i)
                    Array.Copy(rootsBaistow, iVal, rootsBaistow.Length)
                End If
                For i = 0 To 10 ' iVal.Length - 1
                    xk = iVal(i)
                    For nIter = 1 To 150
                        Try
                            Px = Pa.evalCjo_B(xk)
                            If Px Is Nothing Then GoTo sig
                            If Px.IsZero Then
                                Exit For
                            End If
                            xk -= Px / D1.evalCjo(xk)
                        Catch ex As Exception
                            GoTo sig
                        End Try
                    Next
                    Try
                        Dim bHayRaiz As Boolean = False
                        Dim divisor As New Polynomial(1.0)
                        For iX = 0 To 1
                            Dim Cumple As Int64 = 0
                            If iX = 1 AndAlso Math.Abs(xk.pIm.ToDouble) < 0.001 Then
                                Exit For
                            End If
                            If iX = 1 Then xk = Complex.opConjugate(xk)
                            Dim PxOrig As Complex
                            If nIntentos = 0 Then
                                PxOrig = Pcorig.evalCjo(xk)
                            Else
                                PxOrig = Px
                            End If
                            If Px.opNorm < min AndAlso PxOrig.opNorm < min Then
                                Cumple = 4
                            Else
                                Dim sgnRe As Int64 = Math.Sign(PxOrig.pRe.ToDouble)
                                Dim lg As Int64 = 0
                                Dim xkRe As Double = Math.Abs(xk.pRe.ToDouble)
                                If xkRe Then
                                    lg = Math.Log10(Math.Abs(xk.pRe.ToDouble))
                                End If
                                lg -= 14
                                If sgnRe = 0 Then
                                    Cumple += 1
                                Else
                                    Dim incr As New Complex(10 ^ lg)
                                    If sgnRe <> Math.Sign(Pcorig.evalCjo(xk _
                                   + incr).pRe.ToDouble) Then
                                        Cumple += 1
                                    ElseIf sgnRe <> Math.Sign(Pcorig.evalCjo(xk _
                                  - incr).pRe.ToDouble) Then
                                        Cumple += 1
                                    End If
                                End If
                                If Cumple Then
                                    Dim sgnIm As Int64 = Math.Sign(PxOrig.pIm.ToDouble)
                                    If sgnIm = 0 Then
                                        Cumple += 1
                                    Else
                                        Dim incr As New Complex(0, 10 ^ lg)
                                        If sgnIm <> Math.Sign(Pcorig.evalCjo(xk _
                                   + incr).pIm.ToDouble) Then
                                            Cumple += 1
                                        ElseIf sgnIm <> Math.Sign(Pcorig.evalCjo(xk _
                                  - incr).pIm.ToDouble) Then
                                            Cumple += 1
                                        End If
                                    End If
                                End If
                                If Cumple = 2 Then
                                    sgnRe = Math.Sign(Px.pRe.ToDouble)
                                    If sgnRe = 0 Then
                                        Cumple += 1
                                    Else
                                        Dim incr As New Complex(0, 10 ^ lg)
                                        If sgnRe <> Math.Sign(Pa.evalCjo(xk _
                                   + incr).pRe.ToDouble) Then
                                            Cumple += 1
                                        ElseIf sgnRe <> Math.Sign(Pa.evalCjo(xk _
                                  - incr).pRe.ToDouble) Then
                                            Cumple += 1
                                        End If
                                    End If
                                    If Cumple = 3 Then
                                        Dim sgnIm As Int64 = Math.Sign(Px.pIm.ToDouble)
                                        If sgnIm = 0 Then
                                            Cumple += 1
                                        Else
                                            Dim incr As New Complex(0, 10 ^ lg)
                                            If sgnIm <> Math.Sign(Pa.evalCjo(xk _
                                   + incr).pIm.ToDouble) Then
                                                Cumple += 1
                                            ElseIf sgnRe <> Math.Sign(Pa.evalCjo(xk _
                                  - incr).pIm.ToDouble) Then
                                                Cumple += 1
                                            End If
                                        End If
                                    End If
                                End If
                            End If
                            If Cumple = 4 Then
                                bHallado = True
                                bHayRaiz = True
                                root(0) = xk
                                ReDim Preserve rootsG(rootsG.Length)
                                rootsG(rootsG.Length - 1) = root(0)
                                'Pa /= (xP - New Polynomial(root(0)))
                                Try
                                    'xP.cfg.doTimeOut = timeout.never
                                    divisor *= (xP - New Polynomial(root(0)))
                                Catch ex As Exception

                                End Try
                            End If

                        Next
                        If bHayRaiz Then
                            Pa /= divisor
                            degree = Pa.getDegree
                            If degree < 1 OrElse rootsG.Length >= Pcorig.getDegree Then Exit Do
                            Try
                                If Pa.PolyResto IsNot Nothing Then
                                    Pa.PolyResto.cf(0) = New Complex(0.0)
                                End If
                                D1 = Pa.opDerivative(sVar)
                                D2 = D1.opDerivative(sVar)
                                Pa.PolyResto = Nothing
                                Pa.PolyDivisor = Nothing
                            Catch ex As Exception
                                Pa.PolyResto.cf(0) = New Complex(0.0)
                                D1 = Pa.opDerivative(sVar)
                                D2 = D1.opDerivative(sVar)
                            End Try
                            'If degree < 3 Then
                            '    GoTo degreeLt3
                            'End If

                            ' |min| = 0.5*Min[ (|a0/ak|)^(1/k) ]

                            min = Double.MaxValue
                            a0 = Pa.cf(Pa.cf.Length - 1)
                            For i1 As Int64 = 0 To Pa.cf.Length - 1
                                If Not Pa.cf(i1).IsZero Then
                                    Dim exp As Int64 = Pa.exp(i1)(0)
                                    Dim cjo As Complex = a0 / Pa.cf(i1)
                                    cjo = cjo ^ New Complex(1.0 / exp)
                                    Dim norm As Double = cjo.opNorm
                                    If norm < min Then min = norm
                                End If
                            Next
                            min /= 2.0
                            iVal(i) = New Complex(-min, min)
                            iVal(0) = New Complex(min, -min)
                            i = 0
                        End If
                    Catch ex As Exception

                    End Try
sig:
                Next
                Dim ts3 As New TimeSpan(Now.Ticks - t0.Ticks)
                If ts3.TotalMilliseconds >= cfg.timeOutms / 3 AndAlso
                             cfg.doTimeOut <> timeout.never Then
                    Exit Try
                End If
                If bHallado Then
                    nIntentos = 0
                Else
                    nIntentos += 1
                End If
            Loop While degree AndAlso nIntentos < 3

        Catch ex As Exception
            ReDim rootsG(-1)
            Return rootsG
        End Try
        Try
            Polynomial.bDoGCD = bDoGCD
            Dim i As Int64
            Dim re(rootsG.Length - 1) As Double
            For i = 0 To rootsG.Length - 1
                re(i) = rootsG(i).pRe.ToDouble
            Next
            Array.Sort(re, rootsG, 0, re.Length)
        Catch ex As Exception
            ReDim rootsG(-1)
        End Try
        Return rootsG
    End Function

    Public Shared Function opRoots_DuranKernel(ByVal Pa As Polynomial) As Complex()
        Dim pMin() As Complex = Nothing
        Dim nZero As Int64 = 0
        Dim i, j As Int64
        Try
            Dim root() As Complex = {Nothing, Nothing}
            Dim exp1 As Int64 = 0
            If Pa.exp.Length Then
                ' ver si tiene raíz = 0
                exp1 = Pa.exp(Pa.exp.Length - 1)(0)
                Dim nOrigDeg As Int64 = Pa.getDegree
                Dim bOrigDegree As Boolean =
                    IIf(Pa.getDegree = nOrigDeg, True, False)
                Do While exp1 > 0
                    nZero += 1
                    exp1 -= 1
                    For i = 0 To Pa.exp.Length - 1
                        Pa.exp(i)(0) -= 1
                    Next
                Loop
            End If

            Dim Pc2 As New Polynomial(Pa)
            If Pc2.isComplex Then
                Throw New Exception(msg8.msg(1011)) ' is a coeff, there are no roots
            End If
            Dim cfn As Complex = Nothing
            Dim expsDiff(Pa.cf.Length - 1) As Int64
            Pc2.multByPolyDivisor()
            Pc2.opReduceCommonExponents()
            If Pc2.IsMultiVar Then
                'Return New Vector(Pa)
            End If
            Dim degree As Int64 = Pc2.opDegree(cfn, Nothing, expsDiff)
            Dim vVar() As String = Pc2.varAll
            If degree = 0 OrElse vVar.Length = 0 Then
                Throw New Exception(msg8.msg(1011)) ' is a coeff, there are no roots
            ElseIf vVar.Length > 1 Then
                Throw New Exception(msg8.msg(1012)) ' The polynomial has multiple variables: roots n/a
            End If
            If degree = 1 Then
                ' b+ax=0 ,x=-b/a
                If Pc2.cf.Length < 2 Then
                    GoTo SalidaDK
                End If
                ReDim pMin(0)
                pMin(0) = -Pc2.cf(1) / Pc2.cf(0)
                GoTo SalidaDK
            End If

            Dim iPcCoeff As Int64 = 0
            Dim posVar As Int64 = 0

            Pc2 = Polynomial.opNormalize(Pc2)
            Dim difMin As Double = Double.MaxValue
            Dim difMin2 As Double = Double.MaxValue
            Dim p(degree - 1), ini As Complex
            ini = New Complex(0.4, 0.9)
            ini.pRe = New Rational(Math.Pow(Math.Abs(Pc2.cf(Pc2.cf.Length - 1).pRe.ToDouble), 1 / degree))

            Dim j1 As Int64 = Pc2.cf.Length - 1
            ini = New Complex(0.5 * (Pc2.An / Pc2.cf(j)).opNorm)

            For j1 = 2 To Pc2.cf.Length - 2
                Dim exp As Int64 = Pc2.exp(j1)(0)
                Dim mod1 As Double = 0.5 * (Pc2.An / Pc2.cf(j)).opNorm ^ (1 / exp)
                If mod1 < ini.opNorm Then
                    ini = New Complex(mod1)
                End If
            Next


            If p.Length Mod 2 Then
                p(0) = New Complex(1.0, 0.0)
            End If
            For i = p.Length Mod 2 To p.Length - 1
                p(i) = New Complex(ini)
                For j = 1 + p.Length Mod 2 To i
                    p(i) = p(i) * ini
                Next
            Next
            p(0) = New Complex(ini)
            For i = 1 To p.Length - 1
                p(i) = p(i - 1) * ini
            Next
            ReDim pMin(p.Length - 1)
            Dim curVueltas As Int64 = 0
            Dim maxNVueltas As Int64 = 45
            Dim prMax As Double = 1.0E+50
            Dim divisor As Complex
            Do
                Dim prod As New Complex(1, 0)
                For i = 0 To p.Length - 1
                    divisor = Complex.one
                    For j = 0 To p.Length - 1
                        If i <> j Then
                            Try
                                divisor *= (p(i) - p(j))
                            Catch ex As Exception
                                GoTo sig
                            End Try
                        End If
                    Next
                    Try
                        p(i) -= Pc2.evalCjo(p(i)) / divisor
                    Catch ex As Exception

                    End Try
sig:
                Next
                curVueltas += 1
            Loop While curVueltas < maxNVueltas
            pMin = p
SalidaDK:
        Catch ex As Exception
            Throw ex
        End Try
        If nZero Then
            j = pMin.Length
            ReDim Preserve pMin(j + nZero - 1)
            For i = j To pMin.Length - 1
                pMin(i) = New Complex(0.0)
            Next
        End If
        Return pMin
    End Function


    Public Shared Function opRealRoots_Junque(ByVal sVar As String,
                                              ByVal Pc As Polynomial,
                                              ByVal PcOrig As Polynomial,
                                              ByVal origDegree As Int64,
                                              Optional ByVal bIgnoreImgCoeff As Boolean = True,
                                              Optional origDeg2 As Int64 = -1) As Complex()
        Dim ret(-1) As Complex, nzeros As Int64 = 0
        Dim rootsDer(-1)() As Complex
        Dim degree As Int64
        Dim rootsG(-1) As Complex
        Dim Pa As Polynomial
        Try
            If origDeg2 = -1 Then
                origDeg2 = origDegree
            End If
            Pc = Polynomial.opNormalize(Pc)
            Pa = New Polynomial(Pc)

            Dim Der(0) As Polynomial
            Dim PaInIni As Complex = Nothing
            Dim ini, fin As Double
            Dim cjoIni As Complex = Nothing
            Dim left As Double = 0.0
            Dim right As Double = 0.0
            Dim i, j As Int64

            Dim oS As New polynomialSort(Pa.exp, Pa.varAll, Pa.cf)
            degree = oS.degree
            If degree < 1 Then
                Exit Try
            End If
            Dim root() As Complex = {Nothing, Nothing}
            Dim exp1 As Int64 = 0
            If Pa.exp.Length Then
                ' ver si tiene raíz = 0
                Dim nOrigDeg As Int64 = PcOrig.getDegree
                exp1 = Pa.exp(Pa.exp.Length - 1)(0)
                Dim bOrigDegree As Boolean =
                    IIf(Pa.getDegree = nOrigDeg, True, False)
                Do While exp1 > 0
                    ReDim Preserve rootsG(rootsG.Length)
                    rootsG(rootsG.Length - 1) = New Complex(0.0) ' cero es raíz
                    If rootsG.Length = nOrigDeg Then
                        Exit Try
                    End If
                    nzeros += 1
                    exp1 -= 1
                    For i = 0 To Pa.exp.Length - 1
                        Pa.exp(i)(0) -= 1
                    Next
                Loop
                degree = Pa.getDegree
                If bOrigDegree Then
                    'PcOrig = New Polynomial(Pa)
                    origDeg2 = Pa.getDegree
                End If
            End If
            If nzeros Then
                PcOrig /= Polynomial.GetPolynomial(sVar) ^ New Polynomial(nzeros)
            End If

degLT3:
            If degree < 3 Then
                If degree < 1 Then
                    Exit Try
                ElseIf degree = 2 Then
                    Dim a, b, c As Complex
                    a = Pa.cf(0)
                    If Pa.cf.Length = 2 Then
                        ' a*x^2 + c = 0
                        c = Pa.cf(1)
                        root(0) = -(-c / a) ^ Complex.oneHalf
                        ReDim Preserve rootsG(rootsG.Length)
                        rootsG(rootsG.Length - 1) = root(0)
                        root(0) = (-c / a) ^ Complex.oneHalf
                        ReDim Preserve rootsG(rootsG.Length)
                        rootsG(rootsG.Length - 1) = root(0)
                        Exit Try
                    Else
                        ' a*x^2 + b*x + c = 0
                        b = Pa.cf(1)
                        c = Pa.cf(2)
                        root(0) = (-b + (b * b - 4 * a * c) ^ Complex.oneHalf) / (2.0 * a)
                        i = rootsG.Length
                        ReDim Preserve rootsG(rootsG.Length)
                        rootsG(rootsG.Length - 1) = root(0)
                        root(0) = (-b - (b * b - 4 * a * c) ^ Complex.oneHalf) / (2.0 * a)
                        ReDim Preserve rootsG(rootsG.Length)
                        rootsG(rootsG.Length - 1) = root(0)
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
                        Exit Try
                    End If
                Else
                    root(0) = -Pa.cf(1) / Pa.cf(0)
                    ReDim Preserve rootsG(rootsG.Length)
                    rootsG(rootsG.Length - 1) = root(0)
                    Exit Try
                End If
            End If

            Dim vRational(-1) As Double
            Dim r1 As New Rational(PcOrig.cf(0).pRe.ToDouble)
            Dim vQ() As Double = Nothing
            Dim vP() As Double = Nothing
            Dim bTryRationals As Boolean = False
            Dim db1 As Double = Math.Abs(r1.ToDouble)
            If db1 >= 1.0 AndAlso db1 < 10 ^ 7 Then
                vQ = r1.opFactors
                If vQ Is Nothing Then
                    vQ = New Double() {1}
                Else
                    ReDim Preserve vQ(vQ.Length)
                    vQ(vQ.Length - 1) = 1
                End If
                r1 = New Rational(PcOrig.cf(PcOrig.cf.Length - 1).pRe.ToDouble)
                db1 = Math.Abs(r1.ToDouble)
                If db1 >= 1.0 AndAlso db1 < 10 ^ 7 Then
                    vP = r1.opFactors
                    If vP Is Nothing Then
                        vP = New Double() {1}
                    Else
                        ReDim Preserve vP(vP.Length)
                        vP(vP.Length - 1) = 1
                    End If
                    If vP.Length > 1 Then
                        Dim pCurr As Double = vP(0)
                        Dim vP_B(0) As Double, iB As Int32 = 0
                        vP_B(0) = pCurr
                        For i = 1 To vP.Length - 1
                            If vP(i) <> pCurr Then
                                iB += 1
                                ReDim Preserve vP_B(iB)
                                vP_B(iB) = vP(i)
                                pCurr = vP(i)
                            End If
                        Next
                        If iB > -1 Then
                            Dim vp_C(-1) As Double, iC As Int32 = -1
                            For i = 0 To vP_B.Length - 1
                                For j = i + 1 To vP_B.Length - 1
                                    iC += 1
                                    ReDim Preserve vp_C(iC)
                                    vp_C(iC) = vP_B(i) * vP_B(j)
                                Next
                            Next
                            Dim ln As Int32 = vP.Length
                            ReDim Preserve vP(vP.Length + iC)
                            Array.Copy(vp_C, 0, vP, ln, vp_C.Length)
                        End If
                    End If
                    bTryRationals = True
                End If
            End If
            If bTryRationals Then
                ' a simple try for rational roots:
                If True Then
                    For i = 0 To vP.Length - 1
                        For j = 0 To vQ.Length - 1
                            Dim Pr As New Rational(vP(i), vQ(j))
                            root(0) = New Complex(Pr, New Rational(0.0))
                            Dim iFound As Int32 = 0
                            Do
                                iFound = 0
                                If Pa.evalPrecis(root(0).pRe).IsZero Then
                                    iFound = 1
                                ElseIf Pa.evalPrecis(-root(0).pRe).IsZero Then
                                    iFound = -1
                                End If
                                If iFound Then
                                    If iFound = -1 Then
                                        root(0).opChgSgn()
                                    End If
                                    Dim nMultiplicity As Int32 = 1
                                    ReDim Preserve rootsG(rootsG.Length)
                                    rootsG(rootsG.Length - 1) = New Complex(root(0))
                                    Dim _der As Polynomial = Pa.opDerivative(sVar)
                                    Do While _der.getDegree AndAlso
                                    _der.evalPrecis(root(0).pRe).IsZero
                                        nMultiplicity += 1
                                        ReDim Preserve rootsG(rootsG.Length)
                                        rootsG(rootsG.Length - 1) = New Complex(root(0))
                                        _der = _der.opDerivative(sVar)
                                    Loop
                                    Dim x As Polynomial = Polynomial.GetPolynomial(Pa.var(0))
                                    x -= New Polynomial(root(0))
                                    Dim x1 As New Polynomial(x)
                                    For k1 As Int32 = 2 To nMultiplicity
                                        x *= x1
                                    Next
                                    ' 'deflate' Pa:
                                    Pa /= x
                                    Pa.PolyResto = Nothing
                                    Pa.PolyDivisor = Nothing
                                    degree = Pa.getDegree
                                    PcOrig /= x
                                    PcOrig.PolyResto = Nothing
                                    PcOrig.PolyDivisor = Nothing
                                    If degree = 0 Then
                                        Exit Try
                                    End If
                                End If
                            Loop While iFound
                        Next
                    Next
                    degree = Pa.getDegree
                    Dim k As Int32 = 0
                    For i = 0 To rootsG.Length - 1
                        Dim r As Double = rootsG(i).pRe.ToDouble
                        If r Then
                            ReDim Preserve vRational(k)
                            vRational(k) = r
                            k += 1
                        End If
                    Next
                End If
            End If
            If degree = 0 Then
                Exit Try
            End If


            If bIgnoreImgCoeff AndAlso Pa.hasComplexCoeff Then
                Return ret
            End If
            ' Nota, "left" y "right" son las cotas inferior y superior
            ' dentro de las cuales deben estar todas las raíces, i.e.
            ' las raíces deben estar dentro del intervalo [left,right].
            '   ------
            right = Pa.maxReRoot
            left = -right
            ReDim Der(degree)
            Der(degree) = New Polynomial(Pa)
            Dim re(-1) As Double, ire As Int64 = 1
            i = degree - 1
            Dim rootsCjo(-1) As Complex
            If i > 0 Then
                Dim iStop As Int64 = 2
                If degree > 5 Then iStop = degree - 1
                For i = i To iStop Step -1
                    Der(i) = Der(i + 1).opDerivative(sVar)
                Next
                For i = iStop To degree - 1
                    If i < origDegree - 3 Then
                        rootsCjo = opRoots_BairstowDbl(
                        sVar, Der(i), New Polynomial(Der(i)), i)
                        rootsCjo = opRootsOfPoly_Newton(
                         Der(i))
                    Else
                        rootsCjo = opRealRoots_Junque(
                     sVar, Der(i), New Polynomial(Der(i)), origDegree)
                    End If
                    If rootsCjo.Length < Der(i).getDegree Then
                        rootsCjo = opRoots_BairstowDbl(
                        sVar, Der(i), New Polynomial(Der(i)), i)
                    End If
                Next
            End If
            ReDim re(ire)
            For j = 0 To rootsCjo.Length - 1
                ' guardamos en re() los puntos, raíces,
                ' de la derivada
                ReDim Preserve re(ire)
                re(ire) = rootsCjo(j).pRe.ToDouble
                ire += 1
            Next
            ' añadimos en re(), las cotas "left" y "right":

            If left < 0 Then
                re(0) = Math.Floor(left) - 1
            Else
                re(0) = Math.Floor(left) + 1
            End If
            ReDim Preserve re(ire)
            If right < 0 Then
                re(ire) = Math.Floor(right) - 1
            Else
                re(ire) = Math.Floor(right) + 1
            End If
            ' ordenamos de menor a mayor el "array" re():
            Array.Sort(re)
            Dim re1(0) As Double, ir As Int64 = 0
            re1(0) = re(0)
            For i = 1 To re.Length - 1
                If re(i) <> re1(ir) Then
                    ir += 1
                    ReDim Preserve re1(ir)
                    re1(ir) = re(i)
                End If
            Next
            re = re1
            ' Un ejemplo:
            ' Pa= P(x)  = (x-1.1)*(x-1.2)*(x-1.3)*(x-1.4)*(x-1.5)*(x-1.6)*(x-1.7)*(x-1.8)
            ' tiene raíces 1.1, 1.2, ... 1.8
            ' la derivada es:
            ' P'(x) = 8*x^7 -81.2*x^6 +351.96*x^5 -844.48*x^4 +1211.2996*x^3 -1038.63732*x^2 +492.932088*x -99.8853264
            ' y las raíces de la derivada P'(x) son: 
            ' r1 = 1.1310275248288513
            ' r2 = 1.2386929729229006
            ' r3 = 1.3446154642625561
            ' r4 = 1.4499999996745176
            ' r5 = 1.5553845362772731
            ' r6 = 1.6613070267991825
            ' r7 = 1.7689724752347193

            ' al llegar a este punto del código el 'array' re() contendrá
            '   elemento de re()    contenido 
            '   ----------------    ---------
            '       i=0         --> left
            '       i=1         --> r1=1.13102... 
            '       i=2         --> r2=1.2386...
            '       ...
            '       i=7         --> r7=1.76897...
            '       i=8         --> right
            Dim bIsOrig As Boolean =
                (Pa.getDegree = origDeg2)
            'If Not bIsOrig Then
            '    Exit Try
            'End If
            Dim pcorig2 As New Polynomial(Pa)

            Dim divisor As New Polynomial(1.0)
            Dim iPreviousIsRationalRoot As Int64 = -10
            Dim bFountAtFin As Boolean = False
            ' |min| = 0.5*Min[ (|a0/ak|)^(1/k) ]
            Dim min As Double
            Dim max As Double
            Dim a0 As Complex
            Dim minDer As Double
            Dim D As Polynomial = Nothing
            Dim a0Der As Complex
            Dim bMultiplicidad As Boolean = False
            For i = 0 To re.Length - 2
                If D Is Nothing Then
                    ' |min| = 0.5*Min[ (|a0/ak|)^(1/k) ]
                    min = Double.MaxValue
                    max = 0.0
                    a0 = Pa.cf(Pa.cf.Length - 1)
                    For j = 0 To Pa.cf.Length - 1
                        If Not Pa.cf(j).IsZero Then
                            Dim exp As Int64 = Pa.exp(j)(0)
                            Dim cjo As Complex = a0 / Pa.cf(j)
                            cjo = cjo ^ New Complex(1.0 / exp)
                            Dim norm As Double = cjo.opNorm
                            If norm < min Then min = norm
                        End If
                        max += Pa.cf(j).opNorm
                    Next
                    min /= 2.0
                    minDer = Double.MaxValue
                    D = Pa.opDerivative(sVar)
                    a0Der = D.cf(D.cf.Length - 1)
                    For j = 0 To D.cf.Length - 1
                        If Not D.cf(j).IsZero Then
                            Dim exp As Int64 = D.exp(j)(0)
                            Dim cjo As Complex = a0Der / D.cf(j)
                            cjo = cjo ^ New Complex(1.0 / exp)
                            Dim norm As Double = cjo.opNorm
                            If norm < minDer Then minDer = norm
                        End If
                    Next
                    minDer /= 2.0
                End If




                ini = re(i)
                fin = re(i + 1)

                ' Para i=0, ini=re(0)=left, fin=re(1)=r2
                ' y si existe raíz en el intervalo [ini,fin]
                ' se cumplirá siempre SIGNO(P(ini)) <> SIGNO(P(fin))
                ' (si hay raíz múltiple, el valor de ini será raíz, ya que
                ' ini es raíz de la derivada)
                ' luego una simple búsqueda dicotómica (también llamada binaria)
                ' nos dará como resultado una raíz.
                ' Siguiendo el ejemplo ini=left y fin=r2 nos dará la 1ª raíz
                ' de P(x), es decir, el punto 1.1
                ' Para i=1, obtendremos la raíz, el punto, 1.2
                '  "    =2,   "             "              1.3
                ' ...
                '  "    =6,   "             "              1.7
                ' Para i= re.length-2       "              1.8 

                ' (Obsérvese, que para i=0, left< 1.1(=1ª raíz buscada) < r1=1.13102..
                '                      i=1, r1=ini < 1.2(=2ªraíz buscada) < r2=1.2386...
                ' ...
                '                      i=7  r7=1.76897...< 1.8(=última raíz buscada) < right )
                If bMultiplicidad Then
                    ' if root is more than once (repeated), it'll be root in the
                    ' deflated polynomial Pa:
                    root = Functions.busquedaDicotomicaPr(ini, fin, Pa)
                Else
                    root = Functions.busquedaDicotomicaPr(ini, fin, PcOrig)
                End If
                If root IsNot Nothing AndAlso Not root(0).IsZero AndAlso
                root(0).pRe.ToDouble > ini Then
                    Dim Cumple As Int64 = 0
                    Dim PxOrig As Complex = PcOrig.evalCjo(root(0))
                    Dim xk As Complex = root(0)
                    Dim Px As Complex = root(1)
                    If Px.opNorm < min AndAlso PxOrig.opNorm < minDer Then
                        Dim root2() As Complex = Functions.busquedaDicotomicaPr(ini, fin, PcOrig)
                        If root2 IsNot Nothing Then
                            root = root2
                        End If
                        Cumple = 4
                    Else
                        Dim sgnRe As Int64 = Math.Sign(PxOrig.pRe.ToDouble)
                        Dim lg As Int64 = Math.Log10(Math.Abs(xk.pRe.ToDouble))
                        lg -= 14
                        If sgnRe = 0 Then
                            Cumple += 1
                        Else
                            Dim incr As New Complex(10 ^ lg)
                            If sgnRe <> Math.Sign(PcOrig.evalCjo(xk _
                                   + incr).pRe.ToDouble) Then
                                Cumple += 1
                            ElseIf sgnRe <> Math.Sign(PcOrig.evalCjo(xk _
                                  - incr).pRe.ToDouble) Then
                                Cumple += 1
                            End If
                        End If
                        If Cumple Then
                            Dim sgnIm As Int64 = Math.Sign(PxOrig.pIm.ToDouble)
                            If sgnIm = 0 Then
                                Cumple += 1
                            Else
                                Dim incr As New Complex(0, 10 ^ lg)
                                If sgnIm <> Math.Sign(PcOrig.evalCjo(xk _
                                   + incr).pIm.ToDouble) Then
                                    Cumple += 1
                                ElseIf sgnRe <> Math.Sign(PcOrig.evalCjo(xk _
                                  - incr).pIm.ToDouble) Then
                                    Cumple += 1
                                End If
                            End If
                        End If
                        If Cumple = 2 Then
                            sgnRe = Math.Sign(Px.pRe.ToDouble)
                            If sgnRe = 0 Then
                                Cumple += 1
                            Else
                                Dim incr As New Complex(0, 10 ^ lg)
                                If sgnRe <> Math.Sign(Pa.evalCjo(xk _
                                   + incr).pRe.ToDouble) Then
                                    Cumple += 1
                                ElseIf sgnRe <> Math.Sign(Pa.evalCjo(xk _
                                  - incr).pRe.ToDouble) Then
                                    Cumple += 1
                                End If
                            End If
                            If Cumple = 3 Then
                                Dim sgnIm As Int64 = Math.Sign(Px.pIm.ToDouble)
                                If sgnIm = 0 Then
                                    Cumple += 1
                                Else
                                    Dim incr As New Complex(0, 10 ^ lg)
                                    If sgnIm <> Math.Sign(Pa.evalCjo(xk _
                                   + incr).pIm.ToDouble) Then
                                        Cumple += 1
                                    ElseIf sgnIm <> Math.Sign(Pa.evalCjo(xk _
                                  - incr).pIm.ToDouble) Then
                                        Cumple += 1
                                    End If
                                End If
                            End If
                        End If
                    End If
                    If Cumple = 4 Then
                        Dim db As Double = root(0).pRe.ToDouble
                        If db <> ini Then
                            Dim a, b As Double
                            If db < 0 Then
                                a = 1.1 * db
                                b = 0.9 * db
                            Else
                                a = 0.9 * db
                                b = 1.1 * db
                            End If
                            Dim r() As Complex = Functions.busquedaDicotomicaPr(a, b, pcorig2)
                            If r IsNot Nothing Then
                                root = r
                            End If
                        End If
                        ReDim Preserve rootsG(rootsG.Length)
                        rootsG(rootsG.Length - 1) = root(0)
                        Dim x As Polynomial = Polynomial.GetPolynomial(Pa.var(0))
                        ' 'deflate' Pa:
                        divisor *= (x - New Polynomial(root(0)))
                        Pa = pcorig2 / divisor
                        Pa.PolyResto = Nothing
                        Pa.PolyDivisor = Nothing
                        D = Nothing
                        degree = Pa.getDegree
                        If degree < 1 Then Exit For
                        If db = ini Then
                            i -= 1 ' por si hay multiplicidad
                            bMultiplicidad = True
                        Else
                            bMultiplicidad = False
                        End If
                    End If
                End If
sig:
            Next
        Catch ex As Exception
            ReDim rootsG(-1)
            Return rootsG
        End Try
        Try
            Dim i As Int64
            Dim re(rootsG.Length - 1) As Double
            For i = 0 To rootsG.Length - 1
                re(i) = rootsG(i).pRe.ToDouble
            Next
            Array.Sort(re, rootsG, 0, re.Length)
        Catch ex As Exception
            ReDim rootsG(-1)
        End Try
        Return rootsG
    End Function



    Public Shared Function opRoots_BairstowDbl(ByVal sVar As String,
                                              ByVal Pc As Polynomial,
                                              ByVal PcOrig As Polynomial,
                                              ByVal origDegree As Int64,
                                              Optional ByVal bIgnoreImgCoeff As Boolean = True) As Complex()

        Dim ret(-1) As Complex, nRoots As Int64 = 0
        Dim rootsDer(-1)() As Complex
        Dim degree As Int64
        Dim rootsG(-1) As Complex
        Dim Pa As New Polynomial(Pc)
        Try
            Pc = Polynomial.opNormalize(Pc)
            Pa = New Polynomial(Pc)

            Dim Der(0) As Polynomial
            Dim PaInIni As Complex = Nothing
            Dim cjoIni As Complex = Nothing
            Dim left As Double = 0.0
            Dim right As Double = 0.0
            Dim i As Int64

            Dim oS As New polynomialSort(Pa.exp, Pa.varAll, Pa.cf)
            degree = oS.degree
            If degree < 1 Then
                Exit Try
            End If
            Dim n As Int64 = Pa.getDegree
            Do
                Dim root() As Complex = {Nothing, Nothing}
                Dim exp1 As Int64 = 0
                If Pa.exp.Length Then
                    ' ver si tiene raíz = 0
                    exp1 = Pa.exp(Pa.exp.Length - 1)(0)
                    Dim nOrigDeg As Int64 = PcOrig.getDegree
                    Dim bOrigDegree As Boolean =
                        IIf(Pa.getDegree = nOrigDeg, True, False)
                    Do While exp1 > 0
                        ReDim Preserve rootsG(rootsG.Length)
                        rootsG(rootsG.Length - 1) = New Complex(0.0) ' cero es raíz
                        If rootsG.Length = nOrigDeg Then
                            Exit Try
                        End If
                        nRoots += 1
                        exp1 -= 1
                        For i = 0 To Pa.exp.Length - 1
                            Pa.exp(i)(0) -= 1
                        Next
                    Loop
                    degree = Pa.getDegree
                    If bOrigDegree Then
                        PcOrig = New Polynomial(Pa)
                    End If
                End If

                If degree < 3 Then
                    If degree = 0 Then
                        Exit Try
                    ElseIf degree = 2 Then
                        Dim a, b, c As Complex
                        a = Pa.cf(0)
                        If Pa.cf.Length = 2 Then
                            ' a*x^2 + c = 0
                            c = Pa.cf(1)
                            root(0) = -(-c / a) ^ Complex.oneHalf
                            ReDim Preserve rootsG(rootsG.Length)
                            rootsG(rootsG.Length - 1) = root(0)
                            root(0) = (-c / a) ^ Complex.oneHalf
                            ReDim Preserve rootsG(rootsG.Length)
                            rootsG(rootsG.Length - 1) = root(0)
                            Exit Try
                        Else
                            ' a*x^2 + b*x + c = 0
                            b = Pa.cf(1)
                            c = Pa.cf(2)
                            root(0) = (-b + (b * b - 4 * a * c) ^ Complex.oneHalf) / (2.0 * a)
                            i = rootsG.Length
                            ReDim Preserve rootsG(rootsG.Length)
                            rootsG(rootsG.Length - 1) = root(0)
                            root(0) = (-b - (b * b - 4 * a * c) ^ Complex.oneHalf) / (2.0 * a)
                            ReDim Preserve rootsG(rootsG.Length)
                            rootsG(rootsG.Length - 1) = root(0)
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
                            Exit Try
                        End If
                    Else
                        root(0) = -Pa.cf(1) / Pa.cf(0)
                        ReDim Preserve rootsG(rootsG.Length)
                        rootsG(rootsG.Length - 1) = root(0)
                        Exit Try
                    End If
                End If

                If bIgnoreImgCoeff AndAlso Pa.hasComplexCoeff Then
                    Return ret
                End If

                ' Bairstow's method
                ' http://en.wikipedia.org/wiki/Bairstow%27s_method

                ' bn = bn-1 = 0                 fn = fn-1 = 0
                ' bi = ai+2 - u*bi+1- v*bi+2    fi = bi+2 - u*fi+1 - v*fi+2  (i=n-2,...,0)
                ' c  = a1 - u*b0 - v*b1          g = b1 - u*f0 - v*f1
                ' d  = a0 - v*b0                 h = b0 - v*f0

                ' u      u              1           ( - h    g    )    ( c )
                '    =       -    --------------- * (             )  * (   )
                ' v      v        v*g^2 +h*(h-u*g)  ( -g*v  g*u-h )    ( d )
                '
                ' i.e.:
                ' u = u - (-h*c + g*d) / (v*g^2 + h*(h-u*g))
                ' v = v - (-g*v*c + (g*u-h)*d) / (v*g^2 + h*(h-u*g))

                ' initial try
                ' u = an-1 / an    v = an-2 / an-1
                Dim a1(n) As Double
                ' get a() coefficients:
                For i = 0 To Pa.exp.Length - 1
                    If Pa.exp(i) IsNot Nothing AndAlso
                    Pa.exp(i).Length Then
                        exp1 = Pa.exp(i)(0)
                        a1(exp1) = Pa.cf(i).pRe.ToDouble
                    End If
                Next
                i = n - 1
                Dim u As Double = a1(n) ' an=1
                Do While u = 0
                    i -= 1
                    u = a1(i)
                Loop
                i -= 1
                Dim v As Double = a1(i)
                Do While v = 0
                    i -= 1
                    v = a1(i)
                Loop
                u = 0.4
                v = -0.4
                Dim b1(n) As Double
                Dim f(n) As Double
                Dim nIter As Int64 = 0
                Dim diff As Double
                Do
                    ' obtain b() and f()
                    For i = n - 2 To 0 Step -1
                        b1(i) = a1(i + 2) - u * b1(i + 1) - v * b1(i + 2)
                        f(i) = b1(i + 2) - u * f(i + 1) - v * f(i + 2)
                    Next
                    Dim c1 As Double = a1(1) - u * b1(0) - v * b1(1)
                    Dim d As Double = a1(0) - v * b1(0)
                    Dim g As Double = b1(1) - u * f(0) - v * f(1)
                    Dim h As Double = b1(0) - v * f(0)
                    Dim den As Double = v * g * g + h * (h - u * g)
                    Dim u1 As Double = u - (-h * c1 + g * d) / den
                    Dim v1 As Double = v - (-g * v * c1 + (g * u - h) * d) / den
                    diff = (u - u1) ^ 2 + (v - v1) ^ 2
                    u = u1 : v = v1
                    nIter += 1
                Loop While nIter < 400 AndAlso diff
                Dim X As Polynomial = Polynomial.GetPolynomial(Pa.var(0))
                'eP.parse(Pa.var(0) + "^2+" + Pa.var(0) + "+1")
                Dim Q As Polynomial = X * X + X + New Polynomial(1.0) ' eP.ret.curExpr.getPolynomial
                Q.cf(1).pRe = New Rational(u)
                Q.cf(2).pRe = New Rational(v)

                ' resolve 2nd degree x2+ux+v
                Dim a2, b2, c2 As Complex
                a2 = New Complex(1.0)
                b2 = New Complex(u, 0.0)
                c2 = New Complex(v, 0.0)
                root(0) = (-b2 + (b2 * b2 - 4 * a2 * c2) ^ Complex.oneHalf) / (2.0 * a2)
                i = rootsG.Length
                ReDim Preserve rootsG(rootsG.Length)
                rootsG(rootsG.Length - 1) = root(0)
                root(0) = (-b2 - (b2 * b2 - 4 * a2 * c2) ^ Complex.oneHalf) / (2.0 * a2)
                ReDim Preserve rootsG(rootsG.Length)
                rootsG(rootsG.Length - 1) = root(0)
                Dim bVerySmallb As Boolean = False
                If rootsG(i).opModulo * 10 ^ 6 < rootsG(i + 1).opModulo Then
                    bVerySmallb = True
                End If
                If bVerySmallb Then
                    ' if root1 << root2
                    ' c = root1 * root2
                    ' root1 =  c / root2 
                    rootsG(i) = c2 / rootsG(i + 1)
                Else
                    i += 1
                    If rootsG(i).opModulo * 10 ^ 6 < rootsG(i - 1).opModulo Then
                        bVerySmallb = True
                    Else
                        bVerySmallb = False
                    End If
                    ' if root2 << root1
                    ' c = root1 * root2
                    ' root2 = c / root1
                    If bVerySmallb Then
                        rootsG(i) = c2 / rootsG(i - 1)
                    End If
                End If
                Pa /= Q
                Pa.PolyResto = Nothing
                Pa.PolyDivisor = Nothing
                n = Pa.getDegree
                degree = n
            Loop While n
        Catch ex As Exception
            ReDim rootsG(-1)
            Return rootsG
        End Try
        Try
            Dim i As Int64
            Dim re(rootsG.Length - 1) As Double
            For i = 0 To rootsG.Length - 1
                re(i) = rootsG(i).pRe.ToDouble
            Next
            Array.Sort(re, rootsG, 0, re.Length)
        Catch ex As Exception
            ReDim rootsG(-1)
        End Try
        Return rootsG
    End Function


    Public Shared Function opRoots_BairstowPrecis(ByVal sVar As String,
                                              ByVal Pc As Polynomial,
                                              ByVal PcOrig As Polynomial,
                                              ByVal origDegree As Int64,
                                              Optional ByVal bIgnoreImgCoeff As Boolean = True) As Complex()

        Dim ret(-1) As Complex, nRoots As Int64 = 0
        Dim rootsDer(-1)() As Complex
        Dim degree As Int64
        Dim rootsG(-1) As Complex
        Dim Pa As New Polynomial(Pc)
        Try
            Pc = Polynomial.opNormalize(Pc)
            Pa = New Polynomial(Pc)

            Dim Der(0) As Polynomial
            Dim PaInIni As Complex = Nothing
            Dim cjoIni As Complex = Nothing
            Dim left As Double = 0.0
            Dim right As Double = 0.0
            Dim i As Int64

            Dim oS As New polynomialSort(Pa.exp, Pa.varAll, Pa.cf)
            degree = oS.degree
            If degree < 1 Then
                Exit Try
            End If
            Dim n As Int64 = Pa.getDegree
            Do
                Dim root() As Complex = {Nothing, Nothing}
                Dim exp1 As Int64 = 0
                If Pa.exp.Length Then
                    ' ver si tiene raíz = 0
                    exp1 = Pa.exp(Pa.exp.Length - 1)(0)
                    Dim nOrigDeg As Int64 = PcOrig.getDegree
                    Dim bOrigDegree As Boolean =
                        IIf(Pa.getDegree = nOrigDeg, True, False)
                    Do While exp1 > 0
                        ReDim Preserve rootsG(rootsG.Length)
                        rootsG(rootsG.Length - 1) = New Complex(0.0) ' cero es raíz
                        If rootsG.Length = nOrigDeg Then
                            Exit Try
                        End If
                        nRoots += 1
                        exp1 -= 1
                        For i = 0 To Pa.exp.Length - 1
                            Pa.exp(i)(0) -= 1
                        Next
                    Loop
                    degree = Pa.getDegree
                    If bOrigDegree Then
                        PcOrig = New Polynomial(Pa)
                    End If
                End If

                If degree < 3 Then
                    If degree = 0 Then
                        Exit Try
                    ElseIf degree = 2 Then
                        Dim a, b, c As Complex
                        a = Pa.cf(0)
                        If Pa.cf.Length = 2 Then
                            ' a*x^2 + c = 0
                            c = Pa.cf(1)
                            root(0) = -(-c / a) ^ Complex.oneHalf
                            ReDim Preserve rootsG(rootsG.Length)
                            rootsG(rootsG.Length - 1) = root(0)
                            root(0) = (-c / a) ^ Complex.oneHalf
                            ReDim Preserve rootsG(rootsG.Length)
                            rootsG(rootsG.Length - 1) = root(0)
                            Exit Try
                        Else
                            ' a*x^2 + b*x + c = 0
                            b = Pa.cf(1)
                            c = Pa.cf(2)
                            root(0) = (-b + (b * b - 4 * a * c) ^ Complex.oneHalf) / (2.0 * a)
                            i = rootsG.Length
                            ReDim Preserve rootsG(rootsG.Length)
                            rootsG(rootsG.Length - 1) = root(0)
                            root(0) = (-b - (b * b - 4 * a * c) ^ Complex.oneHalf) / (2.0 * a)
                            ReDim Preserve rootsG(rootsG.Length)
                            rootsG(rootsG.Length - 1) = root(0)
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
                            Exit Try
                        End If
                    Else
                        root(0) = -Pa.cf(1) / Pa.cf(0)
                        ReDim Preserve rootsG(rootsG.Length)
                        rootsG(rootsG.Length - 1) = root(0)
                        Exit Try
                    End If
                End If

                If bIgnoreImgCoeff AndAlso Pa.hasComplexCoeff Then
                    Return ret
                End If

                ' Bairstow's method
                ' http://en.wikipedia.org/wiki/Bairstow%27s_method

                ' bn = bn-1 = 0                 fn = fn-1 = 0
                ' bi = ai+2 - u*bi+1- v*bi+2    fi = bi+2 - u*fi+1 - v*fi+2  (i=n-2,...,0)
                ' c  = a1 - u*b0 - v*b1          g = b1 - u*f0 - v*f1
                ' d  = a0 - v*b0                 h = b0 - v*f0

                ' u      u              1           ( - h    g    )    ( c )
                '    =       -    --------------- * (             )  * (   )
                ' v      v        v*g^2 +h*(h-u*g)  ( -g*v  g*u-h )    ( d )
                '
                ' i.e.:
                ' u = u - (-h*c + g*d) / (v*g^2 + h*(h-u*g))
                ' v = v - (-g*v*c + (g*u-h)*d) / (v*g^2 + h*(h-u*g))

                ' initial try
                ' u = an-1 / an    v = an-2 / an-1
                Dim a1(n) As Rational
                ' get a() coefficients:
                For i = 0 To Pa.exp.Length - 1
                    If Pa.exp(i) IsNot Nothing AndAlso
                    Pa.exp(i).Length Then
                        exp1 = Pa.exp(i)(0)
                        a1(exp1) = Pa.cf(i).pRe
                    End If
                Next
                For i = 0 To a1.Length - 1
                    If a1(i) Is Nothing Then
                        a1(i) = New Rational(0.0)
                    End If
                Next
                i = n - 1
                Dim u As New Rational(a1(n)) ' an=1
                Do While u.IsZero
                    i -= 1
                    u = New Rational(a1(i))
                Loop
                i -= 1
                Dim v As New Rational(a1(i))
                Do While v.IsZero
                    i -= 1
                    v = New Rational(a1(i))
                Loop
                u = New Rational(0.9)
                v = New Rational(-0.4)

                Dim b1(n) As Rational
                Dim f(n) As Rational
                'Dim diff2 As Double
                Dim nIter As Int64 = 0
                Dim diff As Rational
                b1(n) = New Rational(0.0)
                b1(n - 1) = New Rational(0.0)
                f(n) = New Rational(0.0)
                f(n - 1) = New Rational(0.0)
                Dim X As Polynomial = Polynomial.GetPolynomial(Pa.var(0))
                Dim Q As Polynomial = X * X + X + New Polynomial(1.0) ' eP.ret.curExpr.getPolynomial
                Do
                    ' obtain b() and f()
                    For i = n - 2 To 0 Step -1
                        b1(i) = a1(i + 2) - u * b1(i + 1) - v * b1(i + 2)
                        f(i) = b1(i + 2) - u * f(i + 1) - v * f(i + 2)
                    Next
                    Dim c1 As Double = (a1(1) - u * b1(0) - v * b1(1)).ToDouble
                    Dim d As Double = (a1(0) - v * b1(0)).ToDouble
                    Dim g As Double = (b1(1) - u * f(0) - v * f(1)).ToDouble
                    Dim h As Double = (b1(0) - v * f(0)).ToDouble
                    Dim den As Double = v.ToDouble * g * g + h * (h - u.ToDouble * g)
                    Dim u1 As Double = u.ToDouble - (-h * c1 + g * d) / den
                    Dim v1 As Double = v.ToDouble - (-g * v.ToDouble * c1 + (g * u.ToDouble - h) * d) / den
                    diff = (u - u1) ^ 2 + (v - v1) ^ 2
                    u = New Rational(u1)
                    v = New Rational(v1)
                    nIter += 1
                Loop While nIter < 500 AndAlso Not diff.IsZero
                'Dim eP As New matrixParser
                'eP.parse(Pa.var(0) + "^2+" + Pa.var(0) + "+1")
                'Dim Q As Polynomial = eP.ret.curExpr.getPolynomial
                'Dim X As Polynomial = Polynomial.GetPolyomial(Pa.var(0))
                'eP.parse(Pa.var(0) + "^2+" + Pa.var(0) + "+1")
                Q.cf(1).pRe = New Rational(u)
                Q.cf(2).pRe = New Rational(v)

                ' resolve 2nd degree x2+ux+v
                Dim a2, b2, c2 As Complex
                a2 = New Complex(1.0)
                b2 = New Complex(u, New Rational(0.0))
                c2 = New Complex(v, New Rational(0.0))
                root(0) = (-b2 + (b2 * b2 - 4 * a2 * c2) ^ Complex.oneHalf) / (2.0 * a2)
                i = rootsG.Length
                ReDim Preserve rootsG(rootsG.Length)
                rootsG(rootsG.Length - 1) = root(0)
                root(0) = (-b2 - (b2 * b2 - 4 * a2 * c2) ^ Complex.oneHalf) / (2.0 * a2)
                ReDim Preserve rootsG(rootsG.Length)
                rootsG(rootsG.Length - 1) = root(0)
                Dim bVerySmallb As Boolean = False
                If rootsG(i).opModulo * 10 ^ 6 < rootsG(i + 1).opModulo Then
                    bVerySmallb = True
                End If
                If bVerySmallb Then
                    ' if root1 << root2
                    ' c = root1 * root2
                    ' root1 =  c / root2 
                    rootsG(i) = c2 / rootsG(i + 1)
                Else
                    i += 1
                    If rootsG(i).opModulo * 10 ^ 6 < rootsG(i - 1).opModulo Then
                        bVerySmallb = True
                    Else
                        bVerySmallb = False
                    End If
                    ' if root2 << root1
                    ' c = root1 * root2
                    ' root2 = c / root1
                    If bVerySmallb Then
                        rootsG(i) = c2 / rootsG(i - 1)
                    End If
                End If
                Pa /= Q
                Pa.PolyResto = Nothing
                Pa.PolyDivisor = Nothing
                n = Pa.getDegree
                degree = n
            Loop While n
        Catch ex As Exception
            ReDim rootsG(-1)
            Return rootsG
        End Try
        Try
            Dim i As Int64
            Dim re(rootsG.Length - 1) As Double
            For i = 0 To rootsG.Length - 1
                re(i) = rootsG(i).pRe.ToDouble
            Next
            Array.Sort(re, rootsG, 0, re.Length)
        Catch ex As Exception
            ReDim rootsG(-1)
        End Try
        Return rootsG
    End Function







    Public Sub opRuffini(a As Complex)
        ' Divide current polynomial P(x) by the binomial (x-a)
        ' (see Ruffini's rule, for ex. at http://en.wikipedia.org/wiki/Ruffini%27s_rule )
        Dim i As Int64 = 0
        Dim n As Int64 = exp(0)(0)
        Dim b(n) As Complex
        Try
            Dim r As Complex = -a
            b(i) = New Complex(cf(0))
            Dim j As Int64 = 0
            Do While i < n AndAlso j < exp.Length
                If exp(j)(0) = n - i Then
                    If b(i) IsNot Nothing Then
                        b(i + 1) = b(i) * r + cf(j)
                    Else
                        b(i + 1) = New Complex(cf(j))
                    End If
                    j += 1
                Else
                    b(i) = Complex.zero
                End If
                i += 1
            Loop
            n -= 1
            ReDim cf(n), exp(n)
            'Array.Reverse(b)
            j = 0
            For i = 0 To n
                If b(i) IsNot Nothing AndAlso
                Not b(i).IsZero Then
                    cf(j) = New Complex(b(i))
                    exp(j) = New Int64() {n - i}
                    j += 1
                End If
            Next
            ReDim Preserve cf(j - 1), exp(j - 1)
        Catch ex As Exception
            Throw ex
        End Try
    End Sub




    Public Shared Function opMult(ByVal a() As Complex, ByVal b() As Complex) As Complex()
        Dim i, j As Int64
        Dim lv As Int64 = a.Length - 1, lvb As Int64 = b.Length - 1
        Dim r(lv + lvb) As Complex
        For i = 0 To lv
            For j = 0 To lvb
                If r(i + j) Is Nothing Then
                    r(i + j) = a(i) * b(j)
                Else
                    r(i + j) += a(i) * b(j)
                End If
            Next
        Next
        Return r
    End Function
    Public Function ToStringFactors(Optional cfg As Config = Nothing) As String
        Dim sb As New StringBuilder(cf.Length * 30)
        Try
            Dim Pa As New Polynomial(Me)
            Dim An As New Complex(Pa.An)
            Pa = Polynomial.opNormalize(Pa)
            If cfg Is Nothing Then cfg = Me.cfg
            Dim vPoly() As Polynomial = Nothing ' Polynomial.opFactor(Pa, cfg)
            If Me.roots IsNot Nothing Then
                Dim X As Polynomial = Polynomial.GetPolynomial(Me.var(0))
                For i As Int64 = 0 To Me.roots.cjoByType.Length - 1
                    For j = 0 To Me.roots.cjoByType(i).Length - 1
                        Dim sb2 As New StringBuilder(35)
                        If i = 0 OrElse i = 3 Then
                            Dim XmnA As Polynomial = X - New Polynomial(
                                Me.roots.cjoByType(i)(j))
                            sb2.Append(
                                   XmnA.toStringPoly(cfg))
                        Else
                            Dim Px2 As Polynomial = X * X -
                                New Polynomial(2.0 * New Complex(
                                roots.cjoByType(i)(j).pRe, New Rational(0.0))) * X +
                                New Polynomial(roots.cjoByType(i)(j).opNorm)
                            sb2.Append(Px2.toStringPoly(cfg))
                            j += 1
                        End If
                        If Regex.IsMatch(sb2.ToString, "[-+]") Then
                            sb2.Insert(0, "(")
                            sb2.Append(")")
                        End If
                        If sb.Length Then
                            sb.Append(" *")
                        End If
                        sb.Append(sb2)
                    Next
                Next
                If Not An.IsReal OrElse An.pRe.ToDouble <> 1.0 Then
                    Dim sAn As String = An.ToStringComplex(cfg)
                    If sAn.Chars(0) = "-" Then
                        sAn = Mid(sAn, 2)
                    End If
                    If Regex.IsMatch(sAn, "[-+]") Then
                        sb.Insert(0, "(" + An.ToStringComplex(cfg) + ") *")
                    Else
                        sb.Insert(0, An.ToStringComplex(cfg) + " *")
                    End If
                End If
                vPoly = Polynomial.opFactor(Pa, cfg)
                Dim sb0 As New StringBuilder(sb.Length)
                For i As Int64 = 0 To vPoly.Length - 1
                    Dim sb2 As New StringBuilder(
                        vPoly(i).toStringPoly(cfg))
                    If Regex.IsMatch(sb2.ToString, "[-+]") Then
                        sb2.Insert(0, "(")
                        sb2.Append(")")
                    End If
                    If sb0.Length Then
                        sb0.Append(" *")
                    End If
                    sb0.Append(sb2)
                Next
                If Not An.IsReal OrElse An.pRe.ToDouble <> 1.0 Then
                    Dim sAn As String = An.ToStringComplex(cfg)
                    If sAn.Chars(0) = "-" Then
                        sAn = Mid(sAn, 2)
                    End If
                    If Regex.IsMatch(sAn, "[-+]") Then
                        sb0.Insert(0, "(" + An.ToStringComplex(cfg) + ") *")
                    Else
                        sb0.Insert(0, An.ToStringComplex(cfg) + " *")
                    End If
                    'sb0.Insert(0, An.toStringComplex(cfg) + " *")
                End If
                If sb0.ToString <> sb.ToString Then
                    sb.Append(" =" + vbCrLf + "= " + sb0.ToString)
                End If
            Else
                vPoly = Polynomial.opFactor(Pa, cfg)
                For i As Int64 = 0 To vPoly.Length - 1
                    Dim sb2 As New StringBuilder(
                        vPoly(i).toStringPoly(cfg))
                    If Regex.IsMatch(sb2.ToString, "[-+]") Then
                        sb2.Insert(0, "(")
                        sb2.Append(")")
                    End If
                    If sb.Length Then
                        sb.Append(" *")
                    End If
                    sb.Append(sb2)
                Next
                If Not An.IsReal OrElse An.pRe.ToDouble <> 1.0 Then
                    sb.Insert(0, An.ToStringComplex(cfg) + " *")
                End If
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return sb.ToString
    End Function
    Public Shared Function opFactor(Pa As Polynomial, Optional cfg As Config = Nothing) As Polynomial()
        Dim Pc(-1) As Polynomial, iP As Int64
        Try

            Dim vPt(-1) As Complex, iPt As Int64 = 0
            Dim x As Polynomial = Polynomial.GetPolynomial(Pa.var(0))
            'Dim Va As New Vector(Pa)
            Dim vRoots() As Complex = Nothing
            If Pa.roots IsNot Nothing AndAlso
            Pa.roots.cjo.Length Then
                vRoots = Pa.roots.cjo
            Else
                vRoots = Polynomial.opRoots(Pa, False, Nothing, True).cjo
            End If
            Dim vR(-1) As Complex
            Dim i As Int64
            If cfg Is Nothing Then
                cfg = Pa.cfg
            End If
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
            For i = 0 To vRoots.Length - 1
                ReDim Preserve Pc(iP)
                Pc(iP) = New Polynomial(x + New Polynomial(-vRoots(i)))
                iP += 1
            Next
        Catch ex As Exception
            Throw ex
        End Try
        Return Pc
    End Function

    Public Function maxReRoot() As Double
        Dim maxRealRoot As Double = 0
        Try
            Dim a(cf.Length - 1) As Double
            For i = 0 To cf.Length - 1
                a(a.Length - 1 - i) = cf(i).pRe.ToDouble
                maxRealRoot += Math.Abs(a(a.Length - 1 - i))
            Next
            maxRealRoot = Math.Max(1, maxRealRoot)
            Dim max2 As Double = 0
            For i = 0 To a.Length - 2
                max2 = Math.Max(a(i) + 1, max2)
            Next
            maxRealRoot = Math.Max(max2, maxRealRoot) + 1.2

        Catch ex As Exception
            Throw ex
        End Try
        Return maxRealRoot
    End Function
    Public ReadOnly Property ToComplex As Complex
        Get
            Return An
        End Get
    End Property
    Public ReadOnly Property ToDouble As Double
        Get
            Return An.pRe.ToDouble
        End Get
    End Property
    Public ReadOnly Property ToDoubleAn As Double
        Get
            Return An.pRe.ToDouble
        End Get
    End Property
    Public Overrides Function ToString() As String
        Return toStringPoly(Config.cfg)
    End Function
    Public Function toStringPoly(ByVal cfg As Config) As String
        Dim e1 As String = ""
        Dim i, j As Int64
        Try
            Dim pA As New Polynomial(Me)
            If cfg.bRounding Then
                Dim bIsZero As Boolean = True
                For i = 0 To pA.cf.Length - 1
                    Dim re As Double = cf(i).pRe.ToDouble
                    Dim im As Double = cf(i).pIm.ToDouble
                    Dim reAbs As Double = Math.Round(Math.Abs(re), 3)
                    Dim imAbs As Double = Math.Round(Math.Abs(im), 3)
                    Dim term As Polynomial = pA.GetPolyFromTerm(i, Nothing)
                    If term.getDegree = 0 Then
                        If Not (reAbs < 0.001 AndAlso imAbs = 0) AndAlso
                            Not (reAbs < 0 AndAlso imAbs = 0.001) Then
                            bIsZero = False
                        Else
                            If reAbs < 0.001 AndAlso imAbs = 0 Then
                                pA.cf(i) = New Complex(re)
                            End If
                            If reAbs = 0 AndAlso imAbs < 0.001 Then
                                pA.cf(i) = New Complex(0.0, im)
                            End If
                        End If
                        'If reAbs = 0 AndAlso imAbs = 0 Then
                        '    pA.cf(i) = Complex.zero
                        'End If
                    End If
                Next
                If bIsZero Then
                    pA = New Polynomial(Me)
                End If
            End If
            For i = 0 To pA.cf.Length - 1
                Dim cff As New Complex(pA.cf(i))
                Dim e2 As String = cff.ToStringComplex(cfg)
                If e2 <> "0" Then
                    If (e2.Chars(0) = "-" AndAlso Regex.IsMatch(Mid(e2, 2), "[-+]")) _
                    OrElse (e2.Chars(0) <> "-" AndAlso Regex.IsMatch(e2, "[-+]")) Then
                        Dim db As Double
                        If Not MathGlobal8.TryParseDbl(e2, db) Then
                            e2 = "(" + e2 + ")"
                        End If
                        'e2 = "(" + e2 + ")"
                    End If
                    If i < exp.Length Then
                        For j = 0 To exp(i).Length - 1
                            Dim e3 As String = ""
                            Dim k As Int64 = exp(i)(j)
                            If k >= 1 OrElse k < 0 Then
                                e3 += var1(j) ' sVars(j1)
                                If k > 1 OrElse k < 0 Then
                                    Dim cjoK As New Complex(k)
                                    e3 += "^" + cjoK.ToStringComplex(cfg)
                                End If
                                If e2 = "1" AndAlso Len(e3) Then
                                    e2 = e3
                                ElseIf e2 = "-1" AndAlso Len(e3) Then
                                    e2 = " -" + e3
                                ElseIf Len(e3) Then
                                    e2 += "*" + e3
                                End If
                            End If
                        Next
                    End If
                    e1 += e2
                    e2 = ""
                    If i < cf.Length - 1 Then
                        e1 += " +"
                    End If
                End If
sigI:
            Next
            If Me.PolyResto IsNot Nothing Then
                'tryReducePolyResto()
            End If
            If Me.PolyResto IsNot Nothing Then
                Dim e2 As String = pA.PolyResto.toStringPoly(cfg)
                If e2 <> "0" AndAlso pA.PolyDivisor IsNot Nothing Then
                    Dim lvarR As Int64 = pA.PolyResto.var1.Length
                    Dim lvar As Int64 = pA.PolyDivisor.var1.Length
                    Dim e2b As String = IIf(e2.Chars(0) = "-", Mid(e2, 2), e2)
                    If Regex.IsMatch(e2b, "[-+]") Then
                        ' numerator needs to be surrounded by ( )
                        Dim db As Double
                        If Not MathGlobal8.TryParseDbl(e2, db) Then
                            e2 = "(" + e2 + ")"
                        End If
                    End If

                    Dim e3 As String = pA.PolyDivisor.toStringPoly(cfg)
                    Dim mc As MatchCollection =
                    Regex.Matches(e3, cfg.mathGlobal.sAll2)
                    Dim m As Match = mc(0)
                    ' avoid parentheses as for ex. in 1/(y) ?
                    If Not (mc.Count = 1 AndAlso
                           MathGlobal8.IsOperand(m)) Then
                        Dim e4 As String = IIf(e3.Chars(0) = "-", Mid(e3, 2), e3)
                        ' avoid parentheses as for ex. in 1/(y^5) ?
                        If Regex.IsMatch(e4, "[-+*]") Then
                            ' denominator needs to be surrounded by ( )
                            Dim db As Double
                            If Not MathGlobal8.TryParseDbl(e3, db) Then
                                e3 = "(" + e3 + ")"
                            End If
                        End If
                    End If
                    e2 += "/" + e3
                    If e2.Chars(0) = "-" Then
                        e1 += e2
                    Else
                        e1 += "+" + e2
                    End If
                    If Left(e1, 2) = "0+" Then e1 = Mid(e1, 3)
                    If Left(e1, 2) = "0-" Then e1 = Mid(e1, 2)
                End If
            End If
            If e1 Is Nothing OrElse
            e1.Length = 0 Then
                e1 = "0"
                Exit Try
            End If
            e1 = Replace(e1, "+-", "-")
            e1 = Replace(e1, "+ -", "-")

            ' supress leading "+" and trailing "-" or "+":
            e1 = Regex.Replace(e1, "(^(\+)+)|([-+]+$)", "")
        Catch ex As Exception
            Throw ex
        End Try
        Return e1
    End Function

    Public Function toStringHorner() As String
        ' salida en la forma: (An*x + An-1)*x + An-2)*x + ....+ A1)*x + A0
        Dim i As Int64
        Dim e1 As String = ""
        Try
            Dim Pa As New Polynomial(Me)
            If Pa.exp.Length > 1 Then
                If Pa.exp(0)(0) < Pa.exp(1)(0) Then
                    Array.Reverse(Pa.exp)
                    Array.Reverse(Pa.cf)
                End If
            End If
            Dim exp1 As Int32 = Pa.exp(0)(0)
            Dim exp2 As Int32 = exp1
            e1 = StrDup(exp1, "(") + Pa.cf(0).toString
            For i = 1 To Pa.cf.Length - 1
                exp2 = Pa.exp(i)(0)
                Do While exp1 > exp2
                    e1 += ")*x"
                    exp1 -= 1
                Loop
                Dim e2 As String = Pa.cf(i).toString
                If e2.Chars(0) = "-" Then
                    e1 += e2
                Else
                    e1 += "+" + e2
                End If
            Next
            Do While exp2 > 0
                e1 += ")*x"
                exp2 -= 1
            Loop
            If PolyResto IsNot Nothing Then
                e1 += "(" + PolyResto.toStringHorner + ")/("
                e1 += PolyDivisor.toStringHorner + ")"
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return e1

    End Function
    Public Function ToStringInfo(ByVal cfg As Config, Optional roots As retOpRoots = Nothing) As String
        Return ToStringInfo2(cfg, "", roots)
    End Function

    Public Function ToStringInfo2(ByVal cfg As Config,
                            ByRef sFn As String, Optional roots As retOpRoots = Nothing) As String
        Dim e1 As String = ""
        Try
            Const maxDegreeFindingZeros As Int64 = MathGlobal8.maxPolyDegree
            Const maxDegreeFindingMaxMin As Int64 = 18
            If roots IsNot Nothing Then
                'Me.roots = roots
            End If
            Dim sMe As String = Me.toStringPoly(cfg)
            If Me.opSuprNegExp Then
                ' habían exponentes negativos
                e1 += sMe + " = 0 => " + vbCrLf
                e1 += Me.toStringPoly(cfg) + " = 0" + vbCrLf
            End If
            Dim i As Int64 = 0
            Dim p1 As New Polynomial(1.0)
            If Me.isComplex Then
                Return toStringPoly(cfg)
            ElseIf Me.var1.Length <> 1 Then
                'If Me.var1.Length = 2 Then
                '    Dim cnc As Conic = Nothing
                '    If Conic.tryParseConic(Me, cnc) Then
                '        Return cnc.ToStringInfo
                '    End If
                'End If
                Return toStringPoly(cfg)
            End If
            Dim oS As New polynomialSort(exp, var, cf)
            Dim grado As Int64 = oS.degree ' Polynomial.opCompare(Me, p1)
            If grado < 1 OrElse
            Me.PolyResto IsNot Nothing OrElse
            Me.dbModuloInOperations <> 0.0 Then
                If grado = 1 Then
                    If Me.cf.Length < 2 Then
                        sFn = "0"
                    Else
                        Dim cf As Complex = -Me.cf(1) / Me.cf(0)
                        Dim xmncf As Polynomial =
                            Polynomial.GetPolynomial(var(0)) _
                            - New Polynomial(cf)
                        sFn = xmncf.toStringPoly(cfg)
                    End If
                End If
                Return toStringPoly(cfg)
            End If
            Dim xi, xf As Double
            e1 += toStringPoly(cfg)
            If grado > maxDegreeFindingZeros Then
            Else
                e1 += " = " + vbCrLf
                Dim oRoots As retOpRoots = Nothing
                If Me.roots IsNot Nothing Then
                    'Dim ret1 As New retOpRoots
                    'ret1.mtx = New Matrix(0.0)
                    'ReDim ret1.mtx.vVect(0)
                    'ret1.mtx.vVect(0) = New Vector(0.0)
                    'ReDim ret1.mtx.vVect(0).vPoly(roots.Length - 1)
                    'For i = 0 To roots.Length - 1
                    '    ret1.mtx.vVect(0).vPoly(i) = New Polynomial(root.cjo(i))
                    'Next
                    'ret1.mtx.vVect(0) = ret1.mtx.vVect(0).sortRoots
                    'ret1.mtx = Matrix.opTranspose(ret1.mtx)
                    'ret1.cjo = roots
                    oRoots = Me.roots
                Else
                    oRoots = Polynomial.opRoots(Me, False, cfg)
                End If
                If Me.PolyResto IsNot Nothing AndAlso
                    Me.var.Length = 1 Then
                    Dim polyNum As Polynomial = Nothing
                    Dim polyDen As Polynomial = Nothing
                    Dim oRootsNum As retOpRoots = Nothing
                    Dim oRootsDen As retOpRoots = Nothing

                    polyNum = New Polynomial(Me.PolyResto)
                    polyDen = New Polynomial(Me.PolyDivisor)
                    Me.PolyResto = Nothing
                    Me.PolyDivisor = Nothing
                    polyNum += Me * polyDen
                    oRootsNum = Polynomial.opRoots(polyNum, False, cfg)
                    oRootsDen = Polynomial.opRoots(polyDen, False, cfg)
                    ' Numerator zeros:
                    'vRootsNum = oRootsNum.opAllRoots()
                    ' Denominator zeros (=poles):
                    'vRootsDen = oRootsDen.opAllRoots()
                    e1 += "= (" + polyNum.toStringPoly(cfg) +
                        ")/(" + polyDen.toStringPoly(cfg) + ")"
                    Dim denUsaParent As Boolean = False
                    If polyDen.cf.Length > 1 OrElse
                            polyDen.cf(0).pRe.ToDouble <> 1 OrElse
                            polyDen.cf(0).pIm.ToDouble <> 0 Then
                        denUsaParent = True
                    End If
                    If oRootsDen Is Nothing OrElse oRootsDen.cjo.Length = 0 Then
                        e1 += " = 0" + vbCrLf
                        If oRootsNum IsNot Nothing AndAlso oRootsNum.cjo.Length Then
                            e1 += "= " + polyNum.ToStringInfo(cfg)
                        End If
                        GoTo salir
                    End If
                    e1 += "/"
                    If denUsaParent Then
                        e1 += "("
                    End If
                    e1 += polyDen.toStringPoly(cfg)
                    If denUsaParent Then
                        e1 += ")"
                    End If
                    ' Supress equal values from num. & denominator:
                    Dim bDoSupr As Boolean = False
                    Dim bSuprNum(oRootsNum.cjo.Length - 1) As Boolean
                    Dim bSuprDen(oRootsDen.cjo.Length - 1) As Boolean
                    For i = 0 To oRootsNum.cjo.Length - 1
                        For j = 0 To oRootsDen.cjo.Length - 1
                            If bSuprNum(i) = False AndAlso bSuprDen(j) = False AndAlso
                                oRootsNum.cjo(i).toString() = oRootsDen.cjo(j).toString() Then
                                bSuprNum(i) = True : bSuprDen(j) = True
                                bDoSupr = True
                                Exit For
                            End If
                        Next
                    Next
                    If bDoSupr Then
                        'e1 += vbCrLf + "= " + _
                        '    oRootsNum.mtx.vVect(0).ToStringFactors(cfg, Me.PolyResto.An, Me.var(0)) + "/"
                        e1 += vbCrLf + "= " + Me.ToStringFactors(cfg)

                        If oRootsDen.cjo.Length > 1 Then
                            e1 += "("
                        End If
                        e1 += oRootsDen.toStringFactors(cfg, Me.PolyDivisor.An, Me.var(0))
                        If oRootsDen.hasRoots > 1 Then
                            e1 += ")"
                        End If
                        'e1 += vbCrLf
                        ' Recompose polyNum and polyDen out from vRootsNum & vRootsDen:
                        For i = oRootsNum.cjo.Length - 1 To 0 Step -1
                            If bSuprNum(i) Then
                                For j As Int64 = i To oRootsNum.cjo.Length - 2
                                    oRootsNum.cjo(j) = oRootsNum.cjo(j + 1)
                                Next
                                ReDim Preserve oRootsNum.cjo(oRootsNum.cjo.Length - 2)
                            End If
                        Next
                        For i = oRootsDen.cjo.Length - 1 To 0 Step -1
                            If bSuprDen(i) Then
                                For j As Int64 = i To oRootsDen.cjo.Length - 2
                                    oRootsDen.cjo(j) = oRootsDen.cjo(j + 1)
                                Next
                                ReDim Preserve oRootsDen.cjo(oRootsDen.cjo.Length - 2)
                            End If
                        Next
                        e1 += vbCrLf + "="
                        If oRootsNum.cjo.Length Then
                            'oRootsNum.cjo(0).var = New String() {Me.var(0)}
                            e1 += oRootsNum.toStringFactors(
                                cfg, Me.PolyResto.An, Me.var(0))
                        Else
                            e1 += "= 1"
                        End If
                        If oRootsDen.cjo.Length Then
                            'oRootsDen.cjo(0).var = New String() {Me.var(0)}
                            e1 += "/"
                            If oRootsDen.cjo.Length > 1 Then
                                e1 += "("
                            End If
                            e1 += oRootsDen.toStringFactors(
                                cfg, Me.PolyDivisor.An, Me.var(0))
                            If oRootsDen.cjo.Length > 1 Then
                                e1 += ")"
                            End If
                            e1 += vbCrLf
                        End If
                    End If
vRootsDenIsNull:
                    If oRootsNum.cjo.Length = 0 Then
                        polyNum = New Polynomial(New Complex(0, "1"))
                        e1 += "= 1/(" + polyDen.ToString + ")"
                        Exit Try
                    Else
                        oRoots.mtx = New Matrix(New Vector(oRoots.cjo))
                        polyNum = Vector.toPolynomialFromRoots(New Vector(oRoots.cjo))
                        If oRootsDen.cjo.Length = 0 Then
                            'oRoots.mtx = New Matrix(New Vector(oRoots.cjo))
                            ' continue as if there had benn no .polyResto and .polyDivisor
                        Else
                            polyDen = Vector.toPolynomialFromRoots(New Vector(oRootsDen.cjo))
                            e1 += vbCrLf + vbCrLf
                            e1 += " A. Numerator's Zeros " + vbCrLf
                            e1 += "====================" + vbCrLf
                            e1 += polyNum.ToStringInfo(cfg) + vbCrLf + vbCrLf + vbCrLf
                            e1 += " B. Denominator's Zeros (Poles) " + vbCrLf
                            e1 += "=============================" + vbCrLf
                            e1 += polyDen.ToStringInfo(cfg)
                            Return e1
                        End If
                    End If
                End If
                If oRoots IsNot Nothing Then ' (oRoots.Pa.Type And Shunting_Yard.exprType.UniVar) Then
                    'MySessionMates5.bUseFractions = True

                    ' Dim vRoots As Vector = New Vector(Matrix.opTranspose(oRoots.mtx).vVect(0))
                    ' Me.roots = vRoots.tovCjo
                    xi = Double.MaxValue
                    xf = -Double.MaxValue
                    For i = 0 To oRoots.cjo.Length - 1
                        'vRoots.vPoly(i).setVars(Me.var)
                        oRoots.mtx.vVect(i).vPoly(0).setVars(Me.var)
                        If oRoots.cjo(i).pIm.IsZero Then
                            Dim dbx As Double = oRoots.cjo(i).pRe.ToDouble
                            If dbx < xi Then
                                xi = dbx
                            End If
                            If dbx > xf Then
                                xf = dbx
                            End If
                        End If
                    Next

                    ' guardamos si hay redondeo pq. ToStringFactors()
                    ' puede modificarlo y luego, después de oRoots.mtx.toStringRoots()
                    ' lo recuperaremos:
                    Dim bOldRounding As Boolean = cfg.bRounding
                    'Dim e2 As String = vRoots.ToStringFactors(cfg, An, var1(0), Me) ', True )
                    Dim e2 As String = Me.ToStringFactors(cfg) ', True )
                    sFn = e2

                    Dim vDivRoots As Vector = Nothing
                    'If oRoots.sDivisor <> "" Then
                    '    e1 += oRoots.sDivisor
                    '    vDivRoots = oRoots.odivRoots.opAllRoots()
                    '    Dim ndivRoot As Int64 = vDivRoots.vE.Length
                    '    Dim e5() As String = vDivRoots.toStringRoots(True)
                    'End If
                    'MySessionMates5.bUseFractions = False
                    'If IsEquation Then
                    '    e1 += vbCrLf + "=> " + e2 + " = 0" + vbCrLf
                    'Else
                    e1 += vbCrLf + "= " + e2 + vbCrLf
                    'End If
                    'If var Is Nothing OrElse var.Length = 0 Then
                    '    var = oRoots.Pa.var
                    'End If
                    'If var Is Nothing OrElse var.Length = 0 Then
                    '    GoTo salir
                    'End If
                    e1 += " ---------------- " + vbCrLf + vbCrLf
                    e1 += "'" + var(0) + "' values that are zeros of the polynomial:" + vbCrLf
                    'oRoots.mtx = New Matrix(New Vector(Me.roots))
                    'oRoots.mtx = Matrix.opTranspose(oRoots.mtx)
                    oRoots.mtx = Me.roots.mtx
                    Dim e4() As String = oRoots.mtx.toStringRoots(cfg)
                    cfg.bRounding = bOldRounding
                    e1 += Join(e4, vbCrLf)
                    If isComplex() Then
                        GoTo salir
                    End If
                    If False AndAlso grado >= 2 AndAlso grado < maxDegreeFindingMaxMin Then

                        ' OBTENER LOS MÁXIMOS Y MÍNIMOS DEL POLINOMIO:
                        Dim D As Polynomial = Me.opDerivative(Me.var(0))
                        Dim oRootsD As retOpRoots = Polynomial.opRoots(D, False, cfg)
                        'Dim vRootsD As Vector = oRootsD.opAllRoots()
                        Dim IsMax(oRootsD.cjo.Length - 1) As Int64
                        Dim vDb(IsMax.Length - 1) As Double
                        Dim i1(IsMax.Length - 1) As Int64
                        Dim fx(oRootsD.cjo.Length - 1) As Complex
                        Dim esPar As Boolean = True
                        For i = 0 To oRootsD.cjo.Length - 1
                            Dim pt As New Complex(
                                     oRootsD.cjo(i).pRe.ToDouble)
                            fx(i) = Me.evalCjo(pt)
                            Dim L, R As Double
                            Dim db As Double = pt.pRe.ToDouble
                            vDb(i) = db * 10 ^ 6 + fx(i).pRe.ToDouble / 10 ^ 6
                            i1(i) = i
                            Dim incr As Double = Math.Abs(db) / 10 ^ 6 + 10 ^ -6
                            pt = New Complex(db - incr, 0)
                            L = D.evalCjo(pt).pRe.ToDouble
                            pt = New Complex(db + incr, 0)
                            R = D.evalCjo(pt).pRe.ToDouble
                            If L > 0 AndAlso R > 0 Then
                                IsMax(i) = 4
                            ElseIf L > 0 Then
                                IsMax(i) = 3
                            ElseIf R > 0 Then
                                IsMax(i) = 2
                            Else
                                IsMax(i) = 1
                            End If
                        Next
                        e1 += vbCrLf + " ---------------- " + vbCrLf + vbCrLf
                        Dim bHasCffCjos As Boolean = False
                        For i = 0 To cf.Length - 1
                            Dim sCf As String = cf(i).toString
                            If InStr(sCf, cfg.sImg) Then
                                bHasCffCjos = True
                                Exit Try
                            End If
                        Next
                        e1 += " Stationary points:" + vbCrLf
                        Dim olde3 As String = ""
                        Dim e3 As String = ""
                        Array.Sort(vDb, i1)
                        Dim j As Int64
                        For j = 0 To oRootsD.cjo.Length - 1
                            i = i1(j)
                            Dim db As Double = oRootsD.cjo(i).pRe.ToDouble
                            If db < xi Then
                                xi = db
                            End If
                            If db > xf Then
                                xf = db
                            End If

                            Dim sDb As String = oRootsD.cjo(i).pRe.ToDouble.ToString(MathGlobal8.us)
                            If cfg.bRounding Then
                                sDb = Math.Round(oRootsD.cjo(i).pRe.ToDouble, 3).ToString(MathGlobal8.us)
                            End If
                            Dim sFx As String = fx(i).toString()
                            If IsMax(i) = 1 Then
                                e3 = String.Format("Point ({0}, P({0})) = ( {1}, {2}) is a falling point of inflexion.", var(0),
                                                  sDb, sFx)
                            ElseIf IsMax(i) = 2 Then
                                e3 = String.Format("Point ({0}, P({0})) = ( {1}, {2}) is a minimum.", var(0),
                                                  sDb, sFx)
                            ElseIf IsMax(i) = 3 Then
                                e3 = String.Format("Point ({0}, P({0})) = ( {1}, {2}) is a maximum.", var(0),
                                                  sDb, sFx)
                            ElseIf IsMax(i) = 4 Then
                                e3 = String.Format("Point ({0}, P({0})) = ( {1}, {2}) is a rising point of inflexion.", var(0),
                                                  sDb, sFx)
                            End If
                            If Len(e3) AndAlso e3 <> olde3 Then
                                e1 += e3 + vbCrLf
                                olde3 = e3
                            End If
                        Next
salir:
                    End If
                Else
                    e1 = "= " + vbCrLf + ToString()
                End If
            End If
            Dim suma As Double = Math.Abs(xf - xi)
            If suma * 1000 <= Math.Abs(xi) Then
                xi -= 5
                xf += 5
            End If
        Catch ex As Exception
            Throw ex
        End Try
        If e1 = "" Then
            Try
                Return toStringPoly(cfg)
            Catch ex As Exception

            End Try
        End If
        Return MathGlobal8.getTable(e1, "navy")
    End Function
    Public Function IsEqual(ByVal polyB As Polynomial, ByRef mult As Complex) As Boolean
        If Me.isReal OrElse polyB.isReal Then
            If Not Me.isReal OrElse Not polyB.isReal Then
                Return False
            End If
            If Me.ToDouble = 0 Then Return False
            mult = New Complex(polyB.ToDouble / Me.ToDouble)
            Return True
        End If
        If Me.isComplex OrElse polyB.isComplex Then
            If Not Me.isComplex OrElse Not polyB.isComplex Then
                Return False
            End If
            mult = polyB.ToComplex / Me.ToComplex
            Return True
        End If
        If Me.cf.Length <> polyB.cf.Length Then
            Return False
        End If
        If (Me.PolyResto Is Nothing AndAlso
        polyB.PolyResto IsNot Nothing) OrElse
        (Me.PolyResto IsNot Nothing AndAlso
         polyB.PolyResto Is Nothing) Then
            Return False
        End If
        Dim vVar() As String = Me.varAll
        Dim vVarB() As String = polyB.varAll
        If vVar.Length <> vVarB.Length Then
            Return False
        End If
        If vVar.Length Then
            Array.Sort(vVar)
            Array.Sort(vVarB)
            If Join(vVar) <> Join(vVarB) Then
                Return False
            End If
        End If
        If cf.Length = 1 AndAlso
        polyB.cf.Length = 1 AndAlso
        (cf(0) - polyB.cf(0)).IsZero Then
            Dim mult2 As Complex = Nothing
            If PolyResto IsNot Nothing AndAlso
            PolyResto.IsEqual(polyB.PolyResto, mult) AndAlso
            PolyDivisor.IsEqual(polyB.PolyDivisor, mult2) AndAlso
            (mult - mult2).IsZero Then
                Return True
            End If
        End If
        Dim polyB2 As New Polynomial(polyB)
        Dim nFound As Int64 = 0

        Dim Pa As New Polynomial(Me)
        Dim Pb As New Polynomial(polyB)
        Pa.opReduceCommonExponents()
        Pb.opReduceCommonExponents()

        Dim termsA() As Polynomial = Pa.splitIntoTerms
        Dim termsB() As Polynomial = Pb.splitIntoTerms
        If termsA.Length <> termsB.Length Then
            Return False
        End If
        If (termsA(0).isReal AndAlso termsA(0).ToDouble = 0.0) OrElse
        (termsB(0).isReal AndAlso termsB(0).ToDouble = 0.0) Then
            Return False
        End If

        nFound = 0
        For i = 0 To termsA.Length - 1
            For j = 0 To termsB.Length - 1
                If termsB(j) IsNot Nothing Then
                    If polyB2.cf(j) IsNot Nothing Then
                        Dim mult2 As Complex = Nothing
                        'If Not termsA(i).IsEqualTerm(0, termsB(j), 0, mult2) Then
                        Dim pDiv As Polynomial = termsA(i) / termsB(j)
                        If Not pDiv.isComplex AndAlso Not pDiv.isReal Then
                        Else
                            mult2 = pDiv.ToComplex
                            If mult Is Nothing Then
                                mult = mult2
                                termsB(j) = Nothing
                                nFound += 1
                                Exit For
                            ElseIf (mult - mult2).esCero Then
                                termsB(j) = Nothing
                                nFound += 1
                                Exit For
                            ElseIf (mult + mult2).esCero Then
                                mult = -mult
                                termsB(j) = Nothing
                                nFound += 1
                                Exit For
                            End If
                        End If
                    End If
                End If
            Next
        Next
        If nFound <> termsA.Length Then
            Return False
        End If
        If nFound <> cf.Length Then
            Return False
        End If
        If PolyResto IsNot Nothing Then
            Dim mult2 As Complex = Nothing
            Dim mult3 As Complex = Nothing
            If PolyResto.IsEqual(polyB.PolyResto, mult2) AndAlso
            PolyDivisor.IsEqual(polyB.PolyDivisor, mult3) AndAlso
            (mult - mult3).IsZero AndAlso
            (mult - mult2).IsZero Then
                Return True
            End If
            Return False
        End If
        Return True

    End Function
    '    Public Function IsEqualTerm(ByVal iTermA As Int64, ByVal polyB As Polynomial, ByVal iTermB As Int64, ByRef mult As Complex) As Boolean
    '        Dim ret As Boolean = False
    '        Try
    '            Dim i As Int64
    '            If (exp Is Nothing OrElse exp.Length = 0) AndAlso _
    '            (polyB.exp Is Nothing OrElse polyB.exp.Length = 0) Then
    '                mult = cf(iTermA) / polyB.cf(iTermB)
    '                ret = True
    '                Exit Try
    '            ElseIf (exp Is Nothing OrElse exp.Length = 0) OrElse _
    '            (polyB.exp Is Nothing OrElse polyB.exp.Length = 0) Then
    '                Exit Try
    '            ElseIf (exp(iTermA) Is Nothing AndAlso polyB.exp(iTermB) IsNot Nothing) OrElse _
    '            (exp(iTermA) IsNot Nothing AndAlso ( _
    '             iTermB >= polyB.exp.Length OrElse polyB.exp(iTermB) Is Nothing)) Then
    '                Exit Try
    '            ElseIf exp(iTermA).Length <> polyB.exp(iTermB).Length Then
    '                If exp(iTermA).Length Then
    '                    For i = 0 To exp(iTermA).Length - 1
    '                        If exp(iTermA)(i) <> 0 Then
    '                            Exit Try
    '                        End If
    '                    Next
    '                End If
    '                If polyB.exp(iTermB).Length Then
    '                    For i = 0 To polyB.exp(iTermB).Length - 1
    '                        If polyB.exp(iTermA)(i) <> 0 Then
    '                            Exit Try
    '                        End If
    '                    Next
    '                End If
    '            End If
    '            For i = 0 To exp(iTermA).Length - 1
    '                If i >= var.Length Then
    '                    If i < polyB.var.Length AndAlso _
    '                    i < polyB.exp(iTermB).Length AndAlso _
    '                    polyB.exp(iTermB)(i) Then
    '                        Exit Try
    '                    End If
    '                    GoTo sigI
    '                End If
    '                Dim sVarA As String = var(i)
    '                Dim posInB As Int64 = Array.IndexOf(polyB.var, sVarA)
    '                If posInB = -1 AndAlso exp(iTermA)(i) Then
    '                    Exit Try
    '                ElseIf i >= polyB.exp(iTermB).Length OrElse _
    '                (posInB = -1 AndAlso exp(iTermA)(i) = 0) OrElse _
    '                (posInB > -1 AndAlso _
    '                 posInB < polyB.exp(iTermB).Length AndAlso _
    '                exp(iTermA)(i) <> polyB.exp(iTermB)(posInB)) Then
    '                    Exit Try
    '                End If
    'sigI:
    '            Next
    '            If Not polyB.cf(iTermB).esCero Then
    '                mult = cf(iTermA) / polyB.cf(iTermB)
    '                ret = True
    '            End If
    '        Catch ex As Exception
    '            Throw ex
    '        End Try
    '        Return ret
    '    End Function
    Public Function tryToIsolateToPolynomial(ByVal sVar As String, ByRef result As Expression) As Boolean
        Dim bRet As Boolean = False
        Try
            result = Nothing
            If PolyResto IsNot Nothing Then
                ' Given P + Rem/Div = 0
                '       Div*(P + Rem/Div) = Div*0 = 0 remains true
                ' i.e. Div*P + Rem = 0
                Dim Resto As New Polynomial(PolyResto)
                Dim Div As New Polynomial(PolyDivisor)
                Me.PolyResto = Nothing
                Me.PolyDivisor = Nothing
                Dim polyC As Polynomial = Me * Div + Resto
                copyToThis(polyC)
            End If
            ' we will isolate only if terms containing variable sVar
            ' have equal exponent for sVar. Examples:
            '  x*y+ 3x+ + y+ 2 = 0 
            ' => x*(y+3) = -(y+2)
            ' => x = -(y+2)/(y+3) 
            ' => result = -(y+2)/(y+3)
            ' ------------
            '  x2*y+ 3x2+ + y+ 2 = 0 
            ' => x2*(y+3) = -(y+2)
            ' => x2 = -(y+2)/(y+3) 
            ' => result = sqr(-(y+2)/(y+3))
            ' ------------
            Dim expX As Int64 = 0
            Dim i As Int64
            Dim iVar As Int64 = Array.IndexOf(var, sVar)
            If iVar = -1 Then
                Return False
            End If
            Dim pWithX As New Polynomial(0.0)
            Dim pWithoutX As New Polynomial(0.0)
            For i = 0 To cf.Length - 1
                If exp(i) IsNot Nothing AndAlso
                iVar < exp(i).Length AndAlso
                exp(i)(iVar) <> 0 Then
                    If expX = 0 Then
                        expX = exp(i)(iVar)
                    ElseIf expX <> exp(i)(iVar) Then
                        ' exponents differ, quit:
                        Exit Try
                    End If
                    Dim pCur As Polynomial = Me.GetPolyFromTerm(i, var)
                    pCur.exp(0)(iVar) = 0
                    pWithX += pCur
                Else
                    pWithoutX += Me.GetPolyFromTerm(i, var)
                End If
            Next
            If expX AndAlso
            pWithX.ToDouble <> 0.0 Then
                bRet = True
                If expX = 1 Then
                    result = New Expression(-pWithoutX / pWithX)
                Else
                    result = New Expression(
                        -pWithoutX / pWithX) ^ New Expression(1.0 / expX)
                End If
            End If
        Catch ex As Exception
            bRet = False
        End Try
        Return bRet
    End Function

    Public Function getRootsMultiplicity() As Int64()
        Dim vmultiplicity(-1) As Int64, ivM As Int64 = 0
        Try
            If PolyResto IsNot Nothing Then
                Return Nothing
            End If
            Dim vPt(-1) As Complex, iPt As Int64 = 0
            ' Find pA's denominator roots:
            'Dim Va As New Vector(pA.PolyDivisor)
            Dim vRoots() As Complex
            If Me.roots IsNot Nothing Then
                vRoots = Me.roots.cjo
            Else
                vRoots = Polynomial.opRoots(Me).cjo
            End If
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
                    iC += 1
                    ReDim Preserve vConjugate(iC), vMultiConj(iC)
                    vConjugate(iC) = Complex.opConjugate(vRoots(i))
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
                    Do
                        vMultiNotConj(iNC) += 1
                        i += 1
                    Loop While i + 1 < vRoots.Length AndAlso
                    (vRoots(i - 1) - vRoots(i)).IsZero
                    iNC += 1
                End If
            Next
            For i = 0 To vRealRoots.Length - 1
                ReDim Preserve vmultiplicity(ivM)
                vmultiplicity(ivM) = vMultiRR(i)
                ivM += 1
            Next
            For i = 0 To vConjugate.Length - 1
                ReDim Preserve vmultiplicity(ivM)
                vmultiplicity(ivM) = vMultiConj(i)
                ivM += 1
            Next
            For i = 0 To vNotConjugate.Length - 1
                ReDim Preserve vmultiplicity(ivM)
                vmultiplicity(ivM) = vMultiNotConj(i)
                ivM += 1
            Next
            For i = 0 To ivM - 1
                If vmultiplicity(i) = 0 Then
                    vmultiplicity(i) = 1
                End If
            Next
        Catch ex As Exception
            Throw ex
        End Try
        Return vmultiplicity
    End Function
    Public Shared Function opLCM(pA As Polynomial, pB As Polynomial) As Polynomial
        Dim LCM As New Polynomial(1.0)
        Try
            ' Returns a Polynomial, Least Common Multiple (LCM) 
            ' of polynomials pA and pB.

            If pA.PolyResto IsNot Nothing OrElse pA.var.Length > 1 Then
                Return Nothing
            End If
            Dim i, j As Int64
            Dim An As Double = pA.An.pRe.ToDouble
            pA = Polynomial.opNormalize(pA)
            For i = 0 To pA.cf.Length - 1
                Dim re As Double = pA.cf(i).pRe.ToDouble
                If re <> Math.Floor(re) Then
                    Throw New Exception(String.Format(msg8.num(83), pA.ToString))
                End If
                Dim im As Double = pA.cf(i).pIm.ToDouble
                If im <> Math.Floor(im) Then
                    Throw New Exception(String.Format(msg8.num(83), pA.ToString))
                End If
            Next
            If pB.PolyResto IsNot Nothing OrElse pB.var.Length > 1 Then
                Return Nothing
            End If
            Dim Bn As Double = pB.An.pRe.ToDouble
            pB = Polynomial.opNormalize(pB)
            For i = 0 To pB.cf.Length - 1
                Dim re As Double = pB.cf(i).pRe.ToDouble
                If re <> Math.Floor(re) Then
                    Throw New Exception(String.Format(msg8.num(83), pB.ToString))
                End If
                Dim im As Double = pB.cf(i).pIm.ToDouble
                If im <> Math.Floor(im) Then
                    Throw New Exception(String.Format(msg8.num(83), pB.ToString))
                End If
            Next

            Dim x As Polynomial = Polynomial.GetPolynomial(pA.var(0))
            Dim multA() As Int64 = pA.getRootsMultiplicity
            Dim rootsA(pA.roots.cjo.Length - 1) As Complex
            Dim multB() As Int64 = pB.getRootsMultiplicity
            Dim rootsB(pB.roots.cjo.Length - 1) As Complex
            Dim multAB(-1)() As Int64, imAB As Int64 = 0
            For i = 0 To rootsA.Length - 1
                rootsA(i) = New Complex(pA.roots.cjo(i))
            Next
            For i = 0 To rootsB.Length - 1
                rootsB(i) = New Complex(pB.roots.cjo(i))
            Next
            For i = 0 To multA.Length - 1
                For j = 0 To multB.Length - 1
                    If multB(j) > 0 Then
                        If (rootsA(i) - rootsB(j)).IsZero Then
                            Dim max As Int64 = Math.Max(multA(i), multB(j))
                            For k As Int64 = 1 To max
                                LCM *= (x - New Polynomial(rootsA(i)))
                            Next
                            multB(j) = -1 ' mark as already treated
                            multA(i) = -1
                            Exit For
                        End If
                    End If
                Next
                If multA(i) > 0 Then
                    For k As Int64 = 1 To multA(i)
                        LCM *= (x - New Polynomial(rootsA(i)))
                    Next
                End If
            Next
            For i = 0 To multB.Length - 1
                If multB(i) > 0 Then
                    For k As Int64 = 1 To multB(i)
                        LCM *= (x - New Polynomial(rootsB(i)))
                    Next
                End If
            Next
            ' obtener el LCM de An y Bn:
            Dim vLCM_GCD() As Double = Functions.LCM_GCD(New Double() {An, Bn})
            ' vLCM_GCD(0)=GCD
            ' vLCM_GCD(1)=LCM
            LCM *= New Polynomial(vLCM_GCD(1))
        Catch ex As Exception
            Throw New Exception(ex.Message)
        End Try
        Return LCM
    End Function
End Class


Public Class oPolySort
    Public expCf(-1) As Int64
    Public index As Int64
    Public cf As Complex
    Public Sub New(ByVal expCf() As Int64, ByVal index As Int64, ByVal cf As Complex)
        Try
            ReDim Me.expCf(expCf.Length - 1)
            Array.Copy(expCf, Me.expCf, expCf.Length)
            Me.index = index
            Me.cf = New Complex(cf)
        Catch ex As Exception
            Throw ex
        End Try
    End Sub
End Class

Public Class polynomialSort
    Implements IComparer(Of oPolySort)


    Public var(-1) As String
    Public cfMaxExp As Complex
    Public ps(-1) As oPolySort
    Dim degree1 As Int64
    Public degreeMaxVar As Int64
    Public minDegree As Int64
    Public iMaxVar As Int64
    Const minExp As Int64 = -10000
    Public Sub New()
    End Sub
    Public Property degree() As Int64
        Get
            If degree1 <= minExp Then
                Return 0
            End If
            Return degree1
        End Get
        Set(ByVal value As Int64)
            degree1 = value
        End Set
    End Property
    Public Sub New(ByRef exp()() As Int64, ByVal var() As String, ByRef cf() As Complex)
        Try
            ReDim Me.var(var.Length - 1)
            Array.Copy(var, Me.var, var.Length)
            ReDim ps(exp.Length - 1)
            Dim i As Int64
            For i = 0 To ps.Length - 1
                ps(i) = New oPolySort(exp(i), i, cf(i))
            Next
            Array.Sort(ps, Me)
            Dim psMax As oPolySort = ps(ps.Length - 1)
            Me.cfMaxExp = New Complex(cf(psMax.index))
            Dim maxExp As Int64 = -100000
            Dim minExp As Int64 = 100000
            For i = 0 To exp(psMax.index).Length - 1
                If maxExp < exp(psMax.index)(i) Then
                    maxExp = exp(psMax.index)(i)
                    iMaxVar = i
                End If
                If minExp > exp(psMax.index)(i) Then
                    minExp = exp(psMax.index)(i)
                End If
            Next
            degreeMaxVar = minExp
            minDegree = minExp
            For i = 0 To psMax.expCf.Length - 1
                If psMax.expCf(i) > degreeMaxVar Then
                    degreeMaxVar = psMax.expCf(i)
                End If
            Next
            Array.Reverse(ps)
            For i = 0 To ps.Length - 1
                exp(i) = ps(i).expCf
                cf(i) = ps(i).cf
            Next
            degree = 0
            For i = 0 To exp(0).Length - 1
                degree += exp(0)(i)
            Next
        Catch ex As Exception
            Throw ex
        End Try
    End Sub
    Public Function Compare(ByVal x As oPolySort, ByVal y As oPolySort) As Integer Implements System.Collections.Generic.IComparer(Of oPolySort).Compare
        Try
            Dim psA As oPolySort = x 'CType(x, oPolySort)
            Dim psB As oPolySort = y ' CType(y, oPolySort)
            Dim sumA As Int64 = 0
            Dim maxExpA As Int64 = -100
            Dim valMaxExpA As Long = 0
            Dim i As Int64
            Dim maxAdmittedNumOfVars = 30
            Dim j As Int64 = psA.expCf.Length - 1
            For i = 0 To psA.expCf.Length - 1
                sumA += psA.expCf(i)
                If psA.expCf(i) > maxExpA Then
                    maxExpA = psA.expCf(i)
                End If
                valMaxExpA += psA.expCf(j) *
                maxAdmittedNumOfVars ^ j
                j -= 1
            Next
            Dim sumB As Int64 = 0
            Dim maxExpB As Int64 = -100
            Dim valMaxExpB As Long = 0
            j = psB.expCf.Length - 1
            For i = 0 To psB.expCf.Length - 1
                sumB += psB.expCf(i)
                If psB.expCf(i) > maxExpB Then
                    maxExpB = psB.expCf(i)
                End If
                valMaxExpB += psB.expCf(j) *
                maxAdmittedNumOfVars ^ j
                j -= 1
            Next
            Dim difMaxExp As Int64 = Math.Sign(maxExpA - maxExpB)
            If difMaxExp Then
                Return difMaxExp
            End If
            Dim difSum As Int64 = Math.Sign(sumA - sumB)
            If difSum Then
                Return difSum
            End If
            ' supuestamente aqui ya no pueden ser iguales,
            ' si se trata de coef. distintos:
            Return -Math.Sign(valMaxExpA - valMaxExpB)
        Catch ex As Exception
            'Dim es As String = ex.ToString()
            Throw ex
        End Try
        Return 0
    End Function
End Class
Public Class retOpRoots
    Public cjo() As Complex
    Public cjoByType()() As Complex
    Public mtx As Matrix
    Public Sub New()
    End Sub
    Public Sub New(retRoots As retOpRoots)
        Try
            If retRoots.cjo IsNot Nothing Then
                ReDim cjo(retRoots.cjo.Length - 1)
                Array.Copy(retRoots.cjo, cjo, cjo.Length)
            End If
            If retRoots.cjoByType IsNot Nothing Then
                ReDim cjoByType(retRoots.cjoByType.Length - 1)
                For i As Int64 = 0 To cjoByType.Length - 1
                    If retRoots.cjoByType(i) IsNot Nothing AndAlso
                    retRoots.cjoByType(i).Length Then
                        ReDim Preserve cjoByType(i)(retRoots.cjoByType(i).Length - 1)
                        Array.Copy(retRoots.cjoByType(i), cjoByType(i), cjoByType(i).Length)
                    Else
                        ReDim cjoByType(i)(-1)
                    End If
                Next
            End If
            If retRoots.mtx IsNot Nothing Then
                mtx = New Matrix(retRoots.mtx)
            End If
        Catch ex As Exception
            Throw ex
        End Try
    End Sub
    Public Function numRoots() As Int64
        If cjo IsNot Nothing Then
            Return cjo.Length
        End If
        Return 0
    End Function
    Public Function hasRoots() As Boolean
        If cjo IsNot Nothing AndAlso cjo.Length Then
            Return True
        End If
        Return False
    End Function
    Public Function toStringFactors(cfg As Config, _
                                    ByVal An As Complex, _
                                    ByVal sVar As String) As String
        mtx.vVect(0) = mtx.vVect(0).sortRoots
        'Return mtx.vVect(0).ToStringFactors(cfg, An, sVar)
        Return mtx.vVect(0).vPoly(0).ToStringFactors(cfg)
    End Function
End Class
