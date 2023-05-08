Imports System.Text.RegularExpressions

<Serializable()>
Partial Public Class Polynomial84
    Const maxPolyDegree As Int32 = 50
    Public cf(0) As Complex84
    Public exp(0)() As Int32
    Dim var1(-1) As String
    Public PolyResto As Polynomial84 = Nothing
    Public PolyDivisor As Polynomial84 = Nothing
    Public roots() As Complex84
    Dim dbMod As Double
    Public cfg As Config84 = Nothing
    Public Sub New()
        ReDim exp(0)
        exp(0) = New Int32() {0}
    End Sub
    Public Sub New(ByVal Re As Double)
        cf(0) = New Complex84(Re)
        ReDim exp(0)
        exp(0) = New Int32() {0}
    End Sub
    Public Sub New(ByVal Re As Double, ByVal Im As Double)
        'ReDim cf(0)
        cf(0) = New Complex84(Re, Im)
        ReDim exp(0)
        exp(0) = New Int32() {0}
    End Sub
    Public Sub New(ByVal cjo As Complex84)
        'ReDim cf(0)
        cf(0) = New Complex84(cjo)
        ReDim exp(0)
        exp(0) = New Int32() {0}
    End Sub
    Public Sub New(ByVal polyA As Polynomial84)
        copyToThis(polyA)
    End Sub

    'Public Shared Function parsePolynomial84(ByVal sPoly As String) As Polynomial84
    '    Dim retPoly As Polynomial84 = Nothing
    '    Try
    '        If sPoly Is Nothing OrElse _
    '        Trim(sPoly).Length = 0 Then
    '            Return Nothing
    '        End If
    '        Dim mtxP As New matrixParser
    '        mtxP.parse(sPoly, "")
    '        retPoly = mtxP.ret.exprMtx.getVector(0).vPoly(0)
    '    Catch ex As Exception
    '        Throw ex
    '    End Try
    '    Return retPoly
    'End Function
    'Public Shared Function tryParsePolynomial84(ByVal sPoly As String, ByRef result As Polynomial84) As Boolean
    '    Try
    '        Dim mtxP As New matrixParser
    '        mtxP.parse(sPoly, "")
    '        result = mtxP.ret.exprMtx.getVector(0).vPoly(0)
    '        Return True
    '    Catch ex As Exception
    '        Return False
    '    End Try
    '    Return False
    'End Function
    Public Function tryReducePolyResto() As Boolean
        Try
            If PolyResto Is Nothing Then
                Return False
            End If
            Dim degR As Int32 = PolyResto.getDegree
            Dim degD As Int32 = PolyDivisor.getDegree
            If degR > 0 AndAlso degD > 0 Then
                Dim gcd As Polynomial84 =
                    opGcd(PolyResto, PolyDivisor)
                If gcd IsNot Nothing AndAlso
                gcd.getDegree > 0 Then
                    PolyResto /= gcd
                    PolyDivisor /= gcd
                    degR = PolyResto.getDegree
                    degD = PolyDivisor.getDegree
                Else
                    ' 'else' added 2013/10/18:
                    Dim cjo As Complex84 = Nothing
                    If PolyResto.IsEqual(PolyDivisor, cjo) Then
                        Me.PolyResto = Nothing
                        Me.PolyDivisor = Nothing
                        Dim polyC = Me + New Polynomial84(cjo)
                        Me.copyToThis(polyC)
                    End If
                End If
            End If

            Me.opReduceCommonExponents()
            'Dim R As New Polynomial84(PolyResto)
            'Dim Q As New Polynomial84(PolyDivisor)
            'If R.getDegree < Q.getDegree Then
            '    Dim An As Complex84 = Q.An
            '    If Not An.IsReal OrElse _
            '    An.pRe.aDouble <> 1.0 Then
            '        PolyResto = R / An
            '        PolyDivisor = Q / An
            '        Return True
            '    End If
            '    Return False
            'End If
            'Dim P As Polynomial84 = Q / R
            'P.opReduceCommonExponents()
            'If P.PolyResto Is Nothing Then
            '    PolyResto = New Polynomial84(1.0)
            '    PolyDivisor = P
            '    Return True
            'End If
        Catch ex As Exception

        End Try
        Return False
    End Function
    Public Sub copyToThis(ByVal polyA As Polynomial84)
        Dim polyC As New Polynomial84
        polyC = CopyFrom(polyA)
        Me.cf = polyC.cf
        If polyC.var1.Length Then
            ReDim Me.var1(polyC.var1.Length - 1)
            Array.Copy(polyC.var1, var1, polyC.var1.Length)
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
    End Sub
    Public Sub varReduce()
        Try
            Dim iVar As Int32 = 0
            Dim j As Int32
            Do While iVar < var.Length
                Dim bSuprVar As Boolean = True
                For i As Int32 = 0 To cf.Length - 1
                    If exp(i) IsNot Nothing AndAlso
                        iVar < exp(i).Length AndAlso
                        exp(i)(iVar) <> 0 Then
                        bSuprVar = False
                        Exit For
                    End If
                Next
                If bSuprVar Then
                    For i As Int32 = 0 To cf.Length - 1
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
    Public ReadOnly Property varlen() As Int32
        Get
            Dim varU(-1) As String
            Dim iu As Int32 = 0
            If PolyResto IsNot Nothing Then
                varU = varUnion(PolyResto, PolyDivisor)
                iu = varU.Length
                For i = 0 To var1.Length - 1
                    Dim pos As Int32 = Array.IndexOf(varU, var1(i), 0, iu)
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
                Dim i, j As Int32
                For i = 0 To cf.Length - 1
                    If i < exp.Length Then
                        For j = 0 To exp(i).Length - 1
                            If exp(i)(j) Then
                                Dim pos As Int32 =
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
                            Dim pos As Int32 =
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
                            Dim pos As Int32 =
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
    Public ReadOnly Property An() As Complex84
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
    Public Shared Function GetPolyomial(ByVal sVar As String) As Polynomial84
        Dim polyC As New Polynomial84
        Try
            polyC.var1 = New String() {sVar}
            polyC.exp = New Int32()() {New Int32() {1}}
            polyC.cf = New Complex84() {New Complex84(1.0)}
        Catch ex As Exception
            Throw ex
        End Try
        Return polyC
    End Function
    Public Shared Function GetPolyomial(ByVal sVar As String, ByVal dbl() As Double) As Polynomial84
        Dim polyC As New Polynomial84
        Try
            polyC.var1 = New String() {sVar}
            ReDim polyC.exp(dbl.Length - 1), polyC.cf(dbl.Length - 1)
            For i = 0 To dbl.Length - 1
                polyC.exp(i) = New Int32() {dbl.Length - 1 - i}
                polyC.cf(i) = New Complex84(dbl(i))
            Next
        Catch ex As Exception
            Throw ex
        End Try
        Return polyC
    End Function
    Public Shared Function GetPolyomial(ByVal sVar As String, ByVal cf As Complex84) As Polynomial84
        Dim polyC As New Polynomial84
        Try
            polyC.var1 = New String() {sVar}
            polyC.exp = New Int32()() {New Int32() {1}}
            polyC.cf = New Complex84() {New Complex84(cf)}
        Catch ex As Exception
            Throw ex
        End Try
        Return polyC
    End Function
    Public Function GetPolyFromTerm(ByVal iTerm As Int32,
                                  ByVal setVars1() As String) As Polynomial84
        Dim polyC As New Polynomial84
        Try
            If iTerm >= Me.exp.Length Then
                Return Nothing
            End If
            Dim bFound As Boolean = False
            ReDim polyC.cf(0), polyC.exp(0), polyC.exp(0)(Me.exp(iTerm).Length - 1)
            If polyC.exp(0).Length Then
                For i As Int32 = 0 To Me.exp(iTerm).Length - 1
                    Dim pos As Int32 = -1
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
                    polyC.cf(0) = New Complex84(Me.cf(iTerm))
                    Exit Try
                End If
                Return Nothing
            End If
            If setVars1 Is Nothing Then
                polyC.setVars(Me.var)
            Else
                polyC.setVars(setVars1)
            End If
            polyC.cf(0) = New Complex84(Me.cf(iTerm))
        Catch ex As Exception
            Throw ex
        End Try
        Return polyC
    End Function
    Public Function getTerms() As Polynomial84()
        Dim vRet(-1) As Polynomial84
        Dim sVar() As String = Nothing
        Try
            Dim i As Int32
            Dim cur As Polynomial84 = GetPolyFromTerm(i, sVar)
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
    Public Shared Function CopyFrom(ByVal polyA As Polynomial84) As Polynomial84
        Dim polyC As New Polynomial84
        Try
            ReDim polyC.cf(polyA.cf.Length - 1), polyC.exp(polyA.exp.Length - 1), polyC.var1(polyA.var1.Length - 1)
            If polyC.cf.Length Then
                Array.Copy(polyA.cf, polyC.cf, polyA.cf.Length)
            End If
            If polyC.exp.Length Then
                Dim i As Int32
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
                polyC.PolyResto = New Polynomial84(polyA.PolyResto)
                polyC.PolyDivisor = New Polynomial84(polyA.PolyDivisor)
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return polyC
    End Function
    Public Shared Function CopyFrom(ByVal polyA As Polynomial) As Polynomial84
        Dim polyC As New Polynomial84
        Try
            ReDim polyC.cf(polyA.cf.Length - 1), polyC.exp(polyA.exp.Length - 1), polyC.var1(polyA.var1.Length - 1)
            If polyC.cf.Length Then
                For i As Int32 = 0 To polyA.cf.Length - 1
                    polyC.cf(i) = New Complex84(polyA.cf(i).pRe.ToDouble, polyA.cf(i).pIm.ToDouble)
                Next
                'Array.Copy(polyA.cf, polyC.cf, polyA.cf.Length)
            End If
            If polyC.exp.Length Then
                Dim i As Int32
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
                polyC.PolyResto = CopyFrom(polyA.PolyResto)
                polyC.PolyDivisor = CopyFrom(polyA.PolyDivisor)
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return polyC
    End Function
    Public Sub opChgSgn()
        Dim i As Int32
        Try
            For i = 0 To cf.Length - 1
                cf(i) = -cf(i)
            Next
        Catch ex As Exception

        End Try
    End Sub
    Public Shared Function opConjugate(ByVal Pa As Polynomial84) As Polynomial84
        Dim Pc As New Polynomial84(Pa)
        Dim i As Int32
        For i = 0 To Pc.cf.Length - 1
            If Pa.cf(i).pIm.ToDouble <> 0 Then
                Pc.cf(i) = New Complex84(Pa.cf(i).pRe, -Pa.cf(i).pIm)
                If Pc.cf(i).pIm.ToDouble = Pa.cf(i).pIm.ToDouble Then
                    Throw New Exception("error in conjugate")
                End If
            End If
        Next
        Return Pc
    End Function
    Public Shared Operator +(ByVal polyA As Polynomial84, ByVal polyB As Polynomial84) As Polynomial84
        Return opSumaResta(True, polyA, polyB)
    End Operator
    Public Shared Operator -(ByVal polyA As Polynomial84) As Polynomial84
        Dim polyC As New Polynomial84(polyA)
        Try
            Dim i As Int32
            For i = 0 To polyC.cf.Length - 1
                polyC.cf(i) = -polyC.cf(i)
                If polyA.dbMod <> 0.0 Then
                    polyC.cf(i).pRe = New Precis84(polyC.cf(i).pRe.ToDouble Mod polyA.dbMod)
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
                            New Precis84(polyC.PolyResto.cf(i).pRe.ToDouble Mod polyA.PolyResto.dbMod)
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
    Public Shared Operator -(ByVal polyA As Polynomial84, ByVal polyB As Polynomial84) As Polynomial84
        Return opSumaResta(False, polyA, polyB)
    End Operator
    Public Shared Function opSumaResta(ByVal bSumar As Boolean, ByVal polyA As Polynomial84, ByVal polyB As Polynomial84) As Polynomial84
        Dim polyC As New Polynomial84
        Try
            If polyB.isComplex AndAlso polyB.cf(0).esCero Then
                polyC = New Polynomial84(polyA)
                Exit Try
            ElseIf polyA.isComplex AndAlso polyA.cf(0).esCero Then
                If bSumar Then
                    polyC = New Polynomial84(polyB)
                Else
                    polyC = New Polynomial84(-polyB)
                End If
                Exit Try
            End If
            polyA.opReduceCommonExponents()
            polyB.opReduceCommonExponents()
            Dim varU() As String = varUnion(polyA, polyB)
            polyC = New Polynomial84(polyA)
            polyC.PolyResto = Nothing
            polyC.PolyDivisor = Nothing
            ReDim polyC.var1(varU.Length - 1)
            Array.Copy(varU, 0, polyC.var1, 0, varU.Length)
            ReDim Preserve polyC.cf(polyA.cf.Length + polyB.cf.Length - 1)
            Array.Copy(polyB.cf, 0, polyC.cf, polyA.cf.Length, polyB.cf.Length)
            ReDim Preserve polyC.exp(polyA.exp.Length + polyB.exp.Length - 1)

            '
            Dim i, j, k As Int32
            For i = 0 To polyB.cf.Length - 1
                k = polyA.exp.Length + i
                polyC.exp(k) = New Int32() {0}
                For j = 0 To polyB.exp(i).Length - 1
                    Dim expVarJ_enB As Int32 = polyB.exp(i)(j)
                    If expVarJ_enB <> 0 Then
                        Dim posVarJ_dePolyB_en_varU As Int32 =
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
                        polyC.PolyResto = New Polynomial84(polyB.PolyResto)
                    Else
                        polyC.PolyResto = New Polynomial84(-polyB.PolyResto)
                    End If
                    polyC.PolyDivisor = New Polynomial84(polyB.PolyDivisor)
                End If
            ElseIf polyB.PolyResto Is Nothing Then
                If polyA.PolyResto IsNot Nothing Then
                    polyC.PolyResto = New Polynomial84(polyA.PolyResto)
                    polyC.PolyDivisor = New Polynomial84(polyA.PolyDivisor)
                End If
            Else
                Dim mult As Complex84 = Nothing
                ' 2013/08/05 added:
                If polyA.PolyDivisor.IsEqual(polyB.PolyDivisor, mult) Then
                    ' PolyC =(Pa + A/C) +/- (Pb + B/C) =
                    '       =(Pa+Pb) +/- (A+B)/C
                    If bSumar Then
                        polyC.PolyResto = polyA.PolyResto + polyB.PolyResto
                    Else
                        polyC.PolyResto = polyA.PolyResto - polyB.PolyResto
                    End If
                    polyC.PolyDivisor = New Polynomial84(polyA.PolyDivisor)
                Else
                    Dim gcd As Polynomial84 = Nothing ' 2013/10/25
                    Dim polyADiv As New Polynomial84(polyA.PolyDivisor)
                    Dim polyBDiv As New Polynomial84(polyB.PolyDivisor)
                    If varU.Length = 1 Then
                        gcd = opGcd(polyA.PolyDivisor, polyB.PolyDivisor)
                        If Not gcd.isReal OrElse gcd.ToDouble <> 1.0 Then
                            polyADiv /= gcd
                            polyBDiv /= gcd
                        End If
                    End If
                    Dim numerator As Polynomial84 = polyA.PolyResto * polyBDiv ' * polyB.PolyDivisor 
                    If bSumar Then
                        numerator += polyB.PolyResto * polyADiv ' * polyA.PolyDivisor 
                    Else
                        numerator -= polyB.PolyResto * polyADiv ' * polyA.PolyDivisor 
                    End If
                    Dim denominator As Polynomial84 = polyA.PolyDivisor * polyB.PolyDivisor
                    If gcd IsNot Nothing AndAlso
                    (Not gcd.isReal OrElse gcd.ToDouble <> 1.0) Then
                        polyC.PolyResto = numerator
                        polyC.PolyDivisor = denominator / gcd
                    Else
                        polyC.PolyResto = numerator
                        polyC.PolyDivisor = denominator
                    End If
                End If
            End If
            If polyC.PolyResto IsNot Nothing Then
                If polyC.isComplex() Then
                    Dim aSumar As New Polynomial84(polyC.PolyResto.cf(0) /
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

    Public Shared Operator *(ByVal db As Double, ByVal polyA As Polynomial84) As Polynomial84
        Dim polyC As New Polynomial84(polyA)
        Dim i As Int32
        Try
            For i = 0 To polyC.cf.Length - 1
                polyC.cf(i) *= db
            Next
        Catch ex As Exception
            Throw ex
        End Try
        Return polyC
    End Operator
    Public Shared Operator *(ByVal polyA As Polynomial84, ByVal db As Double) As Polynomial84
        Dim polyC As New Polynomial84(polyA)
        Dim i As Int32
        Try
            For i = 0 To polyC.cf.Length - 1
                polyC.cf(i) *= db
            Next
        Catch ex As Exception
            Throw ex
        End Try
        Return polyC
    End Operator
    Public Shared Operator *(ByVal polyA As Polynomial84, ByVal polyB As Polynomial84) As Polynomial84
        Dim polyC As Polynomial84
        If (polyA.isReal AndAlso polyA.cf(0).pRe.IsZero) OrElse
        (polyB.isReal AndAlso polyB.cf(0).pRe.IsZero) Then
            Return New Polynomial84(0.0)
        End If
        polyA.opReduceCommonExponents()
        polyB.opReduceCommonExponents()
        If (polyA.isComplex AndAlso polyA.cf(0).esCero) OrElse
        (polyB.isComplex AndAlso polyB.cf(0).esCero) Then
            If polyA.PolyResto Is Nothing AndAlso
            polyB.PolyResto Is Nothing Then
                Return New Polynomial84(0.0)
            End If
        End If
        If polyA.PolyResto Is Nothing AndAlso polyB.PolyResto Is Nothing Then
            polyC = mult(polyA, polyB)
            Return polyC
        End If
        If polyA.PolyResto Is Nothing Then
            '  A * ( B + c/d) = A*( (B*d+c)/d) =
            ' = (A*B*d + A*c) / d
            Dim B1 As New Polynomial84(polyB)
            Dim c1 As New Polynomial84(polyB.PolyResto)
            Dim d1 As New Polynomial84(polyB.PolyDivisor)
            B1.PolyResto = Nothing
            B1.PolyDivisor = Nothing
            polyC = (mult(mult(polyA, B1), d1) + mult(polyA, c1))
            polyC /= d1
        ElseIf polyB.PolyResto Is Nothing Then
            Dim A1 As New Polynomial84(polyA)
            Dim c1 As New Polynomial84(polyA.PolyResto)
            Dim d1 As New Polynomial84(polyA.PolyDivisor)
            ' (A+c/d)*B = (A*d*B+c*B)/d
            A1.PolyResto = Nothing
            polyC = (mult(mult(polyB, A1), d1) + mult(polyB, c1)) / d1
        Else
            Dim A As New Polynomial84(polyA)
            Dim ac As New Polynomial84(polyA.PolyResto)
            Dim ad As New Polynomial84(polyA.PolyDivisor)
            A.PolyResto = Nothing
            A.PolyDivisor = Nothing
            Dim B As New Polynomial84(polyB)
            Dim c As New Polynomial84(polyB.PolyResto)
            Dim d As New Polynomial84(polyB.PolyDivisor)
            B.PolyResto = Nothing
            B.PolyDivisor = Nothing
            ' (a+ ac/ad) * (B + c/d) = [( a*ad + ac)/ad ] * [(B*d+c)/d ] =

            ' = ( a*ad + ac) * (B*d+c) / (ad * d)
            polyC = mult(mult(A, ad) + ac, mult(B, d) + c) / mult(ad, d)
        End If
        Return polyC
    End Operator
    Shared Function mult(ByVal polyA As Polynomial84, ByVal polyB As Polynomial84) As Polynomial84
        Dim polyC As New Polynomial84
        Try
            Dim i, i1, j, j1, k As Int32
            If polyA.isComplex Then
                Dim cjo As Complex84 = polyA.ToComplex
                If cjo.IsZero Then
                    polyC = New Polynomial84(0.0)
                    Exit Try
                End If
                polyC = New Polynomial84(polyB)
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
                Dim cjo As Complex84 = polyB.ToComplex
                If cjo.IsZero Then
                    polyC = New Polynomial84(0.0)
                    Exit Try
                End If
                polyC = New Polynomial84(polyA)
                For i = 0 To polyC.cf.Length - 1
                    polyC.cf(i) *= cjo
                Next
                Exit Try
            End If
            polyA = polyA + New Polynomial84(0.0)
            polyB = polyB + New Polynomial84(0.0)
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
                                    Dim exp As Int32 = polyA.exp(i)(j1)
                                    If exp Then
                                        Dim posCa As Int32 =
                                            Array.IndexOf(varU, polyA.var1(j1), 0, varU.Length)
                                        polyC.exp(i1)(posCa) += exp
                                    End If
                                End If
                                If j1 < polyB.exp(j).Length Then
                                    Dim exp As Int32 = polyB.exp(j)(j1)
                                    If exp Then
                                        Dim posCb As Int32 =
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
                polyC.cf = New Complex84() {polyA.cf(0) * polyB.cf(0)}
            End If

            polyC.opReduceCommonExponents()
        Catch ex As Exception
            Throw ex
        End Try
        Return polyC
    End Function
    Public Shared Operator /(ByVal polyA As Polynomial84, ByVal cf As Complex84) As Polynomial84
        Dim polyC As New Polynomial84(polyA)
        Try
            If cf.esCero Then
                'Throw New Exception(msg884.msg(1005)) ' Division by zero
            End If
            Dim i As Int32
            For i = 0 To polyC.cf.Length - 1
                polyC.cf(i) /= cf
            Next
        Catch ex As Exception
            Throw ex
        End Try
        Return polyC
    End Operator
    Private Function opDegree1var(ByRef cf_N As Complex84) As Int32
        Dim n As Int32 = 0
        Try
            Dim i, j As Int32
            For i = 0 To exp.Length - 1
                Dim nCur As Int32 = 0
                For j = 0 To exp(i).Length - 1
                    nCur += exp(i)(j)
                Next
                If nCur > n Then
                    n = nCur
                    cf_N = New Complex84(cf(i))
                End If
            Next
        Catch ex As Exception
            Throw ex
        End Try
        Return n
    End Function
    Public Function opDegree(ByRef cf_N As Complex84,
                              ByVal varsUnion() As String,
                              ByRef expsDiff() As Int32,
                              Optional ByVal bSortMultiVar As Boolean = False,
                              Optional ByRef iVarMaxExp As Int32 = -1,
                              Optional ByRef sortPoly As Polynomial84Sort = Nothing) As Int32
        Dim n As Int32 = 0
        Try
            If varsUnion Is Nothing Then
                varsUnion = var1
            End If
            If var1.Length = 0 Then
                n = 0
            ElseIf False AndAlso varsUnion.Length = 1 Then

                ' OBSOLETO:

                Dim i, j As Int32
                ' Supposedly opReduceCommonExponents() method
                ' has been called anytime before and there
                ' are no 2 coeff. with equal exponents (coeff. are unique)
                Dim vDegrees(exp.Length - 1) As Int32
                For i = 0 To exp.Length - 1
                    Dim nCur As Int32 = 0
                    For j = 0 To exp(i).Length - 1
                        nCur += exp(i)(j)
                    Next
                    If nCur > n Then
                        n = nCur
                        cf_N = New Complex84(cf(i))
                    End If
                    vDegrees(i) = -nCur ' - sign so highest exponent will go to vDegrees(0) after array.sort()
                Next
                If vDegrees.Length > 1 Then
                    Dim i2(vDegrees.Length - 1) As Int32
                    Array.Copy(vDegrees, i2, i2.Length)
                    Array.Sort(vDegrees, exp) ' sort from lower to higher exponent: for ex. 1+x^2+x^5+...
                    Array.Sort(i2, cf) ' sort the corresponding coeff. with the same criterium
                End If
            Else 'If varsUnion.Length > 1 Then

                ' This piece of code is for Polynomial84 division (or sorting)
                ' when there is more than 1 variable.
                Dim lnDif As Int32 = exp.Length - cf.Length
                If lnDif > 0 Then
                    Dim lncf As Int32 = cf.Length
                    ReDim Preserve cf(exp.Length - 1)
                    For k As Int32 = lncf To cf.Length - 1
                        cf(k) = Complex84.zero
                    Next
                End If
                If exp.Length Then
                    Dim oS As New Polynomial84Sort(exp, var, cf)
                    sortPoly = oS

                    cf_N = oS.cfMaxExp
                    n = oS.degreeMaxVar
                    iVarMaxExp = oS.iMaxVar


                    If expsDiff IsNot Nothing Then
                        ' exponents with max. degree are at exp(0)
                        For j = 0 To exp(0).Length - 1
                            If j < var1.Length Then
                                Dim pos As Int32 = Array.IndexOf(varsUnion, var1(j))
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
                    cf_N = New Complex84(cf(0))
                Else
                    cf_N = New Complex84(0.0)
                End If
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return n
    End Function

    Public Shared Operator /(ByVal polyA As Polynomial84,
                             ByVal polyB As Polynomial84) As Polynomial84
        'If polyB.exp.Length = 0 OrElse _
        '    (polyB.exp.Length = 1 AndAlso _
        '        polyB.exp(0).Length = 1 AndAlso _
        '        polyB.exp(0)(0) = 0) Then
        '    Return New Polynomial84(polyA / polyB.cf(0))
        'End If
        Dim polyC As New Polynomial84(0.0)
        Try
            polyA.opReduceCommonExponents()
            polyB.opReduceCommonExponents()
            If polyB.isComplex Then
                Return New Polynomial84(polyA / polyB.cf(0))
            End If
            If polyA.isComplex AndAlso polyA.cf(0).esCero Then
                Return New Polynomial84(0.0)
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
                Dim num As Polynomial84 = New Polynomial84(polyA.cf(0) / polyB.cf(0))
                Dim den As Polynomial84 = New Polynomial84(1.0)
                ReDim num.exp(0), num.exp(0)(varsAB.Length - 1)
                ReDim den.exp(0), den.exp(0)(varsAB.Length - 1)
                If varsAB.Length Then
                    ReDim num.var1(varsAB.Length - 1), den.var1(varsAB.Length - 1)
                    Array.Copy(varsAB, num.var1, varsAB.Length)
                    Array.Copy(varsAB, den.var1, varsAB.Length)
                End If
                Dim expAB(varsAB.Length - 1) As Int32
                If polyA.exp IsNot Nothing Then
                    For i As Int32 = 0 To polyA.exp.Length - 1
                        If polyA.exp(i) IsNot Nothing Then
                            For j As Int32 = 0 To polyA.exp(i).Length - 1
                                Dim exp As Int32 = polyA.exp(i)(j)
                                If exp Then
                                    Dim pos As Int32 = Array.IndexOf(varsAB, polyA.var(j))
                                    expAB(pos) += exp
                                End If
                            Next
                        End If
                    Next
                End If
                If polyB.exp IsNot Nothing Then
                    For i As Int32 = 0 To polyB.exp.Length - 1
                        If polyB.exp(i) IsNot Nothing Then
                            For j As Int32 = 0 To polyB.exp(i).Length - 1
                                Dim exp As Int32 = polyB.exp(i)(j)
                                If exp Then
                                    Dim pos As Int32 = Array.IndexOf(varsAB, polyB.var(j))
                                    expAB(pos) -= exp
                                End If
                            Next
                        End If
                    Next
                End If
                polyC.PolyResto = New Polynomial84(num.cf(0))
                polyC.PolyDivisor = New Polynomial84(1.0)
                Dim bNotCjo As Boolean = False
                For i As Int32 = 0 To expAB.Length - 1
                    If expAB(i) > 0 Then
                        Dim pI As Polynomial84 = Polynomial84.GetPolyomial(varsAB(i))
                        pI.exp(0)(0) = expAB(i)
                        polyC.PolyResto *= pI
                        bNotCjo = True
                    ElseIf expAB(i) < 0 Then
                        Dim pI As Polynomial84 = Polynomial84.GetPolyomial(varsAB(i))
                        pI.exp(0)(0) = -expAB(i)
                        polyC.PolyDivisor *= pI
                        bNotCjo = True
                    End If
                Next
                If Not bNotCjo Then
                    polyC = New Polynomial84(num.cf(0))
                End If
                GoTo SinReducir
                'Exit Try
            ElseIf polyB.PolyResto Is Nothing AndAlso
            polyB.cf.Length = 1 AndAlso
            polyA.PolyResto Is Nothing Then
                polyC = New Polynomial84(0.0)
                For i As Int32 = 0 To polyA.cf.Length - 1
                    Dim polyTerm As Polynomial84 = polyA.GetPolyFromTerm(i, Nothing)
                    If polyTerm IsNot Nothing Then
                        polyC += polyTerm / polyB
                    End If
                Next
                If polyA.PolyResto IsNot Nothing Then
                    polyC.PolyResto = New Polynomial84(polyA.PolyResto)
                    polyC.PolyDivisor = mult(polyA.PolyDivisor, polyB)
                End If
                polyC.sortVar()
                Exit Try
            End If
            If polyA.PolyResto IsNot Nothing AndAlso polyB.PolyResto IsNot Nothing Then
                Dim a As New Polynomial84(polyA)
                a.PolyResto = Nothing
                Dim b As New Polynomial84(polyA.PolyResto)
                Dim c As New Polynomial84(polyA.PolyDivisor)

                Dim d As New Polynomial84(polyB)
                d.PolyResto = Nothing
                d.PolyDivisor = Nothing
                Dim e As New Polynomial84(polyB.PolyResto)
                Dim f As New Polynomial84(polyB.PolyDivisor)

                ' (a+b/c) / (d+e/f) = ((ac+b)/c) / ((df+e)/f) = f(ac+b)/(c(df+e))
                'polyC = f * (a * c + b) / (c * (d * f + e))
                polyC = New Polynomial84(0.0)
                polyC.PolyResto = f * (a * c + b)
                polyC.PolyDivisor = (c * (d * f + e))
                'polyC.opReduceCommonExponents()
                'Return polyC
            ElseIf polyA.PolyResto IsNot Nothing Then
                Dim a As New Polynomial84(polyA)
                a.PolyResto = Nothing
                Dim b As New Polynomial84(polyA.PolyResto)
                Dim c As New Polynomial84(polyA.PolyDivisor)

                Dim d As New Polynomial84(polyB)
                d.PolyResto = Nothing
                d.PolyDivisor = Nothing

                ' (a+b/c)/d = (a+b/c)/(d/1) = ((ac+b)/c)/(d/1) = (ac+b)/(cd)
                'polyC = (a * c + b) / (c * d)
                polyC = New Polynomial84(0.0)
                polyC.PolyResto = a * c + b
                polyC.PolyDivisor = c * d
                'Return polyC
            ElseIf polyB.PolyResto IsNot Nothing Then
                Dim a As New Polynomial84(polyA)

                Dim d As New Polynomial84(polyB)
                d.PolyResto = Nothing
                d.PolyDivisor = Nothing
                Dim e As New Polynomial84(polyB.PolyResto)
                Dim f As New Polynomial84(polyB.PolyDivisor)

                ' a/(d+e/f) = (a/1)/((df+e)/f) = af/(df+e)
                polyC = New Polynomial84(0.0)
                polyC.PolyResto = a * f
                polyC.PolyDivisor = d * f + e
                'Return polyC
            Else
                Dim R As New Polynomial84(polyA) ' Reminder
                Dim P2 As Polynomial84 = Nothing

                Dim polyB2 As New Polynomial84(polyB)
                Dim Quotient As New Polynomial84(0.0)
                Dim cf_Rn As Complex84 = Nothing
                Dim cf_Bn As Complex84 = Nothing
                Dim maxVarDegreeR, maxVarDegreeB As Int32, degreeP1 As Int32 = 10000
                Dim cfg As Config84 = Nothing
                If polyA.cfg IsNot Nothing AndAlso
                polyA.cfg.bDetail Then
                    cfg = polyA.cfg
                End If
                If polyA.getDegree < polyB.getDegree Then
                    polyC.PolyResto = New Polynomial84(polyA)
                    polyC.PolyDivisor = New Polynomial84(polyB)
                    Exit Try
                End If
                If varsAB.Length = 1 Then
                    Dim polyA_An As Complex84 = New Complex84(polyA.An)
                    Dim polyB_Bn As Complex84 = New Complex84(polyB.An)
                    Dim AB_An As Complex84 = polyA_An / polyB_Bn ' 2010/10/25
                    Dim pA As Polynomial84 = Polynomial84.opNormalize(polyA)
                    Dim pB As Polynomial84 = Polynomial84.opNormalize(polyB)

                    Dim AmnB As Polynomial84 = pA - pB
                    If AmnB.isReal AndAlso AmnB.ToDouble = 0.0 Then
                        polyC = New Polynomial84(AB_An)
                        Exit Try
                    End If
                    Dim curterm As Detall84.addDivTerm = Detall84.addDivTerm.first
                    Dim nVueltas As Int32 = 0
                    Do
                        maxVarDegreeR = R.opDegree1var(cf_Rn)
                        maxVarDegreeB = polyB2.opDegree1var(cf_Bn)
                        If maxVarDegreeR + 1 <= maxVarDegreeB OrElse
                        maxVarDegreeB * maxVarDegreeR = 0 Then
                            Exit Do
                        End If
                        ' 2) P1 = cf_Rn * x^(num_degree - den_degree)
                        Dim P1 As New Polynomial84(cf_Rn / cf_Bn)
                        Dim xN_m As Polynomial84 = Polynomial84.GetPolyomial(polyB2.var1(0))
                        xN_m.exp(0)(0) = maxVarDegreeR - maxVarDegreeB
                        P1 *= xN_m
                        ' 3) Quotient = Quotient + P1
                        Quotient += P1
                        ' 4) P2 = P1 * denominator
                        Dim P1_by_polyB2 As Polynomial84 = P1 * polyB2
                        Dim sum As Polynomial84 = Nothing
                        Dim dif As Polynomial84 = Nothing
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



                        Dim RmnP2 As Polynomial84 = R - P2
                        Dim degRmn2 As Int32 = RmnP2.getDegree
                        Dim degR As Int32 = R.getDegree
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
                            If cfg IsNot Nothing AndAlso cfg.bDetail Then
                                cfg.oDetail.addDivisionRTF(
                                   cfg, R,
                                    polyB, Quotient, P1, P2, varsAB, Detall84.addDivTerm.last)
                            End If
                            R = RmnP2
                            Exit Do
                        End If
                        If cfg IsNot Nothing AndAlso cfg.bDetail Then
                            cfg.oDetail.addDivisionRTF(
                               cfg, R,
                                polyB, Quotient, P1, P2, varsAB, curterm)
                            curterm = Detall84.addDivTerm.other
                        End If
                        R = RmnP2

                        ' 5) R = R - P2
                        'R -= P2
                        nVueltas += 1
                    Loop While nVueltas < 1000
                    polyC = Quotient
                    R.cfg = polyA.cfg
                    polyC.cfg = polyA.cfg
                    polyB2.cfg = polyA.cfg
                    If R.cf.Length = 0 OrElse (R.cf(0).esCero AndAlso (R.exp.Length = 0 OrElse
                    (R.exp.Length = 1 AndAlso
                    R.exp(0).Length = 1 AndAlso
                    R.exp(0)(0) = 0))) Then
                        polyC.PolyResto = Nothing
                        polyC.PolyDivisor = Nothing
                    Else
                        polyC.PolyResto = R
                        polyC.PolyDivisor = New Polynomial84(polyB2)
                        Dim e1 As String = ""
                        If cfg IsNot Nothing AndAlso
                        cfg.bDetail Then
                            e1 = polyC.toStringPoly(cfg)
                        End If
                        polyC.opReduceCommonExponents() ' 20120612 añadido
                        If cfg IsNot Nothing AndAlso cfg.bDetail Then
                            Dim e2 As String = polyC.toStringPoly(cfg)
                            If e1 <> e2 Then
                                cfg.oDetail.Add(e1 + " =")
                                cfg.oDetail.Add("= " + e2)
                            End If
                        End If
                    End If
                    Exit Try
                Else
                    R.sortVar()
                    polyB2.sortVar()
                    Dim unionVars() As String = varUnion(R, polyB2)
                    Dim oSortB As Polynomial84Sort = Nothing
                    Dim oSortR As Polynomial84Sort = Nothing
                    Dim oSortP2 As Polynomial84Sort = Nothing
                    Dim expsDiff(unionVars.Length - 1) As Int32
                    polyB2.opDegree(Nothing, unionVars, Nothing, False, -1, oSortB)
                    polyB2.opReduceCommonExponents()
                    maxVarDegreeB = polyB2.getDegree
                    Dim nVueltas As Int32 = 0
                    unionVars = varUnion(R, polyB2)
                    If unionVars.Length = R.var1.Length + polyB2.var1.Length Then
                        polyC = New Polynomial84(0.0)
                        polyC.PolyResto = New Polynomial84(polyA)
                        polyC.PolyDivisor = New Polynomial84(polyB2)
                        Exit Try
                    End If
                    Dim bDetail As Boolean = False
                    If polyA.cfg IsNot Nothing AndAlso
                    polyA.cfg.bDetail Then
                        bDetail = True
                    End If
                    Dim curterm As Detall84.addDivTerm = Detall84.addDivTerm.first
                    Dim polyAdegree As Int32 = polyA.getDegree
                    Do
                        unionVars = varUnion(R, polyB2)
                        If unionVars.Length = R.var1.Length + polyB2.var1.Length Then
                            Exit Do
                        End If
                        If unionVars.Length > 1 Then
                            Dim iVarMaxExp As Int32
                            R.opDegree(cf_Rn, unionVars, expsDiff, False, iVarMaxExp, oSortR)
                            maxVarDegreeR = R.getDegree
                            If maxVarDegreeR < maxVarDegreeB Then Exit Do
                            ' get a poly. out of the higher term of R:
                            Dim polyRn As Polynomial84 = R.GetPolyFromTerm(0, unionVars)
                            ' divide polyRn by each term of polyB (transformed into a poly.):
                            Dim currentPolyBTerm As Polynomial84
                            Dim i As Int32
                            Dim expsDiff2(expsDiff.Length - 1) As Int32
                            For i = 0 To polyB2.cf.Length - 1
                                currentPolyBTerm =
                                    polyB2.GetPolyFromTerm(i, unionVars)
                                If currentPolyBTerm Is Nothing Then
                                    Exit Do
                                End If
                                ' find the division (substracting exponents):
                                Dim bCurBisDivisorOfPolyRn As Boolean = True
                                Dim bNonZeroExp As Boolean = False ' 2013/08/09
                                Array.Copy(expsDiff, expsDiff2, expsDiff.Length)
                                For j As Int32 = 0 To expsDiff2.Length - 1
                                    If j < currentPolyBTerm.exp(0).Length Then
                                        expsDiff2(j) -= currentPolyBTerm.exp(0)(j)
                                        If expsDiff2(j) < 0 Then
                                            bCurBisDivisorOfPolyRn = False
                                            Exit For
                                        ElseIf currentPolyBTerm.exp(0)(j) Then
                                            bNonZeroExp = True
                                        End If
                                    End If
                                Next
                                If bCurBisDivisorOfPolyRn AndAlso
                                bNonZeroExp Then
                                    Exit For
                                End If
                            Next
                            If i >= polyB2.cf.Length Then
                                Exit Do ' found no divisor that satisfies
                            End If
                            cf_Bn = New Complex84(polyB2.cf(i))
                            ' 1) P1 = cf_Rn * x^(num_degree_max_monomial - den_degree_max_monomial)* ...
                            '  *y^(num_degree_max_monomial - den_degree_max_monomial)*z^(...
                            Dim P1 As New Polynomial84(cf_Rn / cf_Bn)
                            ReDim P1.exp(0)
                            P1.exp(0) = expsDiff2
                            P1.setVars(unionVars) '.setVars (polyB2.var)
                            Dim degreeP1b As Int32 = P1.opDegree(Nothing, unionVars, Nothing)
                            ' 2) Quotient = Quotient + P1
                            ' 3) P2 = P1 * denominator
                            Dim P1_by_polyB2 As Polynomial84 = P1 * polyB2
                            Dim sum As Polynomial84 = Nothing
                            Dim dif As Polynomial84 = Nothing
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

                            ' 4) R = R - P2
                            Dim Rb As New Polynomial84(R)
                            Rb.setVars(unionVars)
                            'Dim oldDgrR = R.opDegree(Nothing, R.var1, Nothing)
                            'Dim oldnCfR As Int32 = R.cf.Length
                            'Dim maxVarDegreeP2 As Int32 = P2.opDegree(Nothing, unionVars, Nothing, False, -1, oSortP2)
                            'If maxVarDegreeP2 > maxVarDegreeR OrElse _
                            '    oSortP2.degree > oSortR.degree Then
                            'Exit Do
                            'End If



                            Dim RmnP2 As Polynomial84 = R - P2
                            Dim degRmn2 As Int32 = RmnP2.getDegree
                            Dim degR As Int32 = R.getDegree
                            If degRmn2 < degR OrElse
                            (degRmn2 = degR AndAlso
                            RmnP2.getMaxExponent > R.getMaxExponent) Then
                                'If cfg IsNot Nothing Then
                                '    If curterm = Detall84.addDivTerm.first Then
                                '        curterm = Detall84.addDivTerm.firstAndLast
                                '    Else
                                '        curterm = Detall84.addDivTerm.last
                                '    End If
                                '    cfg.oDetail.addDivision( _
                                '       cfg, R, _
                                '        polyB, Quotient, P1, P2, varsAB, curterm)
                                'End If
                                R = RmnP2
                                Quotient += P1
                                Exit Do
                            End If
                            'If cfg IsNot Nothing Then
                            '    cfg.oDetail.addDivision( _
                            '       cfg, R, _
                            '        polyB, Quotient, P1, P2, varsAB, curterm)
                            '    curterm = Detall84.addDivTerm.other
                            'End If

                            R = RmnP2
                            Quotient += P1


                            If R.isReal AndAlso
                            R.ToDouble = 0 Then
                                Exit Do
                            End If
                            'R.setVars(unionVars)
                            If degreeP1b > degreeP1 Then
                                R = Rb : Exit Do
                            End If
                            If P2.CoefDblAreZero OrElse
                            degreeP1 < 1 Then
                                Exit Do
                            End If
                            degreeP1 = degreeP1b
                        Else
                            Dim P1 As Polynomial84 = R / polyB2
                            Quotient += P1
                            P2 = P1 * polyB2
                            R -= P2
                        End If
                        R.sortVar()
                        nVueltas += 1
                    Loop While (R.isComplex OrElse R.cf(0).esCero = False) AndAlso
                            nVueltas < 1000
                End If
                polyC = Quotient
                If R.cf.Length = 0 OrElse (R.cf(0).esCero AndAlso (R.exp.Length = 0 OrElse
                (R.exp.Length = 1 AndAlso
                R.exp(0).Length = 1 AndAlso
                R.exp(0)(0) = 0))) Then
                    polyC.PolyResto = Nothing
                    polyC.PolyDivisor = Nothing
                Else
                    polyC.PolyResto = R
                    polyC.PolyDivisor = New Polynomial84(polyB2)
                End If
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Dim bVerify As Boolean = False
        If bVerify Then
            Try
                If polyC.PolyResto Is Nothing Then
                    Dim polyD As Polynomial84 = polyC * polyB - polyA
                    If polyD.isComplex = False OrElse polyD.cf(0).esCero = False Then
                        Stop
                    End If
                ElseIf polyA.PolyResto IsNot Nothing AndAlso polyB.PolyResto IsNot Nothing Then
                    ' if (int(polyC) + n/d)*(int(polyB) + n2/d2) - polyA <> 0 -- > error
                    ' (c + n/d)*(b+n2/d2)= b*c + b*n/d + c*n2/d2 = a + na/da
                    Dim a As New Polynomial84(polyA)
                    a.PolyResto = Nothing
                    Dim na As New Polynomial84(polyA.PolyResto)
                    Dim da As New Polynomial84(polyA.PolyDivisor)
                    Dim b As New Polynomial84(polyB)
                    b.PolyResto = Nothing
                    Dim n2 As New Polynomial84(polyC.PolyResto)
                    Dim d2 As New Polynomial84(polyC.PolyDivisor)
                    Dim c As New Polynomial84(polyC)
                    c.PolyResto = Nothing
                    Dim n As New Polynomial84(polyC.PolyResto)
                    Dim d As New Polynomial84(polyC.PolyDivisor)
                    Dim polyD As Polynomial84 = mult(b, c) - a
                    If polyD.isComplex = False OrElse polyD.cf(0).esCero = False Then
                        Stop
                    End If
                    ' b*n/d + c*n2/d2 - na/da = 0
                    ' d * d2 = da
                    ' b*n*d2+c*n2*d =na
                    polyD = mult(d, d2) - na
                    If polyD.isComplex = False OrElse polyD.cf(0).esCero = False Then
                        Stop
                    End If
                    polyD = b * n * d2 + c * n2 * d - na
                    If polyD.isComplex = False OrElse polyD.cf(0).esCero = False Then
                        Stop
                    End If
                ElseIf polyA.PolyResto IsNot Nothing Then
                    ' (c + n/d)*b= b*c + n*b/d  = a + na/da
                    Dim a As New Polynomial84(polyA)
                    a.PolyResto = Nothing
                    Dim na As New Polynomial84(polyA.PolyResto)
                    Dim da As New Polynomial84(polyA.PolyDivisor)
                    Dim b As New Polynomial84(polyB)
                    b.PolyResto = Nothing
                    Dim c As New Polynomial84(polyC)
                    c.PolyResto = Nothing
                    Dim n As New Polynomial84(polyC.PolyResto)
                    Dim d As New Polynomial84(polyC.PolyDivisor)
                    ' b*c - a = 0
                    Dim polyD As Polynomial84 = mult(b, c) - a
                    If polyD.isComplex = False OrElse polyD.cf(0).esCero = False Then
                        Stop
                    End If
                    ' n*b - na = 0
                    polyD = mult(n, b) - na
                    If polyD.isComplex = False OrElse polyD.cf(0).esCero = False Then
                        Stop
                    End If
                    ' d - da = 0
                    polyD = d - da
                    If polyD.isComplex = False OrElse polyD.cf(0).esCero = False Then
                        Stop
                    End If
                Else
                    Dim polyD As New Polynomial84(polyC)
                    polyD.PolyResto = Nothing
                    If polyD.isComplex = True AndAlso polyD.cf(0).esCero Then
                        ' if ( n/d)*(int(polyB) + n2/d2) - polyA <> 0 -- > error
                        ' n/d *(B +n2/d2) = polyA
                        ' n*b*d2 + n2*d - polyA * d* d2 = 0
                        Dim a As New Polynomial84(polyA)
                        a.PolyResto = Nothing
                        Dim b As New Polynomial84(polyB)
                        b.PolyResto = Nothing
                        Dim n2 As New Polynomial84(polyB.PolyResto)
                        Dim d2 As New Polynomial84(polyB.PolyDivisor)
                        Dim c As New Polynomial84(polyC)
                        c.PolyResto = Nothing
                        Dim n As New Polynomial84(polyC.PolyResto)
                        Dim d As New Polynomial84(polyC.PolyDivisor)
                        polyD = n * b * d2 + n2 * d - a * d * d2
                        If polyD.isComplex = False OrElse polyD.cf(0).esCero = False Then
                            Stop
                        End If
                    Else
                        ' if (int(polyC) + n/d)*(int(polyB) + n2/d2) - polyA <> 0 -- > error
                        ' (c + n/d)*(b+n2/d2)= b*c + b*n/d + c*n2/d2 = a + 0
                        Dim a As New Polynomial84(polyA)
                        a.PolyResto = Nothing
                        Dim b As New Polynomial84(polyB)
                        b.PolyResto = Nothing
                        Dim n2 As New Polynomial84(polyB.PolyResto)
                        Dim d2 As New Polynomial84(polyB.PolyDivisor)
                        Dim c As New Polynomial84(polyC)
                        c.PolyResto = Nothing
                        Dim n As New Polynomial84(polyC.PolyResto)
                        Dim d As New Polynomial84(polyC.PolyDivisor)
                        polyD = b * n * d2 - c * n2 * d
                        If polyD.isComplex = False OrElse polyD.cf(0).esCero = False Then
                            Stop
                        End If
                        ' b*n/d + c*n2/d2 - na/da = 0
                        ' d * d2 = da
                        ' b*n*d2+c*n2*d =na
                        polyD = mult(d, d2)
                        If polyD.isComplex = False OrElse polyD.cf(0).esCero = False Then
                            Stop
                        End If
                        polyD = b * n * d2 + c * n2 * d
                        If polyD.isComplex = False OrElse polyD.cf(0).esCero = False Then
                            Stop
                        End If
                    End If
                End If
            Catch ex As Exception
                Throw ex
            End Try
        End If
        Try
            If polyC.PolyResto IsNot Nothing AndAlso
            polyC.PolyDivisor.cf.Length = 1 AndAlso
            polyC.PolyResto.cf.Length = 1 Then
                polyC.PolyDivisor.opReduceCommonExponents()
                If polyC.PolyDivisor.var.Length = 0 AndAlso
                polyC.PolyResto.var.Length = 0 Then
                    Dim Pr As New Polynomial84(
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

    Public Shared Function opGcd(ByVal pA As Polynomial84,
                             ByVal pB As Polynomial84,
                             Optional ByVal cfg As Config84 = Nothing) As Polynomial84
        Dim polyC As Polynomial84 = Nothing
        Try
            If pA.PolyResto IsNot Nothing OrElse
            pB.PolyResto IsNot Nothing Then
                Exit Try
            End If
            Dim degA As Int32 = pA.getDegree
            Dim degB As Int32 = pB.getDegree
            If degA < 1 OrElse degB < 1 Then
                Exit Try
            End If
            Dim cfAn As Complex84 = New Complex84(pA.An)
            Dim cfbn As Complex84 = New Complex84(pB.An)

            Dim polyA As Polynomial84 = Nothing
            Dim polyB As Polynomial84 = Nothing
            If degA >= degB Then
                polyA = pA
                polyB = pB
            Else
                polyA = pB
                polyB = pA
            End If
            polyA = Polynomial84.opNormalize(polyA)
            polyB = Polynomial84.opNormalize(polyB)

            Dim varA() As String = polyA.varAll
            Dim varB() As String = polyB.varAll
            If varA.Length <> 1 OrElse
            varB.Length <> 1 OrElse
            varA(0) <> varB(0) Then
                Exit Try
            End If

            ' You may find an explanation at:
            ' http://en.wikipedia.org/wiki/Greatest_common_divisor_of_two_Polynomial84s
            '
            Dim q As New Polynomial84(0.0)

            Dim r As New Polynomial84(polyA)
            Dim d As Int32 = polyB.getDegree
            Dim c As Complex84 = polyB.An
            'If cfg IsNot Nothing AndAlso _
            'cfg.bDetail Then
            '    cfg.oDetail.AddAlways(vbCrLf)
            '    cfg.oDetail.AddAlways("GCD(r,b)=GCD(" + r.toStringPoly(cfg) + "," _
            '                     + polyB.toStringPoly(cfg) + ": ")
            '    cfg.oDetail.AddAlways("Degree(r)=" + r.getDegree.ToString)
            '    cfg.oDetail.AddAlways("Degree(" + polyB.toStringPoly(cfg) + ") = " + d.ToString)
            'End If

            Dim nIter As Int32 = 1
            While r.getDegree > d
                Dim cff As New Complex84(r.An / c)
                Dim s As Polynomial84 =
                     Polynomial84.GetPolyomial(varA(0))
                s.cf(0) = cff
                s.exp(0) = New Int32() {r.getDegree - d}
                q += s
                r -= s * polyB
                'If cfg IsNot Nothing AndAlso _
                'cfg.bDetail Then
                '    cfg.oDetail.AddAlways(vbCrLf + "Iteration " + nIter.ToString + ":")
                '    cfg.oDetail.AddAlways("(r's leading term)/(b's leading term) = " + cff.toStringComplex(cfg))
                '    cfg.oDetail.AddAlways("s =" + s.toStringPoly(cfg))
                '    cfg.oDetail.AddAlways("q =" + q.toStringPoly(cfg))
                '    cfg.oDetail.AddAlways("new r =" + r.toStringPoly(cfg))
                'End If
            End While
            Dim mult As Complex84 = Nothing
            If r.IsEqual(polyB, mult) Then
                polyC = Polynomial84.opNormalize(r)
                'If cfg IsNot Nothing AndAlso _
                'cfg.bDetail Then
                '    cfg.oDetail.AddAlways("r = " + mult.toStringComplex(cfg) + "*(" + polyB.toStringPoly(cfg) + ")")
                '    cfg.oDetail.AddAlways("=> GCD(r,b) = <span style='color:red'>" + polyC.toStringPoly(cfg) + "</span>")
                'End If
            Else
                polyC = New Polynomial84(1.0)
                'If cfg IsNot Nothing AndAlso _
                'cfg.bDetail Then
                '    cfg.oDetail.AddAlways("r isn't multiple of b")
                '    cfg.oDetail.AddAlways("=> GCD(r,b) = <span style='color:red'>1</span>")
                'End If
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return polyC
    End Function

    Shared Function varUnion(ByVal polyA As Polynomial84, ByVal polyB As Polynomial84) As String()
        Dim varU(-1) As String
        Dim iU As Int32 = polyA.var1.Length
        Try
            If polyA.var.Length + polyB.var.Length = 0 Then
                Return varU
            End If
            Dim i As Int32
            If polyA.var1.Length Then
                ReDim varU(iU - 1)
                Array.Copy(polyA.var1, varU, iU)
                For i = 0 To polyB.var1.Length - 1
                    ' sólo copiar las variables de polyB
                    ' no presentes en polyA:
                    Dim pos As Int32 = Array.IndexOf(varU, polyB.var1(i), 0, iU)
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
    Public Shared Function opNormalize(ByVal polyA As Polynomial84) As Polynomial84
        Dim polyC As New Polynomial84(polyA)
        Try
            Dim i As Int32
            Dim cf_N As Complex84 = Nothing
            polyC.opDegree(cf_N, polyC.var1, Nothing) ' cf_N returns with the coefficient (of the highest monomial)
            If cf_N.IsZero Then
                Return New Polynomial84(Complex84.zero)
            End If
            If cf_N.pRe.ToDouble <> 1.0 OrElse cf_N.pIm.ToDouble <> 0.0 Then ' if coef=1.0 --> polyC is already normalized
                For i = 0 To polyC.cf.Length - 1
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
            Dim Pa As Polynomial84 = Me
            If Pa.exp.Length Then
                ' ver si tiene 
                ' exponentes negativos:
                Dim exp1 As Int32 = Pa.exp(Pa.exp.Length - 1)(0)
                If exp1 < 0 Then
                    For j As Int32 = 0 To Pa.exp.Length - 1
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
            For i As Int32 = 0 To Me.cf.Length - 1
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
            Dim i, j As Int32
            If exp.Length <> cf.Length Then
                ReDim Preserve exp(cf.Length - 1)
            End If
            For i = 0 To exp.Length - 1
                If var1.Length AndAlso
                exp(i).Length <> var1.Length Then
                    ReDim Preserve exp(i)(var1.Length - 1)
                End If
            Next
            If exp.Length AndAlso var.Length Then
                Dim oS As New Polynomial84Sort(exp, var, cf)
                If oS.minDegree = 0 AndAlso oS.degree = 0 Then
                    ReDim var1(-1)
                End If
            End If
            i = 0
            Do
Repite:
                Dim bReduce As Boolean = IIf(
                cf.Length AndAlso cf(i).esCero, True, False)
                ' if coeff. is zero, reduce
                If Not bReduce AndAlso i + 1 < exp.Length Then
                    bReduce = True
                    For j = 0 To exp(i).Length - 1
                        Dim expA As Int32 = exp(i)(j)
                        Dim expB As Int32 = 0
                        If j < exp(i + 1).Length Then
                            expB = exp(i + 1)(j)
                        End If
                        If expA <> expB Then
                            bReduce = False : Exit For
                        End If
                    Next
                End If
                If bReduce Then
                    If i + 1 < cf.Length Then
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
                        exp(i) = New Int32() {0}
                    End If
                    If i + 1 < exp.Length Then
                        GoTo Repite
                    End If
                End If
                i += 1
            Loop While i < exp.Length
            If Me.dbModuloInOperations Then
                For i = 0 To cf.Length - 1
                    cf(i).pRe = New Precis84(cf(i).pRe.ToDouble Mod Me.dbModuloInOperations)
                Next
            End If

            If PolyResto IsNot Nothing Then
                If var.Length = 0 AndAlso
                varAll.Length Then
                    var1 = varAll
                End If
                If var1.Length = 1 Then
                    Dim gcd As Polynomial84 = opGcd(PolyResto, PolyDivisor)
                    If gcd IsNot Nothing AndAlso Not gcd.isReal Then
                        'If cfg IsNot Nothing AndAlso _
                        'cfg.bDetail Then
                        '    cfg.oDetail.Add(Me.toStringPoly(cfg) + " =")
                        '    Dim entP As New Polynomial84(Me)
                        '    entP.PolyResto = Nothing
                        '    entP.PolyDivisor = Nothing
                        '    Dim resto As New Polynomial84(PolyResto)
                        '    Dim div As New Polynomial84(PolyDivisor)
                        '    cfg.oDetail.Add("=" + entP.toStringPoly(cfg) + "+[(" + _
                        '        resto.toStringPoly(cfg) + ")/(" + gcd.toStringPoly(cfg) + ")] / [(" + _
                        '        div.toStringPoly(cfg) + ")/(" + gcd.toStringPoly(cfg) + ")]")
                        'End If
                        PolyResto /= gcd
                        PolyDivisor /= gcd
                    End If
                End If
            End If


            If PolyResto IsNot Nothing Then
                Dim degR As Int32 = PolyResto.getDegree
                If degR > 0 AndAlso degR < 9 Then
                    Dim A As New Polynomial84(Me)
                    A.PolyResto = Nothing
                    A.PolyDivisor = Nothing
                    A = A * PolyDivisor + PolyResto
                    If A.getDegree < PolyResto.getDegree Then
                        Dim polyC As New Polynomial84(0.0)
                        polyC.PolyDivisor = New Polynomial84(PolyDivisor)
                        polyC.PolyResto = A
                        copyToThis(polyC)
                    End If
                    Dim mult As Complex84 = Nothing
                    Dim numeratorDeg As Int32 =
                        getDegree() + PolyDivisor.getDegree
                    If numeratorDeg = PolyDivisor.getDegree Then
                        ' for ex.: 3+2(x+1)/(x+1) --> deg(3*(x+1))=deg(x+1)
                        Dim polyC As New Polynomial84(Me)
                        polyC.PolyResto = Nothing
                        polyC.PolyDivisor = Nothing
                        polyC = polyC * PolyDivisor + PolyResto
                        If polyC.isReal AndAlso polyC.ToDouble = 0.0 Then
                            ' for ex.: me=0+2*(x+1)/(x+1) then polyC=0*(x+1)+2=0+2
                            ' not included in next 'ElseIf'
                            ' because polyC.IsEqual(PolyDivisor,mult)
                            ' would fail
                            copyToThis(polyC)
                            'ElseIf polyC.IsEqual(PolyDivisor, mult) Then
                            '    ' for ex.: 3+2*(x+1)/(x+1)-->polyC=3+2=5
                            '    PolyResto = Nothing
                            '    PolyDivisor = Nothing
                            '    polyC = New Polynomial84(mult)
                            '    copyToThis(polyC)
                        End If
                    End If
                End If
            End If
            If PolyResto IsNot Nothing Then
                If PolyResto.isReal AndAlso
                PolyResto.ToDouble = 0 Then
                    PolyResto = Nothing
                    PolyDivisor = Nothing
                ElseIf PolyDivisor.isComplex AndAlso PolyDivisor.cf(0).pIm.ToDouble = 0.0 AndAlso
                    PolyDivisor.cf(0).pRe.ToDouble = 1.0 Then
                    Dim polyC As New Polynomial84(PolyResto)
                    copyToThis(polyC)
                    PolyResto = Nothing : PolyDivisor = Nothing
                Else
                    ' Comentado 20120613:
                    'Dim polyC As Polynomial84 = PolyResto / PolyDivisor
                    'If polyC.isComplex OrElse polyC.cf(0).esCero = False Then
                    '    Dim polyD As New Polynomial84(Me)
                    '    polyD.PolyResto = polyC.PolyResto
                    '    polyD.PolyDivisor = polyC.PolyDivisor
                    '    polyD += polyC
                    '    copyToThis(polyD)
                    'Else
                    'End If
                End If
            End If
            'If PolyResto IsNot Nothing AndAlso _
            '        PolyResto.cf.Length = 1 AndAlso _
            '        PolyResto.exp.Length AndAlso _
            '        PolyResto.exp(0).Length = 1 AndAlso _
            '        PolyResto.var.Length = 1 AndAlso _
            '        PolyResto.var.Length = PolyDivisor.var.Length AndAlso _
            '        PolyResto.var(0) = PolyDivisor.var(0) Then
            '    ' reducir en num. y den. el exponente de la 1ª variable
            '    ' por ej.: x2/(x3+x) --> x/(x2+1)
            '    Dim expNum As Int32 = PolyResto.exp(0)(0)
            '    If expNum > 0 Then
            '        Dim min As Int32 = Int32.MaxValue
            '        For i = 0 To PolyDivisor.exp.Length - 1
            '            If PolyDivisor.exp(i)(0) < min Then
            '                min = PolyDivisor.exp(i)(0)
            '            End If
            '        Next
            '        If expNum > 0 AndAlso min > 0 Then
            '            For i = 0 To PolyDivisor.exp.Length - 1
            '                PolyDivisor.exp(i)(0) -= min
            '            Next
            '            PolyResto.exp(0)(0) -= min
            '        End If
            '    End If

            'End If
            If PolyResto IsNot Nothing Then
                ' reducir exponentes comunes al numerador
                ' y denominador, e.g. (x3y3+xy2)/(xy) --> x2y2+y
                Dim vVarsNum() As String = PolyResto.var
                Dim vVarsDen() As String = PolyDivisor.var
                If vVarsNum.Length AndAlso vVarsDen.Length Then
                    Dim vMinExpN(vVarsNum.Length - 1) As Int32
                    For i = 0 To vMinExpN.Length - 1
                        vMinExpN(i) = Int32.MaxValue
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
                    Dim vMinExpD(vVarsDen.Length - 1) As Int32
                    For i = 0 To vMinExpD.Length - 1
                        vMinExpD(i) = Int32.MaxValue
                    Next
                    Dim vVarsUnion(-1) As String, iU As Int32 = 0
                    Dim vMinExpUnion(-1) As Int32
                    Dim vPosNum(-1) As Int32
                    Dim vPosDen(-1) As Int32
                    With PolyDivisor
                        For i = 0 To .exp.Length - 1
                            For j = 0 To .exp(i).Length - 1
                                If j < vVarsDen.Length Then
                                    Dim pos As Int32 =
                                        Array.IndexOf(vVarsNum, vVarsDen(j))
                                    If pos > -1 Then
                                        Dim bIncr As Boolean = False
                                        Dim iU2 As Int32 = Array.IndexOf(vPosDen, j)
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
                        Dim pos As Int32 = vPosNum(iU)
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
                    Next
                End If
            End If
            If PolyResto IsNot Nothing Then
                Dim An As New Complex84(PolyDivisor.An)
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
                cf = New Complex84() {New Complex84(0.0)}
            ElseIf dbMod >= 1 Then
                For i = 0 To cf.Length - 1
                    cf(i).pRe = New Precis84(cf(i).pRe.ToDouble Mod dbMod)
                Next
            End If
            'If var.Length AndAlso _
            'Array.IndexOf(var, MathGlobal8.integralCnst) > -1 Then
            '    Dim pos As Int32 = Array.IndexOf(var, MathGlobal8.integralCnst)
            '    For i = 0 To cf.Length - 1
            '        If exp(i).Length > pos AndAlso _
            '        exp(i)(pos) Then
            '            For j = 0 To exp(i).Length - 1
            '                If j <> pos Then
            '                    exp(i)(j) = 0
            '                Else
            '                    exp(i)(j) = 1
            '                End If
            '            Next
            '            cf(i) = New Complex84(1.0)
            '        End If
            '    Next
            'End If
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
            Dim i, j As Int32
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
            Dim exp2(exp.Length - 1)() As Int32
            For i = 0 To exp.Length - 1
                If exp(i) IsNot Nothing Then
                    ReDim Preserve exp2(i)(exp(i).Length - 1)
                    For j = 0 To exp(i).Length - 1
                        If exp(i)(j) Then
                            Dim sVarOld As String = vvarCpy(j)
                            Dim posNewVar As Int32 = Array.IndexOf(
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
    Public Shared Operator ^(ByVal polyA As Polynomial84, ByVal polyB As Polynomial84) As Polynomial84
        Dim polyC As Polynomial84 = Nothing
        If polyA.var1.Length + polyB.var1.Length = 0 Then
            If polyA.PolyResto Is Nothing AndAlso
            polyB.PolyResto Is Nothing Then
                polyC = New Polynomial84(polyA.cf(0) ^ polyB.cf(0))
                Return polyC
            End If
        End If
        Try
            polyC = New Polynomial84(polyA)
            If polyB.var1.Length = 0 AndAlso
            (polyB.PolyResto Is Nothing OrElse
             polyB.var1.Length = 0) Then
                Dim cf As Complex84 = polyB.cf(0)
                If cf.pIm.IsZero Then
                    If cf.pRe.ToDouble = Math.Floor(cf.pRe.ToDouble) Then
                        Dim i As Int32
                        Dim expB As Int32 = cf.pRe.ToDouble
                        Dim Pa As Polynomial84
                        If expB > 0 Then
                            Pa = New Polynomial84(polyA)
                        Else
                            Pa = New Polynomial84(1.0) / polyA
                        End If
                        Dim exp As Int32 = Math.Abs(expB)
                        If exp > 0 Then
                            If Pa.isReal AndAlso Pa.ToDouble = 0.0 Then
                                polyC = New Polynomial84(0.0) ' 0^exp = 0, if exp>0
                                Exit Try
                            End If
                            Dim log2 As Int32 = Math.Floor(Math.Log10(exp) / Math.Log10(2.0))
                            Dim vPolyC(log2) As Polynomial84
                            Dim vExp(log2) As Int32
                            Dim curExp As Int32 = 2
                            vPolyC(0) = New Polynomial84(Pa)
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
                            polyC = New Polynomial84(1.0) ' x^0 =1 or 0^0=1
                        End If
                    Else
                        'Throw New Exception(msg884.msg(1009)) ' Polynomial84 exponent non integer: n/a
                    End If
                Else
                    'Throw New Exception(msg884.msg(1008)) ' Polynomial84 exponent is imaginary: n/a
                End If
            Else
                'Throw New Exception(msg884.msg(1010)) ' Polynomial84 exponent is a Polynomial84: n/a
            End If
            polyC.opReduceCommonExponents()
        Catch ex As Exception
            Throw ex
        End Try
        Return polyC
    End Operator

    Public Function opDerivative(ByVal respVar As String) As Polynomial84
        Dim polyC As New Polynomial84(Me)
        Try
            Dim i As Int32
            Dim nVar As Int32 = Array.IndexOf(var, respVar)
            If nVar > -1 Then
                For i = 0 To Me.cf.Length - 1
                    polyC.cf(i) *= Me.exp(i)(nVar)
                    polyC.exp(i)(nVar) -= 1
                    If polyC.exp(i)(nVar) < 0 Then
                        polyC.exp(i)(nVar) = 0
                        polyC.cf(i) = New Complex84(0.0)
                    End If
                Next
            Else
                If PolyResto Is Nothing Then
                    Return New Polynomial84(0.0)
                End If
            End If
            'If polyC.PolyResto IsNot Nothing Then
            '    Dim exprR As New Expression(polyC.PolyResto)
            '    Dim exprD As New Expression(polyC.PolyDivisor)
            '    polyC.PolyResto = Nothing
            '    polyC.PolyDivisor = Nothing
            '    Dim deriv As Expression = (exprR.opDeriv(respVar) * exprD -
            '            exprR * exprD.opDeriv(respVar)) / _
            '                   (exprD * exprD)
            '    polyC += deriv.getPolynomial84
            'End If
            polyC.opReduceCommonExponents()
        Catch ex As Exception

        End Try
        Return polyC
    End Function
    'Public Shared Function opOrthog(ByVal Ma As Matrix) As Complex84
    '    Dim ret As Complex84 = Nothing
    '    Try
    '        Dim A As Complex84 = Ma.vVect(0).vPoly(0).cf(0)
    '        Dim B As Complex84 = Ma.vVect(0).vPoly(1).cf(0)
    '        Dim Pproduct As Polynomial84 = Ma.vVect(0).vPoly(2) * Ma.vVect(0).vPoly(3)
    '        Dim Pintegral As Polynomial84 = Pproduct.opIntegral
    '        Dim pA As Complex84 = Pintegral.evalCjo(A)
    '        Dim pB As Complex84 = Pintegral.evalCjo(B)
    '        ret = pB - pA
    '    Catch ex As Exception
    '        Throw ex
    '    End Try
    '    Return ret
    'End Function
    Public Function evalMulti(ByVal Xn() As Complex84) As Complex84
        Dim retCjo As New Complex84(0.0)
        Try
            Dim i, j As Int32
            For i = 0 To cf.Length - 1
                Dim cur As New Complex84(cf(i))
                If Not cur.esCero AndAlso
                i < exp.Length AndAlso exp(i) IsNot Nothing Then
                    For j = 0 To exp(i).Length - 1
                        If exp(i)(j) Then
                            cur *= Xn(j) ^ New Complex84(exp(i)(j))
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
    'Public Function evalMultiCjoToPolynomial84(ByVal oVars As VarsAndFns) As Polynomial84
    '    Dim polyC As New Polynomial84(Me)
    '    Try
    '        Dim i, j As Int32
    '        For i = 0 To cf.Length - 1
    '            Dim cur As New Complex84(cf(i))
    '            If Not cur.esCero AndAlso _
    '            i < exp.Length AndAlso exp(i) IsNot Nothing Then
    '                For j = 0 To exp(i).Length - 1
    '                    If exp(i)(j) Then
    '                        Dim eM As exprMatrix84 = _
    '                            oVars.getValueByName(var(j), False)
    '                        If eM IsNot Nothing AndAlso eM.IsComplex Then
    '                            Dim Xnj As Complex84 = eM.getExpr(0, 0).getPolynomial84.cf(0)
    '                            cur *= Xnj ^ New Complex84(exp(i)(j))
    '                            polyC.exp(i)(j) = 0
    '                        End If
    '                    End If
    '                Next
    '                polyC.cf(i) = cur
    '            End If
    '        Next
    '        If polyC.PolyResto IsNot Nothing Then
    '            polyC.PolyResto = polyC.PolyResto.evalMultiCjoToPolynomial84(oVars)
    '            polyC.PolyDivisor = polyC.PolyDivisor.evalMultiCjoToPolynomial84(oVars)
    '        End If
    '        polyC.varReduce()
    '        polyC.opReduceCommonExponents()
    '    Catch ex As Exception
    '        Throw ex
    '    End Try
    '    Return polyC
    'End Function

    'Public Function evalMultiCjoToExpr(ByVal oVars As VarsAndFns) As Expression
    '    Dim retExpr As Expression = Nothing
    '    Try
    '        If Me.isReal Then
    '            Return New Expression(Me)
    '        End If
    '        Dim i, j As Int32
    '        For i = 0 To cf.Length - 1
    '            Dim curExpr As New Expression(cf(i))
    '            For j = 0 To exp(i).Length - 1
    '                Dim exp1 As Int32 = exp(i)(j)
    '                If exp1 Then
    '                    Dim mtx As exprMatrix84 = oVars.getValueByName(var(j), False)
    '                    If mtx Is Nothing Then
    '                        Dim eVar As New Expression( _
    '                            Polynomial84.GetPolyomial(var(j)))
    '                        If exp1 = 1 Then
    '                            curExpr = Expression.exprOp("*", curExpr, _
    '                                        eVar)
    '                        Else
    '                            curExpr = Expression.exprOp("*", curExpr, _
    '                                Expression.exprOp("^", eVar, _
    '                                New Expression(exp1)))
    '                        End If
    '                    Else
    '                        If exp1 = 1 Then
    '                            curExpr = Expression.exprOp("*", curExpr, _
    '                                                    mtx.getExpr(0, 0))
    '                        Else
    '                            curExpr = Expression.exprOp("*", curExpr, _
    '                                Expression.exprOp("^", _
    '                                mtx.getExpr(0, 0), _
    '                                New Expression(exp1)))
    '                        End If
    '                    End If
    '                    curExpr = curExpr.reduceFactors(False)
    '                End If
    '            Next
    '            If retExpr Is Nothing Then
    '                retExpr = curExpr
    '            Else
    '                retExpr = Expression.exprOp("+", retExpr, curExpr)
    '            End If
    '        Next
    '        If PolyResto IsNot Nothing Then
    '            Dim Num As Expression = PolyResto.evalMultiCjoToExpr(oVars)
    '            Dim Den As Expression = PolyDivisor.evalMultiCjoToExpr(oVars)
    '            Dim div As Expression = Num / Den
    '            retExpr += div
    '        End If
    '    Catch ex As Exception
    '        Throw ex
    '    End Try
    '    Return retExpr
    'End Function
    Public Function evalRe(ByVal x As Double) As Double
        Dim i As Int32
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
                Dim exp1 As Int32 = exp(0)(0)
                Dim exp2 As Int32 = exp1
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
        Dim i As Int32
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
                Dim exp1 As Int32 = exp(0)(0)
                Dim exp2 As Int32 = exp1
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
    Public Function evalPrecis84(ByVal x As Precis84) As Precis84
        Dim i As Int32
        Dim prN As Precis84
        Try
            If exp.Length > 1 Then
                If exp(0)(0) < exp(1)(0) Then
                    Array.Reverse(exp)
                    Array.Reverse(cf)
                End If
            End If
            prN = New Precis84(cf(0).pRe)
            Dim exp1 As Int32 = exp(0)(0)
            Dim exp2 As Int32 = exp1
            For i = 1 To cf.Length - 1
                exp2 = exp(i)(0)
                Dim nVueltas As Int32 = 0
                Do While exp1 > exp2
                    prN *= x
                    exp1 -= 1
                    nVueltas += 1
                Loop
                prN += cf(i).pRe
            Next
            Dim nV2 As Int32 = 0
            Do While exp2 > 0
                prN *= x
                exp2 -= 1
                nV2 += 1
                If nV2 > 50 Then
                    'Throw New Exception(msg884.num(13))
                End If
            Loop
            If PolyResto IsNot Nothing Then
                prN += PolyResto.evalPrecis84(x) / PolyDivisor.evalPrecis84(x)
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return prN
    End Function

    Public Function tryEvalReDecimal(ByVal x As Double, ByRef result As Decimal) As Boolean
        Dim i As Int32
        Dim NDec As Decimal
        Dim xDec As Decimal
        Dim LMax As Int32 =
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
            Dim exp1 As Int32 = exp(0)(0)
            Dim exp2 As Int32 = exp1
            For i = 1 To cf.Length - 1
                exp2 = exp(i)(0)
                Do While exp1 > exp2
                    If NDec = 0 OrElse xDec = 0 Then
                        NDec = 0
                    Else
                        Dim L As Int32 = 2 +
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
                    Dim L As Int32 = 2 +
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
    Public Function evalCjo(ByVal x As Complex84) As Complex84

        ' Evaluate Polynomial84 'Me' at point 'x:
        Dim i As Int32
        Dim cjoN As Complex84
        Try
            Dim Pa As New Polynomial84(Me)
            If Pa.exp.Length > 1 Then
                If Pa.exp(0)(0) < Pa.exp(1)(0) Then
                    Array.Reverse(Pa.exp)
                    Array.Reverse(Pa.cf)
                End If
            End If
            cjoN = New Complex84(Pa.cf(0))
            If Pa.exp.Length AndAlso Pa.exp(0).Length Then
                Dim exp1 As Int32 = Pa.exp(0)(0)
                Dim exp2 As Int32 = exp1
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
                Dim cjoResto As Complex84 = PolyResto.evalCjo(x)
                Dim cjoDivis As Complex84 = PolyDivisor.evalCjo(x)
                cjoN += cjoResto / cjoDivis
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return cjoN
    End Function
    'Public Function evalCjo(ByVal x As Precis84) As Precis84

    '    ' Evaluate Polynomial84 'Me' at point 'x:
    '    Dim i As Int32
    '    Dim cjoN As Precis84
    '    Try
    '        Dim Pa As New Polynomial84(Me)
    '        If Pa.exp.Length > 1 Then
    '            If Pa.exp(0)(0) < Pa.exp(1)(0) Then
    '                Array.Reverse(Pa.exp)
    '                Array.Reverse(Pa.cf)
    '            End If
    '        End If
    '        cjoN = New Precis84(Pa.cf(0).pRe)
    '        If Pa.exp.Length AndAlso Pa.exp(0).Length Then
    '            Dim exp1 As Int32 = Pa.exp(0)(0)
    '            Dim exp2 As Int32 = exp1
    '            Dim cfLen As Int32 = Pa.cf.Length - 1
    '            For i = 1 To Pa.cf.Length - 1
    '                exp2 = Pa.exp(i)(0)
    '                Do While exp1 > exp2
    '                    cjoN *= x
    '                    exp1 -= 1
    '                Loop
    '                cjoN += Pa.cf(i).pRe
    '                cfLen -= 1
    '            Next
    '            Do While exp2 > 0
    '                cjoN *= x
    '                exp2 -= 1
    '            Loop
    '        End If
    '        If PolyResto IsNot Nothing Then
    '            Dim cjoResto As Precis84 = PolyResto.evalCjo(x)
    '            Dim cjoDivis As Precis84 = PolyDivisor.evalCjo(x)
    '            cjoN += cjoResto / cjoDivis
    '        End If
    '    Catch ex As Exception
    '        Throw ex
    '    End Try
    '    Return cjoN
    'End Function

    Public Sub multByPolyDivisor()
        If PolyResto Is Nothing Then
            Return
        End If
        Dim Pc As New Polynomial84(Me)
        Pc.PolyResto = Nothing
        Pc.PolyDivisor = Nothing
        Pc = Pc * Me.PolyDivisor + Me.PolyResto
        copyToThis(Pc)
    End Sub
    Public Function hasComplexCoeff() As Boolean
        Dim i As Int32
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
    Public Function isComplex() As Boolean
        Try
            If PolyResto Is Nothing Then
                For i As Int32 = 0 To cf.Length - 1
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
    Public Function getDegree() As Int32
        Dim degree As Int32 = -1
        Try
            Dim oS As New Polynomial84Sort(exp, var, cf)
            degree = oS.degree ' Pa.opDegree(Nothing, New String() {""}, New Int32() {0})
        Catch ex As Exception
        End Try
        Return degree
    End Function
    Public Function getMaxExponent() As Int32
        Dim max As Int32 = 0
        Try
            Dim i, j As Int32
            For i = 0 To exp.Length - 1
                If exp(i) IsNot Nothing Then
                    For j = 0 To exp(i).Length - 1
                        If exp(i)(j) > max Then
                            max = exp(i)(j)
                        End If
                    Next
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
    Public Sub setVars(ByVal var() As String)
        ReDim Me.var1(var.Length - 1)
        Array.Copy(var, Me.var1, var.Length)
    End Sub
    Public Sub setVar(ByVal iVar As Int32, ByVal index As Int32)
        Try
            If index < var1.Length Then
                var1(index) = iVar
            End If
        Catch ex As Exception

        End Try
    End Sub
    Private Function Grade_Of_a_coeff(Optional ByVal sum_exp_of_coeff_I As Int32 = -1) As Int32
        Dim dbSum As Int32 = 0
        Dim i As Int32
        Try
            ' add exponents of all variables for the same coefficient
            Dim j As Int32 = IIf(sum_exp_of_coeff_I = -1, cf.Length - 1, sum_exp_of_coeff_I)
            For i = 0 To exp.Length - 1
                dbSum += exp(i)(j)
            Next
        Catch ex As Exception
            Throw ex
        End Try
        Return dbSum
    End Function
    Public Shared Function GradePolyn_of_1var(ByVal Pa As Polynomial84) As Int32
        ' Supposedly Pa has been sorted previously
        Return Pa.Grade_Of_a_coeff
    End Function

    Public Shared Function opRoots(ByVal poly As Polynomial84,
                Optional ByVal bByPassFirstDegreeCondition As Boolean = False,
                Optional ByVal cfg As Config84 = Nothing) As retOpRoots84
        Dim ret1 As New retOpRoots84, iRet As Int32 = 0
        Try
            If poly Is Nothing OrElse poly.varAll.Length = 0 Then
                Return Nothing
            End If
            ReDim ret1.cjo(-1)
            Dim sVar As String = poly.varAll(0)
            If Not bByPassFirstDegreeCondition AndAlso
            poly.getDegree > maxPolyDegree Then
                Return Nothing
            End If
            Dim exp1 As Int32 = 0
            Dim Pc As Polynomial84 = New Polynomial84(poly)
            Dim pV As New Polynomial84(poly)
            If Pc.PolyResto IsNot Nothing Then
                Dim Presto As New Polynomial84(Pc.PolyResto)
                Dim Pdiv As New Polynomial84(Pc.PolyDivisor)
                Pc.PolyResto = Nothing
                Pc.PolyDivisor = Nothing
                Pc = Pdiv * Pc + Presto
                pV = Pc
            End If

            Dim Pa As Polynomial84 = pV
            If Pa.exp.Length Then
                ' ver si tiene 
                ' exponentes negativos:
                exp1 = Pa.exp(Pa.exp.Length - 1)(0)
                If exp1 < 0 Then
                    For j As Int32 = 0 To Pa.exp.Length - 1
                        ' incrementar el grado del polynomio:
                        Pa.exp(j)(0) -= exp1
                    Next
                End If
            End If
            ' Find real roots for real coeff. Polynomial84:
            Dim realRoots() As Complex84 = opRealRoots_Junque(
                    sVar,
                    pV, New Polynomial84(pV), pV.getDegree) ' find ALL real roots
            Dim degree As Int32 = pV.getDegree
            If realRoots.Length > 0 Then
                Dim den As Polynomial84 = RootsToPolynomial84(realRoots)
                Dim polyDen As Polynomial84 = den
                polyDen.setVars(New String() {sVar})
                pV /= polyDen
                pV.PolyResto = Nothing
                pV.PolyDivisor = Nothing
                ReDim ret1.cjo(realRoots.Length - 1)
                Dim j As Int32
                For j = 0 To realRoots.Length - 1
                    ret1.cjo(j) = New Complex84(realRoots(j))
                Next
                iRet = realRoots.Length
            End If
            If pV.hasComplexCoeff Then
                Dim Pb As Polynomial84 = Polynomial84.opNormalize(pV)
                Dim cjoRoots() As Complex84 = opRoots_DuranKernel(Pb)
                ReDim Preserve ret1.cjo(iRet + cjoRoots.Length - 1)
                Array.Copy(cjoRoots, 0, ret1.cjo, iRet, cjoRoots.Length)
            Else
                degree = pV.getDegree ' Vc.vPoly(i).opDegree(Nothing, Vc.vPoly(i).varAll, New Int32() {0})
                ' try to find imaginary roots of the form x = b*i 
                ' Substitutiong x = i*y, if there are real roots y=(+/-)r1 for P(y)=0 these
                ' will be purely imaginary x roots = (+/-) i * r1
                'Dim yExpr As New Expression(Vc.vPoly(i))
                'Dim y As Polynomial84 = Polynomial84.GetPolyomial("y")
                'y *= New Polynomial84(New Complex84(0, 1))
                'Dim oVar As New VarsAndFns(New Config84)
                'oVar.AddVar(sVar, New Expression(y))
                'yExpr = yExpr.evalExprToExpr(oVar)
                'Dim yPa As Polynomial84 = yExpr.getPolynomial84
                'degree = Vc.vPoly(i).getDegree
                'If imgRoots.Length > 0 Then

                '    Dim lenR1 As Int32 = realRoots.Length
                '    ReDim Preserve realRoots(lenR1 + imgRoots.Length - 1)
                '    'ReDim Preserve ret1.cjo(lenR1 + imgRoots.Length - 1)
                '    For j = 0 To imgRoots.Length - 1
                '        imgRoots(j) *= -Complex84.i
                '        realRoots(lenR1 + j) = New Complex84(imgRoots(j))
                '    Next
                '    Dim vDen As Vector = New Vector(imgRoots)
                '    Dim polyDen As Polynomial84 = vDen.RootsToPolynomial84
                '    polyDen.setVars(New String() {sVar})
                '    Vc.vPoly(i) /= polyDen
                '    Vc.vPoly(i).PolyResto = Nothing
                '    Vc.vPoly(i).PolyDivisor = Nothing
                '    ret1.cjo = realRoots
                '    iRet = realRoots.Length
                '    degree = Vc.vPoly(i).getDegree ' Vc.vPoly(i).opDegree(Nothing, Vc.vPoly(i).varAll, New Int32() {0})
                'End If
                If True OrElse Not bByPassFirstDegreeCondition Then
                    ' Find only imaginary roots:
                    'If degree > 0 AndAlso pV.cf.Length > 1 Then
                    '    Dim Pb As Polynomial84 = Polynomial84.opNormalize(Vc.vPoly(i))
                    '    imgRoots = Functions84.ImagRoots_Newton_Raphson(Pb, cfg)
                    '    If imgRoots.Length > 0 Then
                    '        Dim vDen As New Vector(imgRoots)
                    '        Dim polyDen As Polynomial84 = vDen.RootsToPolynomial84
                    '        polyDen.setVars(New String() {sVar})
                    '        Vc.vPoly(i) /= polyDen
                    '        Vc.vPoly(i).PolyResto = Nothing
                    '        Vc.vPoly(i).PolyDivisor = Nothing
                    '        ReDim Preserve ret1.cjo(iRet + imgRoots.Length - 1)
                    '        Array.Copy(imgRoots, 0, ret1.cjo, iRet, imgRoots.Length)
                    '        iRet = ret1.cjo.Length
                    '        degree = Vc.vPoly(i).getDegree
                    '    End If
                    'End If

                    If degree > 0 AndAlso pV.cf.Length > 1 _
                    AndAlso True Then ' degree < MathGlobal8.maxPolyDegree Then
                        ' Find Complex84 roots employing Durand Kernel method:
                        Dim Pb As Polynomial84 = Polynomial84.opNormalize(pV)
                        Dim cjoRoots() As Complex84 = opRoots_DuranKernel(Pb)
                        ReDim Preserve ret1.cjo(iRet + cjoRoots.Length - 1)
                        Array.Copy(cjoRoots, 0, ret1.cjo, iRet, cjoRoots.Length)
                    End If
                End If


            End If

            'ret1.mtx = New Matrix(0.0)
            'ReDim ret1.mtx.vVect(0)
            'ret1.mtx.vVect(0) = New Vector
            'ReDim ret1.mtx.vVect(0).vPoly(ret1.cjo.Length - 1)
            'For i = 0 To ret1.cjo.Length - 1
            '    ret1.mtx.vVect(0).vPoly(i) = New Polynomial84(ret1.cjo(i))
            'Next
            'ret1.mtx.vVect(0) = ret1.mtx.vVect(0).sortRoots
            'ret1.mtx = Matrix.opTranspose(ret1.mtx)
            ret1.cjo = Complex84.sortRoots(ret1.cjo)
        Catch ex As Exception
            Throw New Exception(ex.Message)
        End Try
        Return ret1
    End Function
    Public Shared Function RootsToPolynomial84(realRoots() As Complex84) As Polynomial84
        Dim polyC As New Polynomial84(1.0)
        Dim i As Int32
        Try
            Dim polyX As Polynomial84 = Polynomial84.GetPolyomial("x")
            For i = 0 To realRoots.Length - 1
                polyC *= (polyX - New Polynomial84(realRoots(i)))
            Next
        Catch ex As Exception
            Throw New Exception(ex.Message)
        End Try
        Return polyC
    End Function
    Public Shared Function opRoots_DuranKernel(ByVal Pa As Polynomial84) As Complex84()
        Dim pMin() As Complex84 = Nothing
        Try
            Dim i, j As Int32
            Dim Pc2 As New Polynomial84(Pa)
            If Pc2.isComplex Then
                ReDim pMin(-1)
                Return pMin
                'Throw New Exception(msg884.msg(1011)) ' is a coeff, there are no roots
            End If
            Dim cfn As Complex84 = Nothing
            Dim expsDiff(Pa.cf.Length - 1) As Int32
            Pc2.multByPolyDivisor()
            Pc2.opReduceCommonExponents()
            If Pc2.IsMultiVar Then
                'Return New Vector(Pa)
            End If
            Dim degree As Int32 = Pc2.opDegree(cfn, Nothing, expsDiff)
            Dim vVar() As String = Pc2.varAll
            If degree = 0 OrElse vVar.Length = 0 Then
                ReDim pMin(-1)
                Return pMin
                'Throw New Exception(msg884.msg(1011)) ' is a coeff, there are no roots
            ElseIf vVar.Length > 1 Then
                ReDim pMin(-1)
                Return pMin
                'Throw New Exception(msg884.msg(1012)) ' The Polynomial84 has multiple variables: roots n/a
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

            Dim iPcCoeff As Int32 = 0
            Dim posVar As Int32 = 0

            Pc2 = Polynomial84.opNormalize(Pc2)
            Dim difMin As Double = Double.MaxValue
            Dim difMin2 As Double = Double.MaxValue
            Dim p(degree - 1), ini As Complex84
            ini = New Complex84(0.4, 0.9)
            ini.pRe = New Precis84(Math.Pow(Math.Abs(Pc2.cf(Pc2.cf.Length - 1).pRe.ToDouble), 1 / degree))
            If p.Length Mod 2 Then
                p(0) = New Complex84(1.0, 0.0)
            End If
            For i = p.Length Mod 2 To p.Length - 1
                p(i) = New Complex84(ini)
                For j = 1 + p.Length Mod 2 To i
                    p(i) = p(i) * ini
                Next
            Next
            p(0) = New Complex84(ini)
            For i = 1 To p.Length - 1
                p(i) = p(i - 1) * ini
            Next
            ReDim pMin(p.Length - 1)
            Dim curVueltas As Int32 = 0
            Dim maxNVueltas As Int32 = 45
            Dim prMax As Double = 1.0E+50
            Dim divisor As Complex84
            Do
                Dim prod As New Complex84(1, 0)
                For i = 0 To p.Length - 1
                    divisor = Complex84.one
                    For j = 0 To p.Length - 1
                        If i <> j Then
                            divisor *= (p(i) - p(j))
                        End If
                    Next
                    If p(i).opNorm < prMax Then
                        p(i) -= Pc2.evalCjo(p(i)) / divisor
                    End If
                Next
                curVueltas += 1
            Loop While curVueltas < maxNVueltas
            pMin = p
SalidaDK:
        Catch ex As Exception
            Throw ex
        End Try
        Return pMin
    End Function


    Public Shared Function opRealRoots_Junque(ByVal sVar As String,
                                              ByVal Pc As Polynomial84,
                                              ByVal PcOrig As Polynomial84,
                                              ByVal origDegree As Int32,
                                              Optional ByVal bIgnoreImgCoeff As Boolean = True) As Complex84()
        Dim ret(-1) As Complex84, nRoots As Int32 = 0
        Dim rootsDer(-1)() As Complex84
        Dim degree As Int32
        Dim rootsG(-1) As Complex84
        Dim Pa As New Polynomial84(Pc)
        Try
            Pc = Polynomial84.opNormalize(Pc)
            Pa = New Polynomial84(Pc)

            Dim Der(0) As Polynomial84
            Dim PaInIni As Complex84 = Nothing
            Dim ini, fin As Double
            Dim cjoIni As Complex84 = Nothing
            Dim left As Double = 0.0
            Dim right As Double = 0.0
            Dim i As Int32

            Dim oS As New Polynomial84Sort(Pa.exp, Pa.varAll, Pa.cf)
            degree = oS.degree
            If degree < 1 Then
                Exit Try
            End If
            Dim root() As Complex84 = {Nothing, Nothing}
            Dim exp1 As Int32 = 0
            If Pa.exp.Length Then
                ' ver si tiene raíz = 0
                exp1 = Pa.exp(Pa.exp.Length - 1)(0)
                Dim nOrigDeg As Int32 = PcOrig.getDegree
                Dim bOrigDegree As Boolean =
                    IIf(Pa.getDegree = nOrigDeg, True, False)
                Do While exp1 > 0
                    ReDim Preserve rootsG(rootsG.Length)
                    rootsG(rootsG.Length - 1) = New Complex84(0.0) ' cero es raíz
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
                    PcOrig = New Polynomial84(Pa)
                End If
            End If


            If degree = 1 AndAlso degree < 3 Then
                If degree = 0 Then
                    Exit Try
                ElseIf degree = 2 Then
                    Dim a, b, c As Complex84
                    a = Pa.cf(0)
                    If Pa.cf.Length = 2 Then
                        ' a*x^2 + c = 0
                        c = Pa.cf(1)
                        root(0) = -(-c / a) ^ Complex84.oneHalf
                        ReDim Preserve rootsG(rootsG.Length)
                        rootsG(rootsG.Length - 1) = root(0)
                        root(0) = (-c / a) ^ Complex84.oneHalf
                        ReDim Preserve rootsG(rootsG.Length)
                        rootsG(rootsG.Length - 1) = root(0)
                        Exit Try
                    Else
                        ' a*x^2 + b*x + c = 0
                        b = Pa.cf(1)
                        c = Pa.cf(2)
                        root(0) = (-b + (b * b - 4 * a * c) ^ Complex84.oneHalf) / (2.0 * a)
                        i = rootsG.Length
                        ReDim Preserve rootsG(rootsG.Length)
                        rootsG(rootsG.Length - 1) = root(0)
                        root(0) = (-b - (b * b - 4 * a * c) ^ Complex84.oneHalf) / (2.0 * a)
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
            ' Nota, "left" y "right" son las cotas inferior y superior
            ' dentro de las cuales deben estar todas las raíces, i.e.
            ' las raíces deben estar dentro del intervalo [left,right].
            '   ------
            right = Pa.maxReRoot
            left = -right
            ReDim Der(degree)
            Der(degree) = Pa
            Dim re(-1) As Double, ire As Int32 = 1
            i = degree - 1
            Dim rootsCjo(-1) As Complex84
            If i > 0 Then
                Der(i) = Der(i + 1).opDerivative(sVar)
                ' Obtenemos las raíces reales de la primera
                ' derivada de Pa:
                rootsCjo = Polynomial84.opRealRoots_Junque(
                 sVar, Der(i), PcOrig, origDegree, bIgnoreImgCoeff)
                'rootsCjo = Functions84.opRoots_DuranKernel(Der(i))
            End If
            ReDim re(ire)
            Dim j As Int32
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
            Dim re1(0) As Double, ir As Int32 = 0
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
            If Pa.getDegree = PcOrig.getDegree Then

                ' a simple try for rational roots:
                Dim mult As Double = 1.0
                For i = 0 To Pa.cf.Length - 1
                    mult *= Pa.cf(i).pRe.den
                Next
                If Not Double.IsInfinity(mult) AndAlso
                Not Double.IsInfinity(mult) Then
                    Dim num As Double = Pa.cf(Pa.cf.Length - 1).pRe.num * mult
                    Dim den As Double = Pa.cf(0).pRe.den * mult
                    For i = 1 To Math.Min(100, Math.Abs(num))
                        For j = 1 To Math.Min(100, Math.Abs(den))
rationalMultip:
                            Dim Pr As New Precis84(i, j)
                            root(0) = New Complex84(Pr, New Precis84(0.0))
                            If Pa.evalPrecis84(root(0).pRe).IsZero Then
                                ReDim Preserve rootsG(rootsG.Length)
                                rootsG(rootsG.Length - 1) = root(0)
                                Dim x As Polynomial84 = Polynomial84.GetPolyomial(Pa.var(0))
                                ' 'deflate' Pa:
                                Pa /= (x - New Polynomial84(root(0)))
                                Pa.PolyResto = Nothing
                                Pa.PolyDivisor = Nothing
                                GoTo rationalMultip
                            End If
                            If Pa.evalPrecis84(-root(0).pRe).IsZero Then
                                ReDim Preserve rootsG(rootsG.Length)
                                root(0) = -root(0)
                                rootsG(rootsG.Length - 1) = root(0)
                                Dim x As Polynomial84 = Polynomial84.GetPolyomial(Pa.var(0))
                                ' 'deflate' Pa:
                                Pa /= (x - New Polynomial84(root(0)))
                                Pa.PolyResto = Nothing
                                Pa.PolyDivisor = Nothing
                                GoTo rationalMultip
                            End If
                            'End If
                        Next
                    Next
                    If rootsG.Length = PcOrig.getDegree Then
                        Exit Try
                    End If
                End If
            End If
            Dim bIsOrig As Boolean =
                (Pa.getDegree = PcOrig.getDegree)
            For i = 0 To re.Length - 2
                ini = re(i)
                fin = re(i + 1)
                ' Para i=0, ini=re(0)=left, fin=re(1)=r2
                ' y si existe raíz en el intervalo [ini,fin]
                ' se cumplirá siempre que SIGNO(P(ini)) <> SIGNO(P(fin))
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
                root = Functions84.busquedaDicotomicaDbl(ini, fin, Pa)
                If root IsNot Nothing Then
multiplicity:
                    If bIsOrig AndAlso Not root(1).IsZero Then
                        ' added to improve accuracy, 2013/08/17:
                        Dim root2() As Complex84 =
                            Functions84.busquedaDicotomicaDbl(ini, fin, PcOrig)
                        If root2 IsNot Nothing Then
                            root = root2
                        End If
                    End If
                    ReDim Preserve rootsG(rootsG.Length)
                    rootsG(rootsG.Length - 1) = root(0)
                    Dim x As Polynomial84 = Polynomial84.GetPolyomial(Pa.var(0))
                    ' 'deflate' Pa:
                    Pa /= (x - New Polynomial84(root(0)))
                    Pa.PolyResto = Nothing
                    Pa.PolyDivisor = Nothing
                    If Pa.evalCjo(root(0)).IsZero Then
                        GoTo multiplicity
                    End If
                End If
sig:
            Next
        Catch ex As Exception
            ReDim rootsG(-1)
            Return rootsG
        End Try
        Try
            Dim i As Int32
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
    Public Shared Function opPruebaRoots(ByVal sVar As String,
                                              ByVal Pc As Polynomial84,
                                              ByVal PcOrig As Polynomial84,
                                              ByVal origDegree As Int32) As Complex84()
        Dim ret(-1) As Complex84, nRoots As Int32 = 0
        Dim rootsDer(-1)() As Complex84
        Dim degree As Int32
        Dim rootsG(-1) As Complex84
        Dim Pa As New Polynomial84(Pc)
        Try
            Pc = Polynomial84.opNormalize(Pc)
            Pa = New Polynomial84(Pc)

            Dim Der(0) As Polynomial84
            Dim PaInIni As Complex84 = Nothing
            Dim ini, fin As Double
            Dim cjoIni As Complex84 = Nothing
            Dim left As Double = 0.0
            Dim right As Double = 0.0
            Dim i As Int32

            Dim oS As New Polynomial84Sort(Pa.exp, Pa.var, Pa.cf)
            degree = oS.degree ' Pa.opDegree(Nothing, New String() {""}, New Int32() {0})
            If degree < 1 Then
                Exit Try
            End If
            Dim root() As Complex84 = {Nothing, Nothing}
            Dim exp1 As Int32 = 0
            If Pa.exp.Length Then
                ' ver si tiene raíz = 0
                exp1 = Pa.exp(Pa.exp.Length - 1)(0)
                Dim bOrigDegree As Boolean =
                    IIf(Pa.getDegree = PcOrig.getDegree, True, False)
                Do While exp1 > 0
                    ReDim Preserve rootsG(rootsG.Length)
                    rootsG(rootsG.Length - 1) = New Complex84(0.0) ' cero es raíz
                    nRoots += 1
                    exp1 -= 1
                    For i = 0 To Pa.exp.Length - 1
                        Pa.exp(i)(0) -= 1
                    Next
                Loop
                degree = Pa.getDegree
                If bOrigDegree Then
                    PcOrig = New Polynomial84(Pa)
                End If
            End If

repite:
            If Pa.hasComplexCoeff Then
                Return ret
            ElseIf degree < 3 Then
                If degree = 0 Then
                    Exit Try
                ElseIf degree = 2 Then
                    Dim a, b, c As Complex84
                    a = Pa.cf(0)
                    If Pa.cf.Length = 2 Then
                        c = Pa.cf(1)
                        root(0) = -(-c / a) ^ Complex84.oneHalf  ' ^ 0.5 2013/08/09
                        ReDim Preserve rootsG(rootsG.Length)
                        rootsG(rootsG.Length - 1) = root(0)
                        root(0) = (-c / a) ^ Complex84.oneHalf ' ^ 0.5  2013/08/09
                        ReDim Preserve rootsG(rootsG.Length)
                        rootsG(rootsG.Length - 1) = root(0)
                        Exit Try
                    Else
                        b = Pa.cf(1)
                        c = Pa.cf(2)
                        root(0) = (-b + (b * b - 4 * a * c) ^ Complex84.oneHalf) / (2.0 * a) ' 2013/08/09
                        i = rootsG.Length
                        ReDim Preserve rootsG(rootsG.Length)
                        rootsG(rootsG.Length - 1) = root(0)
                        root(0) = (-b - (b * b - 4 * a * c) ^ Complex84.oneHalf) / (2.0 * a) ' 2013/08/09
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

            ' Nota, "left" y "right" son las cotas inferior y superior
            ' dentro de las cuales deben estar todas las raíces, i.e.
            ' las raíces deben estar dentro del intervalo [left,right].
            '   ------
            right = Pa.maxReRoot
            left = -right
            ReDim Der(degree)
            Der(degree) = Pa
            Dim re(-1) As Double, ire As Int32 = 1
            i = degree - 1
            Der(i) = Der(i + 1).opDerivative(sVar)
            Dim rootsCjo() As Complex84 = Nothing
            ' Obtenemos las raíces reales de la primera
            ' derivada de Pa:
            rootsCjo = Polynomial84.opRealRoots_Junque(
             sVar, Der(i), PcOrig, origDegree)
            'rootsCjo = Functions84.opRoots_DuranKernel(Der(i))
            ReDim re(ire)
            Dim j As Int32
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
            Dim re1(0) As Double, ir As Int32 = 0
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
            If Pa.getDegree = PcOrig.getDegree Then
                ' a simple try for rational roots:
                Dim num As Double = Pa.cf(Pa.cf.Length - 1).pRe.ToDouble
                Dim den As Double = Pa.cf(0).pRe.ToDouble
                For i = 1 To Math.Min(100, Math.Abs(num))
                    For j = 1 To Math.Min(100, Math.Abs(den))
rationalMultip:
                        If num / i = Math.Floor(num / i) AndAlso
                        den / i = Math.Floor(den / i) Then
                            Dim Pr As New Precis84(i, j)
                            root(0) = New Complex84(Pr, New Precis84(0.0))
                            If Pa.evalPrecis84(root(0).pRe).IsZero Then
                                ReDim Preserve rootsG(rootsG.Length)
                                rootsG(rootsG.Length - 1) = root(0)
                                Dim x As Polynomial84 = Polynomial84.GetPolyomial(Pa.var(0))
                                ' 'deflate' Pa:
                                Pa /= (x - New Polynomial84(root(0)))
                                Pa.PolyResto = Nothing
                                Pa.PolyDivisor = Nothing
                                GoTo rationalMultip
                            ElseIf Pa.evalPrecis84(-root(0).pRe).IsZero Then
                                ReDim Preserve rootsG(rootsG.Length)
                                root(0) = -root(0)
                                rootsG(rootsG.Length - 1) = root(0)
                                Dim x As Polynomial84 = Polynomial84.GetPolyomial(Pa.var(0))
                                ' 'deflate' Pa:
                                Pa /= (x - New Polynomial84(root(0)))
                                Pa.PolyResto = Nothing
                                Pa.PolyDivisor = Nothing
                                GoTo rationalMultip
                            End If
                        End If
                    Next
                Next
                If rootsG.Length = PcOrig.getDegree Then
                    Exit Try
                End If
            End If
            For i = 0 To re.Length - 2
                ini = re(i)
                fin = re(i + 1)
                ' Para i=0, ini=re(0)=left, fin=re(1)=r2
                ' y si existe raíz en el intervalo [ini,fin]
                ' se cumplirá siempre que SIGNO(P(ini)) <> SIGNO(P(fin))
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
                root = Functions84.busquedaDicotomicaDbl(ini, fin, Pa)
                If root IsNot Nothing Then
multiplicity:
                    ReDim Preserve rootsG(rootsG.Length)
                    rootsG(rootsG.Length - 1) = root(0)
                    Dim x As Polynomial84 = Polynomial84.GetPolyomial(Pa.var(0))
                    ' 'deflate' Pa:
                    Pa /= (x - New Polynomial84(root(0)))
                    Pa.PolyResto = Nothing
                    Pa.PolyDivisor = Nothing
                    If Pa.evalCjo(root(0)).IsZero Then
                        GoTo multiplicity
                    End If
                End If
            Next
            If Pa.getDegree = PcOrig.getDegree AndAlso
            rootsG.Length < Pa.getDegree Then
                For i = 0 To re.Length - 1
                    ini = re(i)
                    Dim roots() As Complex84 =
                      opImgRootsBAD(Pa.var(0), Pa, Pa,
                      Pa.getDegree, ini)
                    For j = 0 To roots.Length - 1
                        ReDim Preserve rootsG(rootsG.Length)
                        rootsG(rootsG.Length - 1) = New Complex84(roots(j))
                    Next
                    If rootsG.Length >= PcOrig.getDegree Then
                        Return rootsG
                    End If
                Next
            End If
        Catch ex As Exception
            ReDim rootsG(-1)
            Return rootsG
        End Try
        Try
            Dim i As Int32
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

    Public Shared Function opImgRoots_b(ByVal sVar As String,
                                              ByVal Pc As Polynomial84,
                                              ByVal PcOrig As Polynomial84,
                                              ByVal origDegree As Int32) As Complex84()
        Dim ret(-1) As Complex84, nRoots As Int32 = 0
        Dim rootsDer(-1)() As Complex84
        Dim degree As Int32
        Dim rootsG(-1) As Complex84
        Dim Pa As New Polynomial84(Pc)
        Try
            Pc = Polynomial84.opNormalize(Pc)

            Dim Der(0) As Polynomial84
            Dim PaInIni As Complex84 = Nothing
            Dim ini, fin As Double
            Dim cjoIni As Complex84 = Nothing
            Dim left As Double = 0.0
            Dim right As Double = 0.0
            Dim i As Int32

            Dim oS As New Polynomial84Sort(Pa.exp, Pa.var, Pa.cf)
            degree = oS.degree ' Pa.opDegree(Nothing, New String() {""}, New Int32() {0})
            If degree < 1 Then
                Exit Try
            End If
            Dim root() As Complex84 = {Nothing, Nothing}
            Dim exp1 As Int32 = 0
            If Pa.exp.Length Then
                ' ver si tiene raíz = 0
                exp1 = Pa.exp(Pa.exp.Length - 1)(0)
                Dim bOrigDegree As Boolean =
                    IIf(Pa.getDegree = PcOrig.getDegree, True, False)
                Do While exp1 > 0
                    ReDim Preserve rootsG(rootsG.Length)
                    rootsG(rootsG.Length - 1) = New Complex84(0.0) ' cero es raíz
                    nRoots += 1
                    exp1 -= 1
                    For i = 0 To Pa.exp.Length - 1
                        Pa.exp(i)(0) -= 1
                    Next
                Loop
                degree = Pa.getDegree
                If bOrigDegree Then
                    PcOrig = New Polynomial84(Pa)
                End If
            End If
repite:
            If Pa.hasComplexCoeff Then
                Return ret
                'ElseIf Pa.cf.Length = 2 Then
                '    Dim exp As Int32 = Pa.exp(0)(0)
                '    root(0) = -Pa.cf(1) / Pa.cf(0)
                '    root(0) = root(0) ^ -exp
                '    ReDim Preserve rootsG(rootsG.Length)
                '    rootsG(rootsG.Length - 1) = root(0) ' cero es raíz
                '    If exp >= 2 Then
                '        ReDim Preserve rootsG(rootsG.Length)
                '        rootsG(rootsG.Length - 1) = -root(0) ' cero es raíz
                '    End If
                '    Exit Try
            ElseIf degree < 3 Then
                If degree = 0 Then
                    Exit Try
                ElseIf degree = 2 Then
                    Dim a, b, c As Complex84
                    a = Pa.cf(0)
                    If Pa.cf.Length = 2 Then
                        c = Pa.cf(1)
                        root(0) = -(-c / a) ^ Complex84.oneHalf  ' 2013/08/09
                        ReDim Preserve rootsG(rootsG.Length)
                        rootsG(rootsG.Length - 1) = root(0)
                        root(0) = (-c / a) ^ Complex84.oneHalf ' 2013/08/09
                        ReDim Preserve rootsG(rootsG.Length)
                        rootsG(rootsG.Length - 1) = root(0)
                        Exit Try
                    Else
                        b = Pa.cf(1)
                        c = Pa.cf(2)
                        root(0) = (-b + (b * b - 4 * a * c) ^ Complex84.oneHalf) / (2.0 * a) ' 2013/08/09
                        i = rootsG.Length
                        ReDim Preserve rootsG(rootsG.Length)
                        rootsG(rootsG.Length - 1) = root(0)
                        root(0) = (-b - (b * b - 4 * a * c) ^ Complex84.oneHalf) / (2.0 * a) ' 2013/08/09
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

            ' Nota, "left" y "right" son las cotas inferior y superior
            ' dentro de las cuales deben estar todas las raíces, i.e.
            ' las raíces deben estar dentro del intervalo [left,right].
            '   ------
            right = Pa.maxReRoot
            left = -right
            ReDim Der(degree)
            Der(degree) = Pa
            Dim re(-1) As Double, ire As Int32 = 1
            i = degree - 1
            Der(i) = Der(i + 1).opDerivative(sVar)
            Dim rootsCjo() As Complex84 = Nothing
            ' Obtenemos las raíces reales de la primera
            ' derivada de Pa:
            rootsCjo = Polynomial84.opRealRoots_Junque(
             sVar, Der(i), PcOrig, origDegree)
            'rootsCjo = Functions84.opRoots_DuranKernel(Der(i))
            ReDim re(ire)
            Dim j As Int32
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
            Dim re1(0) As Double, ir As Int32 = 0
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
            If Pa.getDegree = PcOrig.getDegree Then
                ' a simple try for rational roots:
                Dim num As Double = Pa.cf(Pa.cf.Length - 1).pRe.ToDouble
                Dim den As Double = Pa.cf(0).pRe.ToDouble
                For i = 1 To Math.Min(100, Math.Abs(num))
                    For j = 1 To Math.Min(100, Math.Abs(den))
rationalMultip:
                        If num / i = Math.Floor(num / i) AndAlso
                        den / i = Math.Floor(den / i) Then
                            Dim Pr As New Precis84(i, j)
                            root(0) = New Complex84(Pr, New Precis84(0.0))
                            If Pa.evalPrecis84(root(0).pRe).IsZero Then
                                ReDim Preserve rootsG(rootsG.Length)
                                rootsG(rootsG.Length - 1) = root(0)
                                Dim x As Polynomial84 = Polynomial84.GetPolyomial(Pa.var(0))
                                ' 'deflate' Pa:
                                Pa /= (x - New Polynomial84(root(0)))
                                Pa.PolyResto = Nothing
                                Pa.PolyDivisor = Nothing
                                GoTo rationalMultip
                            ElseIf Pa.evalPrecis84(-root(0).pRe).IsZero Then
                                ReDim Preserve rootsG(rootsG.Length)
                                root(0) = -root(0)
                                rootsG(rootsG.Length - 1) = root(0)
                                Dim x As Polynomial84 = Polynomial84.GetPolyomial(Pa.var(0))
                                ' 'deflate' Pa:
                                Pa /= (x - New Polynomial84(root(0)))
                                Pa.PolyResto = Nothing
                                Pa.PolyDivisor = Nothing
                                GoTo rationalMultip
                            End If
                        End If
                    Next
                Next
                If rootsG.Length = PcOrig.getDegree Then
                    Exit Try
                End If
            End If
            For i = 0 To re.Length - 2
                ini = re(i)
                fin = re(i + 1)
                ' Para i=0, ini=re(0)=left, fin=re(1)=r2
                ' y si existe raíz en el intervalo [ini,fin]
                ' se cumplirá siempre que SIGNO(P(ini)) <> SIGNO(P(fin))
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
                root = Functions84.busquedaDicotomicaDbl(ini, fin, Pa)
                If root IsNot Nothing Then
multiplicity:
                    ReDim Preserve rootsG(rootsG.Length)
                    rootsG(rootsG.Length - 1) = root(0)
                    Dim x As Polynomial84 = Polynomial84.GetPolyomial(Pa.var(0))
                    ' 'deflate' Pa:
                    Pa /= (x - New Polynomial84(root(0)))
                    Pa.PolyResto = Nothing
                    Pa.PolyDivisor = Nothing
                    If Pa.evalCjo(root(0)).IsZero Then
                        GoTo multiplicity
                    End If
                End If
            Next
        Catch ex As Exception
            ReDim rootsG(-1)
            Return rootsG
        End Try
        Try
            Dim i As Int32
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
    Public Shared Function opImgRootsBAD(ByVal sVar As String,
                                              ByVal Pc As Polynomial84,
                                              ByVal PcOrig As Polynomial84,
                                              ByVal origDegree As Int32,
                                              ByVal initialRe As Double) As Complex84()
        Dim ret(-1) As Complex84, nRoots As Int32 = 0
        Dim rootsG(-1) As Complex84
        Dim Pa As New Polynomial84(Pc)
        Dim degree As Int32
        Try
            Pc = Polynomial84.opNormalize(Pc)
            Dim Der(0) As Polynomial84
            Dim PaInIni As Complex84 = Nothing
            Dim cjoIni As Complex84 = Nothing
            Dim left As Double = 0.0
            Dim right As Double = 0.0
            Dim i As Int32

            Dim oS As New Polynomial84Sort(Pa.exp, Pa.var, Pa.cf)
            degree = oS.degree ' Pa.opDegree(Nothing, New String() {""}, New Int32() {0})
            If degree < 1 Then
                Exit Try
            End If
            Dim root() As Complex84 = {Nothing, Nothing}
            Dim exp1 As Int32 = 0
            If Pa.exp.Length Then
                ' ver si tiene raíz = 0
                exp1 = Pa.exp(Pa.exp.Length - 1)(0)
                Dim bOrigDegree As Boolean =
                    IIf(Pa.getDegree = PcOrig.getDegree, True, False)
                Do While exp1 > 0
                    ReDim Preserve rootsG(rootsG.Length)
                    rootsG(rootsG.Length - 1) = New Complex84(0.0) ' cero es raíz
                    nRoots += 1
                    exp1 -= 1
                    For i = 0 To Pa.exp.Length - 1
                        Pa.exp(i)(0) -= 1
                    Next
                Loop
                degree = Pa.getDegree
                If bOrigDegree Then
                    PcOrig = New Polynomial84(Pa)
                End If
            End If
repite:
            If Pa.hasComplexCoeff Then
                Return ret
            ElseIf degree < 3 Then
                If degree = 0 Then
                    Exit Try
                ElseIf degree = 2 Then
                    Dim a2, b2, c2 As Complex84
                    a2 = Pa.cf(0)
                    If Pa.cf.Length = 2 Then
                        c2 = Pa.cf(1)
                        root(0) = -(-c2 / a2) ^ Complex84.oneHalf ' 2013/08/09
                        ReDim Preserve rootsG(rootsG.Length)
                        rootsG(rootsG.Length - 1) = root(0)
                        root(0) = (-c2 / a2) ^ Complex84.oneHalf  ' 2013/08/09
                        ReDim Preserve rootsG(rootsG.Length)
                        rootsG(rootsG.Length - 1) = root(0)
                        Exit Try
                    Else
                        b2 = Pa.cf(1)
                        c2 = Pa.cf(2)
                        root(0) = (-b2 + (b2 * b2 - 4 * a2 * c2) ^ Complex84.oneHalf) / (2.0 * a2) ' 2013/08/09
                        i = rootsG.Length
                        ReDim Preserve rootsG(rootsG.Length)
                        rootsG(rootsG.Length - 1) = root(0)
                        root(0) = (-b2 - (b2 * b2 - 4 * a2 * c2) ^ Complex84.oneHalf) / (2.0 * a2) ' 2013/08/09
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
                            rootsG(i) = c2 / rootsG(i + 1)
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
                                rootsG(i) = c2 / rootsG(i - 1)
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

            Dim n As Int32 = Pc.getDegree - 2
            If n Mod 2 Then
                Exit Try
            End If
            Dim a(n + 2), b(n), c1, c0 As Complex84
            If True Then
                For i = 0 To Pa.cf.Length - 1
                    Dim exp As Int32 = Pa.exp(i)(0)
                    a(exp) = Pa.cf(i)
                Next
                For i = 0 To a.Length - 1
                    If a(i) Is Nothing Then
                        a(i) = Complex84.zero
                    End If
                Next
repite2:
                b(n) = New Complex84(1.0) ' Complex84.one
                Dim nVueltas As Int32 = 0
                Dim maxRe As Double = 1.0
                Dim Re As New Complex84(-0.5)
                Dim im As New Complex84(1.0)
                c1 = New Complex84(-2.0) * Re
                c0 = Re * Re + im * im
                Dim c_1 As New Complex84(10 ^ 6)
                Dim c_0 As New Complex84(10 ^ 6)
                Dim maxVueltas As Int32 = 300
                Dim two As New Complex84(2.0)
                Do
                    If n = 2 Then
                        Exit Try
                        ' If n=2:
                        'x2 + b(1)*x + b(0)
                        '*(x2 + c1*x + c0)
                        '------------------------
                        'x4 + b(1)*x3 + b(0)*x2
                        '       c1*x3 +c1*b(1)*x2 + c1*b(0)*x
                        '               c0*x2     + c0*b(1)*x + c0*b(0)
                        '----------------------------------------------
                        'x4+ A3*x3    + A2*x2     + A1*x      + A0
                        '
                        ' <=>
                        'A3   =          +c1		+b(1)
                        'A2   =c0       +c1*b(1)	+b(0)
                        'A1   =c0*b(1)  +c1*b(0)
                        'A0   =c0*b(0)
                        '
                        '<=>
                        'b(1)  =A3   - c1
                        'b(0)  =A2   - c1*b(1)   - c0
                        'c1    =(A1 -c0*b(1))/b(0)
                        'c0    =A0/b(0)
                        b(0) = c0 * a(0)
                        'c1 = (a(1) - c0 * b(1)) / b(0)
                        If Re.IsZero Then
                            c1 = Complex84.zero
                            im = (a(1) / b(0)) ^ Complex84.oneHalf
                        Else
                            b(1) = (a(1) - c1 * b(0)) / a(1)
                            c1 = a(3) - b(1)
                            c0 = a(2) - b(0) - c1 * b(1)
                            Re = c1 / two
                            Re.pIm = New Precis84(0.0)
                            im = (a(0) / b(0) - Re * Re) ^ Complex84.oneHalf
                            im.pIm = New Precis84(0.0)
                            'c0 = Re * Re + im * im

                            'Re = -c1 / two
                            'im = c0 - Re * Re
                            'im = im ^ Complex84.oneHalf
                            'im.pIm = New Precis84(0.0)
                            'If im.pRe.sgn = -1 Then
                            '    im *= Complex84.minusOne
                            'End If
                        End If
                    Else
                        'An+2*xn+2 +An+1*xn+1+... +A2*x2 + A1*x + A0 =
                        '= Bn * (x2 + c1*x + c0)
                        '
                        'xn + b(n-1)*xn-1 + b(n-2)*xn-2 + ... + b(2)*x2 + b(1)*x + b(0)
                        '*(x2 + c1*x + c0)
                        '--------------------------------------------------------------
                        'xn+2 + b(n-1)*xn+1 + b(n-2)*xn + ... + b(2)*x4  + b(1)*x3 + b(0)*x2
                        '       c1*xn+1     +c1*b(n-1)*xn+ ...+c1*b(3)*x4+c1*b(2)*x3 +c1*b(1)*x2 +c1*b(0)*x
                        '                     c0*xn     + ...  c0*b(4)*x4+c0*b(3)*x3 +c0*b(2)*x2 +c0*b(1)*x + c0*b(0)
                        '--------------------------------------------------------------------------------------------
                        'xn+2+ An+1*xn+1    +An*xn      +....+ A4*x4    +  A3*x3     + A2*x2     + A1*x     + A0
                        '
                        '<=>
                        '
                        'An+1 =		+c1		+b(n-1)
                        'An   =	c0	+c1*b(n-1)	+b(n-2)
                        'An-1 =c0*b(n-1) +c1*b(n-2)	+b(n-3)
                        '....
                        'Ai   =c0*b(i)   +c1*b(i-1)	+b(i-2)
                        '....
                        'A2   =c0*b(2)   +c1*b(1)	+b(0)
                        'A1   =c0*b(1)   +c1*b(0)
                        'A0   =c0*b(0)
                        '
                        '<=>
                        '
                        'b(n-1)=An+1 - c1
                        'b(n-2)=An   - c1*b(n-1)	- c0
                        '...
                        'b(i-2)=Ai   - c1*b(i-1) - c0*b(i)
                        '...
                        'b(i)  =Ai+2 - c1*b(i+1) - c0*b(i+2)
                        '...

                        'b(0)  =(A2   - c1*b(1)   - c0*b(2)
                        'c1    =(A1 -c0*b(1))/b(0) 
                        'c0    =A0/b(0)   (ec.2b)
                        '
                        '
                        ' =================================================
                        ' Si x^2+c1*x+c0 = x^2-2Re*x+(Re^2+Im^2) = (x-Re+i*Im)*(x-Re-i*Im), esto es:
                        '      c1=-2*Re , (Re es la valor real de la raíz)
                        '      c0=r^2+Im^2 , (Im es el valor imaginario de la raíz)
                        '  y, entonces, sustituimos en la ecuación (2b):
                        '
                        ' c0 = Re^2+Im^2 = A0/b(0)
                        ' Hay 2 posibles valores de Im:
                        ' Im = (-/+)sqr(A0/b0 - Re^2) (ec.(5))

                        b(n - 1) = a(n + 1) - c1
                        For i = n - 2 To 0 Step -1
                            b(i) = a(i + 2) - c1 * b(i + 1) - c0 * b(i + 2)
                        Next
                        c_1 = (a(1) - c0 * b(1)) / b(0)
                        Re = -c_1 / two
                        If Re.IsZero Then
                            c1 = Complex84.zero
                            im = (a(1) / b(0)) ^ Complex84.oneHalf
                        Else
                            im = (a(0) / b(0) - Re * Re) ^ Complex84.oneHalf
                        End If
                        c1 = c_1
                        c0 = Re * Re + im * im
                    End If
                    nVueltas += 1

                Loop While nVueltas < maxVueltas
                If nVueltas >= maxVueltas Then
                    'Exit Try
                End If
                'c0 *= maxRe
                'c1 *= maxRe
                Dim x2 As Polynomial84 = Polynomial84.GetPolyomial(Pa.var(0))
                x2 = (x2 - New Polynomial84(1.0)) ^ New Polynomial84(2.0)
                x2.cf(0) = New Complex84(c0)
                x2.cf(1) = New Complex84(c1)
                x2.cf(2) = New Complex84(1.0)
                Dim roots(1) As Complex84
                roots(0) = New Complex84(Re.pRe, im.pRe)
                roots(1) = New Complex84(Re.pRe, -im.pRe)
                ReDim Preserve rootsG(rootsG.Length)
                rootsG(rootsG.Length - 1) = roots(0)
                ReDim Preserve rootsG(rootsG.Length)
                rootsG(rootsG.Length - 1) = roots(1)
                If n = 2 Then
                    x2.cf(0) = New Complex84(b(0))
                    x2.cf(1) = New Complex84(b(1))
                    x2.cf(2) = New Complex84(1.0)
                    Re = -x2.cf(1) / 2.0
                    im = (x2.cf(0) - Re * Re) ^ Complex84.oneHalf
                    roots(0) = New Complex84(Re.pRe, im.pRe)
                    ReDim Preserve rootsG(rootsG.Length)
                    rootsG(rootsG.Length - 1) = roots(0)
                    roots(1) = New Complex84(Re.pRe, -im.pRe)
                    ReDim Preserve rootsG(rootsG.Length)
                    rootsG(rootsG.Length - 1) = roots(1)
                    Exit Try
                End If
                ReDim a(n)
                For i = 0 To n
                    a(i) = New Complex84(b(i))
                Next
                n -= 2
                GoTo repite2
            End If
        Catch ex As Exception
            ReDim rootsG(-1)
        End Try
        Return rootsG
    End Function
    Public Shared Function opMult(ByVal a() As Complex84, ByVal b() As Complex84) As Complex84()
        Dim i, j As Int32
        Dim lv As Int32 = a.Length - 1, lvb As Int32 = b.Length - 1
        Dim r(lv + lvb) As Complex84
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
    Public ReadOnly Property ToComplex As Complex84
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
        Return toStringPoly(New Config84)
    End Function
    'Public ReadOnly Property toStrPoly() As String
    '    Get
    '        Return toStringPoly(New Config84, Nothing)
    '    End Get
    'End Property
    Public Function toStringPoly(ByVal cfg As Config84) As String
        Dim e1 As String = ""
        Dim i, j As Int32
        Try
            For i = 0 To cf.Length - 1
                Dim cff As New Complex84(cf(i))
                Dim e2 As String = cff.toStringComplex(cfg)
                If e2 <> "0" Then
                    If (e2.Chars(0) = "-" AndAlso Regex.IsMatch(Mid(e2, 2), "[-+]")) _
                    OrElse (e2.Chars(0) <> "-" AndAlso Regex.IsMatch(e2, "[-+]")) Then
                        Dim db As Double
                        If Not Config84.TryParseDbl(e2, db) Then
                            e2 = "(" + e2 + ")"
                        End If
                        'e2 = "(" + e2 + ")"
                    End If
                    If i < exp.Length Then
                        For j = 0 To exp(i).Length - 1
                            Dim e3 As String = ""
                            Dim k As Int32 = exp(i)(j)
                            If k >= 1 OrElse k < 0 Then
                                e3 += var1(j) ' sVars(j1)
                                If k > 1 OrElse k < 0 Then
                                    Dim cjoK As New Complex84(k)
                                    e3 += "^" + cjoK.toStringComplex(cfg)
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
                Dim e2 As String = Me.PolyResto.toStringPoly(cfg)
                If e2 <> "0" AndAlso Me.PolyDivisor IsNot Nothing Then
                    Dim lvarR As Int32 = PolyResto.var1.Length
                    Dim lvar As Int32 = PolyDivisor.var1.Length
                    Dim e2b As String = IIf(e2.Chars(0) = "-", Mid(e2, 2), e2)
                    If Regex.IsMatch(e2b, "[-+]") Then
                        ' numerator needs to be surrounded by ( )
                        Dim db As Double
                        If Not Config84.TryParseDbl(e2, db) Then
                            e2 = "(" + e2 + ")"
                        End If
                    End If

                    Dim e3 As String = Me.PolyDivisor.toStringPoly(cfg)
                    '' avoid parentheses as for ex. in 1/(y) ?
                    'If Not (mc.Count = 1 AndAlso _
                    '       MathGlobal8.IsOperand(m)) Then
                    '    Dim e4 As String = IIf(e3.Chars(0) = "-", Mid(e3, 2), e3)
                    '    ' avoid parentheses as for ex. in 1/(y^5) ?
                    '    If Regex.IsMatch(e4, "[-+*]") Then
                    '        ' denominator needs to be surrounded by ( )
                    '        Dim db As Double
                    '        If Not MathGlobal8.TryParseDbl(e3, db) Then
                    Dim e4 As String = IIf(e3.Chars(0) = "-", Mid(e3, 2), e3)
                    If Regex.IsMatch(e4, "[-+*/]") Then
                        e3 = "(" + e3 + ")"
                    End If
                    '        End If
                    '    End If
                    'End If
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
            'If e1.Chars(0) = "+" Then
            '    e1 = Mid(e1, 2)
            'End If

            ' supress leading "+" and trailing "-" or "+":
            e1 = Regex.Replace(e1, "(^(\+)+)|([-+]+$)", "")
        Catch ex As Exception
            Throw ex
        End Try
        Return e1
    End Function

    Public Function toStringHorner() As String
        ' salida en la forma: (An*x + An-1)*x + An-2)*x + ....+ A1)*x + A0
        Dim i As Int32
        Dim e1 As String = ""
        Try
            Dim Pa As New Polynomial84(Me)
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
    Public Function IsEqual(ByVal polyB As Polynomial84, ByRef mult As Complex84) As Boolean
        Dim ret As Boolean = False
        Try
            opReduceCommonExponents()
            polyB.opReduceCommonExponents()
            Dim polyA As New Polynomial84(Me)
            Dim polyC As New Polynomial84(polyB)
            mult = Nothing
            Dim i, j As Int32
            For i = 0 To polyA.cf.Length - 1
                For j = 0 To polyC.cf.Length - 1
                    If polyC.cf(j) IsNot Nothing Then
                        Dim mult2 As Complex84 = Nothing
                        If Not polyA.IsEqualTerm(i, polyC, j, mult2) Then
                            GoTo sigJ
                        ElseIf mult Is Nothing Then
                            mult = mult2
                        ElseIf Not (mult - mult2).esCero Then
                            GoTo sigJ
                        End If
                        polyA.cf(i) = Nothing
                        polyC.cf(j) = Nothing
                        Exit For
                    End If
sigJ:
                Next
            Next
            For i = 0 To polyA.cf.Length - 1
                If polyA.cf(i) IsNot Nothing Then
                    Exit Try
                End If
            Next
            For i = 0 To polyC.cf.Length - 1
                If polyC.cf(i) IsNot Nothing Then
                    Exit Try
                End If
            Next
            If polyA.PolyResto IsNot Nothing OrElse
            polyC.PolyResto IsNot Nothing Then
                If polyA.PolyResto Is Nothing OrElse
                polyC.PolyResto Is Nothing Then
                    Exit Try
                End If
                Dim mult2 As Complex84 = Nothing
                If Not polyA.PolyResto.IsEqual(polyC.PolyResto, mult2) Then
                    Exit Try
                End If
                mult2 -= mult
                If Not mult2.esCero Then
                    Exit Try
                End If
                If Not polyA.PolyDivisor.IsEqual(polyC.PolyDivisor, mult2) Then
                    Exit Try
                End If
                mult2 -= mult
                If Not mult2.esCero Then
                    Exit Try
                End If
            End If
            ret = True
        Catch ex As Exception
            Throw ex
        End Try
        Return ret
    End Function
    Public Function IsEqualTerm(ByVal iTermA As Int32, ByVal polyB As Polynomial84, ByVal iTermB As Int32, ByRef mult As Complex84) As Boolean
        Dim ret As Boolean = False
        Try
            Dim i As Int32
            If (exp Is Nothing OrElse exp.Length = 0) AndAlso
            (polyB.exp Is Nothing OrElse polyB.exp.Length = 0) Then
                mult = cf(iTermA) / polyB.cf(iTermB)
                ret = True
                Exit Try
            ElseIf (exp Is Nothing OrElse exp.Length = 0) OrElse
            (polyB.exp Is Nothing OrElse polyB.exp.Length = 0) Then
                Exit Try
            ElseIf (exp(iTermA) Is Nothing AndAlso polyB.exp(iTermB) IsNot Nothing) OrElse
            (exp(iTermA) IsNot Nothing AndAlso (
             iTermB >= polyB.exp.Length OrElse polyB.exp(iTermB) Is Nothing)) Then
                Exit Try
            ElseIf exp(iTermA).Length <> polyB.exp(iTermB).Length Then
                If exp(iTermA).Length Then
                    For i = 0 To exp(iTermA).Length - 1
                        If exp(iTermA)(i) <> 0 Then
                            Exit Try
                        End If
                    Next
                End If
                If polyB.exp(iTermB).Length Then
                    For i = 0 To polyB.exp(iTermB).Length - 1
                        If polyB.exp(iTermA)(i) <> 0 Then
                            Exit Try
                        End If
                    Next
                End If
                'mult = cf(iTermA) / polyB.cf(iTermB)
                'ret = True
                'Exit Try
            End If
            For i = 0 To exp(iTermA).Length - 1
                If i >= var.Length Then
                    If i < polyB.var.Length AndAlso
                    i < polyB.exp(iTermB).Length AndAlso
                    polyB.exp(iTermB)(i) Then
                        Exit Try
                    End If
                    GoTo sigI
                End If
                Dim sVarA As String = var(i)
                Dim posInB As Int32 = Array.IndexOf(polyB.var, sVarA)
                If posInB = -1 AndAlso exp(iTermA)(i) Then
                    Exit Try
                ElseIf i >= polyB.exp(iTermB).Length OrElse
                (posInB = -1 AndAlso exp(iTermA)(i) = 0) OrElse
                (posInB > -1 AndAlso
                 posInB < polyB.exp(iTermB).Length AndAlso
                exp(iTermA)(i) <> polyB.exp(iTermB)(posInB)) Then
                    Exit Try
                End If
sigI:
            Next
            If Not polyB.cf(iTermB).esCero Then
                mult = cf(iTermA) / polyB.cf(iTermB)
                ret = True
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return ret
    End Function
End Class


Public Class oPolySort84
    Public expCf(-1) As Int32
    Public index As Int32
    Public cf As Complex84
    Public Sub New(ByVal expCf() As Int32, ByVal index As Int32, ByVal cf As Complex84)
        Try
            ReDim Me.expCf(expCf.Length - 1)
            Array.Copy(expCf, Me.expCf, expCf.Length)
            Me.index = index
            Me.cf = New Complex84(cf)
        Catch ex As Exception
            Throw ex
        End Try
    End Sub
End Class

Public Class Polynomial84Sort
    Implements IComparer(Of oPolySort84)


    Public var(-1) As String
    Public cfMaxExp As Complex84
    Public ps(-1) As oPolySort84
    Dim degree1 As Int32
    Public degreeMaxVar As Int32
    Public minDegree As Int32
    Public iMaxVar As Int32
    Const minExp As Int32 = -10000
    Public Sub New()
    End Sub
    Public Property degree() As Int32
        Get
            If degree1 <= minExp Then
                Return 0
            End If
            Return degree1
        End Get
        Set(ByVal value As Int32)
            degree1 = value
        End Set
    End Property
    Public Sub New(ByRef exp()() As Int32, ByVal var() As String, ByRef cf() As Complex84)
        Try
            ReDim Me.var(var.Length - 1)
            Array.Copy(var, Me.var, var.Length)
            ReDim ps(exp.Length - 1)
            Dim i As Int32
            For i = 0 To ps.Length - 1
                ps(i) = New oPolySort84(exp(i), i, cf(i))
            Next
            Array.Sort(ps, Me)
            Dim psMax As oPolySort84 = ps(ps.Length - 1)
            'If cf.Length < exp.Length Then
            '    ReDim Preserve cf(exp.Length - 1)
            '    For i = cf.Length - 1 To 0 Step -1
            '        If cf(i) Is Nothing Then
            '            cf(i) = New Complex84(0.0)
            '        End If
            '    Next
            'End If
            Me.cfMaxExp = New Complex84(cf(psMax.index))
            Dim maxExp As Int32 = -100000
            Dim minExp As Int32 = 100000
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
    Public Function Compare(ByVal x As oPolySort84, ByVal y As oPolySort84) As Integer Implements System.Collections.Generic.IComparer(Of oPolySort84).Compare
        Try
            Dim psA As oPolySort84 = x 'CType(x, oPolySort84)
            Dim psB As oPolySort84 = y ' CType(y, oPolySort84)
            Dim sumA As Int32 = 0
            Dim maxExpA As Int32 = -100
            Dim valMaxExpA As Long = 0
            Dim i As Int32
            Dim maxAdmittedNumOfVars = 30
            Dim j As Int32 = psA.expCf.Length - 1
            For i = 0 To psA.expCf.Length - 1
                sumA += psA.expCf(i)
                If psA.expCf(i) > maxExpA Then
                    maxExpA = psA.expCf(i)
                End If
                valMaxExpA += psA.expCf(j) *
                maxAdmittedNumOfVars ^ j
                j -= 1
            Next
            Dim sumB As Int32 = 0
            Dim maxExpB As Int32 = -100
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
            Dim difMaxExp As Int32 = Math.Sign(maxExpA - maxExpB)
            If difMaxExp Then
                Return difMaxExp
            End If
            Dim difSum As Int32 = Math.Sign(sumA - sumB)
            If difSum Then
                Return difSum
            End If
            ' supuestamente aqui ya no pueden ser iguales,
            ' si se trata de coef. distintos:
            Return -Math.Sign(valMaxExpA - valMaxExpB)
        Catch ex As Exception
            Dim es As String = ex.ToString()
        End Try
        Return 0
    End Function
End Class
Public Class retOpRoots84
    Public cjo() As Complex84
    Public Function numRoots() As Int32
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
End Class
