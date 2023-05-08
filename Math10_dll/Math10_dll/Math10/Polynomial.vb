Imports System.Text.RegularExpressions
Imports System.Text
Imports System.Security
Imports System.Globalization

Public Class Polynomial

    Public resto As New List(Of Term)
    Public divisor As New List(Of Term)
    Public pRootsNum(-1) As Complex
    Public pRootsDen(-1) As Complex
    Friend Shared bReduce As Boolean = True

    Public Sub New()
    End Sub
    Public Sub New(dbl As Double)
        resto.Add(New Term(dbl))
        divisor.Add(New Term(1.0))
    End Sub
    Public Sub New(cplx As Complex)
        resto.Add(New Term(cplx))
        divisor.Add(New Term(1.0))
    End Sub
    Public Sub New(t As Term)
        resto.Add(New Term(t))
        divisor.Add(New Term(1.0))
    End Sub
    Public Sub New(termList As List(Of Term))
        For Each t In termList
            resto.Add(New Term(t))
        Next
        divisor.Add(New Term(1.0))
    End Sub

    Public Sub New(var As String)
        resto.Add(New Term(var, 1))
        divisor.Add(New Term(1.0))
    End Sub
    Public Sub New(var As String, exp As Int32)
        resto.Add(New Term(var, exp))
        divisor.Add(New Term(1.0))
    End Sub
    Public Sub New(polyA As Polynomial)
        Try
            For i As Int32 = 0 To polyA.resto.Count - 1
                resto.Add(New Term(polyA.resto(i)))
            Next
            For i As Int32 = 0 To polyA.divisor.Count - 1
                divisor.Add(New Term(polyA.divisor(i)))
            Next
        Catch ex As Exception
            Throw
        End Try
    End Sub
    Public Shared Function CopyFrom(polyA As Polynomial) As Polynomial
        Dim eC As New Polynomial
        Try
            For i As Int32 = 0 To polyA.resto.Count - 1
                eC.resto.Add(New Term(polyA.resto(i)))
            Next
            For i As Int32 = 0 To polyA.divisor.Count - 1
                eC.divisor.Add(New Term(polyA.divisor(i)))
            Next
        Catch ex As Exception
            Throw
        End Try
        Return eC
    End Function
    Public Shared Function CopyFrom(lstT As List(Of Term)) As List(Of Term)
        Dim eClst As New List(Of Term)
        Try
            For i As Int32 = 0 To lstT.Count - 1
                eClst.Add(New Term(lstT(i)))
            Next
        Catch ex As Exception
            Throw
        End Try
        Return eClst
    End Function
    Public Sub CopyToMe(polyA As Polynomial)
        Try
            resto.Clear()
            divisor.Clear()
            For i As Int32 = 0 To polyA.resto.Count - 1
                resto.Add(New Term(polyA.resto(i)))
            Next
            For i As Int32 = 0 To polyA.divisor.Count - 1
                divisor.Add(New Term(polyA.divisor(i)))
            Next
        Catch ex As Exception
            Throw
        End Try
    End Sub
    Public Sub OpChgSign()
        For i As Int32 = 0 To resto.Count - 1
            resto(i).OpChgSign()
        Next
    End Sub
    Public Function IsRational() As Boolean
        Return divisor.Count = 1 AndAlso divisor(0).IsReal _
            AndAlso resto.Count = 1 AndAlso resto(0).IsReal
    End Function
    Public Function IsComplex() As Boolean
        Return divisor.Count = 1 AndAlso divisor(0).Iscomplex _
            AndAlso resto.Count = 1 AndAlso resto(0).Iscomplex
    End Function
    Public Function ToDouble() As Double
        If Not IsRational() Then Throw New ArgumentOutOfRangeException
        Return resto(0).ToDouble / divisor(0).ToDouble
    End Function
    Public Function ToComplex() As Complex
        If Not IsComplex() Then Throw New ArgumentOutOfRangeException
        Return resto(0).ToComplex / divisor(0).ToComplex
    End Function
    Public Function GetVars() As String()
        Dim vars As New List(Of String)
        Try
            For i As Int32 = 0 To resto.Count - 1
                Dim vStr() As String = resto(i).GetVars
                For j As Int32 = 0 To vStr.Length - 1
                    If Not vars.Contains(vStr(j)) Then
                        vars.Add(vStr(j))
                    End If
                Next
            Next
            For i As Int32 = 0 To divisor.Count - 1
                Dim vStr() As String = divisor(i).GetVars
                For j As Int32 = 0 To vStr.Length - 1
                    If Not vars.Contains(vStr(j)) Then
                        vars.Add(vStr(j))
                    End If
                Next
            Next
        Catch ex As Exception
            Throw
        End Try
        Return vars.ToArray()
    End Function
    Public Function GetVarsNumerator()
        Dim vars As New List(Of String)
        Try
            For i As Int32 = 0 To resto.Count - 1
                Dim vStr() As String = resto(i).GetVars
                For j As Int32 = 0 To vStr.Length - 1
                    If Not vars.Contains(vStr(j)) Then
                        vars.Add(vStr(j))
                    End If
                Next
            Next
        Catch ex As Exception
            Throw
        End Try
        Return vars.ToArray()
    End Function
    Public Function GetVarsDenominator()
        Dim vars As New List(Of String)
        Try
            For i As Int32 = 0 To divisor.Count - 1
                Dim vStr() As String = divisor(i).GetVars
                For j As Int32 = 0 To vStr.Length - 1
                    If Not vars.Contains(vStr(j)) Then
                        vars.Add(vStr(j))
                    End If
                Next
            Next
        Catch ex As Exception
            Throw
        End Try
        Return vars.ToArray()

    End Function
    Public Function PolynomialFromTermsOfADegreeAndVar(deg As Int32, sVar As String) As Polynomial
        Dim pC As New Polynomial(0.0)
        Try
            If Not New Polynomial(divisor).IsComplex Then
                Return Nothing
            End If
            Dim cfDiv As Complex = New Polynomial(divisor).ToComplex
            For i As Int32 = 0 To resto.Count - 1
                Dim t As Term = resto(i)
                Dim exp As Int32 = deg - 1
                For j As Int32 = 0 To t.f.Count - 1
                    If t.f(j).var.GetType Is GetType(String) AndAlso
                    (t.f(j).var = sVar OrElse (deg = 0 AndAlso t.f(j).var = "")) Then
                        If t.f(j).exp = deg Then
                            exp = deg
                        Else
                            exp = deg - 2
                        End If
                    End If
                Next
                If exp = deg OrElse (deg = 0 AndAlso exp = deg - 1) Then
                    pC.resto.Add(New Term(t))
                End If
            Next
        Catch ex As Exception
            Throw
        End Try
        Return pC
    End Function
    Public Function LeadingTerm() As Term
        Dim LT As New Term(Complex.zero)
        Dim tc As New TermComparer
        Try
            If Not divisor.Count = 1 AndAlso divisor(0).Iscomplex Then
                Throw New ArgumentOutOfRangeException
            End If
            Dim v() As Term = resto.ToArray
            Array.Sort(v, tc)
            If v.Length Then
                LT.f.AddRange(v(0).f)
                LT.cf = New Complex(v(0).cf / divisor(0).cf)
            End If
        Catch ex As Exception
            Throw
        End Try
        Return LT
    End Function
    Public Function LowerTerm() As Term
        Dim LT As New Term(Complex.zero)
        Dim tc As New TermComparer
        Try
            If Not divisor.Count = 1 AndAlso divisor(0).Iscomplex Then
                Throw New ArgumentOutOfRangeException
            End If
            Dim v() As Term = resto.ToArray
            Array.Sort(v, tc)
            Array.Reverse(v)
            LT.f.AddRange(v(0).f)
            LT.cf = New Complex(v(0).cf)
        Catch ex As Exception
            Throw
        End Try
        Return LT
    End Function
    Public Function Degree() As Double
        Dim t As Term = LeadingTerm()
        Return Term.Degree(t)
    End Function
    Public Shared Sub SortListOfTerms(lstT As List(Of Term), Optional reverse As Boolean = False)
        Dim tc As New TermComparer
        Dim fc As New FactorComparer
        Dim v() As Term = lstT.ToArray
        Array.Sort(v, tc)
        If reverse Then Array.Reverse(v)
        For i As Int32 = 0 To v.Length - 1
            Dim sf() As Factor = v(i).f.ToArray
            Array.Sort(sf, fc)
            v(i).f.Clear()
            v(i).f.AddRange(sf)
        Next
        lstT.Clear()
        lstT.AddRange(v)
    End Sub

    Public Sub SortTerms(Optional Reverse As Boolean = False)
        SortListOfTerms(resto, Reverse)
        SortListOfTerms(divisor, Reverse)
    End Sub
    Public Function DerivativeOfTermList(termLst As List(Of Term), respVar As String) As List(Of Term)
        Dim tLst As New List(Of Term)
        Try
            tLst.AddRange(termLst)
            For i As Int32 = tLst.Count - 1 To 0 Step -1
                Dim vVarT() As String = tLst(i).GetVars
                Dim j As Int32 = Array.IndexOf(vVarT, respVar)
                If j > -1 Then
                    For k As Int32 = 0 To tLst(i).f.Count - 1
                        If tLst(i).f(k).var = respVar Then
                            tLst(i).cf *= tLst(i).f(k).exp
                            tLst(i).f(k).exp -= 1
                            If tLst(i).f(k).exp = 0 Then
                                tLst(i).f.RemoveAt(k)
                                Exit For
                            End If
                        End If
                    Next
                Else
                    tLst.RemoveAt(i)
                End If
            Next
        Catch ex As Exception
            Throw
        End Try
        Return tLst
    End Function
    Public Function Derivative(respVar As String) As Polynomial
        Dim polyC As New Polynomial
        Try
            Dim dNum As List(Of Term) = DerivativeOfTermList(resto, respVar)
            Dim dDen As List(Of Term) = DerivativeOfTermList(divisor, respVar)
            Dim num As List(Of Term) = Substract(Mult(dNum, divisor), Mult(resto, dDen))
            Dim den As List(Of Term) = Mult(divisor, divisor)
            polyC.resto = num
            polyC.divisor = den
            polyC.Reduce()
        Catch ex As Exception
            Throw
        End Try
        Return polyC
    End Function
    Public Shared Function EvalListOfTerms(termList As List(Of Term), ByVal x As Rational) As Rational
        Dim i As Int64
        Dim prN As Rational
        Try

            SortListOfTerms(termList)
            Dim LT As Term = termList(0)
            prN = New Rational(LT.cf.pRe)
            Dim exp1 As Int64
            If LT.f.Count = 0 Then
                exp1 = 0
            Else
                exp1 = LT.f(0).exp ' exp(0)(0)
            End If
            Dim exp2 As Int64 = exp1
            For i = 1 To termList.Count - 1
                If termList(i).f.Count Then
                    exp2 = termList(i).f(0).exp
                Else
                    exp2 = 0
                End If
                Dim nVueltas As Int64 = 0
                Do While exp1 > exp2
                    prN *= x
                    exp1 -= 1
                    nVueltas += 1
                Loop
                prN += termList(i).cf.pRe
            Next
            Dim nV2 As Int64 = 0
            Do While exp2 > 0
                prN *= x
                exp2 -= 1
                nV2 += 1
            Loop
        Catch ex As Exception
            prN = New Rational(1.0E+100)
        End Try
        Return prN
    End Function
    Public Function EvalPrecis(ByVal x As Rational) As Rational
        Dim prN As Rational = Nothing
        Try
            prN += EvalListOfTerms(resto, x) / EvalListOfTerms(divisor, x)
        Catch ex As Exception
            Throw
        End Try
        Return prN
    End Function
    Public Shared Function EvalListOfTermsCplx(termList As List(Of Term), ByVal x As Complex) As Complex
        Dim i As Int64
        Dim prN As Complex
        Try
            SortListOfTerms(termList)
            Dim LT As Term = termList(0)
            prN = New Complex(LT.cf)
            If LT.f.Count = 0 Then Exit Try
            Dim exp1 As Int64 = LT.f(0).exp ' exp(0)(0)
            Dim exp2 As Int64 = exp1
            For i = 1 To termList.Count - 1
                If termList(i).f.Count Then
                    exp2 = termList(i).f(0).exp
                Else
                    exp2 = 0
                End If
                Dim nVueltas As Int64 = 0
                Do While exp1 > exp2
                    prN *= x
                    prN.pRe.reduce(40)
                    prN.pIm.reduce(40)
                    exp1 -= 1
                    nVueltas += 1
                Loop
                prN += termList(i).cf
            Next
            Dim nV2 As Int64 = 0
            Do While exp2 > 0
                prN *= x
                exp2 -= 1
                nV2 += 1
            Loop
        Catch ex As Exception
            Throw
        End Try
        Return prN
    End Function
    Public Function EvalPrecisCplx(ByVal x As Complex) As Complex
        Dim prN As Complex = Nothing
        Try
            prN = EvalListOfTermsCplx(resto, x) / EvalListOfTermsCplx(divisor, x)
        Catch ex As Exception
            Throw
        End Try
        Return prN
    End Function

    Public Shared Operator -(eA As Polynomial) As Polynomial
        Dim eC As Polynomial = Polynomial.CopyFrom(eA)
        eC.OpChgSign()
        Return eC
    End Operator
    Public Shared Operator -(eA As Polynomial, eB As Polynomial) As Polynomial
        Dim eC As Polynomial = Polynomial.CopyFrom(eB)
        Try
            eC.OpChgSign()
            eC = eA + eC
        Catch ex As Exception
            Throw
        End Try
        Return eC
    End Operator
    Public Shared Operator +(eA As Polynomial, eB As Polynomial) As Polynomial
        Return Add(eA, eB)
    End Operator
    Public Shared Function Substract(eAT As List(Of Term), eBT As List(Of Term)) As List(Of Term)
        Dim eCT As New List(Of Term)
        Try
            eCT.AddRange(eAT)
            For i As Int32 = 0 To eBT.Count - 1
                Dim t As New Term(eBT(i))
                t.OpChgSign()
                eCT.Add(t)
            Next
        Catch ex As Exception
            Throw
        End Try
        Return eCT
    End Function
    Public Shared Function Add(eAT As List(Of Term), eBT As List(Of Term)) As List(Of Term)
        Dim eCT As New List(Of Term)
        Try
            eCT.AddRange(eAT)
            eCT.AddRange(eBT)
            Dim ec As New Polynomial(eCT)
            ec.Reduce()
            eCT = ec.resto
        Catch ex As Exception
            Throw
        End Try
        Return eCT
    End Function
    Private Shared Function Add(eA As Polynomial, eB As Polynomial) As Polynomial
        Dim eC As Polynomial = Polynomial.CopyFrom(eA)
        Try
            eC.resto = Add(Mult(eA.resto, eB.divisor), Mult(eA.divisor, eB.resto))
            eC.divisor = Mult(eA.divisor, eB.divisor)
            eC.Reduce()
        Catch ex As Exception
            Throw
        End Try
        Return eC
    End Function
    Public Shared Operator *(eA As Polynomial, eB As Polynomial) As Polynomial
        Return Mult(eA, eB)
    End Operator
    Public Shared Function Mult(eAT As List(Of Term), eBT As List(Of Term)) As List(Of Term)
        Dim ecT As New List(Of Term)
        Try
            For i = 0 To eAT.Count - 1
                For j = 0 To eBT.Count - 1
                    If eAT(i) = eBT(j) Then
                        Dim t As New Term(eAT(i))
                        t.cf *= eBT(j).cf
                        For k = 0 To eAT(i).f.Count - 1
                            Dim modulo As Int32 = G10.mMod
                            G10.mMod = 0
                            t.f(k).exp += eBT(j).f(k).exp
                            G10.mMod = modulo
                        Next
                        ecT.Add(t)
                    Else
                        ecT.Add(New Term(eAT(i) * eBT(j)))
                    End If
                Next
            Next
        Catch ex As Exception
            Throw
        End Try
        Return ecT
    End Function
    Public Shared Function Mult(eA As Polynomial, eB As Polynomial) As Polynomial
        Dim eC As New Polynomial
        Try
            eC.resto = Mult(eA.resto, eB.resto)
            eC.divisor = Mult(eA.divisor, eB.divisor)

            eC.Reduce()
        Catch ex As Exception
            Throw
        End Try
        Return eC
    End Function
    Public Shared Function EqualListOfTerms(eAT As List(Of Term), eBT As List(Of Term)) As Boolean
        If eAT.Count <> eBT.Count Then Return False
        For i = 0 To eAT.Count - 1
            If eAT(i) <> eBT(i) Then Return False
        Next
        Return True
    End Function
    Public Shared Operator =(eA As Polynomial, eB As Polynomial) As Boolean
        Try
            eA.SortTerms()
            eB.SortTerms()
            If Not EqualListOfTerms(eA.resto, eB.resto) Then Return False
            For i = 0 To eA.resto.Count - 1
                If Not (eA.resto(i).cf - eB.resto(i).cf).IsZero Then Return False
                If eA.resto(i).f.Count <> eB.resto(i).f.Count Then Return False
                For j As Int32 = 0 To eA.resto(i).f.Count - 1
                    If eA.resto(i).f(j).exp <> eB.resto(i).f(j).exp Then Return False
                    If eA.resto(i).f(j).var <> eB.resto(i).f(j).var Then Return False
                Next
            Next
        Catch ex As Exception

        End Try
        Return True
    End Operator
    Public Shared Operator <>(eA As Polynomial, eB As Polynomial) As Boolean
        Return Not (eA = eB)
    End Operator
    Public Shared Operator /(eA As Polynomial, cplx As Complex) As Polynomial
        Return eA / New Polynomial(cplx)
    End Operator
    Public Shared Operator /(eA As Polynomial, eB As Polynomial) As Polynomial
        Dim eC As New Polynomial(0.0)
        Try
            If eA.Degree = eB.Degree Then
                If EqualListOfTerms(eA.divisor, eB.divisor) Then
                    eC = New Polynomial(eA.resto)
                    eC.divisor.Clear()
                    For i As Int32 = 0 To eB.resto.Count - 1
                        eC.divisor.Add(New Term(eB.resto(i)))
                    Next
                    eC.Reduce()
                    Exit Try
                End If
            End If
            ' (ra/da)/(rb/db) = ra*db / (da * rb)
            eC.resto = Mult(eA.resto, eB.divisor)
            eC.divisor = Mult(eA.divisor, eB.resto)
            eC.Reduce()
        Catch ex As Exception
            Throw
        End Try
        Return eC
    End Operator
    'Public Shared Operator ^(eA As Polynomial, eB As Polynomial) As Polynomial
    '    Dim eC As Polynomial
    '    Try
    '        If eB.IsComplex Then
    '            If eA.IsComplex Then
    '                eC = New Polynomial(eA.ToComplex ^ eB.ToComplex)
    '                Exit Try
    '            ElseIf eB.IsRational Then
    '                Dim db As Double = eB.ToDouble
    '                If db > 0.0 AndAlso db = Math.Floor(db) Then
    '                    eC = Polynomial.CopyFrom(eA)
    '                    For i = 2 To db
    '                        eC *= eA
    '                    Next
    '                    Exit Try
    '                End If
    '                Throw New ArgumentOutOfRangeException
    '            End If
    '        End If
    '        Throw New ArgumentOutOfRangeException
    '    Catch ex As Exception
    '        Throw
    '    End Try
    '    eC.Reduce()
    '    Return eC
    'End Operator
    Public Shared Operator ^(polyA As Polynomial, polyB As Polynomial) As Polynomial
        Dim polyC As Polynomial = Nothing
        Try
            If polyA.IsComplex + polyB.IsComplex = 0 Then
                polyC = New Polynomial(polyA.ToComplex ^ polyB.ToComplex)
                Return polyC
            End If
            polyC = New Polynomial(polyA)
            If polyB.IsComplex Then
                Dim cf As Complex = polyB.ToComplex
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
                            If Pa.IsRational AndAlso Pa.ToDouble = 0.0 Then
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
                        Dim var() As String = polyA.GetVars
                        If var.Length = 1 AndAlso polyA.resto.Count = 1 AndAlso
                        polyA.divisor.Count = 1 AndAlso polyB.IsRational Then
                            Dim exp As Double = polyB.ToDouble
                            If exp = Math.Floor(exp) AndAlso exp > 0 Then
                                polyC = New Polynomial(var(0), polyB.ToDouble)
                                polyC.resto(0).cf = (polyA.resto(0).cf / polyA.divisor(0).cf) ^ polyB.ToComplex
                            Else
                                Throw New Exception(Msg8.msg(1009)) ' polynomial exponent non positive integer: n/a
                            End If
                        Else
                            Throw New Exception(Msg8.msg(1009)) ' polynomial exponent non integer: n/a
                        End If
                    End If
                Else
                    Throw New Exception(Msg8.msg(1008)) ' polynomial exponent is imaginary: n/a
                End If
            Else
                Throw New Exception(Msg8.msg(1010)) ' polynomial exponent is a polynomial: n/a
            End If
            polyC.Reduce()
        Catch ex As Exception
            Throw
        End Try
        Return polyC
    End Operator

    Public Shared Sub ReduceListOfTerms(ByRef lstT As List(Of Term))
        Dim tc1 As New TermComparer
        Array.Sort(lstT.ToArray, tc1)
        For i As Int32 = lstT.Count - 1 To 1 Step -1
            If i >= lstT.Count Then i = lstT.Count - 1
            For j As Int32 = i - 1 To 0 Step -1
                If lstT(i) = lstT(j) Then
                    lstT(j).cf += lstT(i).cf
                    lstT.RemoveAt(i)
                    If lstT(j).IsReal AndAlso lstT(j).cf.IsZero Then
                        lstT.RemoveAt(j)
                    End If
                    Exit For
                End If
            Next
        Next
        For i As Int32 = lstT.Count - 1 To 0 Step -1
            If lstT(i).Iscomplex AndAlso lstT(i).ToComplex.IsZero Then
                lstT.RemoveAt(i)
            End If
        Next
    End Sub


    Public Sub Reduce(Optional ByRef IntegerPart As List(Of Term) = Nothing)
        Dim bFra As Boolean = G10.frac
        Try
            If Not bReduce Then Exit Sub
            G10.frac = True
            SortTerms()
            ReduceListOfTerms(resto)
            ReduceListOfTerms(divisor)
            Dim vVar() As String = GetVars()
            For i As Int32 = resto.Count - 1 To 0 Step -1
                If resto(i).cf.IsDouble AndAlso resto(i).ToDouble = 0 Then
                    resto.RemoveAt(i)
                Else
                    For j As Int32 = resto(i).f.Count - 1 To 0 Step -1
                        If resto(i).f(j).exp = 0.0 Then
                            resto(i).f.RemoveAt(j)
                        End If
                    Next
                End If
            Next
            For i As Int32 = divisor.Count - 1 To 0 Step -1
                If divisor(i).cf.IsDouble AndAlso divisor(i).ToDouble = 0 Then
                    divisor.RemoveAt(i)
                Else
                    For j As Int32 = divisor(i).f.Count - 1 To 0 Step -1
                        If divisor(i).f(j).exp = 0.0 Then
                            divisor(i).f.RemoveAt(j)
                        End If
                    Next
                End If
            Next

            If resto.Count = 0 Then
                resto.Add(New Term(0.0))
                divisor.Clear()
                divisor.Add(New Term(1.0))
                Exit Try
            End If
            If divisor.Count = 0 Then
                divisor.Add(New Term(1.0))
            End If
            If New Polynomial(resto) = New Polynomial(divisor) Then
                resto.Clear()
                resto.Add(New Term(1.0))
                divisor.Clear()
                divisor.Add(New Term(1.0))
                Exit Sub
            End If
            If vVar.Length = 1 AndAlso IntegerPart Is Nothing Then
                Dim pA As New Polynomial(resto)
                Dim pB As New Polynomial(divisor)
                Dim b As Boolean = Polynomial.bOnlyRationalRoots
                Polynomial.bOnlyRationalRoots = True
                Dim pGCD As Polynomial = Polynomial.opGcd(pA, pB)
                Polynomial.bOnlyRationalRoots = b
                If Not pGCD.IsComplex Then
                    pGCD.Roots()
                    For i As Int32 = 0 To pGCD.pRootsNum.Length - 1
                        Polynomial.opRuffini(resto, vVar(0), pGCD.pRootsNum(i))
                        Polynomial.opRuffini(divisor, vVar(0), pGCD.pRootsNum(i))
                    Next
                End If
            Else
                If vVar.Length AndAlso IntegerPart IsNot Nothing Then
                    Dim varsDivisor() As String = GetVarsDenominator()
                    Dim g As New Polynomial(divisor)
                    If g.IsComplex Then Exit Try
                    ' Find negative exponents at numerator:
                    Dim minG(vVar.Length - 1) As Int32
                    For i As Int32 = 0 To g.resto.Count - 1
                        For j As Int32 = 0 To g.resto(i).f.Count - 1
                            Dim k As Int32 = Array.IndexOf(vVar, g.resto(i).f(j).var)
                            Dim k1 As Int32 = g.resto(i).f(j).exp
                            If k > -1 AndAlso k1 < minG(k) Then minG(k) = k1
                        Next
                    Next
                    Dim q As New Polynomial(0.0)
                    Dim r As New Polynomial(resto)
                    ' Find negative exponents at denominator:
                    Dim minR(vVar.Length - 1) As Int32
                    For i As Int32 = 0 To r.resto.Count - 1
                        For j As Int32 = 0 To r.resto(i).f.Count - 1
                            Dim k As Int32 = Array.IndexOf(vVar, r.resto(i).f(j).var)
                            Dim k1 As Int32 = r.resto(i).f(j).exp
                            If k > -1 AndAlso k1 < minG(k) Then minG(k) = k1
                        Next
                    Next
                    For k As Int32 = 0 To vVar.Length - 1
                        If minR(k) < minG(k) Then minG(k) = minR(k)
                        If minG(k) Then
                            ' multiply so that there are no negative exponents:
                            g *= New Polynomial(vVar(k), -minG(k))
                            r *= New Polynomial(vVar(k), -minG(k))
                        End If
                    Next

                    ' Input: g, f
                    ' Output: q,r
                    ' q := 0
                    ' r := f
                    ' WHILE r <> 0 AND LT(g) divides LT(r) 
                    ' 	q := q + LT(r)/LT(g)
                    ' 	r := r − (LT(r)/LT(g))g

                    Dim divDeg As Int32 = 1
                    Dim vsr(-1) As String
                    Dim vsrCff(-1) As String
                    Dim sr As String = ""
                    If G10.detail Then
                        vsr = r.ToVStringAllTerms(False, G10.sImg, G10.CI)
                        vsrCff = r.ToVStringAllTerms(True, G10.sImg, G10.CI)
                        sr = Join(vsrCff, "|")
                        G10.sDetail += sr + vbCrLf
                    End If
                    Dim bDetail As Boolean = G10.detail
                    G10.detail = False
                    Do While Not (r.IsRational AndAlso r.ToDouble = 0.0) AndAlso divDeg > 0
                        Dim gxDiv As Polynomial = Nothing
                        r.SortTerms()
                        g.SortTerms()
                        Dim rLT As Term = r.LeadingTerm()
                        Dim gLT As Term = g.LeadingTerm()
                        If gLT.IsReal AndAlso gLT.ToDouble = 0 Then Exit Do
                        Dim gDeg As Int32 = Term.Degree(gLT)
                        Dim div As New Polynomial(rLT / gLT)
                        div.Reduce()
                        For i As Int32 = 0 To div.resto.Count - 1
                            For j As Int32 = 0 To div.resto(i).f.Count - 1
                                If div.resto(i).f(j).exp < 0 Then Exit Do
                            Next
                        Next
                        Dim divT As Term = div.LeadingTerm()
                        divDeg = Term.Degree(divT)
                        If divDeg < 0 Then Exit Do
                        gxDiv = g * div
                        gxDiv.SortTerms()
                        G10.detail = bDetail
                        G10.detail = False
                        Dim vsrCffB(vsr.Length - 1) As String
                        If bDetail Then
                            Dim vsgxDiv() As String = gxDiv.ToVStringAllTerms(False, G10.sImg, G10.CI)
                            Dim vsgxDivCff() As String = gxDiv.ToVStringAllTerms(True, G10.sImg, G10.CI)
                            For i As Int32 = 1 To vsgxDiv.Length - 1
                                Dim pos As Int32 = Array.IndexOf(vsr, vsgxDiv(i))
                                If pos = -1 Then
                                    pos = vsrCffB.Length
                                    ReDim Preserve vsrCffB(pos), vsr(pos)
                                    vsr(pos) = vsgxDiv(i)
                                    vsrCffB(pos) = vsgxDivCff(i)
                                Else
                                    vsrCffB(pos) = vsgxDivCff(i)
                                End If
                            Next
                            vsrCffB(0) = "Substract " + g.ToStringPolynomial(15, G10.sImg, G10.CI) + " times <span style='color:red'>" +
                            div.ToStringPolynomial(15, G10.sImg, G10.CI) + "</span>: "
                            G10.sDetail += Join(vsrCffB, "|") + vbCrLf
                        End If
                        G10.detail = False
                        q += div
                        r -= gxDiv
                        r.Reduce()
                        If bDetail Then
                            G10.sDetail += "-----------------" + vbCrLf
                            Dim vsrB() As String = r.ToVStringAllTerms(False, G10.sImg, G10.CI)
                            Dim vsrBCff() As String = r.ToVStringAllTerms(True, G10.sImg, G10.CI)
                            Array.Clear(vsrCffB, 0, vsrCffB.Length) ' dim vsrcffb(vsrCff.Length - 1) 'As String
                            For i As Int32 = 1 To vsrB.Length - 1
                                Dim pos As Int32 = Array.IndexOf(vsr, vsrB(i))
                                If pos = -1 Then
                                    pos = vsrCffB.Length - 1
                                    ReDim Preserve vsrCffB(pos), vsr(pos)
                                    vsr(pos) = vsrB(i)
                                    vsrCffB(pos) = vsrBCff(i)
                                Else
                                    vsrCffB(pos) = vsrBCff(i)
                                End If
                            Next
                            vsrCffB(0) = "Reminder:"

                            If vsrB.Length = 1 Then
                                Dim pos As Int32 = Array.IndexOf(vsr, "")
                                If pos = -1 Then
                                    pos = vsrCffB.Length - 1
                                End If
                                vsrCffB(pos) = "0"
                            End If
                            G10.sDetail += Join(vsrCffB, "|") + vbCrLf
                        End If
                    Loop
                    Dim polyC As New Polynomial()
                    For i As Int32 = 0 To resto.Count - 1
                        polyC.resto.Add(resto(i))
                    Next
                    IntegerPart = q.resto
                    resto.Clear()
                    For i As Int32 = 0 To polyC.resto.Count - 1
                        resto.Add(polyC.resto(i))
                    Next
                    resto = r.resto
                    divisor = CopyFrom(g.resto)
                    ReDim minG(vVar.Length - 1)
                    For i As Int32 = 0 To divisor.Count - 1
                        For j As Int32 = 0 To divisor(i).f.Count - 1
                            Dim k As Int32 = Array.IndexOf(vVar, divisor(i).f(j).var)
                            Dim k1 As Int32 = divisor(i).f(j).exp
                            If k > -1 AndAlso k1 < minG(k) Then minG(k) = k1
                        Next
                    Next
                    ReDim minR(vVar.Length - 1)
                    For i As Int32 = 0 To r.resto.Count - 1
                        For j As Int32 = 0 To r.resto(i).f.Count - 1
                            Dim k As Int32 = Array.IndexOf(vVar, r.resto(i).f(j).var)
                            Dim k1 As Int32 = r.resto(i).f(j).exp
                            If k > -1 AndAlso k1 < minR(k) Then minR(k) = k1
                        Next
                    Next
                    For k As Int32 = 0 To vVar.Length - 1
                        If minR(k) < minG(k) Then minG(k) = minR(k)
                        If minG(k) Then
                            ' multiply so that there are no negative exponents:
                            divisor = Mult(divisor, New Polynomial(vVar(k), -minG(k)).resto)
                            r *= New Polynomial(vVar(k), -minG(k))
                        End If
                    Next
                    G10.detail = bDetail
                End If

            End If
            Dim divLT As Complex = divisor(0).cf
            If Not divLT.IsDouble OrElse divLT.ToDouble <> 1.0 Then
                For Each t As Term In resto
                    t.cf /= divLT
                Next
                For Each t As Term In divisor
                    t.cf /= divLT
                Next
            End If

            ReduceListOfTerms(resto)
            If resto.Count = 0 Then
                resto.Add(New Term(0.0))
                divisor.Clear()
                divisor.Add(New Term(1.0))
            Else
                ReduceListOfTerms(divisor)
            End If
        Catch ex As Exception
            Throw
        Finally
            G10.frac = bFra
        End Try
    End Sub

    Sub replaceConstants()
        Try
            For Each t1 As Term In resto
                For j As Int32 = t1.f.Count - 1 To 0 Step -1
                    If t1.f(j).var = "π" Then
                        t1.cf *= New Complex(Math.PI ^ t1.f(j).exp, 0)
                        t1.f.RemoveAt(j)
                    End If
                Next
            Next
            For Each t1 As Term In divisor
                For j As Int32 = t1.f.Count - 1 To 0 Step -1
                    If t1.f(j).var = "π" Then
                        t1.cf *= New Complex(Math.PI ^ t1.f(j).exp, 0)
                        t1.f.RemoveAt(j)
                    End If
                Next
            Next
        Catch ex As Exception
            Throw
        End Try
    End Sub
    Public Function ToVStringAllTerms(includeCoefficients As Boolean, sImg As String, CI As Globalization.CultureInfo) As String()
        Dim r() As String
        Dim tc As New TermComparer
        Try
            Dim v() As Term = resto.ToArray
            Array.Sort(v, tc)
            ReDim r(v.Length) ' reserve r(0) for comments
            For i As Int32 = 0 To v.Length - 1
                If includeCoefficients Then
                    r(i + 1) = v(i).ToString
                Else
                    r(i + 1) = ""
                    Dim t As Term = Term.CopyFrom(v(i))
                    For j As Int32 = 0 To t.f.Count - 1
                        If Len(r(i + 1)) Then r(i + 1) += "*"
                        r(i + 1) += t.f(j).var
                        If t.f(j).exp <> 1 Then
                            r(i + 1) += "^" + t.f(j).exp.ToString
                        End If
                    Next
                End If
            Next
        Catch ex As Exception
            Throw
        End Try
        Return r
    End Function

    Public Overrides Function ToString() As String
        Return ToStringPolynomial(G10.nDec, G10.sImg, G10.CI)
    End Function
    Public Shared Function ToStringListOfTerms(lstT As List(Of Term)) As String
        Dim sb As New StringBuilder
        Try
            SortListOfTerms(lstT)
            Dim tc As New TermComparer
            Dim v() As Term = lstT.ToArray
            Array.Sort(v, tc)
            Dim i As Int32
            Dim sIndep As String = ""
            For i = 0 To v.Count - 2
                If v(i).IsReal Then
                    sIndep = v(i).ToString
                Else
                    sb.Append(v(i).ToString + "+")
                End If
            Next
            If v.Count Then
                sb.Append(v(i).ToString())
            End If
            If Len(sIndep) Then
                If sb.Length Then
                    sb.Append("+")
                End If
                sb.Append(sIndep)
            End If
            If sb.Length = 0 Then
                sb.Append("0")
            End If
        Catch ex As Exception
            Throw
        End Try
        Return sb.ToString
    End Function
    Public Overloads Function ToStringPolynomial(numDecimals As Int32, sImg As String,
                                                cultureInfo As Globalization.CultureInfo)
        Dim sb As New StringBuilder
        Try
            bReduce = True
            Reduce()
            If Me.IsComplex Then
                Return Me.ToComplex.toString()
            End If
            Dim sIntPart As String = ""
            Dim IntegerPart As New List(Of Term)
            Reduce(IntegerPart)
            sIntPart += ToStringListOfTerms(IntegerPart)
            Dim pResto As New Polynomial(resto)
            Dim pDiv As New Polynomial(divisor)
            If pDiv.IsRational AndAlso pDiv.ToDouble = -1 Then
                pResto = -pResto
                pDiv = -pDiv
            End If
            Dim sResto As String
            sResto = ToStringListOfTerms(pResto.resto)
            If sResto = "0" Then sResto = ""
            If sIntPart = "0" AndAlso Len(sResto) Then
                sIntPart = ""
            End If
            Dim sDiv As String = ToStringListOfTerms(pDiv.resto)
            If Len(sResto) > 1 AndAlso Regex.IsMatch(Mid(sResto, 2), "\-|\+") AndAlso
            sDiv <> "1" AndAlso sDiv <> "-1" Then
                sResto = "(" + sResto + ")"
            End If
            If Regex.IsMatch(Mid(sDiv, 2), "\-|\+") Then
                sDiv = "(" + sDiv + ")"
            End If
            Dim sDiv2 As String = Regex.Replace(sDiv, "&h|&o| ", "")
            If sResto = "0" Then
                sResto = ""
                sDiv = ""
            ElseIf sDiv2 = "1" Then
                sDiv = ""
            ElseIf sDiv2 = "-1" Then
                sResto = "-" + sResto
                sDiv = ""
            Else
                If Regex.IsMatch(Mid(sDiv, 2), "\-|\+|\*") Then
                    sDiv = removeStartEndParentheses(sDiv)
                    sDiv = "(" + sDiv + ")"
                End If
                sDiv = "/" + sDiv
            End If
            If Len(sIntPart) Then
                sb.Append(sIntPart)
                If Len(sResto) Then sb.Append("+")
            End If
            sb.Append(sResto + sDiv)
        Catch ex As Exception
            Throw
        End Try
        Dim e1 As String = Regex.Replace(sb.ToString, "\+\s*\-", "-")
        Return e1
    End Function
    Public Shared Function removeStartEndParentheses(s As String) As String
        Try
            Do While Left(s, 1) = "(" AndAlso Right(s, 1) = ")"
                Dim s1 As String = Mid(s, 2, Len(s) - 2)
                Dim lp As Int32 = InStr(s1, "(")
                Dim rp As Int32 = InStr(s1, ")")
                If lp < rp OrElse lp + rp = 0 Then
                    s = s1
                Else
                    Exit Do
                End If
            Loop
        Catch ex As Exception

        End Try
        Return s
    End Function

    Public Class TermComparer
        Implements IComparer
        Public Function Compare(x1 As Object, y1 As Object) As Integer Implements IComparer.Compare
            Dim x As Term = CType(x1, Term)
            Dim y As Term = CType(y1, Term)
            Dim sb1 As New StringBuilder
            Dim sb2 As New StringBuilder
            Try
                Dim i As Int32
                For i = 0 To x.f.Count - 1
                    Dim s As String = x.f(i).var
                    sb1.Append(s)
                    sb1.Append(String.Format("{0:000000}", 10 ^ 5 - x.f(i).exp))
                Next
                If sb1.Length = 0 Then
                    sb1.Append(ChrW(255))
                Else
                    sb1.Append("}")
                End If
                For i = 0 To y.f.Count - 1
                    Dim s As String = y.f(i).var
                    sb2.Append(s)
                    sb2.Append(String.Format("{0:000000}", 10 ^ 5 - y.f(i).exp))
                Next
                If sb2.Length = 0 Then
                    sb2.Append(ChrW(255))
                Else
                    sb2.Append("}")
                End If
            Catch ex As Exception
                Throw New NotImplementedException()
            End Try
            If sb1.ToString < sb2.ToString Then
                Return -1
            ElseIf sb1.ToString > sb2.ToString Then
                Return 1
            End If
            Return 0
        End Function
    End Class
    Public Shared Function opGcd(ByVal p0 As Polynomial,
                             ByVal p1 As Polynomial) As Polynomial
        Dim polyC As New Polynomial(1.0)
        Try
            Dim pA As New Polynomial(p0)
            Dim pB As New Polynomial(p1)
            Dim i, j As Int64
            Dim varA() As String = pA.GetVars
            Dim varB() As String = pB.GetVars
            If varA.Length <> 1 OrElse
            varB.Length <> 1 OrElse
            varA(0) <> varB(0) Then
                If varA.Length = 0 AndAlso pA.IsRational Then
                    Dim num As Double = pA.ToDouble
                    Dim sgn As Int32 = Math.Sign(num)
                    num = Math.Abs(num)
                    Dim vFac() As Double = Rational.opFactors(num)
                    If vFac.Length = 0 OrElse vFac(0) = 1.0 Then Exit Try
                    For i = 0 To vFac.Length - 1
                        For j = 0 To pB.resto.Count - 1
                            pB.resto(j).cf.pRe = New Rational(
                                pB.resto(j).cf.pRe.ToDouble / vFac(i))
                        Next
                        For j = 0 To pA.resto.Count - 1
                            pA.resto(j).cf.pRe = New Rational(
                                pA.resto(j).cf.pRe.ToDouble / vFac(i))
                        Next
                    Next
                End If
                Exit Try
            End If
            Dim degA As Int64 = pA.Degree
            Dim degB As Int64 = pB.Degree
            If degA < 1 OrElse degB < 1 Then
                Exit Try
            End If
            Dim cfAn As Complex = New Complex(pA.resto(0).cf) ' pA.an)
            Dim mult As Complex = Nothing
            Dim cfBn As Complex = New Complex(pB.resto(0).cf) ' pB.An)
            If Not cfAn.pIm.IsZero Then
                Throw New Exception(String.Format(Msg8.num(83), pA.ToString))
            End If
            If Not cfBn.pIm.IsZero Then
                Throw New Exception(String.Format(Msg8.num(83), pB.ToString))
            End If
            Dim rootsA() As Complex = pA.RootsNumerator  ' opRoots(pA).cjo
            Dim rootsB() As Complex = pB.RootsNumerator  ' opRoots(pB).cjo
            Dim x As Polynomial = New Polynomial(varA(0), 1) '.GetPolynomial(pA.var(0))
            Dim lstA As New List(Of Complex)
            Dim lstB As New List(Of Complex)
            lstA.AddRange(rootsA)
            lstB.AddRange(rootsB)
            Dim commonCjo As New List(Of Complex)
            For i = lstA.Count - 1 To 0 Step -1
                For j = lstB.Count - 1 To 0 Step -1
                    If (lstB(j) - lstA(i)).IsZero Then
                        commonCjo.Add(lstA(i))
                        lstB.RemoveAt(j)
                        lstA.RemoveAt(i)
                        Exit For
                    End If
                Next
            Next

            For i = 0 To commonCjo.Count - 1
                polyC *= x - New Polynomial(commonCjo(i))
            Next
        Catch ex As Exception
            Throw
        End Try
        Return polyC
    End Function
    Public Shared Function LCM_GCD(a() As Double, Optional ByRef sDetail As String = "") As Double()
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
                    Throw New Exception(Msg8.num(9)) ' Argument(s) not valid.
                End If
                For j As Int64 = 0 To Rational.vPrime.Length - 1
                    Dim prime As Double = Rational.vPrime(j)
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
            If G10.detail Then
                'Dim ni As Globalization.NumberFormatInfo = MathGlobal8.us.NumberFormat.Clone
                For i As Int64 = 0 To cols - 1
                    Dim e1 As String = String.Empty
                    e1 += a(i).ToString + " = "
                    For j = 0 To iv
                        If vExp(i)(j) Then
                            If Len(e1) Then
                                e1 += " * "
                            End If
                            If vExp(i)(j) = 1 Then
                                e1 += Rational.vPrime(j).ToString
                            Else
                                e1 += Rational.vPrime(j).ToString + "^" + vExp(i)(j).ToString
                            End If
                        End If
                    Next
                    G10.sDetail += e1
                Next
            End If
            For j = 0 To iv
                If vMax(j) Then
                    lcm *= Rational.vPrime(j) ^ vMax(j)
                End If
                If vMin(j) Then
                    gcd *= Rational.vPrime(j) ^ vMin(j)
                End If
            Next
            dbRet(0) = gcd
            dbRet(1) = lcm
        Catch ex As Exception
            Throw
        End Try
        Return dbRet
    End Function
    Public Shared Function getRootsMultiplicity(vRoots() As Complex) As Int64()
        Dim vmultiplicity(-1) As Int64, ivM As Int64 = 0
        Try
            Dim vPt(-1) As Complex, iPt As Int64 = 0
            ' Find pA's denominator roots:
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
            Throw
        End Try
        Return vmultiplicity
    End Function

    Public Shared Function opLCM(pA As Polynomial, pB As Polynomial) As Polynomial
        Dim LCM As New Polynomial(1.0)
        Try

            Dim i, j, k As Int64
            Dim An As Double = pA.LeadingTerm.cf.ToDouble
            pA /= New Polynomial(An) ' Polynomial.opNormalize(pA)
            For Each t As Term In pA.resto
                If Not t.cf.IsDouble Then
                    Throw New Exception(String.Format(Msg8.num(83), pA.ToString))
                End If
            Next
            If pA.GetVars.Length > 1 Then
                Return Nothing
            End If
            If pB.GetVars.Length > 1 Then
                Return Nothing
            End If
            Dim Bn As Double = pB.LeadingTerm.cf.ToDouble
            pB /= New Polynomial(Bn) ' Polynomial.opNormalize(pB)
            For Each t As Term In pB.resto
                If Not t.cf.IsDouble Then
                    Throw New Exception(String.Format(Msg8.num(83), pA.ToString))
                End If
            Next

            Dim x As New Polynomial(pA.GetVars(0), 1) ' = Polynomial.GetPolynomial(pA.var(0))
            Dim rootsA() As Complex = pA.RootsNumerator
            Dim rootsB() As Complex = pB.RootsNumerator
            Dim lA As New List(Of Complex)
            Dim lB As New List(Of Complex)
            For i = 0 To rootsA.Length - 1
                If i = 0 OrElse Not (rootsA(i) - rootsA(i - 1)).IsZero Then
                    lA.Add(rootsA(i))
                End If
            Next
            For i = 0 To rootsB.Length - 1
                If i = 0 OrElse Not (rootsB(i) - rootsB(i - 1)).IsZero Then
                    lB.Add(rootsB(i))
                End If
            Next
            Dim multA() As Int64 = getRootsMultiplicity(rootsA)
            Dim multB() As Int64 = getRootsMultiplicity(rootsB)
            For i = 0 To lA.Count - 1
                For j = 0 To lB.Count - 1
                    If multB(j) > 0 Then
                        If (lA(i) - lB(j)).IsZero Then
                            Dim max As Int64 = Math.Max(multA(i), multB(j))
                            For k = 1 To max
                                LCM *= (x - New Polynomial(lA(i)))
                            Next
                            multA(i) = -1 ' mark as treated
                            multB(j) = -1 ' mark as treated
                            Exit For
                        End If
                    End If
                Next
            Next
            For i = 0 To lA.Count - 1
                If multA(i) > 0 Then
                    For k = 1 To multA(i)
                        LCM *= (x - New Polynomial(lA(i)))
                    Next
                End If
            Next
            For i = 0 To lB.Count - 1
                If multB(i) > 0 Then
                    For k = 1 To multB(i)
                        LCM *= (x - New Polynomial(lB(i)))
                    Next
                End If
            Next
            ' obtener el LCM de An y Bn:
            Dim signA As Int32 = Math.Sign(An)
            An = Math.Abs(An)
            Dim signB As Int32 = Math.Sign(Bn)
            Bn = Math.Abs(Bn)
            Dim vLCM_GCD() As Double = ExprMatrix.LCM_GCD(New Double() {An, Bn})
            LCM *= New Polynomial(vLCM_GCD(1))
        Catch ex As Exception
            Throw
        End Try
        Return LCM
    End Function

End Class
Public Class Term
    Public f As New List(Of Factor)
    Public _cf As Complex
    Public Shared Infinity As New Term("∞", 1)
    Public Shared mnInfinity As Term = -Infinity
    Public Shared Indeterminate As New Term("Indeterminate", 1)
    Public Sub New(dbl As Double)
        cf = New Complex(dbl)
    End Sub
    Public Sub New(cplx As Complex)
        cf = New Complex(cplx)
    End Sub
    Public Sub New(t As Term)
        cf = New Complex(t.cf)
        For i = 0 To t.f.Count - 1
            f.Add(Factor.CopyFrom(t.f(i)))
        Next
    End Sub
    Public Sub New(f As Factor)
        cf = Complex.one
        Me.f.Add(f)
    End Sub
    Public Sub New(var As String, exp As Int32)
        cf = Complex.one
        f.Add(New Factor(var, exp))
    End Sub
    Public Shared Function CopyFrom(tA As Term) As Term
        Dim tC As New Term(tA.cf)
        For i As Int32 = 0 To tA.f.Count - 1
            tC.f.Add(Factor.CopyFrom(tA.f(i)))
        Next
        Return tC
    End Function
    Public Property cf As Complex
        Get
            Return _cf
        End Get
        Set(value As Complex)
            If G10.mMod < 2 Then
                _cf = value
            Else
                If Not value.IsDouble Then
                    Throw New Exception(Msg10.Num(112))
                End If
                Dim dbValue As Double = value.ToDouble
                If dbValue <> Math.Floor(dbValue) Then
                    _cf = value
                    Exit Property
                End If
                dbValue = dbValue Mod G10.mMod
                If dbValue < 0 Then
                    dbValue += G10.mMod
                End If
                _cf = New Complex(dbValue)
            End If
        End Set
    End Property
    Public Function IsReal() As Boolean
        For Each f1 As Factor In f
            If Not f1.exp = 0 Then Return False
        Next
        Return cf.IsDouble
    End Function
    Public Function ToDouble() As Double
        Return cf.ToDouble
    End Function
    Public Function Iscomplex() As Boolean
        If cf.IsZero Then Return True
        For Each f1 As Factor In f
            If Not f1.exp = 0 Then Return False
        Next
        Return True
    End Function
    Public Function ToComplex() As Complex
        Return cf
    End Function
    Public Sub OpChgSign()
        cf = -cf
    End Sub
    Public Function GetVars() As String()
        Dim vars As New List(Of String)
        Try
            For i As Int32 = 0 To Me.f.Count - 1
                vars.Add(f(i).var)
            Next
        Catch ex As Exception
            Throw
        End Try
        Return vars.ToArray()
    End Function
    Public Shared Function Degree(t As Term) As Double
        Dim d As Double = 0.0
        Try
            For i = 0 To t.f.Count - 1
                d += Factor.Degree(t.f(i))
                Exit For
            Next
        Catch ex As Exception
            Throw
        End Try
        Return d
    End Function
    Public Shared Operator =(tA As Term, tB As Term) As Boolean
        Try
            Dim tc As New Polynomial.TermComparer
            If tA.f.Count <> tB.f.Count Then Exit Try
            For i As Int32 = 0 To tA.f.Count - 1
                If tA.f(i) <> tB.f(i) Then Exit Try
            Next
            Return True
        Catch ex As Exception
            Throw
        End Try
        Return False
    End Operator
    Public Shared Operator <>(tA As Term, tB As Term) As Boolean
        Return Not (tA = tB)
    End Operator
    Public Shared Operator -(tA As Term) As Term
        Dim tC As New Term(tA)
        tC.OpChgSign()
        Return tC
    End Operator
    Public Shared Operator *(tA As Term, tB As Term) As Term
        Dim tC As New Term(tA)
        Dim tD As New Term(tB)
        Try
            tC.cf *= tB.cf
            For i As Int32 = tC.f.Count - 1 To 0 Step -1
                For j As Int32 = tD.f.Count - 1 To 0 Step -1
                    If tC.f(i).var = tD.f(j).var Then
                        Dim modulo As Int32 = G10.mMod
                        G10.mMod = 0
                        tC.f(i).exp += tD.f(j).exp
                        G10.mMod = modulo
                        tD.f.RemoveAt(j)
                        If tC.f(i).exp = 0 Then
                            tC.f.RemoveAt(i)
                        End If
                        Exit For
                    End If
                Next
            Next
            tC.f.AddRange(tD.f)
            If G10.detail AndAlso Not (tB.IsReal AndAlso Math.Abs(tB.ToDouble) = 1) Then
                Dim s As String = ""
                s += "(" + tA.ToString() + ")*(" + tB.ToString + ")" + vbCrLf
                s += tC.ToString + vbCrLf
                G10.sDetail += s
            End If
        Catch ex As Exception
            Throw
        End Try
        Return tC
    End Operator
    Public Shared Operator /(tA As Term, tB As Term) As Term
        Dim tC As New Term(tA)
        Dim tD As New Term(tB)
        Try
            tC.cf /= tB.cf
            For i As Int32 = tC.f.Count - 1 To 0 Step -1
                For j As Int32 = tD.f.Count - 1 To 0 Step -1
                    If tC.f(i).var = tD.f(j).var Then
                        tC.f(i).exp -= tD.f(j).exp
                        tD.f.RemoveAt(j)
                        If tC.f(i).exp = 0 Then
                            tC.f.RemoveAt(i)
                        End If
                        Exit For
                    End If
                Next
            Next
            For i = 0 To tD.f.Count - 1
                Dim f As New Factor(tD.f(i).var, -tD.f(i).exp)
                tC.f.Add(f)
            Next
            If G10.detail Then
                Dim s As String = ""
                s += "(" + tA.ToString() + ")/(" + tB.ToString + ")" + vbCrLf
                s += tC.ToString + vbCrLf
                G10.detail = G10.detail
            End If
        Catch ex As Exception
            Throw
        End Try
        Return tC
    End Operator
    Public Overrides Function ToString() As String
        Return ToString(G10.nDec, G10.sImg, G10.CI)
    End Function
    Public Overloads Function ToString(numDecimals As Int32, sImg As String, cultureInfo As Globalization.CultureInfo)
        Dim sb As New StringBuilder
        Try
            If cf.IsZero Then Exit Try
            Dim sCf As String = cf.ToStringComplex(numDecimals, sImg, cultureInfo)
            Dim i As Int32
            For i = 0 To Me.f.Count - 1
                sb.Append(f(i).ToString(numDecimals, sImg, cultureInfo))
                If i < f.Count - 1 Then sb.Append("*")
            Next
            If Left(sb.ToString, 2) = "0*" Then sb.Clear()
            Dim scf2 As String = Regex.Replace(sCf, "&h|&o", "")
            If scf2 = "1" AndAlso sb.Length Then
            ElseIf scf2 = "-1" AndAlso sb.Length Then
                sb.Insert(0, "-")
            ElseIf sb.Length Then
                If Len(sCf) > 1 AndAlso Regex.IsMatch(Mid(sCf, 2), "[-+]") Then
                    sb.Insert(0, "(" + sCf + ")*")
                Else
                    sb.Insert(0, sCf + "*")
                End If
            Else
                sb.Append(sCf)
            End If
        Catch ex As Exception
            Throw
        End Try
        Return sb.ToString
    End Function
End Class

Public Class Factor
    Public var As String
    Public exp As Double = 1
    Public Sub New(variable As String)
        var = variable
    End Sub
    Public Sub New(variable As String, exponent As Double)
        var = variable
        exp = exponent
    End Sub
    Public Shared Function CopyFrom(fA As Factor) As Factor
        Dim fC As New Factor(fA.var)
        fC.exp = fA.exp
        Return fC
    End Function
    Public Shared Function Degree(f As Factor) As Double
        Return f.exp
    End Function
    Public Shared Operator =(fA As Factor, fB As Factor) As Boolean
        Dim r As Boolean = False
        Try
            If fA.var <> fB.var OrElse
                    fA.exp <> fB.exp Then
                Exit Try
            End If
            r = True
        Catch ex As Exception
            Throw
        End Try
        Return r
    End Operator
    Public Shared Operator <>(fA As Factor, fB As Factor) As Boolean
        Return Not (fA = fB)
    End Operator
    Public Shared Operator ^(fA As Factor, dbl As Double) As Factor
        Dim fC As Factor = Factor.CopyFrom(fA)
        Try
            fC.exp *= dbl
        Catch ex As Exception

        End Try
        Return fC
    End Operator

    Public Overrides Function ToString() As String
        Return ToString(15, G10.sImg, New Globalization.CultureInfo("en-US"))
    End Function
    Public Overloads Function ToString(numDecimals As Int32, sImg As String, cultureInfo As Globalization.CultureInfo)
        Dim sb As New StringBuilder
        Try
            sb.Append(var)
            If exp <> 1 Then
                sb.Append("^" + exp.ToString)
            End If
        Catch ex As Exception
            Throw
        End Try
        Return sb.ToString
    End Function
End Class
Public Class FactorComparer
    Implements IComparer
    Public Function Compare(x1 As Object, y1 As Object) As Integer Implements IComparer.Compare
        Dim x As Factor = CType(x1, Factor)
        Dim y As Factor = CType(y1, Factor)
        Dim sb1 As New StringBuilder
        Dim sb2 As New StringBuilder
        Try
            sb1.Append(x.var)
            sb1.Append(String.Format("{0:000000}", 10 ^ 5 - x.exp))
            If sb1.Length = 0 Then
                sb1.Append(ChrW(255))
            Else
                sb1.Append("}")
            End If
            sb2.Append(y.var)
            sb2.Append(String.Format("{0:000000}", 10 ^ 5 - y.exp))
            If sb2.Length = 0 Then
                sb2.Append(ChrW(255))
            Else
                sb2.Append("}")
            End If
        Catch ex As Exception
            Throw New NotImplementedException()
        End Try
        If sb1.ToString < sb2.ToString Then
            Return -1
        ElseIf sb1.ToString > sb2.ToString Then
            Return 1
        End If
        Return 0
    End Function
End Class
