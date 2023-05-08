Imports System.Text.RegularExpressions
Imports System.Text
Imports System.Globalization

Public Class Expression

    Public t As New List(Of ExprTerm)
    Public resto As List(Of ExprTerm)
    Public divisor As List(Of ExprTerm)
    Public IsEquation As Boolean
    Public Shared _bReduce As Boolean = True
    Public Shared Property bReduce As Boolean
        Get
            Return _bReduce
        End Get
        Set(value As Boolean)
            _bReduce = value
        End Set
    End Property
    Public Sub New()
    End Sub
    Public Sub New(dbl As Double)
        t.Add(New ExprTerm(dbl))
    End Sub
    Public Sub New(cplx As Complex)
        t.Add(New ExprTerm(cplx))
    End Sub
    Public Sub New(t As ExprTerm)
        Me.t.Add(t)
    End Sub
    Public Sub New(tList As List(Of ExprTerm))
        Me.t.AddRange(tList)
    End Sub
    Public Sub New(var As String, exp As Int32)
        t.Add(New ExprTerm(var, exp))
    End Sub
    Public Sub New(var As String, exp As Expression)
        Dim f As New ExprFactor(var, exp)
        t.Add(New ExprTerm(f))
    End Sub
    Public Sub New(var As String, exp As Expression, argument As Expression)
        If argument Is Nothing Then
            Dim expr As New Expression(var, exp)
            CopyExprToMe(expr)
        Else
            Dim f As ExprFactor
            f = New ExprFactor(var, exp, argument)
            t.Add(New ExprTerm(f))
        End If
    End Sub
    Public Sub New(expr As Expression)
        Try
            If expr IsNot Nothing Then
                For i As Int32 = 0 To expr.t.Count - 1
                    t.Add(New ExprTerm(expr.t(i)))
                Next
                resto = New List(Of ExprTerm)
                divisor = New List(Of ExprTerm)
                If expr.resto IsNot Nothing Then
                    resto.AddRange(expr.resto)
                    divisor.AddRange(expr.divisor)
                End If
            Else
                Throw New Exception(Msg8.num(13))
            End If
            IsEquation = expr.IsEquation
        Catch ex As Exception
            Throw
        End Try
    End Sub
    Public Sub New(lstT As List(Of Term))
        Try
            For i As Int32 = 0 To lstT.Count - 1
                If Not lstT(i).cf.IsZero Then
                    t.Add(New ExprTerm(lstT(i)))
                End If
            Next
        Catch ex As Exception

        End Try
    End Sub
    Public Sub New(polyA As Polynomial)
        Try
            resto = New List(Of ExprTerm)
            divisor = New List(Of ExprTerm)
            For i = 0 To polyA.resto.Count - 1
                resto.Add(New ExprTerm(polyA.resto(i)))
            Next
            For i = 0 To polyA.divisor.Count - 1
                divisor.Add(New ExprTerm(polyA.divisor(i)))
            Next
        Catch ex As Exception
            Throw
        End Try
    End Sub
    Public Sub CopyExprToMe(expr As Expression)
        Try
            t.Clear()
            t.AddRange(expr.t)
            If expr.resto IsNot Nothing Then
                resto = New List(Of ExprTerm)
                divisor = New List(Of ExprTerm)
                resto.AddRange(expr.resto)
                divisor.AddRange(expr.divisor)
            Else
                resto = Nothing
                divisor = Nothing
            End If
        Catch ex As Exception

        End Try
    End Sub
    Public Shared Function AddArg(var As String, arg As Expression) As Expression
        Dim eC As New Expression(var, 1)
        eC.t(0).f(0).args.Add(arg)
        Return eC
    End Function
    Public Shared Function CopyFrom(eA As Expression) As Expression
        Dim eC As New Expression
        Try
            For i As Int32 = 0 To eA.t.Count - 1
                eC.t.Add(New ExprTerm(eA.t(i)))
            Next
            If eA.resto IsNot Nothing Then
                eC.resto = New List(Of ExprTerm)
                eC.resto.AddRange(eA.resto)
                eC.divisor = New List(Of ExprTerm)
                eC.divisor.AddRange(eA.divisor)
            End If
        Catch ex As Exception
            Throw
        End Try
        Return eC
    End Function
    Public Sub OpChgSign()
        For i As Int32 = 0 To t.Count - 1
            t(i).OpChgSign()
        Next
        If resto IsNot Nothing Then
            For i As Int32 = 0 To resto.Count - 1
                resto(i).OpChgSign()
            Next
        End If
    End Sub
    Public Function IsZero() As Boolean
        Return IsDouble() AndAlso ToDouble() = 0.0
    End Function
    Public Shared Function IsDouble(lst As List(Of ExprTerm)) As Boolean
        If lst IsNot Nothing Then
            For Each t As ExprTerm In lst
                If Not t.IsDouble Then Return False
            Next
        End If
        Return True
    End Function
    Public Shared Function IsComplex(lst As List(Of ExprTerm)) As Boolean
        If lst IsNot Nothing Then
            For Each t As ExprTerm In lst
                If Not t.Iscomplex Then Return False
            Next
        End If
        Return True
    End Function
    Public Function IsDouble() As Boolean
        If Not IsDouble(t) Then Return False
        If Not IsDouble(resto) Then Return False
        If Not IsDouble(divisor) Then Return False
        Return True
    End Function
    Public Function IsComplex() As Boolean
        If Not IsComplex(t) Then Return False
        If Not IsComplex(resto) Then Return False
        If Not IsComplex(divisor) Then Return False
        Return True
    End Function
    Public Function ToDouble() As Double
        If resto IsNot Nothing AndAlso resto.Count Then
            Return resto(0).ToDouble / divisor(0).ToDouble
        End If
        If t.Count = 0 Then Return 0.0
        Return t(0).ToDouble
    End Function

    Public Shared Function IsPolynomial(lst As List(Of ExprTerm)) As Boolean
        For i = 0 To lst.Count - 1
            If Not lst(i).IsPolynomial Then Return False
        Next
        Return True
    End Function

    Public Function IsPolynomial() As Boolean
        If Not IsPolynomial(t) Then Return False
        If resto IsNot Nothing Then
            If Not IsPolynomial(resto) Then Return False
            If Not IsPolynomial(divisor) Then Return False
        End If
        Return True
    End Function
    Public Function ToPolynomial() As Polynomial
        Dim polyC As New Polynomial
        Try
            ' polyC = ent + num/den = (ent+den + num)/den
            Dim ent As New List(Of Term)
            For i As Int32 = Me.t.Count - 1 To 0 Step -1
                Dim t As New Term(Me.t(i).Cf)
                For j As Int32 = 0 To Me.t(i).f.Count - 1
                    t.f.Add(New Factor(Me.t(i).f(j).var, Me.t(i).f(j).exp.ToDouble))
                Next
                If Not t.cf.IsZero Then
                    ent.Add(t)
                Else
                    Me.t.RemoveAt(i)
                End If
            Next
            Dim lNum As New List(Of Term)
            Dim lden As New List(Of Term)
            If resto IsNot Nothing Then
                If resto.Count = 0 Then
                    resto = Nothing
                    divisor = Nothing
                Else
                    For i As Int32 = resto.Count - 1 To 0 Step -1
                        If Not resto(i).Cf.IsZero Then
                            Dim t As New Term(resto(i).Cf)
                            For j As Int32 = 0 To resto(i).f.Count - 1
                                t.f.Add(New Factor(resto(i).f(j).var, resto(i).f(j).exp.ToDouble))
                            Next
                            lNum.Add(t)
                        Else
                            resto.RemoveAt(i)
                        End If
                    Next
                    For i As Int32 = divisor.Count - 1 To 0 Step -1
                        If Not divisor(i).Cf.IsZero Then
                            Dim t As New Term(divisor(i).Cf)
                            For j As Int32 = 0 To divisor(i).f.Count - 1
                                t.f.Add(New Factor(divisor(i).f(j).var, divisor(i).f(j).exp.ToDouble))
                            Next
                            lden.Add(t)
                        Else
                            divisor.RemoveAt(i)
                        End If
                    Next
                    ent = Polynomial.Mult(ent, lden)
                    ent = Polynomial.Add(ent, lNum)
                End If
            End If
            polyC = New Polynomial(ent)
            If lden.Count Then
                polyC.divisor = lden
            End If
            polyC.Reduce()
        Catch ex As Exception
            Throw
        End Try
        Return polyC
    End Function
    Public Shared Function ToPolynomial(lstT As List(Of ExprTerm)) As Polynomial
        Dim polyC As New Polynomial
        Try
            Dim lst As New List(Of Term)
            For i As Int32 = 0 To lstT.Count - 1
                Dim t As New Term(lstT(i).Cf)
                For j As Int32 = 0 To lstT(i).f.Count - 1
                    t.f.Add(New Factor(lstT(i).f(j).var, lstT(i).f(j).exp.ToDouble))
                Next
                lst.Add(t)
            Next
            polyC = New Polynomial(lst)
        Catch ex As Exception
            Throw
        End Try
        Return polyC
    End Function
    Public Function ToComplex() As Complex
        Dim cjo As New Complex(0.0)
        Dim i As Int32
        For i = t.Count - 1 To 0 Step -1
            cjo += t(i).ToComplex
        Next
        If resto IsNot Nothing Then
            Dim cjoResto As New Complex(0.0)
            For i = resto.Count - 1 To 0 Step -1
                cjoResto += resto(i).ToComplex
            Next
            Dim cjoDiv As New Complex(0.0)
            If divisor.Count = 0 Then
                divisor.Add(New ExprTerm(1.0))
            End If
            For i = divisor.Count - 1 To 0 Step -1
                cjoDiv += divisor(i).ToComplex
            Next
            cjo += cjoResto / cjoDiv
        End If
        Return cjo
    End Function
    Public Sub Get_Fn_Exponent_Argument(ByRef sFn As String, ByRef exp As Expression, ByRef arg As List(Of ExprTerm))
        Try
            sFn = ""
            exp = Nothing
            arg = Nothing
            If t.Count > 1 Then Exit Try
            If t.Count AndAlso resto.Count Then Exit Try
            Dim lstExpr As New List(Of ExprTerm)
            If t.Count = 1 Then
                If t.Count > 1 Then Exit Try
                For i As Int32 = 0 To t.Count - 1
                    lstExpr.Add(New ExprTerm(t(i)))
                Next
            Else
                Dim eDiv As New Expression(divisor)
                If Not eDiv.IsComplex Then Exit Try
                If resto.Count > 1 Then Exit Try
                For i As Int32 = 0 To resto.Count - 1
                    lstExpr.Add(New ExprTerm(resto(i)))
                Next
            End If
            If lstExpr(0).f.Count <> 1 Then Exit Try
            If lstExpr(0).f(0).var.GetType <> GetType(String) Then Exit Try
            Dim m As Match = Regex.Match(lstExpr(0).f(0).var, G10.sImFn)
            If Not m.Success Then Exit Try
            sFn = m.Value
            exp = New Expression(lstExpr(0).f(0).exp)
            If lstExpr(0).f(0).args.Count Then
                arg = New List(Of ExprTerm)
                Dim args() As Expression = lstExpr(0).f(0).args.ToArray
                If args.Count <> 1 OrElse args(0).t.Count <> 1 OrElse args(0).t(0).f.Count _
                OrElse args(0).resto.Count Then
                    sFn = "" : exp = Nothing : arg = Nothing
                    Exit Try
                End If
                arg.Add(New ExprTerm(args(0).t(0)))
            End If
        Catch ex As Exception
            Throw
        End Try
    End Sub
    Public Shared Function GetVarsOfListOfExprTerm(lst As List(Of ExprTerm)) As String()
        Dim vars As New List(Of String)
        Try
            For i As Int32 = 0 To lst.Count - 1
                Dim vStr() As String = lst(i).GetVars
                For j As Int32 = 0 To vStr.Length - 1
                    If Not vars.Contains(vStr(j)) Then
                        vars.Add(vStr(j))
                    End If
                Next
            Next
        Catch ex As Exception

        End Try
        Return vars.ToArray
    End Function
    Public Function GetVars() As String()
        Dim vars As New List(Of String)
        Try
            vars.AddRange(GetVarsOfListOfExprTerm(t))
            If resto IsNot Nothing Then
                Dim lNum As New List(Of String)
                lNum.AddRange(GetVarsNumerator())
                For j As Int32 = 0 To lNum.Count - 1
                    If Not vars.Contains(lNum(j)) Then
                        vars.Add(lNum(j))
                    End If
                Next
                Dim lDen As New List(Of String)
                lDen.AddRange(GetVarsDenominator())
                For j As Int32 = 0 To lDen.Count - 1
                    If Not vars.Contains(lDen(j)) Then
                        vars.Add(lDen(j))
                    End If
                Next
            End If
            vars.Sort()
            For i = vars.Count - 1 To 1 Step -1
                If vars(i) = vars(i - 1) Then
                    vars.RemoveAt(i)
                End If
            Next
        Catch ex As Exception
            Throw
        End Try
        Return vars.ToArray()
    End Function
    Public Function ToVStringAllExprTerms(includeCoefficients As Boolean, sImg As String, CI As Globalization.CultureInfo) As String()
        Dim r() As String
        Dim tc As New ExprTermComparer
        Try
            Dim v() As ExprTerm = t.ToArray
            Array.Sort(v, tc)
            ReDim r(v.Length) ' reserve r(0) for comments
            For i As Int32 = 0 To v.Length - 1
                Dim t As ExprTerm = ExprTerm.CopyFrom(v(i))
                If includeCoefficients OrElse t.f.Count = 0 Then
                    r(i + 1) = t.Cf.ToStringComplex(15, sImg, CI)
                Else
                    r(i + 1) = ""
                End If
                For j As Int32 = 0 To t.f.Count - 1
                    For k As Int32 = 0 To t.f(j).var.Count - 1
                        If Len(r(i + 1)) Then r(i + 1) += "*"
                        r(i + 1) += t.f(j).var
                        If Not (t.f(j).exp.IsDouble AndAlso t.f(j).exp.ToDouble = 1) Then
                            r(i + 1) += "^" + t.f(j).exp.ToString
                        End If
                    Next
                    If includeCoefficients Then
                        If Left(r(i + 1), 3) = "-1*" Then
                            r(i + 1) = "-" + Mid(r(i + 1), 4)
                        ElseIf Left(r(i + 1), 2) = "1*" Then
                            r(i + 1) = Mid(r(i + 1), 3)
                        End If
                    End If
                Next
            Next
        Catch ex As Exception
            Throw
        End Try
        Return r
    End Function
    Public Function LeadingExprTerm() As ExprTerm
        Dim LT As New ExprTerm(Complex.zero)
        Dim tc As New ExprTermComparer
        Try
            Dim v() As ExprTerm = t.ToArray
            Array.Sort(v, tc)
            LT.f.AddRange(v(0).f)
            LT.Cf = New Complex(v(0).Cf)
        Catch ex As Exception
            Throw
        End Try
        Return LT
    End Function
    Public Function Degree() As Int32
        Dim t As ExprTerm = LeadingExprTerm()
        Return ExprTerm.Degree(t)
    End Function
    Public Sub SortExprTerms(Optional Reverse As Boolean = False)
        Dim tc As New ExprTermComparer
        Dim fc As New ExprFactorComparer(False)
        Dim v() As ExprTerm = t.ToArray
        Array.Sort(v, tc)
        If Reverse Then Array.Reverse(v)
        For i As Int32 = 0 To v.Length - 1
            Dim sf() As ExprFactor = v(i).f.ToArray
            Array.Sort(sf, fc)
            v(i).f.Clear()
            v(i).f.AddRange(sf)
        Next
        t.Clear()
        t.AddRange(v)
        For i = t.Count - 1 To 0 Step -1
            If t(i).Cf.IsZero Then
                t.RemoveAt(i)
            End If
        Next
        If t.Count = 0 Then t.Add(New ExprTerm(0.0))
    End Sub

    Public Function LowerExprTerm() As ExprTerm
        Dim LT As New ExprTerm(Complex.zero)
        Dim tc As New ExprTermComparer
        Try
            Dim v() As ExprTerm = t.ToArray
            Array.Sort(v, tc)
            Array.Reverse(v)
            LT.f.AddRange(v(0).f)
            LT.Cf = New Complex(v(0).Cf)
        Catch ex As Exception
            Throw
        End Try
        Return LT
    End Function
    Public Shared Operator -(eA As Expression) As Expression
        Dim eC As Expression = Expression.CopyFrom(eA)
        eC.OpChgSign()
        Return eC
    End Operator
    Public Shared Operator -(eA As Expression, eB As Expression)
        Dim eC As Expression = Expression.CopyFrom(eB)
        Try
            If eA.IsComplex AndAlso eB.IsComplex Then
                Return New Expression(eA.ToComplex - eB.ToComplex)
            End If
            eC.OpChgSign()
            eC = eA + eC
        Catch ex As Exception
            Throw
        End Try
        Return eC
    End Operator
    Public Shared Function Add(lstA As List(Of ExprTerm), lstB As List(Of ExprTerm))
        Dim eC As New List(Of ExprTerm)
        Try
            eC.AddRange(lstA)
            eC.AddRange(lstB)
        Catch ex As Exception
            Throw
        End Try
        Return eC
    End Function
    Public Shared Operator +(eA As Expression, eB As Expression) As Expression
        If eA.IsComplex AndAlso eB.IsComplex Then
            Return New Expression(eA.ToComplex + eB.ToComplex)
        End If
        Return Add(eA, eB)
    End Operator
    Public Shared Function MultET(eAT As List(Of ExprTerm), eBT As List(Of ExprTerm)) As List(Of ExprTerm)
        Dim ecT As New List(Of ExprTerm)
        Try
            For i = 0 To eAT.Count - 1
                For j = 0 To eBT.Count - 1
                    If eAT(i) = eBT(j) Then
                        Dim t As New ExprTerm(eAT(i))
                        t.Cf *= eBT(j).Cf
                        For k = 0 To eAT(i).f.Count - 1
                            t.f(k).exp += eBT(j).f(k).exp
                        Next
                        ecT.Add(t)
                    Else
                        ecT.Add(New ExprTerm(eAT(i) * eBT(j)))
                    End If
                Next
            Next
        Catch ex As Exception
            Throw
        End Try
        Return ecT
    End Function
    Public Shared Function AddET(eAT As List(Of ExprTerm), eBT As List(Of ExprTerm)) As List(Of ExprTerm)
        Dim eCT As New List(Of ExprTerm)
        Try
            For i As Int32 = 0 To eAT.Count - 1
                eCT.Add(New ExprTerm(eAT(i)))
            Next
            For i As Int32 = 0 To eBT.Count - 1
                eCT.Add(New ExprTerm(eBT(i)))
            Next
            Dim ec As New Expression(eCT)
            ec.Reduce()
            eCT = ec.t
        Catch ex As Exception
            Throw
        End Try
        Return eCT
    End Function

    Private Shared Function Add(eA As Expression, eB As Expression) As Expression
        Dim eC As Expression = Expression.CopyFrom(eA)
        Try
            If eA.IsPolynomial AndAlso eB.IsPolynomial Then
                eC = New Expression(eA.ToPolynomial + eB.ToPolynomial)
                Exit Try
            End If
            For i As Int32 = 0 To eB.t.Count - 1
                eC.t.Add(New ExprTerm(eB.t(i)))
            Next
            If eA.resto IsNot Nothing AndAlso eB.resto IsNot Nothing Then

                ' (A + Na/Da) + (B + Nb/Db) = (A+B) + (Na*DB + Nb*Da)/(Da * Db)

                Dim Na As List(Of ExprTerm) = eA.resto
                Dim Nb As List(Of ExprTerm) = eB.resto
                Dim Da As List(Of ExprTerm) = eA.divisor
                Dim Db As List(Of ExprTerm) = eB.divisor
                eC.resto = AddET(MultET(Na, Db), MultET(Nb, Da))
                eC.divisor = MultET(Da, Db)
            ElseIf eB.resto IsNot Nothing Then
                eC.resto = New List(Of ExprTerm)
                eC.divisor = New List(Of ExprTerm)
                For i As Int32 = 0 To eB.resto.Count - 1
                    eC.resto.Add(New ExprTerm(eB.resto(i)))
                Next
                For i As Int32 = 0 To eB.divisor.Count - 1
                    eC.divisor.Add(New ExprTerm(eB.divisor(i)))
                Next
            End If
            eC.Reduce()
        Catch ex As Exception
            Throw
        End Try
        If G10.detail Then
            G10.sDetail = eC.ToString
        End If
        Return eC
    End Function
    Public Shared Operator *(eA As Expression, eB As Expression) As Expression
        If eA.IsComplex AndAlso eB.IsComplex Then
            Return New Expression(eA.ToComplex * eB.ToComplex)
        ElseIf eA.IsPolynomial AndAlso eB.IsPolynomial Then
            Return New Expression(eA.ToPolynomial * eB.ToPolynomial)
        End If
        Return Mult(eA, eB)
    End Operator
    Public Shared Function Mult(eA As List(Of ExprTerm), eB As List(Of ExprTerm)) As List(Of ExprTerm)
        Dim eC As New List(Of ExprTerm)
        Try
            For i = 0 To eA.Count - 1
                For j = 0 To eB.Count - 1
                    If eA(i) = eB(j) Then
                        Dim t As New ExprTerm(eA(i))
                        t.Cf *= eB(j).Cf
                        For k = 0 To eA(i).f.Count - 1
                            If eA(i).f(k).var = "exp" AndAlso eA(i).f(k).args.Count Then
                                t.f(k).args(0) += eB(j).f(k).args(0)
                            Else
                                t.f(k).exp += New Expression(eB(j).f(k).exp)
                            End If
                        Next
                        eC.Add(t)
                    Else
                        eC.Add(New ExprTerm(eA(i) * eB(j)))
                    End If
                    If G10.detail Then
                        G10.sDetail += eA(i).sDetail
                    End If
                Next
            Next
        Catch ex As Exception
            Throw
        End Try
        Return eC
    End Function
    Public Shared Function Mult(eA As Expression, eB As Expression) As Expression
        Dim eC As New Expression
        Try
            eC.t.AddRange(Mult(eA.t, eB.t)) ' A*B

            If eA.resto Is Nothing AndAlso eB.resto IsNot Nothing Then
                ' A * (B + rB / dB)
                ' = A*B + ( A*rB/dB)
                eC.resto = Mult(eA.t, eB.resto)
                eC.divisor = New List(Of ExprTerm)
                For i As Int32 = 0 To eB.divisor.Count - 1
                    eC.divisor.Add(New ExprTerm(eB.divisor(i)))
                Next
            ElseIf eA.resto IsNot Nothing AndAlso eB.resto Is Nothing Then
                ' ( A + rA/dA) * B
                ' = A*B + (B*rA/dA)
                eC.resto = Mult(eB.t, eA.resto)
                eC.divisor = New List(Of ExprTerm)
                For i As Int32 = 0 To eA.divisor.Count - 1
                    eC.divisor.Add(New ExprTerm(eA.divisor(i)))
                Next
            ElseIf eA.resto IsNot Nothing AndAlso eB.resto IsNot Nothing Then
                ' (A + rA/dA) * (B + rB/dB) = ...
                ' = (A*dA + rA)/dA * (B*dB + rB/dB)
                ' = (A*da*B*dB + A*rB + rA*B*dB + rA*rB)/(dA*dB)
                ' = (eC.t*dA*dB * A*rB +rA*B*dB + rA*rB)/(dA*dB)
                Dim sum1 As List(Of ExprTerm) = Mult(Mult(eC.t, eA.divisor), eB.divisor)
                Dim sum2 As List(Of ExprTerm) = Mult(eA.t, eB.resto)
                Dim sum3 As List(Of ExprTerm) = Mult(Mult(eA.resto, eB.t), eB.divisor)
                Dim sum4 As List(Of ExprTerm) = Mult(eA.resto, eB.resto)
                Dim div As List(Of ExprTerm) = Mult(eA.divisor, eB.divisor)
                eC.resto = Add(Add(Add(sum1, sum2), sum3), sum4)
                eC.divisor = div
                'Dim ent1 As New List(Of ExprTerm)
                'ent1 = Mult(eA.t, eA.divisor)
                'ent1 = Mult(ent1, eB.resto)
                'Dim ent2 As New List(Of ExprTerm)
                'ent2 = Mult(eB.t, eA.resto)
                'ent2 = Mult(ent2, eB.divisor)
                'Dim ent3 As New List(Of ExprTerm)
                'ent3 = Mult(eA.resto, eB.resto)
                'ent3 = Mult(ent3, eB.resto)

                'ent1 = Add(ent1, ent2)
                'ent1 = Add(ent1, ent3)

                'eC.resto = ent1
                'eC.divisor = Mult(eA.divisor, eB.divisor)
            End If
            eC.Reduce()
        Catch ex As Exception
            Throw
        End Try
        Return eC
    End Function
    Public Shared Operator =(eA As Expression, eB As Expression) As Boolean
        Try
            If eA.IsComplex AndAlso eB.IsComplex Then
                If (eA.ToComplex - eB.ToComplex).IsZero Then
                    Return True
                End If
                Return False
            End If
            If eA.t.Count <> eB.t.Count Then Exit Try
            eA.SortExprTerms()
            eB.SortExprTerms()
            For i As Int32 = 0 To eA.t.Count - 1
                If eA.t(i) <> eB.t(i) Then Exit Try
            Next
            Return True
        Catch ex As Exception

        End Try
        Return False
    End Operator
    Public Shared Operator <>(eA As Expression, eB As Expression) As Boolean
        Return Not (eA = eB)
    End Operator
    Public Shared Operator /(eA As Expression, eB As Expression) As Expression
        Dim eC As Expression
        Try
            If eA.IsComplex AndAlso eB.IsComplex Then
                eC = New Expression(eA.ToComplex / eB.ToComplex)
                Exit Try
            ElseIf eA.IsPolynomial AndAlso eB.IsPolynomial Then
                eC = New Expression(eA.ToPolynomial / eB.ToPolynomial)
                Exit Try
            End If
            eA.Reduce()
            eB.Reduce()
            If eA.resto Is Nothing AndAlso eB.resto Is Nothing Then
                eC = New Expression(0.0)
                eC.resto = New List(Of ExprTerm)
                eC.divisor = New List(Of ExprTerm)
                For i As Int32 = 0 To eA.t.Count - 1
                    eC.resto.Add(New ExprTerm(eA.t(i)))
                Next
                For i As Int32 = 0 To eB.t.Count - 1
                    eC.divisor.Add(New ExprTerm(eB.t(i)))
                Next
                'eC.divisor.AddRange(eB.t)
            ElseIf eB.resto Is Nothing Then
                ' ( eA+ra/da ) / eB =
                ' = (da*eA + ra) /( da*eb)
                If eA.divisor.Count = 0 Then
                    eA.divisor.Add(New ExprTerm(1.0))
                End If
                Dim da As New List(Of ExprTerm)
                For i As Int32 = 0 To eA.divisor.Count - 1
                    da.Add(New ExprTerm(eA.divisor(i)))
                Next
                Dim eaT As New List(Of ExprTerm)
                For i As Int32 = 0 To eA.t.Count - 1
                    eaT.Add(New ExprTerm(eA.t(i)))
                Next
                Dim ra As New List(Of ExprTerm)
                For i As Int32 = 0 To eA.resto.Count - 1
                    ra.Add(New ExprTerm(eA.resto(i)))
                Next

                eaT = Mult(eaT, da)
                eaT = Add(eaT, ra)

                Dim eBT As New List(Of ExprTerm)
                For i = 0 To eB.t.Count - 1
                    eBT.Add(New ExprTerm(eB.t(i)))
                Next
                eBT = Mult(eBT, da)
                eC = New Expression(0.0)
                eC.resto = eaT
                eC.divisor = eBT

            ElseIf eA.resto Is Nothing Then
                ' eA / (eB+rb/db) =
                ' db*eA / ( db*eB + rb )
                If eB.divisor.Count = 0 Then
                    eB.divisor.Add(New ExprTerm(1.0))
                End If
                Dim dB As New List(Of ExprTerm)
                For i As Int32 = 0 To eB.divisor.Count - 1
                    dB.Add(New ExprTerm(eB.divisor(i)))
                Next
                Dim eD As New List(Of ExprTerm) '= New Expression(eA)
                For i As Int32 = 0 To eA.t.Count - 1
                    eD.Add(New ExprTerm(eA.t(i)))
                Next
                eD = Mult(eD, dB)

                Dim eE As New List(Of ExprTerm) ' New Expression(eB)
                For i As Int32 = 0 To eB.t.Count - 1
                    eE.Add(New ExprTerm(eB.t(i)))
                Next
                eE = Mult(eE, dB)

                eE = Add(eE, eB.resto)

                eC = New Expression(0.0)
                eC.resto = eD
                eC.divisor = eE
            Else
                ' (eA + ra/da)/(eB + rb/db) =
                ' = (da*eA + ra)/(da*eb + da*rb/db) =
                ' = (da*db*eA + db*ra)/(da*db*eb + da*rb)
                Dim da As New List(Of ExprTerm) ' = eA.divisor
                If eA.divisor.Count = 0 Then
                    eA.divisor.Add(New ExprTerm(1.0))
                End If
                If eB.divisor.Count = 0 Then
                    eB.divisor.Add(New ExprTerm(1.0))
                End If
                For i As Int32 = 0 To eA.divisor.Count - 1
                    da.Add(New ExprTerm(eA.divisor(i)))
                Next
                Dim db As New List(Of ExprTerm) '  = eB.divisor
                For i As Int32 = 0 To eB.divisor.Count - 1
                    db.Add(New ExprTerm(eB.divisor(i)))
                Next
                Dim dadb As List(Of ExprTerm) = Mult(da, db)

                Dim ra As New List(Of ExprTerm)
                For i As Int32 = 0 To eA.resto.Count - 1
                    ra.Add(New ExprTerm(eA.resto(i)))
                Next
                Dim rb As New List(Of ExprTerm)
                For i As Int32 = 0 To eB.resto.Count - 1
                    rb.Add(New ExprTerm(eB.resto(i)))
                Next

                Dim t1 As List(Of ExprTerm) = Mult(dadb, eA.t)
                Dim t2 As List(Of ExprTerm) = Mult(db, ra)

                Dim t3 As List(Of ExprTerm) = Mult(dadb, eB.t)
                Dim t4 As List(Of ExprTerm) = Mult(da, rb)

                eC = New Expression(0.0)
                eC.resto = Add(t1, t2)
                eC.divisor = Add(t3, t4)

            End If
            Dim bR As Boolean = Expression.bReduce
            Expression.bReduce = True
            eC.Reduce()
            Expression.bReduce = bR
        Catch ex As Exception
            Throw
        End Try
        Return eC
    End Operator
    Public Shared Operator ^(eA As Expression, eB As Expression) As Expression
        Dim eC As Expression
        Try
            Dim eA1 As New Expression(eA)
            Dim eB1 As New Expression(eB)
            'eA1.ReplaceConstants()
            'eB1.ReplaceConstants()
            If eB1.IsComplex Then
                If eA1.IsComplex Then
                    eC = New Expression(eA1.ToComplex ^ eB1.ToComplex)
                    Exit Try
                ElseIf eA1.IsPolynomial AndAlso eB1.IsDouble AndAlso eB1.ToDouble = Math.Floor(eB1.ToDouble) Then
                    eC = New Expression(eA1.ToPolynomial ^ eB1.ToPolynomial)
                    Exit Try
                ElseIf eB1.IsDouble Then
                    Dim db As Double = eB1.ToDouble
                    Dim abs As Double = Math.Abs(db)
                    If db = Math.Floor(db) Then
                        eC = Expression.CopyFrom(eA1)
                        For i = 2 To abs
                            eC *= eA
                        Next
                        If db < 0.0 Then
                            Dim eD As New Expression(eC)
                            eC = New Expression(0.0)
                            eC.resto = New List(Of ExprTerm)
                            eC.resto.Add(New ExprTerm(1.0))
                            eC.divisor = New List(Of ExprTerm)
                            eC.divisor.AddRange(eD.t)
                        End If
                        Exit Try
                    End If
                End If
            End If
            If eA.t.Count = 1 Then
                If eA.t(0).f.Count = 0 Then
                    eC = New Expression(eA.ToComplex.toString, eB)
                    Exit Try
                End If
                eC = New Expression(eA)
                For i = 0 To eC.t(0).f.Count - 1
                    eC.t(0).f(i).exp *= eB
                Next
                Exit Try
            End If
            Dim f As New ExprFactor(eA)
            f.exp = New Expression(eB)
            Dim t As New ExprTerm(f)
            eC = New Expression(t)
        Catch ex As Exception
            Throw
        End Try
        eC.Reduce()
        If G10.detail Then
            G10.sDetail += eC.ToString() + vbCrLf
        End If
        Return eC
    End Operator
    Public Function GetVarsNumerator()
        Dim vars As New List(Of String)
        Try
            vars.AddRange(GetVarsOfListOfExprTerm(resto))
        Catch ex As Exception
            Throw
        End Try
        Return vars.ToArray()
    End Function
    Public Function GetVarsDenominator()
        Dim vars As New List(Of String)
        Try
            vars.AddRange(GetVarsOfListOfExprTerm(divisor))
        Catch ex As Exception
            Throw
        End Try
        Return vars.ToArray()
    End Function

    Public Sub Reduce()
        Try
            If Not bReduce Then Exit Try
            If IsPolynomial() Then
                ToPolynomial.Reduce()
                Exit Try
            End If
            If resto IsNot Nothing Then
                Dim IsZero As Boolean = True
                Dim i As Int32
                For i = resto.Count - 1 To 0 Step -1
                    If resto(i).Cf.IsZero Then
                        resto.RemoveAt(i)
                    End If
                Next
                If resto.Count = 0 Then
                    resto = Nothing
                    divisor = Nothing
                End If
            End If
            If resto IsNot Nothing Then
                For i As Int32 = resto.Count - 1 To 0 Step -1
                    If resto(i).IsDouble AndAlso resto(i).ToDouble = 0.0 Then
                        resto.RemoveAt(i)
                    End If
                Next
                If resto.Count = 0 Then
                    resto = Nothing
                    divisor = Nothing
                End If
            End If
            If resto IsNot Nothing Then
                'ReplaceConstants()
                Dim nVars() As String = GetVarsNumerator()
                Dim dVars() As String = GetVarsDenominator()
                Dim sVar As String = ""
                If IsPolynomial(resto) AndAlso IsPolynomial(divisor) Then
                    Dim pResto As Polynomial = Expression.ToPolynomial(resto)
                    Dim pDiv As Polynomial = Expression.ToPolynomial(divisor)
                    Dim dif As Polynomial = pResto - pDiv
                    Dim expr As New Expression(Me)
                    If dif.IsRational AndAlso dif.ToDouble = 0.0 Then
                        expr.resto = Nothing
                        expr.divisor = Nothing
                        expr += New Expression(1.0)
                        expr.resto = Nothing
                        expr.divisor = Nothing
                        CopyExprToMe(expr)
                    ElseIf New Expression(Me.t).IsPolynomial Then
                        Dim pA As Polynomial = pResto / pDiv
                        expr.resto = Nothing
                        expr.divisor = Nothing
                        expr += New Expression(pA)
                        CopyExprToMe(expr)
                    End If
                End If

            End If
            Dim tc1 As New ExprTermComparer
            Array.Sort(t.ToArray, tc1)
            SortExprTerms()
            For i As Int32 = t.Count - 1 To 1 Step -1
                If i >= t.Count Then i = t.Count - 1
                For j As Int32 = i - 1 To 0 Step -1
                    If t(i) = t(j) Then
                        t(j).Cf += t(i).Cf
                        t.RemoveAt(i)
                        If t(j).IsDouble AndAlso t(j).Cf.IsZero Then
                            t.RemoveAt(j)
                        End If
                        Exit For
                    End If
                Next
            Next
            For i As Int32 = t.Count - 1 To 0 Step -1
                If t(i).Cf.IsDouble AndAlso t(i).ToDouble = 0 Then
                    t.RemoveAt(i)
                Else
                    For j As Int32 = t(i).f.Count - 1 To 0 Step -1
                        If t(i).f(j).exp.IsComplex AndAlso t(i).f(j).exp.IsZero Then
                            t(i).f.RemoveAt(j)
                        End If
                    Next
                End If
            Next
            If t.Count = 0 Then
                t.Add(New ExprTerm(0.0))
            End If
            If resto IsNot Nothing Then
                For i As Int32 = resto.Count - 1 To 0 Step -1
                    If resto(i).Cf.IsDouble AndAlso resto(i).ToDouble = 0 Then
                        resto.RemoveAt(i)
                    Else
                        For j As Int32 = resto(i).f.Count - 1 To 0 Step -1
                            If resto(i).f(j).exp.IsComplex AndAlso resto(i).f(j).exp.IsZero Then
                                resto(i).f.RemoveAt(j)
                            End If
                        Next
                    End If
                Next
                If resto.Count = 0 Then
                    resto = Nothing
                    divisor = Nothing
                Else
                    For i As Int32 = divisor.Count - 1 To 0 Step -1
                        If divisor(i).Cf.IsDouble AndAlso divisor(i).ToDouble = 0 Then
                            divisor.RemoveAt(i)
                        Else
                            For j As Int32 = divisor(i).f.Count - 1 To 0 Step -1
                                If divisor(i).f(j).exp.IsComplex AndAlso divisor(i).f(j).exp.IsZero Then
                                    divisor(i).f.RemoveAt(j)
                                End If
                            Next
                        End If
                    Next
                End If
            End If



        Catch ex As Exception
            Throw
        End Try
    End Sub
    Public Function EvalToComplex(value As Complex) As Complex
        Dim cplx As Complex = Complex.zero
        Try
            For i As Int32 = t.Count - 1 To 0 Step -1
                Dim ti As Complex = t(i).evalToComplex(value)
                cplx += ti
            Next
            If resto IsNot Nothing Then
                Dim n As New Expression(resto)
                Dim d As New Expression(divisor)
                Dim n1 As Complex = n.EvalToComplex(value)
                Dim d1 As Complex = d.EvalToComplex(value)
                cplx += n1 / d1
            End If
        Catch ex As Exception
            Throw
        End Try
        Return cplx
    End Function
    Public Function ConvertTrigToExp() As Expression
        Dim eC As New Expression(0.0)
        Try
            If t.Count = 0 Then eC = New Expression(Me)
            For i As Int32 = 0 To t.Count - 1
                Dim j As Int32 = 0
                If Not (t(i).f.Count = 1 AndAlso t(i).f(j).args.Count = 1 AndAlso t(i).f(j).exp.IsDouble) Then
                    eC.t.Add(t(i))
                Else
                    Dim f As New Expression(t(i).Cf)
                    For j = 0 To t(i).f.Count - 1
                        Dim exponent As Expression = t(i).f(j).exp
                        Dim one As New Expression(1.0)
                        If Not (t(i).f(j).var.GetType Is GetType(String)) Then
                            Continue For
                        End If
                        Select Case t(i).f(j).var
                            Case "sin"
                                Dim exprI As New Expression(Complex.i)
                                Dim expx As New Expression("exp", one, t(i).f(j).args(0) * exprI)
                                Dim expmnx As New Expression("exp", one, -t(i).f(j).args(0) * exprI)
                                Dim num As Expression = (expx - expmnx) ^ exponent
                                Dim den As Expression = (New Expression(2.0) * exprI) ^ exponent
                                num.Reduce() : den.Reduce()
                                Dim expr As Expression = num / den
                                f *= expr
                            Case "cos"
                                Dim exprI As New Expression(Complex.i)
                                Dim expx As New Expression("exp", one, t(i).f(j).args(0) * exprI)
                                Dim expmnx As New Expression("exp", one, -t(i).f(j).args(0) * exprI)
                                Dim num As Expression = (expx + expmnx) ^ exponent
                                Dim den As Expression = New Expression(2.0) ^ exponent
                                num.Reduce() : den.Reduce()
                                Dim expr As Expression = num / den
                                f *= expr
                            Case "tan" ' tan = sen / cos
                                Dim exprI As New Expression(Complex.i)
                                Dim expx As New Expression("exp", one, t(i).f(j).args(0) * exprI)
                                Dim expmnx As New Expression("exp", one, -t(i).f(j).args(0) * exprI)
                                Dim num As Expression = (-exprI) ^ exponent * (expx - expmnx) ^ exponent
                                Dim den As Expression = (expx + expmnx) ^ exponent
                                num.Reduce() : den.Reduce()
                                Dim expr As Expression = num / den
                                f *= expr
                            Case "sec" ' sec = 1/cos
                                Dim exprI As New Expression(Complex.i)
                                Dim expx As New Expression("exp", one, t(i).f(j).args(0) * exprI)
                                Dim expmnx As New Expression("exp", one, -t(i).f(j).args(0) * exprI)
                                Dim expr As Expression = New Expression(2.0) ^ exponent / (expx + expmnx) ^ exponent
                                f *= expr
                            Case "csc" ' csc = 1/ sen
                                Dim exprI As New Expression(Complex.i)
                                Dim expx As New Expression("exp", one, t(i).f(j).args(0) * exprI)
                                Dim expmnx As New Expression("exp", one, -t(i).f(j).args(0) * exprI)
                                Dim expr As Expression = (New Expression(2.0) * exprI) ^ exponent / (expx - expmnx) ^ exponent
                                f *= expr
                            Case "cot" ' cot = 1/tan
                                Dim exprI As New Expression(Complex.i)
                                Dim expx As New Expression("exp", one, t(i).f(j).args(0) * exprI)
                                Dim expmnx As New Expression("exp", one, -t(i).f(j).args(0) * exprI)
                                Dim expr As Expression = exprI * (expx + expmnx) ^ exponent / (expx - expmnx) ^ exponent
                                f *= expr
                            Case "sinh"
                                Dim expx As New Expression("exp", one, t(i).f(j).args(0))
                                Dim expmnx As New Expression("exp", one, -t(i).f(j).args(0))
                                Dim expr As Expression = (expx - expmnx) / New Expression(2.0)
                                f *= expr
                            Case "cosh"
                                Dim expx As New Expression("exp", one, t(i).f(j).args(0))
                                Dim expmnx As New Expression("exp", one, -t(i).f(j).args(0))
                                Dim expr As Expression = (expx + expmnx) / New Expression(2.0)
                                f *= expr
                            Case "tanh"
                                Dim expmn2x As New Expression("exp", one, New Expression(-2.0) * t(i).f(j).args(0))
                                Dim expr As Expression = (New Expression(1.0) - expmn2x) ^ exponent /
                                    (New Expression(1.0) + expmn2x) ^ exponent
                                f *= expr
                            Case Else
                                f *= New Expression(New ExprTerm(t(j).f(j)))
                        End Select
                    Next
                    eC += f
                End If
            Next
            eC.Reduce()
            'If eC.resto IsNot Nothing AndAlso eC.resto.Count Then
            '    Dim num As New Expression(eC.resto)
            '    Dim den As New Expression(eC.divisor)
            '    num = num.ConvertTrigToExp
            '    den = den.ConvertTrigToExp
            '    eC.resto = num.t
            '    eC.divisor = den.t
            'End If
        Catch ex As Exception
            Throw
        End Try
        Return eC
    End Function
    Sub ReplaceConstants()
        Try
            For Each t1 As ExprTerm In t
                For j As Int32 = t1.f.Count - 1 To 0 Step -1
                    Dim sVar As String = t1.f(j).var.ToString
                    Dim exp As Double
                    If t1.f(j).exp.IsDouble Then
                        exp = t1.f(j).exp.ToDouble
                        If sVar = "π" Then
                            t1.Cf *= New Complex(Math.PI ^ exp, 0)
                            t1.f.RemoveAt(j)
                        ElseIf sVar = "e" Then
                            t1.Cf *= New Complex(Math.E ^ exp, 0)
                            t1.f.RemoveAt(j)
                        ElseIf sVar = "φ" Then
                            t1.Cf *= New Complex(G10.phi ^ exp, 0)
                            t1.f.RemoveAt(j)
                        End If
                    End If
                Next
            Next
            If resto IsNot Nothing Then
                For Each t1 As ExprTerm In resto
                    For j As Int32 = t1.f.Count - 1 To 0 Step -1
                        Dim sVar As String = t1.f(j).var.ToString
                        Dim exp As Double
                        If t1.f(j).exp.IsDouble Then
                            exp = t1.f(j).exp.ToDouble
                            If sVar = "π" Then
                                t1.Cf *= New Complex(Math.PI ^ exp, 0)
                                t1.f.RemoveAt(j)
                            ElseIf sVar = "e" Then
                                t1.Cf *= New Complex(Math.E ^ exp, 0)
                                t1.f.RemoveAt(j)
                            ElseIf sVar = "φ" Then
                                t1.Cf *= New Complex(G10.phi ^ exp, 0)
                                t1.f.RemoveAt(j)
                            End If
                        End If
                    Next
                Next
                For Each t1 As ExprTerm In divisor
                    For j As Int32 = t1.f.Count - 1 To 0 Step -1
                        Dim sVar As String = t1.f(j).var.ToString
                        Dim exp As Double
                        If t1.f(j).exp.IsDouble Then
                            exp = t1.f(j).exp.ToDouble
                            If sVar = "π" Then
                                t1.Cf *= New Complex(Math.PI ^ exp, 0)
                                t1.f.RemoveAt(j)
                            ElseIf sVar = "e" Then
                                t1.Cf *= New Complex(Math.E ^ exp, 0)
                                t1.f.RemoveAt(j)
                            ElseIf sVar = "φ" Then
                                t1.Cf *= New Complex(G10.phi ^ exp, 0)
                                t1.f.RemoveAt(j)
                            End If
                        End If
                    Next
                Next
            End If
        Catch ex As Exception
            Throw
        End Try
    End Sub
    Public Overrides Function ToString() As String
        Return ToStringExpression(G10.nDec, G10.sImg, G10.CI)
    End Function
    Public Overloads Function ToStringExpression(numDecimals As Int32, sImg As String,
                                                cultureInfo As Globalization.CultureInfo) As String
        If G10.mathml Then
            'Dim s1 = "<math xmlns=""http://www.w3.org/1998/Math/MathML"">"
            'Return s1 + ToStringExpression2(numDecimals, sImg, cultureInfo) + "</math>"
            Return ToStringExpression2(numDecimals, sImg, cultureInfo)
        End If
        Return ToStringExpression2(numDecimals, sImg, cultureInfo)
    End Function
    Public Shared Function ToStringListOrExprTerms(lst As List(Of ExprTerm)) As String
        Dim sb As New StringBuilder
        Try
            Dim tc As New ExprTermComparer
            Dim v() As ExprTerm = lst.ToArray
            Array.Sort(v, tc)
            Dim i As Int32
            Dim sIndep As String = ""
            For i = 0 To v.Count - 2
                If v(i).IsDouble Then
                    sIndep = v(i).ToString()
                Else
                    Dim sv As String = v(i).ToString
                    If Len(sv) Then
                        sb.Append(sv + "+")
                    End If
                End If
            Next
            If v.Count Then
                sb.Append(v(i).ToString())
            End If
            If sb.Length AndAlso sb.Chars(sb.Length - 1) = "+" Then
                sb = sb.Remove(sb.Length - 1, 1)
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
    Public Function ToStringExpression2(numDecimals As Int32, sImg As String,
                                                cultureInfo As Globalization.CultureInfo) As String
        Dim sb As New StringBuilder
        Try
            SortExprTerms()
            Reduce()
            If IsPolynomial() Then
                Return ToPolynomial.ToStringPolynomial(numDecimals, sImg, cultureInfo)
            End If
            If 0 > numDecimals OrElse numDecimals > 15 Then numDecimals = 15
            If sImg Is Nothing OrElse sImg = "" Then sImg = "i"
            If cultureInfo Is Nothing Then cultureInfo = G10.CI

            Dim sT As String = ToStringListOrExprTerms(t)
            Dim sResto As String = ""
            Dim sDiv As String = ""
            If resto IsNot Nothing AndAlso resto.Count _
            AndAlso Not (resto(0).IsDouble AndAlso resto(0).ToDouble = 0) Then
                sResto = ToStringListOrExprTerms(resto)
                sDiv = ToStringListOrExprTerms(divisor)
                Dim sDiv2 As String = Regex.Replace(sDiv, "&h|&o| ", "")
                If Len(sResto) > 1 AndAlso Regex.IsMatch(Mid(sResto, 2), "\-|\+") AndAlso
                sDiv2 <> "1" AndAlso sDiv2 <> "-1" Then
                    sResto = "(" + sResto + ")"
                End If
                If sDiv2 = "1" Then
                    sDiv = ""
                ElseIf sDiv2 = "-1" Then
                    sResto = "-" + sResto
                    sDiv = ""
                Else
                    If Regex.IsMatch(Mid(sDiv, 2), "\-|\+|\*") AndAlso Not Regex.IsMatch(sDiv, "\(.*\)\^\(.*\)") Then ' avoid extra parentheses as in m/((a+b)^(c/d)^e)
                        sDiv = RemoveStartEndParentheses(sDiv)
                        sDiv = "(" + sDiv + ")"
                    End If
                    sDiv = "/" + sDiv
                End If
                If sT = "0" Then
                    sT = ""
                Else
                    sT += "+"
                End If
            End If
            sb.Append(sT + sResto + sDiv)
        Catch ex As Exception
            Throw
        End Try
        Dim e1 As String = sb.ToString
        Try
            e1 = e1.Replace("++", "+")
            e1 = Regex.Replace(e1, "\+\s*\-", "-")
            e1 = Regex.Replace(e1, "<mo>\+</mo><mo>\-</mo>", "<mo>-</mo>")
            If Left(e1, 3) = "(+)" Then
                e1 = ""
            Else
                e1 = Regex.Replace(e1, "\(\+\)/\([^)]+\)", "")
                e1 = Regex.Replace(e1, "^0?\+/.*", "")
                e1 = Regex.Replace(e1, "^0/.*", "")
            End If
            e1 = e1.Replace("++", "+")
            If Right(e1, 1) = "+" Then
                e1 = Left(e1, Len(e1) - 1)
            End If
            If e1 = "" Then e1 = "0"
        Catch ex As Exception

        End Try
        Return e1
    End Function
    Public Shared Function RemoveStartEndParentheses(s As String) As String
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

    Public Class ExprTermComparer
        Implements IComparer
        Public Function Compare(x1 As Object, y1 As Object) As Integer Implements IComparer.Compare
            Dim x As ExprTerm = CType(x1, ExprTerm)
            Dim y As ExprTerm = CType(y1, ExprTerm)
            Dim sb1 As New StringBuilder
            Dim sb2 As New StringBuilder
            Try
                Dim i As Int32
                For i = 0 To x.f.Count - 1
                    Dim s As String = x.f(i).var.ToString
                    sb1.Append(s)
                    sb1.Append(ChrW(255) + x.f(i).exp.ToString)
                    If x.f(i).args IsNot Nothing Then
                        sb1.Append(x.f(i).args.ToString)
                    End If
                Next
                If sb1.Length = 0 Then
                    sb1.Append(ChrW(255))
                Else
                    sb1.Append("}")
                End If
                For i = 0 To y.f.Count - 1
                    Dim s As String = y.f(i).var.ToString
                    sb2.Append(s)
                    sb2.Append(" " + y.f(i).exp.ToString)
                    If y.f(i).args IsNot Nothing Then
                        sb1.Append(y.f(i).args.ToString)
                    End If
                Next
                If sb2.Length = 0 Then
                    sb2.Append(" ")
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

End Class
Public Class ExprTerm
    Public f As New List(Of ExprFactor)
    Public _cf As Complex
    Friend detail As Boolean
    Friend sDetail As String
    'Public Sub New()
    'End Sub
    Public Sub New(dbl As Double)
        Cf = New Complex(dbl)
    End Sub
    Public Sub New(cplx As Complex)
        Cf = New Complex(cplx)
    End Sub
    Public Sub New(t As ExprTerm)
        Cf = New Complex(t.Cf)
        For i = 0 To t.f.Count - 1
            f.Add(ExprFactor.CopyFrom(t.f(i)))
        Next
    End Sub
    Public Sub New(t As Term)
        Cf = New Complex(t.cf)
        Dim iMod As Int32 = G10.mMod
        G10.mMod = 0
        For i = 0 To t.f.Count - 1
            Dim fact As New ExprFactor(t.f(i).var, t.f(i).exp)
            f.Add(fact)
        Next
        G10.mMod = iMod
    End Sub
    Public Sub New(f As ExprFactor)
        Cf = Complex.one
        Me.f.Add(f)
    End Sub
    Public Sub New(var As String, exp As Int32)
        Cf = Complex.one
        Dim iMod As Int32 = G10.mMod
        G10.mMod = 0
        f.Add(New ExprFactor(var, exp))
        G10.mMod = iMod
    End Sub
    Public Sub New(var As String, exp As Expression)
        Cf = Complex.one
        Dim iMod As Int32 = G10.mMod
        G10.mMod = 0
        f.Add(New ExprFactor(var, exp))
        G10.mMod = iMod
    End Sub
    Public Sub New(var As String, exp As Expression, arg As Expression)
        Cf = Complex.one
        f.Add(New ExprFactor(var, exp, arg))
    End Sub
    Public Sub New(var As String, exp As Expression, vArg() As Expression)
        Cf = Complex.one
        f.Add(New ExprFactor(var, exp, vArg))
    End Sub
    Public Property Cf As Complex
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

    Public Shared Function CopyFrom(tA As ExprTerm) As ExprTerm
        Dim tC As New ExprTerm(tA)
        For i As Int32 = 0 To tA.f.Count - 1
            tC.f.Add(ExprFactor.CopyFrom(tA.f(i)))
        Next
        Return tC
    End Function

    Public Function IsDouble() As Boolean
        If (f.Count = 0 AndAlso Cf.IsDouble) Then Return True
        If Cf.IsZero Then Return True
        Return False
    End Function
    Public Function IsPolynomial() As Boolean
        For i = 0 To f.Count - 1
            If Not f(i).IsExponentIntegerAndNoArg Then Return False
        Next
        Return True
    End Function
    Public Function ToDouble() As Double
        Return Cf.ToDouble
    End Function
    Public Function Iscomplex() As Boolean
        Return (f.Count = 0)
    End Function
    Public Function ToComplex() As Complex
        Return Cf
    End Function
    Public Sub OpChgSign()
        Cf = New Complex(-Cf)
    End Sub
    Public Function GetVars() As String()
        Dim vars As New List(Of String)
        Dim bReduceB As Boolean = Expression.bReduce
        Try
            Expression.bReduce = False
            For i As Int32 = 0 To Me.f.Count - 1
                If f(i).var Is Nothing Then
                    Continue For
                End If
                If f(i).args IsNot Nothing AndAlso f(i).args.Count Then
                    For k As Int32 = 0 To f(i).args.Count - 1
                        Dim vvar() As String = f(i).args(k).GetVars
                        For j As Int32 = 0 To vvar.Length - 1
                            If Not vars.Contains(vvar(j)) Then
                                vars.Add(vvar(j))
                            End If
                        Next
                    Next
                End If
                If f(i).var.GetType Is GetType(Expression) Then
                    Dim vvar() As String = CType(f(i).var, Expression).GetVars
                    For j As Int32 = 0 To vvar.Length - 1
                        If Not vars.Contains(vvar(j)) Then
                            vars.Add(vvar(j))
                        End If
                    Next
                ElseIf Array.IndexOf(G10.vImFn, f(i).var) > -1 Then
                    vars.AddRange(f(i).args(0).GetVars)
                ElseIf f(i).var.GetType Is GetType(String) AndAlso
                Not IsNumeric(Replace(f(i).var, ",", ".")) AndAlso
                    f(i).var.Length Then
                    If Not vars.Contains(f(i).var) Then
                        vars.Add(f(i).var)
                    End If
                ElseIf f(i).var.ToString = "∞" Then
                    If Not vars.Contains("∞") Then
                        vars.Add("∞")
                    End If
                End If
                Dim vExp() As String = f(i).exp.GetVars
                If vExp.Length Then
                    For j As Int32 = 0 To vExp.Length - 1
                        If Not vars.Contains(vExp(j)) Then
                            vars.Add(vExp(j))
                        End If
                    Next
                End If
                If f(i).var.GetType Is GetType(Expression) Then
                    Dim vVarsE() As String = f(i).var.GetVars
                    For j = 0 To vVarsE.Length - 1
                        If Not vars.Contains(vVarsE(j)) Then
                            vars.Add(vVarsE(j))
                        End If
                    Next
                End If
            Next
            vars.Sort()
            For i = vars.Count - 1 To 1 Step -1
                If vars(i) = vars(i - 1) Then
                    vars.RemoveAt(i)
                End If
            Next
        Catch ex As Exception
            Throw
        Finally
            Expression.bReduce = bReduceB
        End Try
        Return vars.ToArray()
    End Function
    Public Shared Function Degree(t As ExprTerm) As Int32
        Dim d As Int32 = 0
        Try
            For i As Int32 = 0 To t.f.Count - 1
                d += ExprFactor.Degree(t.f(i))
                Exit For
            Next
        Catch ex As Exception
            Throw
        End Try
        Return d
    End Function
    Public Shared Operator =(tA As ExprTerm, tB As ExprTerm) As Boolean
        Try
            Dim tc As New Expression.ExprTermComparer
            If tA.f.Count <> tB.f.Count Then Exit Try
            For i As Int32 = 0 To tA.f.Count - 1
                If tA.f(i).args.Count <> tB.f(i).args.Count Then
                    Exit Try
                End If
                For j As Int32 = 0 To tA.f(i).args.Count - 1
                    If Not tA.f(i).args(j).ToString = tB.f(i).args(j).ToString Then
                        Exit Try
                    End If
                Next
                If tA.f(i).ToString <> tB.f(i).ToString Then Exit Try
            Next
            Return True
        Catch ex As Exception
            Throw
        End Try
        Return False
    End Operator
    Public Shared Operator <>(tA As ExprTerm, tB As ExprTerm) As Boolean
        Return Not (tA = tB)
    End Operator
    Public Shared Operator *(tA As ExprTerm, tB As ExprTerm) As ExprTerm
        Dim tC As New ExprTerm(tA)
        Dim tD As New ExprTerm(tB)
        Try
            tC.Cf *= tB.Cf
            For i As Int32 = tC.f.Count - 1 To 0 Step -1
                For j As Int32 = tD.f.Count - 1 To 0 Step -1
                    If tC.f(i).var.ToString = tD.f(j).var.ToString AndAlso tC.f(i).var.ToString <> "exp" Then
                        If tC.f(i).args.Count = tD.f(j).args.Count Then
                            Dim k As Int32
                            For k = 0 To tC.f(i).args.Count - 1
                                If tC.f(i).args(k).ToString <> tD.f(j).args(k).ToString Then
                                    Exit For
                                End If
                            Next
                            If k >= tC.f(i).args.Count Then
                                tC.f(i).exp += tD.f(j).exp
                                tD.f.RemoveAt(j)
                                If tC.f(i).exp.IsDouble AndAlso tC.f(i).exp.ToDouble = 0 Then
                                    tC.f.RemoveAt(i)
                                End If
                                Exit For
                            End If
                        End If
                    ElseIf tC.f(i).var.ToString = tD.f(j).var.ToString AndAlso tC.f(i).var = "exp" Then
                        ' exp(a) * exp(b) =exp(a+b)
                        tC.f(i).args(0) += tD.f(j).args(0)
                        tD.f.RemoveAt(j)
                        If tC.f(i).args(0).IsDouble AndAlso tC.f(i).args(0).ToDouble = 0 Then
                            tC.f(i) = New ExprFactor(1.0)
                        End If
                        Exit For
                    End If
                Next
            Next
            If tD.f.Count Then
                tC.f.AddRange(tD.f)
            End If
            If tC.f.Count = 1 AndAlso tC.f(0).args.Count = 0 Then
                If IsNumeric(tC.f(0).var) AndAlso tC.f(0).exp.IsDouble Then
                    Dim dbl As Double
                    If Double.TryParse(tC.f(0).var, dbl) Then
                        dbl = dbl ^ tC.f(0).exp.ToDouble
                        tC.Cf *= dbl
                        tC.f.RemoveAt(0)
                    End If
                End If

            End If
            If tA.detail AndAlso Not (tB.IsDouble AndAlso Math.Abs(tB.ToDouble) = 1) Then
                Dim s As String = ""
                s += "(" + tA.ToString() + ")*(" + tB.ToString + ")" + vbCrLf
                s += tC.ToString + vbCrLf
                tC.sDetail = tA.sDetail
            End If
        Catch ex As Exception
            Throw
        End Try
        Return tC
    End Operator
    Public Shared Operator /(tA As ExprTerm, tB As ExprTerm) As ExprTerm
        Dim tC As New ExprTerm(tA)
        Dim tD As New ExprTerm(tB)
        Try
            tC.Cf /= tB.Cf
            For i As Int32 = tC.f.Count - 1 To 0 Step -1
                For j As Int32 = tD.f.Count - 1 To 0 Step -1
                    If ExprFactor.IsEqual(tC.f(i).var, tD.f(j).var) Then
                        tC.f(i).exp -= tD.f(j).exp
                        tD.f.RemoveAt(j)
                        If tC.f(i).exp.IsDouble AndAlso tC.f(i).exp.ToDouble = 0 Then
                            tC.f.RemoveAt(i)
                        End If
                        Exit For
                    End If
                Next
            Next
            For i As Int32 = 0 To tD.f.Count - 1
                Dim f As New ExprFactor(tD.f(i).var.ToString, -tD.f(i).exp)
                For j = 0 To tD.f(i).args.Count - 1
                    f.args.Add(New Expression(tD.f(i).args(j)))
                Next
                tC.f.Add(f)
            Next
            If tA.detail Then
                Dim s As String = ""
                s += "(" + tA.ToString() + ")/(" + tB.ToString + ")" + vbCrLf
                s += tC.ToString + vbCrLf
                tC.sDetail = tA.sDetail
            End If
        Catch ex As Exception
            Throw
        End Try
        Return tC
    End Operator

    Public Function evalToComplex(value As Complex) As Complex
        Dim cplx As Complex = Cf
        Try
            For i As Int32 = 0 To f.Count - 1
                cplx *= f(i).evalToComplex(value)
            Next
        Catch ex As Exception
            Throw
        End Try
        Return cplx
    End Function
    Public Overrides Function ToString() As String
        Return ToString(G10.nDec, G10.sImg, G10.CI)
    End Function
    Public Overloads Function ToString(numDecimals As Int32, sImg As String, cultureInfo As Globalization.CultureInfo)
        Dim sb As New StringBuilder
        Try
            Dim sCf As String = Cf.ToStringComplex(numDecimals, sImg, cultureInfo)
            If sCf = "" OrElse sCf = "0" OrElse sCf = "-0" Then
                Return ""
            End If
            Dim i As Int32
            Dim fc As New ExprFactorComparer(True)
            Dim v() As ExprFactor = f.ToArray
            Array.Sort(v, fc)
            For i = 0 To v.Count - 1
                sb.Append(v(i).ToString(numDecimals, sImg, cultureInfo))
                If sb.Chars(0) = "/" Then
                    If G10.mathml Then
                        sb.Remove(0, 1)
                        sb.Insert(0, "<mfrac><mrow><mn>1</mn></mrow>")
                        sb.Append("</mfrac>")
                    Else
                        sb.Insert(0, "1")
                    End If
                End If
                If i < f.Count - 1 Then
                    If G10.mathml Then
                        sb.Append("<mo>*</mo>")
                    Else
                        sb.Append("*")
                    End If
                End If
            Next
            If sCf = "1" AndAlso sb.Length Then
            ElseIf G10.mathml AndAlso sCf = "<mn>1</mn>" AndAlso sb.Length Then
            ElseIf sCf = "-1" AndAlso sb.Length Then
                sb.Insert(0, "-")
            ElseIf G10.mathml AndAlso sCf = "<mn>-1</mn>" AndAlso sb.Length Then
                sb.Insert(0, "<mo>-</mo>")
            ElseIf sb.Length Then
                If Len(sCf) > 1 AndAlso Regex.IsMatch(Mid(sCf, 2), "[-+]") Then
                    If G10.mathml Then
                        sb.Insert(0, "<mo>(</mo>" + sCf + "<mo>)</mo><mo>*</mo>")
                    Else
                        sb.Insert(0, "(" + sCf + ")*")
                    End If
                Else
                    If G10.mathml Then
                        sb.Insert(0, sCf + "<mo>*</mo>")
                    Else
                        sb.Insert(0, sCf + "*")
                    End If
                End If
            Else
                sb.Append(sCf)
            End If
            Dim e1 As String = sb.ToString
            If Left(e1, 2) = "0*" OrElse Left(e1, 3) = "-0*" _
            OrElse Left(e1, 2) = "0/" OrElse Left(e1, 3) = "-0/" Then
                e1 = ""
            End If
            e1 = Regex.Replace(e1, "^<mo>1</mo><mo>\*</mo>", "")
            e1 = Regex.Replace(e1, "^<mo>\(</mo><mo>-1</mo><mo>\)</mo><mo>\*</mo>", "<mo>-</mo>")
            e1 = Regex.Replace(e1, "^<mo>\(</mo><mo>1</mo><mo>\)</mo><mo>\*</mo>", "")
            e1 = Regex.Replace(e1, "^<mo>\-</mo><mo>1</mo><mo>\*</mo>", "<mo>-</mo>")
            sb = New StringBuilder(e1)
        Catch ex As Exception
            Throw
        End Try
        If G10.mathml Then
            sb = New StringBuilder(Regex.Replace(sb.ToString, "<mo>\(</mo><mn>(?<num>[-\d]+)</mn><mo>\)</mo><mo>\*</mo>", "<mn>${num}*"))
            Return sb.ToString.Replace("<mo>*</mo><mo>/</mo>", "").Replace("<mo>*</mo><mn>1<mn>/", "")
        End If
        Return sb.ToString.Replace("*/", "/").Replace("*1/", "/")

    End Function
End Class

Public Class ExprFactor
    Public Class Variable
        Dim _var As String
        Dim _varExpr As Expression
        Public Property var
            Get
                If _varExpr IsNot Nothing Then
                    Return _varExpr
                End If
                Return _var
            End Get
            Set(value)
                If value.GetType Is GetType(String) Then
                    _var = CType(value, String)
                    _varExpr = Nothing
                ElseIf value.GetType Is GetType(Expression) Then
                    _var = ""
                    _varExpr = CType(value, Expression)
                Else
                    Throw New ArgumentException
                End If
            End Set
        End Property
        Public Overrides Function ToString() As String
            If _varExpr IsNot Nothing Then
                Return _varExpr.ToString
            End If
            Return _var
        End Function
    End Class
    Public var As Object
    Public exp As New Expression(1.0)
    Public args As New List(Of Expression)
    Public Sub New(variable As String)
        var = variable
    End Sub
    Public Sub New(variable As String, exponent As Int32)
        var = variable
        exp = New Expression(exponent)
    End Sub
    Public Sub New(variable As String, exponent As Expression, arg As Expression)
        var = variable
        Dim iMod As Int32 = G10.mMod
        G10.mMod = 0
        exp = New Expression(exponent)
        G10.mMod = iMod
        args.Add(New Expression(arg))
    End Sub
    Public Sub New(variable As String, exponent As Expression, vArg() As Expression)
        var = variable
        Dim iMod As Int32 = G10.mMod
        G10.mMod = 0
        exp = New Expression(exponent)
        G10.mMod = iMod
        For i As Int32 = 0 To vArg.Length - 1
            args.Add(New Expression(vArg(i)))
        Next
    End Sub
    Public Sub New(variable As String, expr As Expression)
        var = variable
        exp = New Expression(expr)
    End Sub
    Public Sub New(varExpr As Expression)
        var = New Expression(varExpr)
        exp = New Expression(1.0)
    End Sub
    Public Shared Function CopyFrom(fA As ExprFactor) As ExprFactor
        Dim fC As New ExprFactor("")
        fC.exp = New Expression(fA.exp)
        For i As Int32 = 0 To fA.args.Count - 1
            fC.args.Add(New Expression(fA.args(i)))
        Next
        fC.var = fA.var
        Return fC
    End Function
    Public Function IsExponentIntegerAndNoArg() As Boolean
        If var.GetType IsNot GetType(String) Then Return False
        Return (args.Count = 0 AndAlso exp.IsDouble AndAlso Math.Floor(exp.ToDouble) = exp.ToDouble)
    End Function
    Public Function IsComplex() As Boolean
        If var.GetType IsNot GetType(String) Then Return False
        If (var.Count + args.Count = 0) Then Return True
        Dim db As Double
        If Double.TryParse(var, db) AndAlso exp.IsComplex AndAlso args.Count = 0 Then
            Return True
        End If
        Return False
    End Function

    Public Shared Function Degree(f As ExprFactor) As Int32
        Return f.exp.ToDouble
    End Function
    Public Shared Function IsEqual(fA As Object, fB As Object)
        Dim r As Boolean = False
        Try
            Dim fAIsStr As Boolean = IIf(fA.GetType Is GetType(String), True, False)
            Dim fBIsStr As Boolean = IIf(fB.GetType Is GetType(String), True, False)
            If Not fAIsStr Then
                If Not fBIsStr Then
                    Exit Try
                Else
                    Return fA.var = fB.var
                End If
            End If
            If fAIsStr <> fBIsStr Then Exit Try
            If fAIsStr AndAlso fBIsStr Then
                r = (fA = fB)
                Exit Try
            End If
            If fA.args.Count <> fB.args.Count Then Exit Try
            If fA.var <> fB.var OrElse
                    fA.exp <> fB.exp Then
                Exit Try
            End If
            For i As Int32 = 0 To fA.args.Count - 1
                If fA.args(i).ToString <> fB.args(i).ToString Then Exit Try
            Next
            r = True
        Catch ex As Exception
            Throw
        End Try
        Return r
    End Function
    Public Shared Operator =(fA As ExprFactor, fB As String) As Boolean
        Return IsEqual(fA, fB)
    End Operator
    Public Shared Operator =(fA As String, fB As ExprFactor) As Boolean
        Return IsEqual(fA, fB)
    End Operator
    Public Shared Operator =(fA As ExprFactor, fB As ExprFactor) As Boolean
        Return IsEqual(fA, fB)
    End Operator
    Public Shared Operator <>(fA As ExprFactor, fB As ExprFactor) As Boolean
        Return Not IsEqual(fA, fB)
    End Operator
    Public Shared Operator <>(fA As ExprFactor, fB As String) As Boolean
        Return Not IsEqual(fA, fB)
    End Operator
    Public Shared Operator <>(fA As String, fB As ExprFactor) As Boolean
        Return Not IsEqual(fA, fB)
    End Operator
    Public Shared Operator ^(fA As ExprFactor, dbl As Double) As ExprFactor
        Dim fC As ExprFactor = ExprFactor.CopyFrom(fA)
        Try
            fC.exp ^= New Expression(dbl)
        Catch ex As Exception

        End Try
        Return fC
    End Operator
    Public Function evalToComplex(value As Complex) As Complex
        Dim cplx As Complex = Complex.zero
        Try

            Dim exponent As Complex
            If exp.IsComplex Then
                exponent = exp.ToComplex
            Else
                exponent = exp.EvalToComplex(value)
            End If
            If var.GetType IsNot GetType(String) Then
                cplx = var.EvalToComplex(cplx) ^ exponent
            ElseIf var = "exp" Then
                cplx = Me.args(0).EvalToComplex(value)
                cplx = ParseExpression.EvalFn(var, cplx) ^ exponent
            Else
                If Array.IndexOf(G10.vImFn, var) > -1 Then
                    If args.Count Then
                        cplx = Me.args(0).EvalToComplex(value)
                        cplx = ParseExpression.EvalFn(var, cplx) ^ exponent
                    Else
                        cplx = ParseExpression.EvalFn(var, value) ^ exponent
                    End If
                ElseIf IsNumeric(Replace(var, ",", ".")) Then
                    Dim db As Double = Double.Parse(var, G10.CI)
                    cplx = New Complex(db) ^ exponent
                Else
                    cplx = value ^ exponent
                End If
            End If
        Catch ex As Exception
            Throw
        End Try
        Return cplx
    End Function

    Public Overrides Function ToString() As String
        Return ToString(15, G10.sImg, New Globalization.CultureInfo("en-US"))
    End Function
    Public Overloads Function ToString(numDecimals As Int32, sImg As String, cultureInfo As Globalization.CultureInfo)
        Dim sb As New StringBuilder
        Try
            If var.GetType IsNot GetType(String) AndAlso exp IsNot Nothing AndAlso
            Not (exp.IsDouble AndAlso exp.IsZero) Then
                sb.Append(var.ToString)
            Else
                If G10.mathml Then
                    sb.Append("<mi>" + var + "</mi>")
                Else
                    sb.Append(var)
                End If
            End If
            If args.Count Then
                If G10.mathml Then
                    sb.Append("<mo>(</mo>")
                Else
                    sb.Append("(")
                End If
                For i As Int32 = 0 To args.Count - 1
                    sb.Append(args(i).ToStringExpression(numDecimals, sImg, cultureInfo))
                    If i < args.Count - 1 Then
                        sb.Append(",")
                    End If
                Next
                If G10.mathml Then
                    sb.Append("<mo>)</mo>")
                Else
                    sb.Append(")")
                End If
            End If
            If Not (exp.IsDouble AndAlso exp.ToDouble = 1) Then
                Dim e As Rational
                If Regex.IsMatch(sb.ToString, "[-+*/^]") Then
                    sb = New StringBuilder("(" + sb.ToString + ")")
                End If
                If exp.IsDouble Then
                    Dim db As Double = exp.ToDouble
                    e = New Rational(db)
                    If Regex.IsMatch(sb.ToString, "[-+]") Then
                        sb = New StringBuilder("(" + sb.ToString + ")")
                    End If
                    If db = -1 Then
                        If False AndAlso G10.mathml Then
                            sb = New StringBuilder("/<mrow>" + sb.ToString + "</mrow>")
                        Else
                            sb = New StringBuilder("/" + sb.ToString)
                        End If
                    Else
                        Dim se As String = e.toString
                        If False AndAlso G10.mathml Then
                            sb.Insert(0, "<msup>")
                            sb.Append("<mn>" + se + "</mn></msup>")
                        Else
                            If InStr(se, "/") Then se = "(" + se + ")"
                            sb.Append("^" + se)
                        End If
                    End If
                Else
                    Dim s As String = exp.ToStringExpression(numDecimals, sImg, cultureInfo)
                    If Regex.IsMatch(s, "[-+*/÷]") Then
                        If G10.mathml Then
                            sb.Insert(0, "<msup>")
                            sb.Append("<mi>" + s + "</mi></msup>")
                        Else
                            sb.Append("^(" + s + ")")
                        End If
                    Else
                        If G10.mathml Then
                            sb.Insert(0, "<msup>")
                            sb.Append(s + "</msup>")
                        Else
                            sb.Append("^" + s)
                        End If
                    End If
                End If
            End If
        Catch ex As Exception
            Throw
        End Try
        Return sb.ToString
    End Function
End Class
Public Class ExprFactorComparer
    Implements IComparer
    Dim _sortByExponents As Boolean
    Public Sub New(sortByExponents As Boolean)

    End Sub
    Public Function Compare(x1 As Object, y1 As Object) As Integer Implements IComparer.Compare
        Dim x As ExprFactor = CType(x1, ExprFactor)
        Dim y As ExprFactor = CType(y1, ExprFactor)
        Dim sb1 As New StringBuilder
        Dim sb2 As New StringBuilder
        Try
            If Not _sortByExponents Then
                sb1.Append(x.var)
            End If
            If x.exp.IsDouble Then
                sb1.Append(String.Format("{0:000000}", 10 ^ 5 - x.exp.ToDouble))
            Else
                sb1.Append(ChrW(255) + x.exp.ToString)
            End If
            If _sortByExponents Then
                sb1.Append(x.var)
            End If
            For i As Int32 = 0 To x.args.Count - 1
                If x.args(i).IsDouble Then
                    sb1.Append(String.Format("{0:000000}", 10 ^ 5 - x.args(i).ToDouble))
                Else
                    sb1.Append(ChrW(255) + x.args(i).ToString)
                End If
            Next
            If sb1.Length = 0 Then
                sb1.Append(ChrW(255))
            Else
                sb1.Append("}")
            End If
            If Not _sortByExponents Then
                sb2.Append(y.var)
            End If
            If y.exp.IsDouble Then
                sb2.Append(String.Format("{0:000000}", 10 ^ 5 - y.exp.ToDouble))
            Else
                sb2.Append(ChrW(255) + y.exp.ToString)
            End If
            If _sortByExponents Then
                sb2.Append(y.var)
            End If
            For i = 0 To y.args.Count - 1
                If y.args(i).IsDouble Then
                    sb2.Append(String.Format("{0:000000}", 10 ^ 5 - y.args(i).ToDouble))
                Else
                    sb2.Append(" " + y.args(i).ToString) ' 255
                End If
            Next
            If sb2.Length = 0 Then
                sb2.Append(" ") ' 255
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
