Imports System.Text.RegularExpressions
Imports System.Text

Public Class ReduceExprUsingPolynomials
    Public Shared Function TryToReduce(eA As Expression) As Expression
        Dim b As Boolean = G10.mathml
        G10.mathml = False
        Dim eC As New Expression(eA)
        Dim eD As New Expression
        Dim eB As New Expression(0.0)
        Dim IsEquation As Boolean = eB.IsEquation
        Try
            If Not Polynomial.bReduce Then
                eC = eA
                Exit Try
            End If
            Dim bIsPolynomial As Boolean = eA.IsPolynomial
            If eA.IsComplex OrElse bIsPolynomial Then
                eC = New Expression(eA)
                eC.Reduce()
                Exit Try
            End If
            Dim i, j As Int32
            For i = 0 To eA.t.Count - 1
                Dim e1 As New Expression(eA.t(i))
                Dim e2 As Expression = e1.ConvertTrigToExp
                eB += e2
            Next
            If Not eB.ToString = "0" AndAlso eB.IsPolynomial AndAlso eA.resto Is Nothing Then
                Dim pB As Polynomial = eB.ToPolynomial
                pB.Reduce()
                eC = New Expression(pB)
                Exit Try
            End If
            For i = 0 To eB.t.Count - 1
                Dim t As New ExprTerm(0.0)
                For j = 0 To eB.t(i).f.Count - 1
                    If eB.t(i).f(j).args.Count = 1 Then
                        t.f.Add(New ExprFactor(eB.t(i).f(j).var.ToString + "@" _
                          + "(" + eB.t(i).f(j).args(0).ToString + ")" +
                          eB.t(i).f(j).exp.ToString, 1))
                    Else
                        t.f.Add(eB.t(i).f(j))
                    End If
                Next
                If Not eB.t(i).Cf.IsZero Then
                    t.Cf = New Complex(eB.t(i).Cf)
                    eD.t.Add(t)
                End If
            Next
            If eD.t.Count = 0 Then eD.t.Add(New ExprTerm(0.0))
            If eB.resto IsNot Nothing Then
                Dim eF As New Expression
                eF.resto = New List(Of ExprTerm)
                eF.divisor = New List(Of ExprTerm)
                For i = 0 To eB.resto.Count - 1
                    Dim t As New ExprTerm(0.0)
                    For j = 0 To eB.resto(i).f.Count - 1
                        If eB.resto(i).f(j).args.Count = 1 Then
                            t.f.Add(New ExprFactor(eB.resto(i).f(j).var.ToString + "@" _
                          + "(" + eB.resto(i).f(j).args(0).ToString + ")" +
                          eB.resto(i).f(j).exp.ToString, 1))
                        Else
                            t.f.Add(eB.resto(i).f(j))
                        End If
                    Next
                    t.Cf = New Complex(eB.resto(i).Cf)
                    If Not t.Cf.IsZero Then
                        eF.resto.Add(t)
                    End If
                Next
                If eF.resto.Count = 0 Then eF.resto.Add(New ExprTerm(0.0))
                For i = 0 To eB.divisor.Count - 1
                    Dim t As New ExprTerm(0.0)
                    For j = 0 To eB.divisor(i).f.Count - 1
                        If eB.divisor(i).f(j).args.Count = 1 Then
                            t.f.Add(New ExprFactor(eB.divisor(i).f(j).var.ToString + "@" _
                          + "(" + eB.divisor(i).f(j).args(0).ToString + ")" +
                          eB.divisor(i).f(j).exp.ToString, 1))
                        Else
                            t.f.Add(eB.divisor(i).f(j))
                        End If
                    Next
                    t.Cf = New Complex(eB.divisor(i).Cf)
                    If Not t.Cf.IsZero Then
                        eF.divisor.Add(t)
                    End If
                Next
                ' s + b / c = (a*c + b)
                Dim AddToResto As List(Of ExprTerm) = Expression.Mult(eD.t, eF.divisor)
                eF.resto = Expression.Add(eF.resto, AddToResto)
                eD.t.Clear()
                eD.resto = eF.resto
                eD.divisor = eF.divisor
            End If
            If Not eD.IsPolynomial Then
                eC = New Expression(eB)
            Else
                Dim pC As Polynomial = eD.ToPolynomial
                pC.Reduce()
                eD = New Expression(pC)
                For i = 0 To eD.t.Count - 1
                    Dim t As New ExprTerm(0.0)
                    For j = 0 To eD.t(i).f.Count - 1
                        Dim vs() As String = Split(eD.t(i).f(j).var, "@")
                        If vs.Length = 1 Then
                            t.f.Add(eD.t(i).f(j))
                        ElseIf vs.Length = 2 Then
                            Dim eP As New ParseExpression
                            Polynomial.bReduce = False
                            Dim arg As Expression = eP.Evaluate(vs(1)).vExpr(0)
                            Polynomial.bReduce = True
                            t.f.Add(New ExprFactor(vs(0), eD.t(i).f(j).exp, arg))
                        Else
                            Throw New Exception(Msg8.num(13))
                        End If
                    Next
                    t.Cf = eD.t(i).Cf
                    eC.t.Add(t)
                    eC.Reduce()
                Next
                eD.Reduce()
            End If
        Catch ex As Exception
            Throw
        Finally
            eC.IsEquation = eA.IsEquation
        End Try
        Try
            If eC.resto IsNot Nothing AndAlso eC.divisor.Count = 1 AndAlso
            Not (eC.divisor.Count = 1 AndAlso eC.divisor(0).IsDouble AndAlso eC.divisor(0).ToDouble = 1.0) Then
                Dim ent As New Expression(eC.t)
                Dim eE As New Expression(eC.resto)
                Dim i As Int32
                For i = 0 To eE.t.Count - 1
                    Dim div As ExprTerm = eE.t(i) / eC.divisor(0)
                    If InStr(div.ToString, "/") Then Exit For
                    eE.t(i) = div
                Next
                If i >= eE.t.Count Then
                    eC = ent + eE
                End If
            End If
        Catch ex As Exception

        End Try
        Try
            G10.mathml = b
            If eA.ToString.Length <= eC.ToString.Length Then
                eC = eA
                If Not eC.IsPolynomial Then
                    Dim i, j As Int32
                    For i = eC.t.Count - 1 To 0 Step -1
                        Dim t As ExprTerm = eC.t(i)
                        For j = t.f.Count - 1 To 0 Step -1
                            If t.f(j).exp.IsComplex AndAlso t.f(j).exp.ToComplex.IsZero Then
                                t.f.RemoveAt(j)
                                If t.f.Count = 0 Then
                                    t = New ExprTerm(t.Cf)
                                End If
                                Exit For
                            End If
                        Next
                    Next
                End If
            End If
        Catch ex As Exception
            Throw
        Finally
            eC.IsEquation = eA.IsEquation
        End Try
        Return eC
    End Function
End Class
