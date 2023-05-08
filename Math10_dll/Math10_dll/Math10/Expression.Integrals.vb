Partial Public Class Expression
    Public Function opInmediateIntegral(Optional respectVar As String = "") As Expression
        Dim ret As Expression = Nothing
        Try
            If IsPolynomial() Then
                ret = New Expression(ToPolynomial.opIntegral(respectVar))
                Exit Try
            End If
            Dim vVar() As String = GetVars()
            Dim sVar As String
            If respectVar <> "" Then
                sVar = respectVar
            End If
            Dim sFn As String = ""
            Dim exp As Expression = Nothing
            Dim arg As List(Of ExprTerm) = Nothing
            Me.Get_Fn_Exponent_Argument(sFn, exp, arg)
            If sFn = "" Then Exit Try
            If Not exp.IsDouble Then Exit Try
            Dim eArg As New Expression(arg)
            If Not eArg.IsPolynomial Then Exit Try
            Dim pArg As Polynomial = eArg.ToPolynomial
            Dim vVarArg() As String = pArg.GetVars

            Select Case sFn
                Case "sin"
            End Select
        Catch ex As Exception

        End Try
        Return ret
    End Function
End Class
