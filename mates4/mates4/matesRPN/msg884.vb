Public Class msg884
    Inherits ApplicationException

    Dim msg As String = String.Empty
    Dim eP As exprParser84
    Public Const rtfInit As String = "{\rtf1\ansi\deff0 {\fonttbl {\f0 Calibri;}}" + vbCrLf +
    "{\colortbl;\red0\green0\blue0;\red255\green0\blue0;\red204\green85\blue0;}" + vbCrLf

    Public Sub New(eP As exprParser84)
        Me.eP = eP
    End Sub
    Public Sub New(eP As exprParser84, message As String)
        If eP.cfg.outputFormat = outputMessage.RichTextFormat Then
            msg = rtfInit
        End If
        msg += message
        Me.eP = eP
    End Sub
    Public Sub New(eP As exprParser84, ByVal n As Int32,
                   ParamArray Arr() As Object)
        Me.eP = eP
        msg = msgN(n, Arr)
    End Sub
    Public Sub New(eP As exprParser84, n As Int32)
        Me.eP = eP
        msg = msgN(n)
    End Sub
    Private Function msgN(n As Int32, Optional Arr() As String = Nothing) As String
        Dim e1 As String = String.Empty
        Try
            Select Case n
                Case 1 : e1 = "Empty expression."
                Case 2 : e1 = "Token sequence: [{0}{1}] is not valid."
                Case 3 : e1 = "End token ""{0}"" is not valid."
                Case 4 : e1 = "Start token ""{0}"" is not valid."
                Case 5 : e1 = "n/a, the expression is incomplete/unintelligible."
                Case 6 : e1 = "n/a, token ""{0}"" is unknown, not allowed or mislocated."
                    'Case 7 : e1 = "Argument out of bounds."
                Case 8 : e1 = "n/a, stack is empty."
                Case 9 : e1 = "n/a, missing one or more matching left parenthesis."
                Case 10 : e1 = "n/a, missing one or more matching right parenthesis."
                Case 11 : e1 = "n/a, could not found variable ""{0}"" or its value."
                Case 12 : e1 = "n/a, couldn't find variable ""{0}""."
                Case Else : e1 = "n/a"
            End Select
            Dim e2 As String = String.Empty
            If Arr IsNot Nothing Then
                If eP.cfg.outputFormat <> outputMessage.plainText Then
                    ' If HTML, highlight the token(s)/parameter(s) in red:
                    For i As Int32 = 0 To Arr.Length - 1
                        If eP.cfg.outputFormat = outputMessage.RichTextFormat Then
                            e1 = Replace(e1, "{" + i.ToString + "}",
                              "\cf2 {@}\cf1 ")
                            'e1 = Replace(e1, " [", "")
                            'e1 = Replace(e1, "] ", "")
                            Arr(i) = Replace(Arr(i), "\", "\\")
                            e1 = Replace(e1, "@", i.ToString)
                        Else
                            e1 = Replace(e1, "{" + i.ToString + "}",
                              "<span style=""color:red"">{@}</span>")
                        End If
                        e1 = Replace(e1, "@", i.ToString)
                    Next
                End If
                e1 = e1 + vbCrLf
                Dim sL As String = ""
                Dim pos As Int32 = eP.iRe - IIf(Arr.Length, 1, 0)
                If pos > 0 Then sL = eP.sbExpr.ToString.Substring(0, pos)
                Dim sR As String = ""
                If pos + Join(Arr, "").Length < eP.sbExpr.Length Then
                    sR = eP.sbExpr.ToString.Substring(pos + Join(Arr, "").Length)
                End If

                If eP.cfg.outputFormat = outputMessage.RichTextFormat Then
                    e1 += sL + "\cf2 " + Join(Arr) + "\cf1 " + sR
                    e1 = Replace(e1, vbCrLf, "\line" + vbCrLf)
                Else
                    e1 += sL + "<span style=""color:red""> " + Join(Arr) + "</span> " + sR
                End If
                e1 = String.Format(e1, Arr)
            End If
            If eP.cfg.outputFormat = outputMessage.RichTextFormat Then
                e1 = rtfInit + e1
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return e1
    End Function
    Public ReadOnly Property num(ByVal i As Int32, ParamArray Arr() As Object) As Exception
        Get
            Try
                If i < 1 Then
                    msg = "n/a"
                ElseIf Arr IsNot Nothing AndAlso Arr.Length Then
                    Dim e1 As String = msgN(i)
                    msg = String.Format(e1, Arr)
                Else
                    msg = msgN(i)
                End If
            Catch ex As Exception
                Throw ex
            End Try
            Return Me
        End Get
    End Property
    Public Overrides ReadOnly Property Message As String
        Get
            Return msg
        End Get
    End Property
    Public Overrides Function ToString() As String
        Return msg
    End Function
End Class
