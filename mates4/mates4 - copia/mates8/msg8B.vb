Public Class msg8B
    Inherits ApplicationException

    Dim msg As String = String.Empty
    Dim curr As currentMatch
    Public Const rtfInit As String = "{\rtf1\ansi\deff0 {\fonttbl {\f0 Calibri;}}" + vbCrLf + _
    "{\colortbl;\red0\green0\blue0;\red255\green0\blue0;\red204\green85\blue0;}" + vbCrLf

    Public Sub New(curr As currentMatch)
        MyBase.New 
        Me.curr = curr
    End Sub
    Public Sub New(curr As currentMatch, message As String)
        If curr.cfg.outputFormat = outputMsgFormat.RichTextFormat Then
            msg = rtfInit
        End If
        msg += message
        Me.curr = curr
    End Sub
    Public Sub New(curr As currentMatch, ByVal n As Int64, _
                   ParamArray Arr() As Object)
        Me.curr = curr
        msg = msgN(n, Arr)
    End Sub
    Public Sub New(curr As currentMatch, n As Int64)
        Me.curr = curr
        msg = msgN(n)
    End Sub
    Private Function msgN(n As Int64, Optional Arr() As String = Nothing) As String
        Dim e1 As String = String.Empty
        Try
            Select Case n
                Case 1 : e1 = "Empty expression."
                Case 2 : e1 = "Token sequence: [{0}{1}] is not valid."
                Case 3 : e1 = "End token ""{0}"" is not valid."
                Case 4 : e1 = "Start token ""{0}"" is not valid."
                Case 5 : e1 = "n/a, the expression is incomplete/unintelligible."
                Case 6 : e1 = "n/a, token ""{0}"" seems unknown, mislocated or unproperly employed."
                    'Case 7 : e1 = "Argument for  out of bounds."
                Case 8 : e1 = "n/a, stack is empty."
                Case 9 : e1 = "n/a, missing one or more matching left parenthesis."
                Case 10 : e1 = "n/a, missing one or more matching right parenthesis."
                Case 11 : e1 = "n/a, could not found variable ""{0}"" or its value."
                Case 12 : e1 = "n/a, couldn't find variable ""{0}""."
                Case 14 : e1 = "The expression/equation at ""{0}"" is incomplete."
                Case Else : e1 = "n/a"
            End Select
            Dim e2 As String = String.Empty
            Dim Arr2(Arr.Length - 1) As String
            If Arr IsNot Nothing Then
                If curr.cfg.outputFormat <> outputMsgFormat.plainText Then
                    ' If HTML, highlight the token(s)/parameter(s) in red:
                    For i As Int64 = 0 To Arr.Length - 1
                        Arr2(i) = Replace(Arr(i), vbCrLf, "|")
                        If curr.cfg.outputFormat = outputMsgFormat.RichTextFormat Then
                            e1 = Replace(e1, "{" + i.ToString + "}", _
                              "\cf2 {@}\cf1 ")
                            'e1 = Replace(e1, " [", "")
                            'e1 = Replace(e1, "] ", "")
                            Arr(i) = Replace(Arr(i), "\", "\\")
                        Else
                            e1 = Replace(e1, "{" + i.ToString + "}", _
                              "<span style=""color:red"">{@}</span>")
                        End If
                        e1 = Replace(e1, "@", i.ToString)
                    Next
                Else
                    For i As Int64 = 0 To Arr.Length - 1
                        Arr2(i) = Replace(Arr(i), vbCrLf, "|")
                        e1 = Replace(e1, "@", i.ToString)
                    Next
                End If
                'e1 = e1 + vbCrLf
                Dim desde As Int64 = curr.iCur ' curr.imc - 1
                Do While desde > 0 AndAlso _
                curr.vMc(desde).ToString = " "
                    desde -= 1
                Loop
                Dim hasta As Int64 = desde
                If Arr.Length > 1 Then
                    hasta += 1
                    Do While hasta < curr.vMc.Length AndAlso _
                    curr.vMc(hasta).ToString = " "
                        hasta += 1
                    Loop
                End If
                Dim sCur As String = curr.toStrCurMatch("", True, _
                       curr.cfg.outputFormat, desde, hasta)
                'sCur = Replace(sCur, " ", "")
                'sCur = Replace(sCur, vbCrLf, "|")
                'Dim sArr As String = Join(Arr, "")
                'Dim sArr2 As String = Join(Arr2, "")
                'Dim sFormatL As String
                'Dim sFormatR As String
                'If curr.cfg.outputFormat = outputMsgFormat.HTML Then
                '    sFormatL = "<span style=""color:red"">"
                '    sFormatR = "</span>"
                'ElseIf curr.cfg.outputFormat = outputMsgFormat.RichTextFormat Then
                '    sFormatL = "\cf2 "
                '    sFormatR = " \cf1 "
                'Else
                '    sFormatL = " [ "
                '    sFormatR = " ] "
                'End If

                'sCur = Replace(sCur, sArr2, sFormatL + Join(Arr2, "") + sFormatR, 1, 1)
                'sCur = Replace(sCur, sArr, sFormatL + Join(Arr2, "") + sFormatR, 1, 1)
                If InStr(Join(Arr, ""), "|") Then
                    If curr.cfg.outputFormat = outputMsgFormat.HTML Then
                        sCur += "<br />"
                    ElseIf curr.cfg.outputFormat = outputMsgFormat.RichTextFormat Then
                        sCur += vbCrLf
                    Else
                        sCur += vbCrLf
                    End If
                    sCur += "Here, pipe character | is equivalent to a Carriage Return character (CR)." + _
                        vbCrLf + "(Should you check 'Ignore CR'?)"
                End If
                e1 = String.Format(e1, Arr2)
                If curr.cfg.outputFormat = outputMsgFormat.HTML Then
                    e1 += "<br />" + sCur
                ElseIf curr.cfg.outputFormat = outputMsgFormat.RichTextFormat Then
                    e1 += vbCrLf + sCur
                Else
                    e1 += vbCrLf + sCur
                End If

                If curr.cfg.outputFormat = outputMsgFormat.RichTextFormat Then
                    e1 = Replace(e1, vbCrLf, "\line" + vbCrLf)
                End If
            End If
            If curr.cfg.outputFormat = outputMsgFormat.RichTextFormat Then
                e1 = rtfInit + e1
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return e1
    End Function
    Public ReadOnly Property num(ByVal i As Int64, ParamArray Arr() As Object) As Exception
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
    Public Sub setMessage(message As String)
        Me.msg = message
    End Sub
    Public Overrides ReadOnly Property Message As String
        Get
            Return msg
        End Get
    End Property
    Public Overrides Function ToString() As String
        Return msg
    End Function
End Class
