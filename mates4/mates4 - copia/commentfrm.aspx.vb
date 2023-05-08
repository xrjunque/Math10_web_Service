Imports System.IO
Imports myEmail = mates4.email

Partial Public Class commentfrm
    Inherits System.Web.UI.Page

    Dim nID As Int32
    Dim mx, mn As Int32
    Dim sOper As String

    Private Sub Page_PreRender(sender As Object, e As EventArgs) Handles Me.PreRender
        Try
            C_sessionWeb.contador(
                Page,
                Request,
                Response)
        Catch ex2 As Exception

        End Try
        Try
            nID = C_SessionWeb.getID(Page)
            If Not Page.IsPostBack Then
                loadRecaptcha()
            End If
        Catch ex As Exception
        End Try
    End Sub
    Private Sub btnReload_Click(sender As Object, e As EventArgs) Handles btnReloadCa.Click, btnReloadEn.Click, btnReloadEs.Click
        loadRecaptcha()
    End Sub

    Sub loadRecaptcha()
        Try
            If Page.IsPostBack Then
                Exit Sub
            End If
            Dim dblNt As Double = Now.Ticks Mod 10 ^ 6
            Dim rnd As New Random(Math.Floor(dblNt))
            Dim vA() As Int32 = {1, 3, 5, 7}
            Dim vB() As Int32 = {2, 4, 6, 8}
            Dim vOp() As String = {"-", "+", "*"}
            Dim oA As Int32 = vA(rnd.Next(0, vA.Length))
            Dim oB As Int32 = vB(rnd.Next(0, vB.Length))
            Dim sOp As String = vOp(rnd.Next(0, vOp.Length))
            Dim min As Int32 = Math.Min(oA, oB)
            Dim max As Int32 = Math.Max(oA, oB)
            mx = max : mn = min : sOper = sOp
            Dim R As Int32
            Select Case sOp
                Case "-" : R = max - min
                Case "+" : R = max + min
                Case "*" : R = max * min
            End Select
            'Session.Add("Recaptcha", R)
            C_sessionWeb.recaptcha(nID) = R
            tbRecaptcha.Text = R.ToString

            'Session.Add("max", max)
            'Session.Add("min", min)
            'Session.Add("sOp", sOp)
            lblEn.Text = String.Format("If you are a human (not a robot), please, type in the result of {0}{1}{2}:", max, sOp, min)
            lblEs.Text = String.Format("Si eres un humano (no un robot), por favor, escribe el resultado de {0}{1}{2}:", max, sOp, min)
            lblCa.Text = String.Format("Si sou un humà (no pas un robot), sis plau, escriviu el resultat de {0}{1}{2}:", max, sOp, min)
        Catch ex As Exception

        End Try
    End Sub
    Private Sub submit(ByVal sender As Object, ByVal e As System.EventArgs)
        Try
            Dim t As String = ""
            t += "First:   " + first.Text + vbCrLf
            t += "Initial: " + initial.Text + vbCrLf
            t += "Last:    " + lastname.Text + vbCrLf
            t += "Email:   " + email.Text + vbCrLf
            t += "Subjet:  " + subject.Text + vbCrLf
            t += "Comment: " + Comment.Text + vbCrLf
            C_sessionWeb.logFile(Nothing, t, "", Page, 0, "comments.txt")
        Catch ex As Exception
            C_sessionWeb.logFile(ex, "", "", Page, 0, "comments.txt")
        End Try
    End Sub
    Public Function getHostName(ByVal strIP As String) As String
        Dim myIP As System.Net.IPAddress = System.Net.IPAddress.Parse(strIP)
        Dim hostInfo As System.Net.IPHostEntry = _
        System.Net.Dns.GetHostEntry(myIP)
        Return hostInfo.HostName
    End Function

    Private Sub btnSubmitEn_Click(sender As Object, e As EventArgs) Handles btnSubmitEn.Click, btnSubmitEs.Click, btnSubmitCa.Click
        Try
            Dim i As Int32

            'Dim r As Int32 = Session("Recaptcha")
            Dim R As Int32
            Int32.TryParse(tbRecaptcha.Text, R)
            'mx = Session("max")
            'mn = Session("min")
            'sOper = Session("sOp")
            'Select Case sOper
            '    Case "-" : R = mx - mn
            '    Case "+" : R = mx - mn
            '    Case "*" : R = mx - mn
            'End Select
            If Int32.TryParse(tbEn.Text, i) Then
                If i = R Then
                    submit(Nothing, Nothing)
                    Response.Redirect("thanks.aspx")
                    Exit Try
                End If
            End If
            If Int32.TryParse(tbEs.Text, i) Then
                If i = R Then
                    submit(Nothing, Nothing)
                    Response.Redirect("thanks.aspx")
                    Exit Try
                End If
            End If
            If Int32.TryParse(tbCa.Text, i) Then
                If i = R Then
                    submit(Nothing, Nothing)
                    Response.Redirect("thanks.aspx")
                    Exit Try
                End If
            End If
            loadRecaptcha()
        Catch ex As Exception

        End Try
    End Sub
End Class