Imports System.Configuration
Imports System.Web.UI

Public Class Site1
    Inherits System.Web.UI.MasterPage

    Protected Sub Page_PreInit(sender As Object, e As EventArgs) Handles Me.Init
        Try
            'Session.Timeout = 1
            'Dim e1 = Request.QueryString.Item("desktop")
            'If e1 Is Nothing Then e1 = "" + ""
            'If Len(e1) = 0 AndAlso C_sessionWeb.BrowserIsMobile Then
            '    Response.Redirect("~/Mobile.aspx", True)
            'End If

            '----- DESCOMENTAR AQUI -------
            'VV                          VV
            'If C_sessionWeb.BrowserIsMobile Then
            '    Dim ses = Session("serviceSession")
            '    If ses Is Nothing Then
            '        ses = 1
            '        'Response.Redirect("~/Mobile.aspx", False)
            '    End If
            'End If

        Catch ex As Exception
            'Try
            '    System.Threading.Thread.Sleep(2000)
            '    Response.Redirect("~/Mobile.aspx", False)
            '    System.Threading.Thread.Sleep(1000)
            'Catch ex2 As Exception

            'End Try
        End Try
    End Sub


    Private Sub Site1_PreRender(sender As Object, e As EventArgs) Handles Me.PreRender
        Dim sScript As String = "<script language=javascript>" + vbCrLf +
               "function toggle(elementId) {" + vbCrLf +
                "    var ele = document.getElementById(elementId);" + vbCrLf +
               "     if (ele.style.display == 'block') {" + vbCrLf +
               "         ele.style.display = 'none';" + vbCrLf +
               "     }" + vbCrLf +
               "     else {" + vbCrLf +
               "         ele.style.display = 'block';" + vbCrLf +
               "     }" + vbCrLf +
               " }" + vbCrLf +
               "function img3() { this.src='/images/download3.bmp'; }" + vbCrLf +
               "</script>" + vbCrLf

        Page.ClientScript.RegisterStartupScript(Me.GetType(), "scriptID", sScript)

    End Sub
End Class

