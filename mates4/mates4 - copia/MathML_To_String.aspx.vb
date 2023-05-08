Public Class MathML_To_String
    Inherits System.Web.UI.Page

    Dim nID As Int32
    Private Sub Page_PreRender(sender As Object, e As EventArgs) Handles Me.PreRender
        Try
            C_sessionWeb.contador(Page, Request, Response)
        Catch ex As Exception
        End Try
        Try
            nID = C_sessionWeb.getID(Page)
            C_sessionWeb.sProvieneDe(nID) = Request.Url.AbsoluteUri
        Catch ex As Exception
        End Try
        Try
            'Session.Abandon()
            'Response.Cookies.Add(New HttpCookie("ASP.NET_SessionId", ""))
        Catch ex As Exception

        End Try
    End Sub

End Class