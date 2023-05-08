Public Partial Class Links
    Inherits System.Web.UI.Page

    Private Sub Page_PreRender(sender As Object, e As EventArgs) Handles Me.PreRender
        Try
            C_sessionWeb.contador(Page, Request, Response)
        Catch ex As Exception

        End Try
    End Sub

End Class