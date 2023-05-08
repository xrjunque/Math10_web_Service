Public Class quickStart
    Inherits System.Web.UI.Page

    Dim nID As Int32

    Protected Sub Page_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
        Try

            C_SessionWeb.contador( _
                Page, _
                Request, _
                Response)
        Catch ex2 As Exception

        End Try
        Try
            nID = C_SessionWeb.getID(Page)
            C_SessionWeb.sProvieneDe(nID) = Request.Url.AbsoluteUri

        Catch ex As Exception

        End Try
    End Sub

End Class