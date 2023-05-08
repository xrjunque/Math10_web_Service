Public Class WebServiceInfo
    Inherits System.Web.UI.Page

    Protected Sub Page_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
        Literal1.Text = "There is a web service located at <a href='https://xrjunque.nom.es/Math10'>https://xrjunque.nom.es/Math10</a>"
        Literal1.Text += " This service calls all the new functionality of Math10 CAS calculator. "
        Literal1.Text += "Exceptions are integrals and diferential equations for which "
        Literal1.Text += "Mates8 is employed."
    End Sub

End Class