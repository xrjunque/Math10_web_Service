Imports mates4.C_sessionWeb

Public Class _Default
    Inherits System.Web.UI.Page

    Dim sToday As String = C_sessionWeb.sCurrVersion
    Dim nID As Int32


    Private Sub _Default_PreRender(sender As Object, e As EventArgs) Handles Me.PreRender

        'Introducir aquí el código de usuario para inicializar la página

        Try
            If Not Page.IsPostBack Then
                C_sessionWeb.contador(
                    Page,
                    Request,
                    Response)
                'C_sessionWeb.initMenu(Page)
            End If
        Catch ex2 As Exception

        End Try
        Try
            nID = C_sessionWeb.getID(Page)
            C_sessionWeb.sProvieneDe(nID) = Request.Url.AbsoluteUri
            'Session.Clear()
            'Session.Add("tb1", "")
            'session.Add("tb3", "")
            'C_sessionWeb.path(C_sessionWeb.getID(Page)) = MapPath("/")
            Label1.ForeColor = Drawing.Color.White
            inicio("en")
            'Session.Abandon()
            'Response.Cookies.Add(New HttpCookie("ASP.NET_SessionId", ""))
        Catch ex As Exception
        End Try
    End Sub
    Sub logFile(ByVal ex As Exception, ByVal e1 As String, ByVal e2 As String)


        C_sessionWeb.logFile(ex, e1, e2, Page, C_sessionWeb.getID(Page), "polycalc.txt")

    End Sub
    Public Function getHostName(ByVal strIP As String) As String
        Dim myIP As System.Net.IPAddress = System.Net.IPAddress.Parse(strIP)
        Dim hostInfo As System.Net.IPHostEntry =
        System.Net.Dns.GetHostEntry(myIP)
        Return hostInfo.HostName
    End Function
    Sub inicio(ByVal sIdioma As String)

        ' ****************************************************************************************************
        Dim revisado As String = sToday  ' < ========================================   R E V I S A D O
        ' ****************************************************************************************************

        Dim bIsChristmas As Boolean = False

        'Select Case sIdioma
        If Not bIsChristmas Then
            Label1.Text = "Welcome"
        Else
            Label1.Text = "Merry Christmas"
        End If
        lblUpdated.Text = "(Last update: " + sToday + ")"
        'Label2.Text = "Free Online Polynomial Calculator."
        'Label3.Text = "last update: " + revisado
        Me.HyperLink3.Text = "Comments"
        Me.HyperLink4.Text = "Link to CodeProject Article"
        Me.HyperLink5.Text = "Links"
        'End Select

        Dim sQ As String = "?ln=" + sIdioma
        If True OrElse Request.UserHostAddress = "127.0.0.1" Then
            'Label1.ForeColor = Drawing.Color.White
            C_sessionWeb.local(C_sessionWeb.getID(Page)) = True
            'Me.HyperLink1.NavigateUrl = "polycalc.aspx" + sQ
            'Me.HyperLink2.NavigateUrl = "rootfinder.aspx" + sQ
            Me.HyperLink3.NavigateUrl = "commentfrm.aspx" + sQ
            Me.HyperLink4.NavigateUrl = "https://www.codeproject.com/Tips/5331794/MathML-to-from-Plain-Text-Converter"
            Me.HyperLink5.NavigateUrl = "links.aspx" + sQ
            'Me.HyperLink6.NavigateUrl = "gphframes.aspx" + sQ
            'Me.HyperLink6.NavigateUrl = "polycalc2.aspx" + sQ
        Else
            C_sessionWeb.local(C_sessionWeb.getID(Page)) = False
            'Me.HyperLink1.NavigateUrl = "precis/polycalc.aspx" + sQ
            'Me.HyperLink2.NavigateUrl = "precis/rootfinder.aspx" + sQ
            Me.HyperLink3.NavigateUrl = "precis/commentfrm.aspx" + sQ
            Me.HyperLink4.NavigateUrl = "https://www.codeproject.com/Tips/5331794/MathML-to-from-Plain-Text-Converter"
            Me.HyperLink5.NavigateUrl = "precis/links.aspx" + sQ
            'Me.HyperLink6.NavigateUrl = "precis/gphframes.aspx" + sQ
            'Me.HyperLink6.NavigateUrl = "precis/polycalc2.aspx" + sQ
        End If
        Me.HyperLink7.Text = "Mates8v8.4 versions Programming Overview Tutorial"
        Me.HyperLink7b.Text = "Mates8v8.4 Programming Overview Tutorial (Daniweb)"
        Me.HyperLink8.Text = "Check the time in every time zone"

        Me.HyperLink7.NavigateUrl = "tutorialv8_4.aspx" + sQ
        Me.HyperLink7b.NavigateUrl = "https://www.daniweb.com/software-development/vbnet/tutorials/475629/math-parser-and-evaluator-programming-overview-tutorial"
        Me.HyperLink8.NavigateUrl = "swdownload.aspx?#timezones"
        Me.HyperLink9.NavigateUrl = "ConvertAlg2RPN_RPL.aspx"
        Me.HyperLink10.NavigateUrl = "Mobile.aspx"
        Me.HyperLink11.NavigateUrl = "MathML_To_String.aspx"
        Me.HyperLink12.NavigateUrl = "WebServiceInfo.aspx"
    End Sub

End Class